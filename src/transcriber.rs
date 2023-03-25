use crate::{
    matcher::{MacroRepOp, MacroRepSep, PatternItems, RawMatch},
    token_fragment::TokenStringBuilder,
    utils::{to_close_str, to_open_str, RangeBuilder},
    ParseStreamEx, ResultStringBuilder, Source,
};
use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt};
use std::{ops::Range, str::FromStr};
use structmeta::{Parse, ToTokens};
use syn::{
    ext::IdentExt,
    parse::{Parse, ParseStream},
    parse_str,
    spanned::Spanned,
    token, Error, Result, Token,
};

/// Replacement pattern.
///
/// `Transcriber` corresponds to `MacroTranscriber` (excluding outermost brace) in [`Macros By Example`](https://doc.rust-lang.org/reference/macros-by-example.html).
///
/// A `Transcriber` created using [`FromStr::from_str`] preserves whitespace in the original string as much as possible.
#[derive(Debug)]
pub struct Transcriber {
    items: TranscriberItems,
    is_ready_string: bool,
}

impl Parse for Transcriber {
    fn parse(input: ParseStream) -> Result<Self> {
        Self::parse(&mut ParseStreamEx::new(input, 0))
    }
}

impl FromStr for Transcriber {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let source = Source::new(s, parse_str(s)?);
        let tokens = source.tokens.clone();
        let mut to = ParseStreamEx::parse_from_tokens(tokens, 0, Self::parse)?;
        to.items.ready_string(&source);
        to.is_ready_string = true;
        Ok(to)
    }
}

impl Transcriber {
    fn parse(input: &mut ParseStreamEx) -> Result<Self> {
        Ok(Self {
            items: TranscriberItems::parse(input)?,
            is_ready_string: false,
        })
    }
    pub(crate) fn attach(&mut self, p: &PatternItems) -> Result<()> {
        self.items.attach(p)
    }
    pub(crate) fn apply_tokens(&self, m: &RawMatch) -> TokenStream {
        self.items.apply_tokens(m)
    }
    pub(crate) fn apply_tokens_to(&self, m: &RawMatch, output: &mut TokenStream) {
        self.items.apply_tokens_to(m, output)
    }
    pub(crate) fn apply_string(&self, m: &RawMatch, b: &mut ResultStringBuilder) {
        self.items.apply_string(self.is_ready_string, m, b)
    }
}

#[derive(Debug)]
struct TranscriberItems {
    items: Vec<TranscriberItem>,
}

impl TranscriberItems {
    fn parse(input: &mut ParseStreamEx) -> Result<Self> {
        let mut items = Vec::new();
        let mut tokens = Vec::new();
        let mut tfs_range = RangeBuilder::new();
        while !input.is_empty() {
            if input.peek(token::Paren) || input.peek(token::Brace) || input.peek(token::Bracket) {
                push_tokens(&mut tokens, &mut items, &mut tfs_range);
                let g = input.parse_group(|g, input| {
                    Ok(TranscriberGroup {
                        delimiter: g.group.delimiter(),
                        content: Self::parse(input)?,
                        tfs_range_open: g.tfs_range_open,
                        tfs_range_close: g.tfs_range_close,
                        span: g.group.span(),
                    })
                })?;
                items.push(TranscriberItem::Group(g));
                continue;
            }
            if input.peek(Token![$]) {
                if input.peek2(Ident::peek_any) {
                    push_tokens(&mut tokens, &mut items, &mut tfs_range);
                    items.push(TranscriberItem::Var(input.parse()?));
                    continue;
                }
                if input.peek2(token::Paren) {
                    push_tokens(&mut tokens, &mut items, &mut tfs_range);
                    items.push(TranscriberItem::Rep(TranscriberRep::parse(input)?));
                    continue;
                }
            }
            let tfs_start = input.tfs_offset;
            let token: TokenTree = input.parse().unwrap();
            let tfs_end = input.tfs_offset;
            tfs_range.push(tfs_start..tfs_end);
            tokens.push(token);
        }
        push_tokens(&mut tokens, &mut items, &mut tfs_range);
        Ok(Self { items })
    }
    fn ready_string(&mut self, source: &Source) {
        self.ready_string_with(source, &mut RangeBuilder::new());
    }
    fn ready_string_with(&mut self, source: &Source, tfs_range: &mut RangeBuilder) {
        for item in &mut self.items {
            item.ready_string_with(source, tfs_range);
        }
    }
    fn attach(&mut self, p: &PatternItems) -> Result<()> {
        for i in &mut self.items {
            i.attach(p)?;
        }
        Ok(())
    }
    fn get_var(&self) -> Option<MacroTranscriberVar> {
        for i in &self.items {
            if let Some(b) = i.get_var() {
                return Some(b);
            }
        }
        None
    }
    fn apply_tokens(&self, m: &RawMatch) -> TokenStream {
        let mut output = TokenStream::new();
        self.apply_tokens_to(m, &mut output);
        output
    }
    fn apply_tokens_to(&self, m: &RawMatch, output: &mut TokenStream) {
        for item in &self.items {
            item.apply_tokens_to(m, output);
        }
    }
    fn apply_string(&self, is_ready_string: bool, m: &RawMatch, b: &mut ResultStringBuilder) {
        for item in &self.items {
            item.apply_string(is_ready_string, m, b)
        }
    }
}
fn push_tokens(
    tokens: &mut Vec<TokenTree>,
    items: &mut Vec<TranscriberItem>,
    tfs_range: &mut RangeBuilder,
) {
    if let Some(tfs_range) = tfs_range.take() {
        if !tokens.is_empty() {
            let tokens = TokenStream::from_iter(tokens.drain(..));
            items.push(TranscriberItem::Tokens(TranscriberTokens {
                tokens,
                tfs_range,
            }));
        }
    }
    items.push(TranscriberItem::String(String::new()));
}

#[derive(Debug)]
enum TranscriberItem {
    Tokens(TranscriberTokens),
    Group(TranscriberGroup),
    String(String),
    Var(TranscriberVar),
    Rep(TranscriberRep),
}
impl TranscriberItem {
    fn ready_string_with(&mut self, source: &Source, tfs_range: &mut RangeBuilder) {
        match self {
            Self::Tokens(t) => tfs_range.push(t.tfs_range.clone()),
            Self::Group(g) => g.ready_string_with(source, tfs_range),
            Self::String(ref mut s) => {
                if let Some(tfs_range) = tfs_range.take() {
                    let mut b = TokenStringBuilder::new(source);
                    b.push_tfs(tfs_range);
                    *s = b.s;
                }
            }
            Self::Var(_) => {}
            Self::Rep(r) => r.content.ready_string(source),
        }
    }
    fn attach(&mut self, p: &PatternItems) -> Result<()> {
        match self {
            Self::Tokens(_) | Self::String(_) => Ok(()),
            Self::Group(g) => g.content.attach(p),
            Self::Var(v) => v.attach(p),
            Self::Rep(r) => r.attach(p),
        }
    }
    fn get_var(&self) -> Option<MacroTranscriberVar> {
        match self {
            TranscriberItem::Tokens(_) => None,
            TranscriberItem::Group(g) => g.content.get_var(),
            TranscriberItem::String(_) => None,
            TranscriberItem::Var(v) => Some(v.var.clone()),
            TranscriberItem::Rep(r) => Some(r.var.clone()),
        }
    }
    fn apply_tokens_to(&self, m: &RawMatch, output: &mut TokenStream) {
        match self {
            TranscriberItem::Tokens(t) => t.tokens.to_tokens(output),
            TranscriberItem::String(_) => {}
            TranscriberItem::Group(g) => g.apply_tokens_to(m, output),
            TranscriberItem::Var(v) => v.apply_tokens_to(m, output),
            TranscriberItem::Rep(r) => r.apply_tokens_to(m, output),
        }
    }

    fn apply_string(&self, is_ready_string: bool, m: &RawMatch, b: &mut ResultStringBuilder) {
        match self {
            TranscriberItem::Tokens(tokens) => tokens.apply_string(is_ready_string, b),
            TranscriberItem::Group(g) => g.apply_string(is_ready_string, m, b),
            TranscriberItem::String(s) => b.b.push_str(s),
            TranscriberItem::Var(v) => v.apply_string(m, b),
            TranscriberItem::Rep(r) => r.apply_string(is_ready_string, m, b),
        }
    }
}

#[derive(Debug)]
struct TranscriberTokens {
    tokens: TokenStream,
    tfs_range: Range<usize>,
}
impl TranscriberTokens {
    fn apply_string(&self, is_ready_string: bool, b: &mut ResultStringBuilder) {
        if !is_ready_string {
            b.b.push_tokens(&self.tokens)
        }
    }
}

#[derive(Debug)]
struct TranscriberGroup {
    delimiter: Delimiter,
    content: TranscriberItems,
    span: Span,
    tfs_range_open: Range<usize>,
    tfs_range_close: Range<usize>,
}
impl TranscriberGroup {
    fn ready_string_with(&mut self, source: &Source, tfs_range: &mut RangeBuilder) {
        tfs_range.push(self.tfs_range_open.clone());
        self.content.ready_string_with(source, tfs_range);
        tfs_range.push(self.tfs_range_close.clone());
    }

    fn apply_tokens_to(&self, m: &RawMatch, output: &mut TokenStream) {
        let mut g = Group::new(self.delimiter, self.content.apply_tokens(m));
        g.set_span(self.span);
        output.append(g);
    }

    fn apply_string(&self, is_ready_string: bool, m: &RawMatch, b: &mut ResultStringBuilder) {
        if !is_ready_string {
            b.b.push_str(to_open_str(self.delimiter));
        }
        self.content.apply_string(is_ready_string, m, b);
        if !is_ready_string {
            b.b.push_str(to_close_str(self.delimiter));
        }
    }
}

#[derive(Parse, ToTokens, Debug, Clone)]
struct MacroTranscriberVar {
    dollar_token: Token![$],
    name: Ident,
}

#[derive(Debug)]
struct TranscriberVar {
    var: MacroTranscriberVar,
    var_index: usize,
}
impl Parse for TranscriberVar {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            var: input.parse()?,
            var_index: usize::MAX,
        })
    }
}

impl TranscriberVar {
    fn attach(&mut self, p: &PatternItems) -> Result<()> {
        let name = self.var.name.to_string();
        let span = self.var.span();
        if let Some(b) = p.vars.get(&name) {
            if b.depth != 0 {
                bail!(span, "variable '{name}' is still repeating at this depth",);
            }
            self.var_index = b.var_index_or_rep_index;
            Ok(())
        } else {
            bail!(span, "attempted to repeat an expression containing no syntax variables matched as repeating at this depth")
        }
    }
    fn apply_tokens_to(&self, m: &RawMatch, output: &mut TokenStream) {
        m.vars[self.var_index].tokens.to_tokens(output);
    }

    fn apply_string(&self, m: &RawMatch, b: &mut ResultStringBuilder) {
        b.b.push_tfs(m.vars[self.var_index].tfs_range.clone());
    }
}

#[derive(Debug)]
struct TranscriberRep {
    content: TranscriberItems,
    sep: MacroRepSep,
    op: MacroRepOp,
    span: Span,
    var: MacroTranscriberVar,
    rep_index: usize,
}
impl TranscriberRep {
    fn parse(input: &mut ParseStreamEx) -> Result<Self> {
        let _dollar_token: Token![$] = input.parse()?;
        input.expect(token::Paren)?;
        let content = input.parse_group(|_g, input| TranscriberItems::parse(input))?;
        let sep = input.parse()?;
        let op: MacroRepOp = input.parse()?;
        let span = _dollar_token.span();
        let span = span.join(op.span()).unwrap_or(span);
        let Some(var) = content.get_var() else {
            bail!(span, "attempted to repeat an expression containing no syntax variables");
        };
        Ok(Self {
            content,
            sep,
            op,
            span,
            var,
            rep_index: usize::MAX,
        })
    }
    fn attach(&mut self, p: &PatternItems) -> Result<()> {
        let var_name = self.var.name.to_string();
        if let Some(b) = p.vars.get(&var_name) {
            if b.depth > 0 {
                self.rep_index = b.var_index_or_rep_index;
                if let Some(r) = p.find_rep(&var_name) {
                    if self.op != r.op {
                        bail!(
                            self.span,
                            "mismatch repeat operator. expected {:?}, found {:?}",
                            r.op,
                            self.op
                        );
                    }
                    return self.content.attach(&r.content);
                }
            }
        }
        bail!(self.var.span(), "attempted to repeat an expression containing no syntax variables matched as repeating at this depth")
    }

    fn apply_tokens_to(&self, m: &RawMatch, output: &mut TokenStream) {
        let mut is_next = false;
        for m in &m.reps[self.rep_index].0 {
            if is_next {
                if let Some(sep) = &self.sep.0 {
                    sep.to_tokens(output);
                }
            }
            is_next = true;
            self.content.apply_tokens_to(m, output)
        }
    }

    fn apply_string(&self, is_ready_string: bool, m: &RawMatch, b: &mut ResultStringBuilder) {
        let mut is_next = false;
        for m in &m.reps[self.rep_index].0 {
            if is_next {
                if let Some(sep) = &self.sep.0 {
                    b.b.push_tokens(&sep.to_token_stream());
                }
            }
            is_next = true;
            self.content.apply_string(is_ready_string, m, b)
        }
    }
}
