use crate::{
    matcher::{
        MacroRepOp, MacroRepSep, MatchStringBuilder, MatchTokensBuilder, PatternItems, RawMatch,
    },
    token_entry::TokenStringBuilder,
    utils::{to_close_str, to_open_str, RangeBuilder},
    ParseStreamEx, Rule, Source,
};
use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt};
use std::{ops::Range, str::FromStr};
use structmeta::{Parse, ToTokens};
use syn::{
    ext::IdentExt,
    parse::{Parse, ParseStream},
    spanned::Spanned,
    token, Error, Result, Token,
};

/// Replacement pattern.
///
/// `Transcriber` corresponds to `MacroTranscriber` (excluding outermost brace) in [`Macros By Example`](https://doc.rust-lang.org/reference/macros-by-example.html).
///
/// A `Transcriber` created using [`FromStr::from_str`] preserves whitespace in the original string as much as possible.
#[derive(Debug, Clone)]
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
        let (source, input) = Source::from_str(s)?;
        let mut to = ParseStreamEx::parse_from_tokens(input, 0, Self::parse)?;
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
    pub(crate) fn apply_tokens_to(&self, m: &RawMatch, b: &mut MatchTokensBuilder) {
        self.items.apply_tokens_to(m, b)
    }
    pub(crate) fn apply_string(
        &self,
        m: &RawMatch,
        rule: &Rule,
        tes_len: usize,
        b: &mut TokenStringBuilder,
    ) {
        let mut b = MatchStringBuilder {
            b,
            rule,
            tes_len,
            is_ready_string: self.is_ready_string,
        };
        self.items.apply_string(m, &mut b)
    }
}

#[derive(Debug, Clone)]
struct TranscriberItems {
    items: Vec<TranscriberItem>,
}

impl TranscriberItems {
    fn parse(input: &mut ParseStreamEx) -> Result<Self> {
        let mut items = Vec::new();
        let mut tokens = Vec::new();
        let mut tes_range = RangeBuilder::new();
        while !input.is_empty() {
            if input.peek(token::Paren) || input.peek(token::Brace) || input.peek(token::Bracket) {
                push_tokens(&mut tokens, &mut items, &mut tes_range);
                let g = input.parse_group(|g, input| {
                    Ok(TranscriberGroup {
                        delimiter: g.group.delimiter(),
                        content: Self::parse(input)?,
                        tes_range_open: g.tes_range_open,
                        tes_range_close: g.tes_range_close,
                        span: g.group.span(),
                    })
                })?;
                items.push(TranscriberItem::Group(g));
                continue;
            }
            if input.peek(Token![$]) {
                if input.peek2(Ident::peek_any) {
                    push_tokens(&mut tokens, &mut items, &mut tes_range);
                    items.push(TranscriberItem::Var(input.parse()?));
                    continue;
                }
                if input.peek2(token::Paren) {
                    push_tokens(&mut tokens, &mut items, &mut tes_range);
                    items.push(TranscriberItem::Rep(TranscriberRep::parse(input)?));
                    continue;
                }
            }
            let tes_start = input.tes_offset;
            let token: TokenTree = input.parse().unwrap();
            let tes_end = input.tes_offset;
            tes_range.push(tes_start..tes_end);
            tokens.push(token);
        }
        push_tokens(&mut tokens, &mut items, &mut tes_range);
        Ok(Self { items })
    }
    fn ready_string(&mut self, source: &Source) {
        self.ready_string_with(source, &mut RangeBuilder::new());
    }
    fn ready_string_with(&mut self, source: &Source, tes_range: &mut RangeBuilder) {
        for item in &mut self.items {
            item.ready_string_with(source, tes_range);
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
    fn apply_tokens_to(&self, m: &RawMatch, b: &mut MatchTokensBuilder) {
        for item in &self.items {
            item.apply_tokens_to(m, b);
        }
    }
    fn apply_string(&self, m: &RawMatch, b: &mut MatchStringBuilder) {
        for item in &self.items {
            item.apply_string(m, b)
        }
    }
}
fn push_tokens(
    tokens: &mut Vec<TokenTree>,
    items: &mut Vec<TranscriberItem>,
    tes_range: &mut RangeBuilder,
) {
    if let Some(tes_range) = tes_range.take() {
        if !tokens.is_empty() {
            let tokens = TokenStream::from_iter(tokens.drain(..));
            items.push(TranscriberItem::Tokens(TranscriberTokens {
                tokens,
                tes_range,
            }));
        }
    }
    items.push(TranscriberItem::String(String::new()));
}

#[derive(Debug, Clone)]
enum TranscriberItem {
    Tokens(TranscriberTokens),
    Group(TranscriberGroup),
    String(String),
    Var(TranscriberVar),
    Rep(TranscriberRep),
}
impl TranscriberItem {
    fn ready_string_with(&mut self, source: &Source, tes_range: &mut RangeBuilder) {
        match self {
            Self::Tokens(t) => tes_range.push(t.tes_range.clone()),
            Self::Group(g) => g.ready_string_with(source, tes_range),
            Self::String(ref mut s) => {
                if let Some(tes_range) = tes_range.take() {
                    let mut b = TokenStringBuilder::new(source);
                    b.push_tes(tes_range);
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
    fn apply_tokens_to(&self, m: &RawMatch, b: &mut MatchTokensBuilder) {
        match self {
            TranscriberItem::Tokens(t) => t.tokens.to_tokens(b.tokens),
            TranscriberItem::String(_) => {}
            TranscriberItem::Group(g) => g.apply_tokens_to(m, b),
            TranscriberItem::Var(v) => v.apply_tokens_to(m, b),
            TranscriberItem::Rep(r) => r.apply_tokens_to(m, b),
        }
    }

    fn apply_string(&self, m: &RawMatch, b: &mut MatchStringBuilder) {
        match self {
            TranscriberItem::Tokens(tokens) => tokens.apply_string(b),
            TranscriberItem::Group(g) => g.apply_string(m, b),
            TranscriberItem::String(s) => b.b.push_str(s),
            TranscriberItem::Var(v) => v.apply_string(m, b),
            TranscriberItem::Rep(r) => r.apply_string(m, b),
        }
    }
}

#[derive(Debug, Clone)]
struct TranscriberTokens {
    tokens: TokenStream,
    tes_range: Range<usize>,
}
impl TranscriberTokens {
    fn apply_string(&self, b: &mut MatchStringBuilder) {
        if !b.is_ready_string {
            b.b.push_tokens(&self.tokens)
        }
    }
}

#[derive(Debug, Clone)]
struct TranscriberGroup {
    delimiter: Delimiter,
    content: TranscriberItems,
    span: Span,
    tes_range_open: Range<usize>,
    tes_range_close: Range<usize>,
}
impl TranscriberGroup {
    fn ready_string_with(&mut self, source: &Source, tes_range: &mut RangeBuilder) {
        tes_range.push(self.tes_range_open.clone());
        self.content.ready_string_with(source, tes_range);
        tes_range.push(self.tes_range_close.clone());
    }

    fn apply_tokens_to(&self, m: &RawMatch, b: &mut MatchTokensBuilder) {
        let mut stream = TokenStream::new();
        self.content.apply_tokens_to(
            m,
            &mut MatchTokensBuilder {
                tokens: &mut stream,
                ..*b
            },
        );
        let mut g = Group::new(self.delimiter, stream);
        g.set_span(self.span);
        b.tokens.append(g);
    }

    fn apply_string(&self, m: &RawMatch, b: &mut MatchStringBuilder) {
        if !b.is_ready_string {
            b.b.push_str(to_open_str(self.delimiter));
        }
        self.content.apply_string(m, b);
        if !b.is_ready_string {
            b.b.push_str(to_close_str(self.delimiter));
        }
    }
}

#[derive(Parse, ToTokens, Debug, Clone)]
struct MacroTranscriberVar {
    dollar_token: Token![$],
    name: Ident,
}

#[derive(Debug, Clone)]
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
    fn apply_tokens_to(&self, m: &RawMatch, b: &mut MatchTokensBuilder) {
        m.vars[self.var_index].apply_tokens_to(b)
    }

    fn apply_string(&self, m: &RawMatch, b: &mut MatchStringBuilder) {
        m.vars[self.var_index].apply_string(b)
    }
}

#[derive(Debug, Clone)]
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

    fn apply_tokens_to(&self, m: &RawMatch, b: &mut MatchTokensBuilder) {
        let mut is_next = false;
        for m in &m.reps[self.rep_index].0 {
            if is_next {
                if let Some(sep) = &self.sep.0 {
                    sep.to_tokens(b.tokens);
                }
            }
            is_next = true;
            self.content.apply_tokens_to(m, b)
        }
    }

    fn apply_string(&self, m: &RawMatch, b: &mut MatchStringBuilder) {
        let mut is_next = false;
        for m in &m.reps[self.rep_index].0 {
            if is_next {
                if let Some(sep) = &self.sep.0 {
                    b.b.push_tokens(&sep.to_token_stream());
                }
            }
            is_next = true;
            self.content.apply_string(m, b)
        }
    }
}
