use crate::parser::{MacroFlagSpec, MacroMatches, MacroRepOp};
use parser::MacroTranscriberItems;
use proc_macro2::{Delimiter, Ident, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use std::collections::HashMap;
use syn::{
    braced, bracketed,
    ext::IdentExt,
    parenthesized,
    parse::{discouraged::Speculative, ParseStream, Parser},
    parse2, Block, Expr, Item, Lifetime, Lit, Meta, Pat, Path, Result, Type, Visibility,
};
use syntax::{parse_macro_pat, parse_macro_stmt};

#[macro_use]
mod syn_utils;

mod parser;
mod syntax;

/// Search pattern corresponding to `MacroMatch*` in [`Macros By Example`](https://doc.rust-lang.org/reference/macros-by-example.html).
#[derive(Debug)]
pub struct Matcher(PatternItems);

impl Matcher {
    /// Create a new `Matcher` from a `TokenStream`.
    pub fn from_token_stream(tokens: TokenStream) -> Result<Self> {
        Ok(Self(parse2::<MacroMatches>(tokens)?.to_pattern()?))
    }

    fn try_match(&self, input: ParseStream) -> Result<Match> {
        self.0.try_match(input)
    }
}

#[derive(Debug)]
struct PatternItems {
    items: Vec<PatternItem>,
    binds: HashMap<String, MacroBindRef>,
    bind_count: usize,
    rep_count: usize,
}

impl PatternItems {
    fn new(items: Vec<PatternItem>) -> Result<Self> {
        let mut binds = HashMap::new();
        let mut bind_index = 0;
        let mut rep_index = 0;
        for item in &items {
            match item {
                PatternItem::Token(_) => {}
                PatternItem::Group(g) => {
                    g.get_binds(&mut bind_index, &mut rep_index, &mut binds)?
                }
                PatternItem::Bind(b) => b.get_binds(&mut bind_index, &mut binds)?,
                PatternItem::Rep(r) => r.get_binds(&mut rep_index, &mut binds)?,
            }
        }
        Ok(Self {
            items,
            binds,
            bind_count: bind_index,
            rep_count: rep_index,
        })
    }
    fn contains_bind_or_rep(&self) -> bool {
        self.items.iter().any(|i| i.contains_bind_or_rep())
    }
    fn find_item(&self, name: &str) -> Option<&PatternItem> {
        for item in &self.items {
            match item {
                PatternItem::Token(_) => {}
                PatternItem::Group(g) => {
                    if let Some(item) = g.content.find_item(name) {
                        return Some(item);
                    }
                }
                PatternItem::Bind(b) => {
                    if b.name.as_deref() == Some(name) {
                        return Some(item);
                    }
                }
                PatternItem::Rep(r) => {
                    if r.content.binds.contains_key(name) {
                        return Some(item);
                    }
                }
            }
        }
        None
    }
    fn try_match(&self, input: ParseStream) -> Result<Match> {
        let mut m = Match::new();
        self.try_match_to(input, &mut m)?;
        Ok(m)
    }
    fn try_match_to(&self, input: ParseStream, m: &mut Match) -> Result<()> {
        for item in &self.items {
            item.try_match(input, m)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
enum PatternItem {
    Token(String),
    Group(GroupPattern),
    Bind(BindPattern),
    Rep(RepPattern),
}
impl PatternItem {
    fn contains_bind_or_rep(&self) -> bool {
        match self {
            Self::Token(_) => false,
            Self::Group(g) => g.content.contains_bind_or_rep(),
            Self::Bind(_) => true,
            Self::Rep(_) => true,
        }
    }

    fn try_match(&self, input: ParseStream, m: &mut Match) -> Result<()> {
        match self {
            PatternItem::Token(token) => {
                try_match_token(input, token)?;
                m.is_exists = true;
                Ok(())
            }
            PatternItem::Group(g) => g.try_match_to(input, m),
            PatternItem::Bind(b) => {
                m.binds.push(b.try_match(input)?);
                m.is_exists = true;
                Ok(())
            }
            PatternItem::Rep(r) => {
                let m1 = r.try_match(input)?;
                m.is_exists |= !m1.0.is_empty();
                m.reps.push(m1);
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
struct GroupPattern {
    delimiter: Delimiter,
    content: PatternItems,
}
impl GroupPattern {
    fn get_binds(
        &self,
        bind_index: &mut usize,
        rep_index: &mut usize,
        binds: &mut HashMap<String, MacroBindRef>,
    ) -> Result<()> {
        for b in &self.content.binds {
            let depth = b.1.depth;
            let bind_index_or_rep_index =
                if depth == 0 { *bind_index } else { *rep_index } + b.1.bind_index_or_rep_index;
            insert_bind(
                binds,
                b.0,
                MacroBindRef {
                    depth: b.1.depth,
                    bind_index_or_rep_index,
                    span: b.1.span,
                },
            )?;
        }
        *bind_index += self.content.bind_count;
        *rep_index += self.content.rep_count;
        Ok(())
    }

    fn try_match_to(&self, input: ParseStream, m: &mut Match) -> Result<()> {
        let content;
        match self.delimiter {
            Delimiter::Parenthesis => {
                parenthesized!(content in input);
            }
            Delimiter::Brace => {
                braced!(content in input);
            }
            Delimiter::Bracket => {
                bracketed!(content in input);
            }
            Delimiter::None => bail!(input.span(), "unsupported delimiter: None"),
        }
        self.content.try_match_to(&content, m)?;
        m.is_exists = true;
        Ok(())
    }
}

#[derive(Debug)]
struct BindPattern {
    name: Option<String>,
    name_span: Span,
    flag: MacroFlagSpec,
}
impl BindPattern {
    fn get_binds(
        &self,
        bind_index: &mut usize,
        binds: &mut HashMap<String, MacroBindRef>,
    ) -> Result<()> {
        if let Some(name) = &self.name {
            insert_bind(
                binds,
                name,
                MacroBindRef {
                    depth: 0,
                    bind_index_or_rep_index: *bind_index,
                    span: self.name_span,
                },
            )?;
            *bind_index += 1;
        }
        Ok(())
    }

    fn try_match(&self, input: ParseStream) -> Result<TokenStream> {
        Ok(match self.flag {
            MacroFlagSpec::Block(_) => input.parse::<Block>()?.to_token_stream(),
            MacroFlagSpec::Expr(_) => input.parse::<Expr>()?.to_token_stream(),
            MacroFlagSpec::Ident(_) => Ident::parse_any(input)?.to_token_stream(),
            MacroFlagSpec::Item(_) => input.parse::<Item>()?.to_token_stream(),
            MacroFlagSpec::Lifetime(_) => input.parse::<Lifetime>()?.to_token_stream(),
            MacroFlagSpec::Literal(_) => input.parse::<Lit>()?.to_token_stream(),
            MacroFlagSpec::Meta(_) => input.parse::<Meta>()?.to_token_stream(),
            MacroFlagSpec::Pat(_) => parse_macro_pat(input)?.to_token_stream(),
            MacroFlagSpec::PatParam(_) => input.parse::<Pat>()?.to_token_stream(),
            MacroFlagSpec::Path(_) => input.parse::<Path>()?.to_token_stream(),
            MacroFlagSpec::Stmt(_) => parse_macro_stmt(input)?.to_token_stream(),
            MacroFlagSpec::Tt(_) => input.parse::<TokenTree>()?.to_token_stream(),
            MacroFlagSpec::Ty(_) => input.parse::<Type>()?.to_token_stream(),
            MacroFlagSpec::Vis(_) => input.parse::<Visibility>()?.to_token_stream(),
        })
    }
}

#[derive(Debug)]
struct RepPattern {
    content: PatternItems,
    sep: Option<String>,
    op: MacroRepOp,
}
impl RepPattern {
    fn get_binds(
        &self,
        rep_index: &mut usize,
        binds: &mut HashMap<String, MacroBindRef>,
    ) -> Result<()> {
        for b in &self.content.binds {
            insert_bind(
                binds,
                b.0,
                MacroBindRef {
                    depth: 1 + b.1.depth,
                    bind_index_or_rep_index: *rep_index,
                    span: b.1.span,
                },
            )?;
        }
        *rep_index += 1;
        Ok(())
    }

    fn try_match(&self, input: ParseStream) -> Result<MatchRep> {
        let mut ms = Vec::new();
        let mut is_next = false;
        while !input.is_empty() {
            let fork = input.fork();
            if is_next {
                if let Some(sep) = &self.sep {
                    try_match_token(&fork, sep)?;
                }
            }
            if let Ok(m) = self.content.try_match(&fork) {
                if m.is_exists {
                    ms.push(m);
                    input.advance_to(&fork);
                    if self.op.is_zero_or_one() {
                        break;
                    }
                    is_next = true;
                    continue;
                }
            }
            if self.op.is_one_or_more() && !is_next {
                bail!(input.span(), "expected at least one repetition")
            }
            break;
        }
        Ok(MatchRep(ms))
    }
}

#[derive(Debug)]
struct MacroBindRef {
    depth: usize,

    // if depth == 0, this is the index of `Match::binds`
    // if depth > 0, this is the index of `Match::reps`
    bind_index_or_rep_index: usize,
    span: Span,
}
fn insert_bind(
    binds: &mut HashMap<String, MacroBindRef>,
    name: &str,
    bind: MacroBindRef,
) -> Result<()> {
    if binds.contains_key(name) {
        bail!(bind.span, "duplicate binding `name`")
    }
    binds.insert(name.to_owned(), bind);
    Ok(())
}

/// Replacement pattern corresponding to `MacroTranscriber` in [`Macros By Example`](https://doc.rust-lang.org/reference/macros-by-example.html).
#[derive(Debug)]
pub struct Transcriber(MacroTranscriberItems);

impl Transcriber {
    /// Creates a new `Transcriber` from a `TokenStream`.
    ///
    /// The pattern corresponds to `MacroTranscriber` in [`Macros By Example`](https://doc.rust-lang.org/reference/macros-by-example.html),
    /// but does not include the outermost brace.
    pub fn from_token_stream(tokens: TokenStream) -> Result<Self> {
        Ok(Self(parse2::<MacroTranscriberItems>(tokens)?))
    }
}

#[derive(Debug)]
struct TranscriberItems {
    items: Vec<TranscriberItem>,
}
impl TranscriberItems {
    fn get_bind(&self) -> Option<(String, Span)> {
        for item in &self.items {
            match item {
                TranscriberItem::Token(_) => {}
                TranscriberItem::Group(g) => {
                    if let Some((name, span)) = g.content.get_bind() {
                        return Some((name, span));
                    }
                }
                TranscriberItem::Bind(b) => return Some((b.name.clone(), b.span)),
                TranscriberItem::Rep(r) => return Some((r.bind_name.clone(), r.bind_span)),
            }
        }
        None
    }

    fn attach(&mut self, p: &PatternItems) -> Result<()> {
        for i in &mut self.items {
            i.attach(p)?;
        }
        Ok(())
    }
    fn apply(&self, m: &Match, ts: &mut TokenStream) {
        for item in &self.items {
            item.apply(m, ts);
        }
    }
}

#[derive(Debug)]
enum TranscriberItem {
    Token(TokenTree),
    Group(TranscriberGroup),
    Bind(TranscriberBind),
    Rep(TranscriberRep),
}
impl TranscriberItem {
    fn attach(&mut self, p: &PatternItems) -> Result<()> {
        match self {
            Self::Token(_) => Ok(()),
            Self::Group(g) => g.attach(p),
            Self::Bind(b) => b.attach(p),
            Self::Rep(r) => r.attach(p),
        }
    }

    fn apply(&self, m: &Match, ts: &mut TokenStream) {
        match self {
            TranscriberItem::Token(t) => ts.extend(t.to_token_stream()),
            TranscriberItem::Group(g) => g.apply(m, ts),
            TranscriberItem::Bind(b) => b.apply(m, ts),
            TranscriberItem::Rep(r) => r.apply(m, ts),
        }
    }
}

#[derive(Debug)]
struct TranscriberGroup {
    delimiter: Delimiter,
    content: TranscriberItems,
}
impl TranscriberGroup {
    fn attach(&mut self, p: &PatternItems) -> Result<()> {
        self.content.attach(p)
    }

    fn apply(&self, m: &Match, ts: &mut TokenStream) {
        let mut content = TokenStream::new();
        self.content.apply(m, &mut content);
        match self.delimiter {
            Delimiter::Parenthesis => ts.extend(quote!((#content))),
            Delimiter::Brace => ts.extend(quote!({#content})),
            Delimiter::Bracket => ts.extend(quote!([#content])),
            Delimiter::None => ts.extend(content),
        }
    }
}

#[derive(Debug)]
struct TranscriberBind {
    bind_index: usize,
    name: String,
    span: Span,
}
impl TranscriberBind {
    fn attach(&mut self, p: &PatternItems) -> Result<()> {
        let name = &self.name;
        let span = self.span;
        if let Some(b) = p.binds.get(name) {
            if b.depth != 0 {
                bail!(span, "variable '{name}' is still repeating at this depth",);
            }
            self.bind_index = b.bind_index_or_rep_index;
            Ok(())
        } else {
            bail!(span, "attempted to repeat an expression containing no syntax variables matched as repeating at this depth")
        }
    }

    fn apply(&self, m: &Match, ts: &mut TokenStream) {
        ts.extend(m.binds[self.bind_index].clone());
    }
}
#[derive(Debug)]
struct TranscriberRep {
    rep_index: usize,
    content: TranscriberItems,
    op: MacroRepOp,
    sep: Option<TokenTree>,
    span: Span,
    bind_name: String,
    bind_span: Span,
}
impl TranscriberRep {
    fn attach(&mut self, p: &PatternItems) -> Result<()> {
        if let Some(b) = p.binds.get(&self.bind_name) {
            if b.depth > 0 {
                self.rep_index = b.bind_index_or_rep_index;
                if let Some(PatternItem::Rep(r)) = p.find_item(&self.bind_name) {
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
        bail!(self.bind_span,"attempted to repeat an expression containing no syntax variables matched as repeating at this depth")
    }

    fn apply(&self, m: &Match, ts: &mut TokenStream) {
        let mut is_next = false;
        for m in &m.reps[self.rep_index].0 {
            if is_next {
                if let Some(sep) = &self.sep {
                    ts.extend(sep.to_token_stream());
                }
            }
            is_next = true;
            self.content.apply(m, ts)
        }
    }
}

/// Pair [`Matcher`] and [`Transcriber`].
pub struct Rule {
    from: Matcher,
    to: TranscriberItems,
}

impl Rule {
    pub fn new(from: Matcher, to: Transcriber) -> Result<Self> {
        let mut to = to.0.to_transcriber(&from.0.binds)?;
        to.attach(&from.0)?;
        Ok(Rule { from, to })
    }

    /// Replaces all non-overlapping matches in `input` with the provided transcriber.
    pub fn replace_all(&self, input: TokenStream) -> TokenStream {
        Parser::parse2(|input: ParseStream| Ok(self.replace_raw(input)), input).unwrap()
    }
    fn replace_raw(&self, input: ParseStream) -> TokenStream {
        let mut ts = TokenStream::new();
        while !input.is_empty() {
            let fork = input.fork();
            if let Ok(m) = self.from.try_match(&fork) {
                if m.is_exists {
                    self.to.apply(&m, &mut ts);
                    input.advance_to(&fork);
                    continue;
                }
            }
            let token: TokenTree = input.parse().unwrap();
            ts.extend(token.to_token_stream());
        }
        ts
    }
}

#[derive(Default)]
struct Match {
    binds: Vec<TokenStream>,
    reps: Vec<MatchRep>,
    is_exists: bool,
}
impl Match {
    fn new() -> Self {
        Self::default()
    }
}
struct MatchRep(Vec<Match>);

fn try_match_token(input: ParseStream, s: &str) -> Result<()> {
    let t = input.parse::<TokenTree>()?;
    if s == t.to_string() {
        Ok(())
    } else {
        bail!(input.span(), "mismatch")
    }
}
