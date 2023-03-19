use crate::parser::{MacroFlagSpec, MacroMatches, MacroRepOp};
use parser::MacroTranscriberItems;
use proc_macro2::{Delimiter, Ident, LineColumn, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use std::{collections::HashMap, str::FromStr};
use structmeta::Parse;
use syn::{
    braced, bracketed,
    buffer::Cursor,
    ext::IdentExt,
    parenthesized,
    parse::{discouraged::Speculative, Parse, ParseStream, Parser},
    parse_str, Block, Error, Expr, Item, Lifetime, Lit, Meta, Pat, Path, Result, Type, Visibility,
};
use syntax::parse_macro_stmt;
use text::Text;

#[macro_use]
mod syn_utils;

mod parser;
mod syntax;
mod text;

/// Search pattern corresponding to `MacroMatch*` in [`Macros By Example`](https://doc.rust-lang.org/reference/macros-by-example.html).
#[derive(Debug)]
pub struct Matcher(PatternItems);

impl Parse for Matcher {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self(input.parse::<MacroMatches>()?.to_pattern()?))
    }
}
impl FromStr for Matcher {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        parse_str(s)
    }
}

impl Matcher {
    fn try_match(&self, input: ParseStream) -> Result<RawMatch> {
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
    fn try_match(&self, input: ParseStream) -> Result<RawMatch> {
        let mut m = RawMatch::new();
        self.try_match_to(input, &mut m)?;
        Ok(m)
    }
    fn try_match_to(&self, input: ParseStream, m: &mut RawMatch) -> Result<()> {
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

    fn try_match(&self, input: ParseStream, m: &mut RawMatch) -> Result<()> {
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

    fn try_match_to(&self, input: ParseStream, m: &mut RawMatch) -> Result<()> {
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
            MacroFlagSpec::Pat(_) => Pat::parse_multi_with_leading_vert(input)?.to_token_stream(),
            MacroFlagSpec::PatParam(_) => Pat::parse_single(input)?.to_token_stream(),
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
///
/// Does not include the outermost brace.
#[derive(Debug, Parse)]
pub struct Transcriber(MacroTranscriberItems);

impl FromStr for Transcriber {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        parse_str(s)
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
    fn apply(&self, m: &RawMatch, ts: &mut TokenStream) {
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

    fn apply(&self, m: &RawMatch, ts: &mut TokenStream) {
        match self {
            TranscriberItem::Token(t) => t.to_tokens(ts),
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

    fn apply(&self, m: &RawMatch, ts: &mut TokenStream) {
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

    fn apply(&self, m: &RawMatch, ts: &mut TokenStream) {
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

    fn apply(&self, m: &RawMatch, ts: &mut TokenStream) {
        let mut is_next = false;
        for m in &m.reps[self.rep_index].0 {
            if is_next {
                if let Some(sep) = &self.sep {
                    sep.to_tokens(ts);
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
    pub fn find_all(&self, input: TokenStream) -> FindAll {
        self.find_all_raw(input, None)
    }
    pub fn find_all_str(&self, input: &str) -> Result<FindAll> {
        Ok(self.find_all_raw(parse_str(input)?, Some(Text::new(input.to_owned()))))
    }
    fn find_all_raw(&self, input: TokenStream, text: Option<Text>) -> FindAll {
        Parser::parse2(
            |input: ParseStream| Ok(self.find_all_parser(input, text)),
            input,
        )
        .unwrap()
    }
    fn find_all_parser(&self, input: ParseStream, text: Option<Text>) -> FindAll {
        let mut b = FindAllBuilder::new(text, input.cursor());
        while !input.is_empty() {
            let fork = input.fork();
            if let Ok(m) = self.from.try_match(&fork) {
                if m.is_exists {
                    let mut to_tokens = TokenStream::new();
                    self.to.apply(&m, &mut to_tokens);
                    b.push(input.cursor(), fork.cursor(), to_tokens);
                    input.advance_to(&fork);
                    continue;
                }
            }
            let _: TokenTree = input.parse().unwrap();
        }
        b.finish(input.cursor())
    }

    /// Replaces all non-overlapping matches in `input` with the provided transcriber.
    pub fn replace_all(&self, input: TokenStream) -> TokenStream {
        let mut tokens = TokenStream::new();
        for p in self.find_all(input).parts() {
            p.output.to_tokens(&mut tokens);
        }
        tokens
    }

    /// Replaces all non-overlapping matches in input with the provided transcriber.
    ///
    /// Unlike creating `TokenStream` from `str` and then calling [`Rule::replace_all`],
    /// the original string is preserved for the parts that are not replaced.
    pub fn replace_all_str(&self, input: &str) -> Result<String> {
        use std::fmt::Write;
        let mut s = String::new();
        let text = Some(Text::new(input.to_owned()));
        let input = parse_str(input)?;
        for p in self.find_all_raw(input, text).parts() {
            write!(&mut s, "{}", &p.output).unwrap();
        }
        Ok(s)
    }

    /// If the entire `input` matches the entire `from`, do the conversion. Otherwise, return an error.
    pub fn apply(&self, input: TokenStream) -> Result<TokenStream> {
        Parser::parse2(|input: ParseStream| self.apply_parser(input), input)
    }
    fn apply_parser(&self, input: ParseStream) -> Result<TokenStream> {
        let mut ts = TokenStream::new();
        let m = self.from.try_match(input)?;
        self.to.apply(&m, &mut ts);
        Ok(ts)
    }
}

/// [`TokenStream`] with original (pre-formatting) string.
#[derive(Clone)]
pub struct UnformattedTokenStream {
    tokens: TokenStream,
    text: Option<String>,
}

impl ToTokens for UnformattedTokenStream {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.tokens.clone())
    }
}
impl std::fmt::Display for UnformattedTokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(text) = &self.text {
            write!(f, "{text}")
        } else {
            write!(f, "{}", &self.tokens)
        }
    }
}
impl std::fmt::Debug for UnformattedTokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

struct Match {
    input: UnformattedTokenStream,
    output: UnformattedTokenStream,
}

/// A subsequence of [`UnformattedTokenStream`]
pub struct Part<'a> {
    pub input: &'a UnformattedTokenStream,
    pub output: &'a UnformattedTokenStream,
    pub is_match: bool,
}

/// Result of [`Rule::find_all`].
pub struct FindAll {
    items_match: Vec<Match>,
    items_unmatch: Vec<UnformattedTokenStream>,
}

impl FindAll {
    pub fn parts(&self) -> impl Iterator<Item = Part> {
        pub struct Parts<'a> {
            this: &'a FindAll,
            n: usize,
        }
        impl<'a> Iterator for Parts<'a> {
            type Item = Part<'a>;
            fn next(&mut self) -> Option<Self::Item> {
                let n = self.n;
                if n <= self.this.items_match.len() * 2 {
                    self.n += 1;
                    let is_match = n % 2 != 0;
                    let (input, output) = if is_match {
                        let item = &self.this.items_match[n / 2];
                        (&item.input, &item.output)
                    } else {
                        let item = &self.this.items_unmatch[n / 2];
                        (item, item)
                    };
                    Some(Part {
                        input,
                        output,
                        is_match,
                    })
                } else {
                    None
                }
            }
        }
        Parts { this: self, n: 0 }
    }
}

struct FindAllBuilder<'a> {
    text: Option<Text>,
    items_match: Vec<Match>,
    items_unmatch: Vec<UnformattedTokenStream>,
    text_start: usize,
    tokens_start: Cursor<'a>,
}
impl<'a> FindAllBuilder<'a> {
    fn new(text: Option<Text>, tokens_start: Cursor<'a>) -> Self {
        Self {
            text,
            items_match: Vec::new(),
            items_unmatch: Vec::new(),
            text_start: 0,
            tokens_start,
        }
    }
    fn push(&mut self, start: Cursor<'a>, end: Cursor<'a>, output_tokens: TokenStream) {
        self.push_unmatch(start);
        let input = self.read_unformatted(end, false);
        let output = UnformattedTokenStream {
            tokens: output_tokens,
            text: None,
        };
        self.items_match.push(Match { input, output })
    }
    fn push_unmatch(&mut self, end: Cursor<'a>) {
        let sep = self.read_unformatted(end, true);
        self.items_unmatch.push(sep);
    }
    fn read_unformatted(
        &mut self,
        end: Cursor<'a>,
        include_last_space: bool,
    ) -> UnformattedTokenStream {
        let (tokens, last) = tokens_from_start_end(self.tokens_start, end);
        let line_column = match (last, include_last_space, end.eof()) {
            (_, _, true) => None,
            (Some(last), false, false) => Some(last.span().end()),
            (_, _, false) => Some(end.span().start()),
        };
        let text = self.read_text(line_column);
        self.tokens_start = end;
        UnformattedTokenStream { tokens, text }
    }
    fn read_text(&mut self, line_column: Option<LineColumn>) -> Option<String> {
        let text = self.text.as_ref()?;
        if let Some(line_column) = line_column {
            if line_column.line == 0 {
                return None;
            }
            let start = self.text_start;
            let end = text.offset_of(line_column);
            self.text_start = end;
            Some(text.get(start, end).to_owned())
        } else {
            let start = self.text_start;
            let end = text.end();
            self.text_start = end;
            Some(text.get(start, end).to_owned())
        }
    }

    fn finish(mut self, end: Cursor<'a>) -> FindAll {
        self.push_unmatch(end);
        FindAll {
            items_match: self.items_match,
            items_unmatch: self.items_unmatch,
        }
    }
}

#[derive(Default)]
struct RawMatch {
    binds: Vec<TokenStream>,
    reps: Vec<MatchRep>,
    is_exists: bool,
}
impl RawMatch {
    fn new() -> Self {
        Self::default()
    }
}
struct MatchRep(Vec<RawMatch>);

fn try_match_token(input: ParseStream, s: &str) -> Result<()> {
    let t = input.parse::<TokenTree>()?;
    if s == t.to_string() {
        Ok(())
    } else {
        bail!(input.span(), "mismatch")
    }
}
fn tokens_from_start_end<'a>(
    start: Cursor<'a>,
    end: Cursor<'a>,
) -> (TokenStream, Option<Cursor<'a>>) {
    let mut ts = TokenStream::new();
    let mut cursor = start;
    let mut last = None;
    while cursor != end {
        last = Some(cursor);
        let token = cursor.token_tree().unwrap();
        token.0.to_tokens(&mut ts);
        cursor = token.1;
    }
    (ts, last)
}
