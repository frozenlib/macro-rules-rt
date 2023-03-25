use crate::{
    token_entry::{
        cts_len, CursorToken, CursorTokenTree, LongToken, LongTokenTree, ParseStreamEx,
        ResultStringBuilder, TokenEntry,
    },
    utils::{parse_macro_stmt, to_delimiter},
    Transcriber,
};
use proc_macro2::{Delimiter, Group, Span, TokenStream};
use quote::{ToTokens, TokenStreamExt};
use std::{collections::HashMap, ops::Range, str::FromStr};
use structmeta::{Parse, ToTokens};
use syn::{
    buffer::Cursor,
    ext::IdentExt,
    parse::{Parse, ParseStream, Parser},
    parse_str,
    spanned::Spanned,
    token, Block, Error, Expr, Ident, Item, Lifetime, Lit, MacroDelimiter, Meta, Pat, Path, Result,
    Token, Type, Visibility,
};

pub use self::macro_flag_spec::MacroFlagSpec;

#[derive(Debug)]
pub struct FindAllParts(Vec<FindAllPart>);

impl FindAllParts {
    pub fn apply_tokens(
        &self,
        index: &mut usize,
        to: &Transcriber,
        input: TokenStream,
    ) -> TokenStream {
        let mut output = TokenStream::new();
        (|input: ParseStream| {
            self.apply_tokens_to(index, to, input, &mut output);
            Ok(())
        })
        .parse2(input)
        .unwrap();
        output
    }
    fn apply_tokens_to(
        &self,
        index: &mut usize,
        to: &Transcriber,
        input: ParseStream,
        output: &mut TokenStream,
    ) {
        while *index < self.0.len() {
            match &self.0[*index] {
                FindAllPart::NoMatch(p) => p.apply_tokens_to(input, output),
                FindAllPart::Match(p) => p.apply_tokens_to(input, to, output),
                FindAllPart::GroupOpen => {
                    *index += 1;
                    let Ok(g) = input.parse::<Group>() else {
                        unreachable!()
                    };
                    let mut g_new =
                        Group::new(g.delimiter(), self.apply_tokens(index, to, g.stream()));
                    g_new.set_span(g.span());
                    output.append(g_new);
                    assert!(matches!(self.0[*index], FindAllPart::GroupClose));
                }
                FindAllPart::GroupClose => return,
            }
            *index += 1;
        }
    }
    pub(crate) fn apply_string(&self, to: &Transcriber, b: &mut ResultStringBuilder) {
        for p in &self.0 {
            match p {
                FindAllPart::NoMatch(p) => b.push_no_match(p.tts_and_tes_len),
                FindAllPart::Match(p) => p.apply_string(to, b),
                FindAllPart::GroupOpen | FindAllPart::GroupClose => b.push_no_match(1),
            }
        }
    }
}

#[derive(Debug)]
enum FindAllPart {
    NoMatch(FindAllPartNoMatch),
    Match(FindAllPartMatch),
    GroupOpen,
    GroupClose,
}

#[derive(Debug)]
struct FindAllPartNoMatch {
    tts_and_tes_len: usize,
}
impl FindAllPartNoMatch {
    fn apply_tokens_to(&self, input: ParseStream, output: &mut TokenStream) {
        for _ in 0..self.tts_and_tes_len {
            let t: CursorToken = input.parse().unwrap();
            t.to_tokens(output);
        }
    }
}

#[derive(Debug)]
struct FindAllPartMatch {
    m: RawMatch,
    cts_len: usize,
    tes_len: usize,
}
impl FindAllPartMatch {
    fn apply_tokens_to(&self, input: ParseStream, to: &Transcriber, output: &mut TokenStream) {
        for _ in 0..self.cts_len {
            let _: CursorTokenTree = input.parse().unwrap();
        }
        to.apply_tokens_to(&self.m, output)
    }
    fn apply_string(&self, to: &Transcriber, b: &mut ResultStringBuilder) {
        b.commit_no_match(self.tes_len);
        to.apply_string(&self.m, b);
    }
}

/// Search pattern.
///
/// `Matcher` corresponds to `MacroMatch*` (excluding outermost brace) in [`Macros By Example`](https://doc.rust-lang.org/reference/macros-by-example.html).
#[derive(Debug)]
pub struct Matcher(pub(crate) PatternItems);

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
    pub fn is_empty(&self) -> bool {
        self.0.items.is_empty()
    }
    pub(crate) fn try_match(&self, input: &mut ParseStreamEx) -> Result<RawMatch> {
        self.0.try_match(input)
    }

    pub(crate) fn find_all(&self, input: TokenStream) -> FindAllParts {
        let mut parts = Vec::new();
        self.find_all_parts(input, &mut parts);
        FindAllParts(parts)
    }

    fn find_all_parts(&self, input: TokenStream, parts: &mut Vec<FindAllPart>) -> bool {
        ParseStreamEx::parse_from_tokens(input, 0, |input: &mut ParseStreamEx| {
            Ok(self.find_all_parts_parser(input, parts))
        })
        .unwrap()
    }
    fn find_all_parts_parser(
        &self,
        input: &mut ParseStreamEx,
        parts: &mut Vec<FindAllPart>,
    ) -> bool {
        let mut tts_and_tes_len = 0;
        let mut is_match = false;
        while !input.is_empty() {
            let mut fork = input.fork();
            if let Ok(m) = self.try_match(&mut fork) {
                if !m.is_empty {
                    parts.push(FindAllPart::NoMatch(FindAllPartNoMatch { tts_and_tes_len }));
                    parts.push(FindAllPart::Match(match_part(
                        m,
                        input.cursor(),
                        fork.cursor(),
                    )));
                    input.advance_to(&fork);
                    tts_and_tes_len = 0;
                    is_match = true;
                    continue;
                }
            }
            if input.peek(token::Paren) || input.peek(token::Brace) || input.peek(token::Bracket) {
                input
                    .parse_group(|_, input| {
                        parts.push(FindAllPart::NoMatch(FindAllPartNoMatch { tts_and_tes_len }));
                        parts.push(FindAllPart::GroupOpen);
                        is_match |= self.find_all_parts_parser(input, parts);
                        parts.push(FindAllPart::GroupClose);
                        tts_and_tes_len = 0;
                        Ok(())
                    })
                    .unwrap();
                continue;
            }
            if let Ok(t) = input.parse::<LongToken>() {
                tts_and_tes_len += t.len();
            } else {
                // End after non-delimiter Group
                break;
            }
        }
        parts.push(FindAllPart::NoMatch(FindAllPartNoMatch { tts_and_tes_len }));
        is_match
    }
}
fn match_part(m: RawMatch, start: Cursor, end: Cursor) -> FindAllPartMatch {
    FindAllPartMatch {
        m,
        cts_len: cts_len(start, end),
        tes_len: TokenEntry::len_from_cursor(start, end),
    }
}

#[derive(Debug)]
pub struct PatternItems {
    items: Vec<PatternItem>,
    pub vars: HashMap<String, MacroVarRef>,
    var_count: usize,
    rep_count: usize,
}

impl PatternItems {
    fn new(items: Vec<PatternItem>) -> Result<Self> {
        let mut vars = HashMap::new();
        let mut var_index = 0;
        let mut rep_index = 0;
        for item in &items {
            match item {
                PatternItem::Token(_) => {}
                PatternItem::Group(g) => g.get_vars(&mut var_index, &mut rep_index, &mut vars)?,
                PatternItem::Var(v) => v.get_vars(&mut var_index, &mut vars)?,
                PatternItem::Rep(r) => r.get_vars(&mut rep_index, &mut vars)?,
            }
        }
        Ok(Self {
            items,
            vars,
            var_count: var_index,
            rep_count: rep_index,
        })
    }
    pub fn find_rep(&self, name: &str) -> Option<&RepPattern> {
        if let Some(PatternItem::Rep(r)) = self.find_item(name) {
            Some(r)
        } else {
            None
        }
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
                PatternItem::Var(v) => {
                    if v.name.as_deref() == Some(name) {
                        return Some(item);
                    }
                }
                PatternItem::Rep(r) => {
                    if r.content.vars.contains_key(name) {
                        return Some(item);
                    }
                }
            }
        }
        None
    }
    fn try_match(&self, input: &mut ParseStreamEx) -> Result<RawMatch> {
        let mut m = RawMatch::new();
        self.try_match_to(input, &mut m)?;
        Ok(m)
    }
    fn try_match_to(&self, input: &mut ParseStreamEx, m: &mut RawMatch) -> Result<()> {
        for item in &self.items {
            item.try_match(input, m)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
enum PatternItem {
    Token(TokenPattern),
    Group(GroupPattern),
    Var(VarPattern),
    Rep(RepPattern),
}
impl PatternItem {
    fn try_match(&self, input: &mut ParseStreamEx, m: &mut RawMatch) -> Result<()> {
        match self {
            PatternItem::Token(t) => t.try_match_to(input, m),
            PatternItem::Group(g) => g.try_match_to(input, m),
            PatternItem::Var(v) => {
                let tes_start = input.tes_offset;
                let tokens = input.parse_with(|input| v.try_match(input))?;
                let tes_end = input.tes_offset;
                m.vars.push(MatchVar {
                    tokens,
                    tes_range: tes_start..tes_end,
                });
                m.is_empty = false;
                Ok(())
            }
            PatternItem::Rep(r) => {
                let m1 = r.try_match_to(input, m)?;
                m.is_empty &= m1.0.is_empty();
                m.reps.push(m1);
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
struct TokenPattern {
    s: String,
}

impl TokenPattern {
    fn new(tt: LongToken) -> Self {
        let s = tt.to_string();
        Self { s }
    }
    fn eq_token(&self, tt: &LongToken) -> bool {
        self.s == tt.to_string()
    }
    fn try_match_to(&self, input: &mut ParseStreamEx, m: &mut RawMatch) -> Result<()> {
        let span = input.span();
        if self.eq_token(&input.parse()?) {
            m.is_empty = false;
            Ok(())
        } else {
            bail!(span, "mismatch");
        }
    }
}

#[derive(Debug)]
struct GroupPattern {
    delimiter: Delimiter,
    content: PatternItems,
}
impl GroupPattern {
    fn get_vars(
        &self,
        var_index: &mut usize,
        rep_index: &mut usize,
        vars: &mut HashMap<String, MacroVarRef>,
    ) -> Result<()> {
        for b in &self.content.vars {
            let depth = b.1.depth;
            let var_index_or_rep_index =
                if depth == 0 { *var_index } else { *rep_index } + b.1.var_index_or_rep_index;
            insert_var(
                vars,
                b.0,
                MacroVarRef {
                    depth: b.1.depth,
                    var_index_or_rep_index,
                    span: b.1.span,
                },
            )?;
        }
        *var_index += self.content.var_count;
        *rep_index += self.content.rep_count;
        Ok(())
    }

    fn try_match_to(&self, input: &mut ParseStreamEx, m: &mut RawMatch) -> Result<()> {
        match self.delimiter {
            Delimiter::Parenthesis => input.expect(token::Paren)?,
            Delimiter::Brace => input.expect(token::Brace)?,
            Delimiter::Bracket => input.expect(token::Bracket)?,
            Delimiter::None => input.expect(token::Group)?,
        }
        input.parse_group(|_g, input| {
            self.content.try_match_to(input, m)?;
            m.is_empty = false;
            Ok(())
        })
    }
}

#[derive(Debug)]
struct VarPattern {
    name: Option<String>,
    name_span: Span,
    flag: MacroFlagSpec,
}
impl VarPattern {
    fn get_vars(
        &self,
        var_index: &mut usize,
        vars: &mut HashMap<String, MacroVarRef>,
    ) -> Result<()> {
        if let Some(name) = &self.name {
            insert_var(
                vars,
                name,
                MacroVarRef {
                    depth: 0,
                    var_index_or_rep_index: *var_index,
                    span: self.name_span,
                },
            )?;
            *var_index += 1;
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
            MacroFlagSpec::Tt(_) => input.parse::<LongTokenTree>()?.to_token_stream(),
            MacroFlagSpec::Ty(_) => input.parse::<Type>()?.to_token_stream(),
            MacroFlagSpec::Vis(_) => input.parse::<Visibility>()?.to_token_stream(),
        })
    }
}

#[derive(Debug)]
pub struct RepPattern {
    pub content: PatternItems,
    sep: Option<TokenPattern>,
    pub op: MacroRepOp,
}
impl RepPattern {
    fn get_vars(
        &self,
        rep_index: &mut usize,
        vars: &mut HashMap<String, MacroVarRef>,
    ) -> Result<()> {
        for b in &self.content.vars {
            insert_var(
                vars,
                b.0,
                MacroVarRef {
                    depth: 1 + b.1.depth,
                    var_index_or_rep_index: *rep_index,
                    span: b.1.span,
                },
            )?;
        }
        *rep_index += 1;
        Ok(())
    }

    fn try_match_to(&self, input: &mut ParseStreamEx, m: &mut RawMatch) -> Result<MatchRep> {
        let mut ms = Vec::new();
        let mut is_next = false;
        while !input.is_empty() {
            let mut fork = input.fork();
            if is_next {
                if let Some(sep) = &self.sep {
                    sep.try_match_to(&mut fork, m)?;
                }
            }
            if let Ok(m) = self.content.try_match(&mut fork) {
                if !m.is_empty || self.sep.is_some() {
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
pub struct MacroVarRef {
    pub depth: usize,

    // if depth == 0, this is the index of `Match::vars`
    // if depth > 0, this is the index of `Match::reps`
    pub var_index_or_rep_index: usize,
    span: Span,
}
fn insert_var(vars: &mut HashMap<String, MacroVarRef>, name: &str, var: MacroVarRef) -> Result<()> {
    if vars.contains_key(name) {
        bail!(var.span, "duplicate metavariable `{name}`")
    }
    vars.insert(name.to_owned(), var);
    Ok(())
}

struct MacroMatches(Vec<MacroMatch>);

impl Parse for MacroMatches {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut ms = Vec::new();
        while !input.is_empty() {
            ms.push(input.parse()?);
        }
        Ok(Self(ms))
    }
}
impl ToTokens for MacroMatches {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for m in &self.0 {
            m.to_tokens(tokens);
        }
    }
}

impl MacroMatches {
    fn to_pattern(&self) -> Result<PatternItems> {
        PatternItems::new(
            self.0
                .iter()
                .map(|m| m.to_pattern())
                .collect::<Result<Vec<PatternItem>>>()?,
        )
    }
}

#[derive(ToTokens)]
enum MacroMatch {
    Token(LongToken),
    Matcher(MacroMatcher),
    Var(MacroVar),
    Rep(MacroRep),
}
impl MacroMatch {
    fn to_pattern(&self) -> Result<PatternItem> {
        Ok(match self {
            Self::Token(token) => PatternItem::Token(TokenPattern::new(token.clone())),
            Self::Matcher(m) => m.to_pattern()?,
            Self::Var(f) => PatternItem::Var(f.to_pattern()?),
            Self::Rep(r) => PatternItem::Rep(r.to_pattern()?),
        })
    }
}
impl Parse for MacroMatch {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(token::Bracket) || input.peek(token::Brace) || input.peek(token::Paren) {
            Ok(Self::Matcher(input.parse()?))
        } else {
            if input.peek(Token![$]) {
                if input.peek2(Ident) {
                    return Ok(Self::Var(input.parse()?));
                } else if input.peek2(token::Paren) {
                    return Ok(Self::Rep(input.parse()?));
                }
            }
            Ok(Self::Token(input.parse()?))
        }
    }
}

#[derive(Parse, ToTokens)]
struct MacroMatcher {
    #[to_tokens("(")]
    delimiter: MacroDelimiter,
    content: MacroMatches,
}
impl MacroMatcher {
    fn to_pattern(&self) -> Result<PatternItem> {
        let delimiter = to_delimiter(&self.delimiter);
        let content = self.content.to_pattern()?;
        Ok(PatternItem::Group(GroupPattern { delimiter, content }))
    }
}

#[derive(Parse, ToTokens)]
struct MacroVar {
    dollar_token: Token![$],
    name: MacroVarName,
    colon_token: Token![:],
    flag: MacroFlagSpec,
}
impl MacroVar {
    fn to_pattern(&self) -> Result<VarPattern> {
        Ok(VarPattern {
            name: self.name.to_option_string(),
            name_span: self.name.span(),
            flag: self.flag,
        })
    }
}

#[derive(Clone, Debug, Parse, ToTokens)]
enum MacroVarName {
    Underscore(#[parse(peek)] token::Underscore),
    Ident(#[parse(peek, any)] Ident),
}
impl MacroVarName {
    fn to_option_string(&self) -> Option<String> {
        match self {
            Self::Underscore(_) => None,
            Self::Ident(i) => Some(i.to_string()),
        }
    }
}

mod macro_flag_spec {
    use structmeta::{Parse, ToTokens};
    use syn::custom_keyword;

    custom_keyword!(block);
    custom_keyword!(expr);
    custom_keyword!(ident);
    custom_keyword!(item);
    custom_keyword!(lifetime);
    custom_keyword!(literal);
    custom_keyword!(meta);
    custom_keyword!(pat);
    custom_keyword!(pat_param);
    custom_keyword!(path);
    custom_keyword!(stmt);
    custom_keyword!(tt);
    custom_keyword!(ty);
    custom_keyword!(vis);

    #[derive(Copy, Clone, Debug, Parse, ToTokens)]
    pub enum MacroFlagSpec {
        Block(#[parse(peek)] block),
        Expr(#[parse(peek)] expr),
        Ident(#[parse(peek)] ident),
        Item(#[parse(peek)] item),
        Lifetime(#[parse(peek)] lifetime),
        Literal(#[parse(peek)] literal),
        Meta(#[parse(peek)] meta),
        Pat(#[parse(peek)] pat),
        PatParam(#[parse(peek)] pat_param),
        Path(#[parse(peek)] path),
        Stmt(#[parse(peek)] stmt),
        Tt(#[parse(peek)] tt),
        Ty(#[parse(peek)] ty),
        Vis(#[parse(peek)] vis),
    }
}

#[derive(Debug)]
pub struct RawMatch {
    pub vars: Vec<MatchVar>,
    pub reps: Vec<MatchRep>,
    is_empty: bool,
}
impl RawMatch {
    fn new() -> Self {
        Self {
            vars: Vec::new(),
            reps: Vec::new(),
            is_empty: true,
        }
    }
}

#[derive(Debug)]
pub struct MatchVar {
    pub tokens: TokenStream,
    pub tes_range: Range<usize>,
}

#[derive(Debug)]
pub struct MatchRep(pub Vec<RawMatch>);

#[derive(Parse, ToTokens)]
struct MacroRep {
    dollar_token: Token![$],
    #[to_tokens("(")]
    paren_token: token::Paren,
    content: MacroMatches,
    #[to_tokens(")")]
    sep: MacroRepSep,
    op: MacroRepOp,
}
impl MacroRep {
    fn to_pattern(&self) -> Result<RepPattern> {
        let content = self.content.to_pattern()?;
        Ok(RepPattern {
            content,
            sep: self.sep.0.as_ref().map(|tt| TokenPattern::new(tt.clone())),
            op: self.op,
        })
    }
}
#[derive(ToTokens, Debug)]
pub struct MacroRepSep(pub Option<LongToken>);

impl Parse for MacroRepSep {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self(if input.fork().parse::<MacroRepOp>().is_ok() {
            None
        } else {
            Some(input.parse()?)
        }))
    }
}

#[derive(Clone, Copy, Debug, Parse, ToTokens, PartialEq)]
pub enum MacroRepOp {
    ZeroOrMore(#[parse(peek)] Token![*]),
    OneOrMore(#[parse(peek)] Token![+]),
    ZeroOrOne(#[parse(peek)] Token![?]),
}

impl MacroRepOp {
    pub fn is_one_or_more(self) -> bool {
        matches!(self, Self::OneOrMore(_))
    }
    pub fn is_zero_or_one(self) -> bool {
        matches!(self, Self::ZeroOrOne(_))
    }
}
