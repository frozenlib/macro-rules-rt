use crate::{
    text::Text,
    utils::{to_close_str, to_open_str},
};
use proc_macro2::{extra::DelimSpan, Delimiter, Group, LineColumn, Span, TokenStream, TokenTree};
use quote::ToTokens;
use std::{
    fmt::{Display, Formatter, Write},
    mem::take,
    ops::Range,
};
use structmeta::{Parse, ToTokens};
use syn::{
    buffer::Cursor,
    parse::{discouraged::Speculative, Parse, ParseBuffer, ParseStream, Parser, Peek},
    spanned::Spanned,
    Lifetime, Result, Token,
};

#[derive(Parse, ToTokens, Debug, Clone)]
pub enum LongPunct {
    AndAnd(Token![&&]),
    AndEq(Token![&=]),
    CaretEq(Token![^=]),
    DotDot(Token![..]),
    DotDotDot(Token![...]),
    DotDotEq(Token![..=]),
    EqEq(Token![==]),
    FatArrow(Token![=>]),
    Ge(Token![>=]),
    LArrow(Token![<-]),
    Le(Token![<=]),
    MinusEq(Token![-=]),
    Ne(Token![!=]),
    OrEq(Token![|=]),
    OrOr(Token![||]),
    PathSep(Token![::]),
    PercentEq(Token![%=]),
    PlusEq(Token![+=]),
    RArrow(Token![->]),
    Shl(Token![<<]),
    ShlEq(Token![<<=]),
    Shr(Token![>>]),
    ShrEq(Token![>>=]),
    SlashEq(Token![/=]),
    StarEq(Token![*=]),
}
impl LongPunct {
    fn len(&self) -> usize {
        match self {
            LongPunct::AndAnd(..) => 2,
            LongPunct::AndEq(..) => 2,
            LongPunct::CaretEq(..) => 2,
            LongPunct::DotDot(..) => 2,
            LongPunct::DotDotDot(..) => 3,
            LongPunct::DotDotEq(..) => 3,
            LongPunct::EqEq(..) => 2,
            LongPunct::FatArrow(..) => 2,
            LongPunct::Ge(..) => 2,
            LongPunct::LArrow(..) => 2,
            LongPunct::Le(..) => 2,
            LongPunct::MinusEq(..) => 2,
            LongPunct::Ne(..) => 2,
            LongPunct::OrEq(..) => 2,
            LongPunct::OrOr(..) => 2,
            LongPunct::PathSep(..) => 2,
            LongPunct::PercentEq(..) => 2,
            LongPunct::PlusEq(..) => 2,
            LongPunct::RArrow(..) => 2,
            LongPunct::Shl(..) => 2,
            LongPunct::ShlEq(..) => 3,
            LongPunct::Shr(..) => 2,
            LongPunct::ShrEq(..) => 3,
            LongPunct::SlashEq(..) => 2,
            LongPunct::StarEq(..) => 2,
        }
    }
}

#[derive(Parse, ToTokens, Debug, Clone)]
pub enum LongTokenTree {
    LongPunct(LongPunct),
    TokenTree(CursorTokenTree),
}
impl LongTokenTree {
    pub fn len(&self) -> usize {
        match self {
            LongTokenTree::LongPunct(p) => p.len(),
            LongTokenTree::TokenTree(_) => 1,
        }
    }
}
impl Display for LongTokenTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_token_stream())
    }
}

#[derive(ToTokens, Debug, Clone)]
pub enum CursorToken {
    Ident(proc_macro2::Ident),
    Punct(proc_macro2::Punct),
    Literal(proc_macro2::Literal),
    Lifetime(Lifetime),
}
impl Parse for CursorToken {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| {
            if let Some((token, next)) = cursor_token(*cursor) {
                Ok((token, next))
            } else {
                Err(cursor.error("expected token"))
            }
        })
    }
}
fn cursor_token(cursor: Cursor) -> Option<(CursorToken, Cursor)> {
    if let Some((ident, next)) = cursor.ident() {
        Some((CursorToken::Ident(ident), next))
    } else if let Some((punct, next)) = cursor.punct() {
        Some((CursorToken::Punct(punct), next))
    } else if let Some((lit, next)) = cursor.literal() {
        Some((CursorToken::Literal(lit), next))
    } else if let Some((lt, next)) = cursor.lifetime() {
        Some((CursorToken::Lifetime(lt), next))
    } else {
        None
    }
}

#[derive(ToTokens, Debug, Clone)]
pub struct SomeGroup(pub proc_macro2::Group);

impl Parse for SomeGroup {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| {
            if let Some((inside, delimiter, delim_span, next)) = some_group(*cursor) {
                let mut group = Group::new(delimiter, inside.token_stream());
                group.set_span(delim_span.span());
                Ok((Self(group), next))
            } else {
                Err(cursor.error("expected group"))
            }
        })
    }
}
fn some_group(cursor: Cursor) -> Option<(Cursor, Delimiter, DelimSpan, Cursor)> {
    for delimiter in [Delimiter::Parenthesis, Delimiter::Brace, Delimiter::Bracket] {
        if let Some((inside, delim_span, next)) = cursor.group(delimiter) {
            return Some((inside, delimiter, delim_span, next));
        }
    }
    None
}

#[derive(Parse, ToTokens, Debug, Clone)]
pub enum CursorTokenTree {
    Token(CursorToken),
    Group(SomeGroup),
}

pub fn cts_len(start: Cursor, end: Cursor) -> usize {
    let mut cursor = start;
    let mut len = 0;
    let mut fail = false;
    while cursor != end {
        if let Some((_, next)) = cursor_token(cursor) {
            fail = false;
            cursor = next;
            len += 1;
            continue;
        }
        if let Some((_, _, _, next)) = some_group(cursor) {
            fail = false;
            cursor = next;
            len += 1;
            continue;
        }
        if fail {
            panic!("cts_len failed");
        }
        fail = true;
    }
    len
}

#[derive(Debug, Clone)]
pub enum TokenFragment {
    Token(CursorToken),
    GroupOpen { span: Span, delimiter: Delimiter },
    GroupClose { span: Span, delimiter: Delimiter },
}
impl TokenFragment {
    pub fn span(&self) -> Span {
        match self {
            TokenFragment::Token(t) => t.span(),
            TokenFragment::GroupOpen { span, .. } => *span,
            TokenFragment::GroupClose { span, .. } => *span,
        }
    }
    pub fn build(tokens: TokenStream) -> Vec<Self> {
        let mut items = Vec::new();
        TokenFragment::for_each_tokens(tokens, &mut |t| items.push(t.clone()));
        items
    }
    pub fn for_each_tokens(tokens: TokenStream, f: &mut impl FnMut(&TokenFragment)) {
        (|input: ParseStream| {
            input.step(|cursor| Ok(((), Self::for_each_cursor(*cursor, None, f))))
        })
        .parse2(tokens)
        .unwrap();
    }
    pub fn for_each_cursor<'a>(
        start: Cursor<'a>,
        end: Option<Cursor<'a>>,
        f: &mut impl FnMut(&TokenFragment),
    ) -> Cursor<'a> {
        let mut cursor = start;
        let mut fail = false;
        while !cursor.eof() && Some(cursor) != end {
            if let Some((token, next)) = cursor_token(cursor) {
                fail = false;
                f(&TokenFragment::Token(token));
                cursor = next;
                continue;
            }
            if let Some((inner, delimiter, delim_span, next)) = some_group(cursor) {
                fail = false;
                f(&TokenFragment::GroupOpen {
                    span: delim_span.open(),
                    delimiter,
                });
                Self::for_each_cursor(inner, None, f);
                f(&TokenFragment::GroupClose {
                    span: delim_span.close(),
                    delimiter,
                });
                cursor = next;
                continue;
            }
            if fail {
                if let Some((tt, next)) = cursor.token_tree() {
                    if let TokenTree::Group(group) = &tt {
                        if group.delimiter() == Delimiter::None && next.token_stream().is_empty() {
                            break;
                        }
                    }
                    let stream = next.token_stream();
                    panic!("TokenFragmenet traverse failed\nstream = {stream}\ntt = {tt:?}");
                } else {
                    break;
                }
            }
            fail = true;
        }
        cursor
    }
    pub fn len_from_cursor<'a>(start: Cursor<'a>, end: Cursor<'a>) -> usize {
        let mut count = 0;
        Self::for_each_cursor(start, Some(end), &mut |_| count += 1);
        count
    }
}

pub struct GroupEx {
    pub group: Group,
    pub tfs_range_open: Range<usize>,
    pub tfs_range_close: Range<usize>,
}

enum CowParseBuffer<'a> {
    Owned(ParseBuffer<'a>),
    Borrowed(&'a ParseBuffer<'a>),
}
impl<'a> std::ops::Deref for CowParseBuffer<'a> {
    type Target = ParseBuffer<'a>;
    fn deref(&self) -> &Self::Target {
        match self {
            CowParseBuffer::Owned(x) => x,
            CowParseBuffer::Borrowed(x) => x,
        }
    }
}

pub struct ParseStreamEx<'a> {
    input: CowParseBuffer<'a>,
    pub tfs_offset: usize,
}
impl<'a> ParseStreamEx<'a> {
    pub fn new(input: ParseStream<'a>, tfs_offset: usize) -> Self {
        Self {
            input: CowParseBuffer::Borrowed(input),
            tfs_offset,
        }
    }
    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        self.parse_with(Parse::parse)
    }
    pub fn parse_with<T>(&mut self, parser: impl FnOnce(ParseStream) -> Result<T>) -> Result<T> {
        let mut fork = self.input.fork();
        let value = parser(&mut fork)?;
        self.tfs_offset += TokenFragment::len_from_cursor(self.input.cursor(), fork.cursor());
        self.input.advance_to(&fork);
        Ok(value)
    }
    pub fn parse_group<T>(
        &mut self,
        f: impl FnOnce(GroupEx, &mut ParseStreamEx) -> Result<T>,
    ) -> Result<T> {
        let tfs_range_open = self.tfs_offset..self.tfs_offset + 1;
        let tfs_offset = self.tfs_offset + 1;
        let group = self.parse()?;
        let tfs_range_close = self.tfs_offset - 1..self.tfs_offset;
        let g = GroupEx {
            group,
            tfs_range_open,
            tfs_range_close,
        };
        let tokens = g.group.stream();
        Self::parse_from_tokens(tokens, tfs_offset, |input| f(g, input))
    }
    pub fn fork(&self) -> Self {
        Self {
            input: CowParseBuffer::Owned(self.input.fork()),
            tfs_offset: self.tfs_offset,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.input.is_empty()
    }
    pub fn peek(&self, token: impl Peek) -> bool {
        self.input.peek(token)
    }
    pub fn peek2(&self, token: impl Peek) -> bool {
        self.input.peek2(token)
    }
    pub fn expect(&self, token: impl Peek) -> Result<()> {
        let l = self.input.lookahead1();
        if l.peek(token) {
            Ok(())
        } else {
            Err(l.error())
        }
    }

    pub fn span(&self) -> Span {
        self.input.span()
    }
    pub fn cursor(&self) -> Cursor {
        self.input.cursor()
    }

    pub fn parse_from_tokens<T>(
        tokens: TokenStream,
        tfs_offset: usize,
        f: impl FnOnce(&mut ParseStreamEx) -> Result<T>,
    ) -> Result<T> {
        (|input: ParseStream| f(&mut ParseStreamEx::new(input, tfs_offset))).parse2(tokens)
    }

    pub fn advance_to(&mut self, fork: &ParseStreamEx) {
        self.tfs_offset = fork.tfs_offset;
        self.input.advance_to(&fork.input);
    }
}

#[derive(Debug)]
struct TokenOffsets(Vec<Option<usize>>);

impl TokenOffsets {
    fn new(tfs: &[TokenFragment], text: &Text) -> Self {
        let mut items = Vec::new();
        let mut end_lc = LineColumn { line: 1, column: 0 };
        let mut end = 0;
        for tf in tfs {
            let span = tf.span();
            let start_lc = span.start();
            let start = text.offset_of(start_lc);
            items.push(if end <= start {
                Some(if end_lc.line == start_lc.line {
                    end
                } else {
                    text.offset_of_line(end_lc.line + 1)
                })
            } else {
                None
            });
            end_lc = span.end();

            end = text.offset_of(end_lc);
        }
        items.push(Some(text.str.len()));
        items[0] = Some(0);
        Self(items)
    }
}

#[derive(Debug)]
pub struct Source<'a> {
    text: Text<'a>,
    pub tokens: TokenStream,
    tfs: Vec<TokenFragment>,
    offsets: TokenOffsets,
}

impl<'a> Source<'a> {
    pub fn new(str: &'a str, tokens: TokenStream) -> Self {
        let text = Text::new(str);
        let tfs = TokenFragment::build(tokens.clone());
        let offsets = TokenOffsets::new(&tfs, &text);
        Self {
            text,
            tfs,
            tokens,
            offsets,
        }
    }

    fn get(&self, range: Range<usize>) -> (&[TokenFragment], &str, &[TokenFragment]) {
        let mut start = range.start;
        let mut end = range.end;
        let mut start_offset = None;
        let mut end_offset = None;
        while start < end {
            start_offset = self.offsets.0[start];
            if start_offset.is_some() {
                break;
            }
            start += 1;
        }
        while start < end {
            end_offset = self.offsets.0[end];
            if end_offset.is_some() {
                break;
            }
            end -= 1;
        }
        let s = if let (Some(start_offset), Some(end_offset)) = (start_offset, end_offset) {
            &self.text.str[start_offset..end_offset]
        } else {
            ""
        };
        (&self.tfs[range.start..start], s, &self.tfs[end..range.end])
    }
}

pub struct TokenStringBuilder<'a> {
    input: &'a Source<'a>,
    pub s: String,
}

impl<'a> TokenStringBuilder<'a> {
    pub fn new(input: &'a Source<'a>) -> Self {
        Self {
            input,
            s: String::new(),
        }
    }
    pub fn push_tfs(&mut self, tfs_range: Range<usize>) {
        let (fs0, s, fs1) = self.input.get(tfs_range);
        self.push_tokens_by(fs0);
        self.push_str(s);
        self.push_tokens_by(fs1);
    }
    fn push_tokens_by(&mut self, fs: &[TokenFragment]) {
        let mut ts = TokenStream::new();
        for f in fs {
            match f {
                TokenFragment::Token(t) => t.to_tokens(&mut ts),
                TokenFragment::GroupOpen { delimiter, .. } => {
                    self.push_tokens(&take(&mut ts));
                    self.push_str(to_open_str(*delimiter));
                }
                TokenFragment::GroupClose { delimiter, .. } => {
                    self.push_tokens(&take(&mut ts));
                    self.push_str(to_close_str(*delimiter));
                }
            }
        }
        self.push_tokens(&ts);
    }
    pub fn push_tokens(&mut self, ts: &TokenStream) {
        if ts.is_empty() {
            return;
        }
        self.try_push_separator();
        write!(&mut self.s, "{ts}").unwrap();
    }
    pub fn push_str(&mut self, s: &str) {
        if let Some(c) = s.chars().next() {
            if !is_code_space(c) {
                self.try_push_separator();
            }
        } else {
            return;
        }
        self.s.push_str(s);
    }
    fn try_push_separator(&mut self) {
        if let Some(c) = self.s.chars().last() {
            if !is_code_space(c) {
                self.s.push(' ');
            }
        }
    }
}

pub struct ResultStringBuilder<'a> {
    pub b: TokenStringBuilder<'a>,
    no_match_start: usize,
    no_match_end: usize,
}

impl<'a> ResultStringBuilder<'a> {
    pub fn new(input: &'a Source<'a>) -> Self {
        Self {
            b: TokenStringBuilder::new(input),
            no_match_start: 0,
            no_match_end: 0,
        }
    }
    pub fn push_no_match(&mut self, tfs_len: usize) {
        self.no_match_end += tfs_len;
    }
    pub fn commit_no_match(&mut self, tfs_len: usize) {
        self.b.push_tfs(self.no_match_start..self.no_match_end);
        self.no_match_end += tfs_len;
        self.no_match_start = self.no_match_end;
    }
    pub fn build(mut self) -> String {
        self.commit_no_match(0);
        self.b.s
    }
}
fn is_code_space(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r')
}
