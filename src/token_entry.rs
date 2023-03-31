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
    parse_str,
    spanned::Spanned,
    Lifetime, Result, Token,
};

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
pub enum LongToken {
    LongPunct(LongPunct),
    TokenTree(CursorToken),
}
impl LongToken {
    pub fn len(&self) -> usize {
        match self {
            LongToken::LongPunct(p) => p.len(),
            LongToken::TokenTree(_) => 1,
        }
    }
}
impl Display for LongToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_token_stream())
    }
}

#[derive(Parse, ToTokens, Debug, Clone)]
pub enum LongTokenTree {
    Token(LongToken),
    Group(SomeGroup),
}

#[derive(Debug, Clone)]
pub enum TokenEntry {
    Token(CursorToken),
    GroupOpen { span: Span, delimiter: Delimiter },
    GroupClose { span: Span, delimiter: Delimiter },
}
impl TokenEntry {
    pub fn span(&self) -> Span {
        match self {
            TokenEntry::Token(t) => t.span(),
            TokenEntry::GroupOpen { span, .. } => *span,
            TokenEntry::GroupClose { span, .. } => *span,
        }
    }
    pub fn build(tokens: TokenStream) -> Vec<Self> {
        let mut items = Vec::new();
        TokenEntry::for_each_tokens(tokens, &mut |t| items.push(t.clone()));
        items
    }
    pub fn for_each_tokens(tokens: TokenStream, f: &mut impl FnMut(&TokenEntry)) {
        (|input: ParseStream| {
            input.step(|cursor| Ok(((), Self::for_each_cursor(*cursor, None, f))))
        })
        .parse2(tokens)
        .unwrap();
    }
    pub fn for_each_cursor<'a>(
        start: Cursor<'a>,
        end: Option<Cursor<'a>>,
        f: &mut impl FnMut(&TokenEntry),
    ) -> Cursor<'a> {
        let mut cursor = start;
        let mut fail = false;
        while !cursor.eof() && Some(cursor) != end {
            if let Some((token, next)) = cursor_token(cursor) {
                fail = false;
                f(&TokenEntry::Token(token));
                cursor = next;
                continue;
            }
            if let Some((inner, delimiter, delim_span, next)) = some_group(cursor) {
                fail = false;
                f(&TokenEntry::GroupOpen {
                    span: delim_span.open(),
                    delimiter,
                });
                Self::for_each_cursor(inner, None, f);
                f(&TokenEntry::GroupClose {
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
    pub tes_range_open: Range<usize>,
    pub tes_range_close: Range<usize>,
}

enum MaybeOwnedParseBuffer<'a> {
    Owned(ParseBuffer<'a>),
    Borrowed(&'a ParseBuffer<'a>),
}
impl<'a> std::ops::Deref for MaybeOwnedParseBuffer<'a> {
    type Target = ParseBuffer<'a>;
    fn deref(&self) -> &Self::Target {
        match self {
            Self::Owned(x) => x,
            Self::Borrowed(x) => x,
        }
    }
}

pub struct ParseStreamEx<'a> {
    input: MaybeOwnedParseBuffer<'a>,
    pub tes_offset: usize,
}
impl<'a> ParseStreamEx<'a> {
    pub fn new(input: ParseStream<'a>, tes_offset: usize) -> Self {
        Self {
            input: MaybeOwnedParseBuffer::Borrowed(input),
            tes_offset,
        }
    }
    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        self.parse_with(Parse::parse)
    }
    pub fn parse_with<T>(&mut self, parser: impl FnOnce(ParseStream) -> Result<T>) -> Result<T> {
        let mut fork = self.input.fork();
        let value = parser(&mut fork)?;
        self.tes_offset += TokenEntry::len_from_cursor(self.input.cursor(), fork.cursor());
        self.input.advance_to(&fork);
        Ok(value)
    }
    pub fn parse_group<T>(
        &mut self,
        f: impl FnOnce(GroupEx, &mut ParseStreamEx) -> Result<T>,
    ) -> Result<T> {
        let tes_range_open = self.tes_offset..self.tes_offset + 1;
        let tes_offset = self.tes_offset + 1;
        let group = self.parse()?;
        let tes_range_close = self.tes_offset - 1..self.tes_offset;
        let g = GroupEx {
            group,
            tes_range_open,
            tes_range_close,
        };
        let tokens = g.group.stream();
        Self::parse_from_tokens(tokens, tes_offset, |input| f(g, input))
    }
    pub fn fork(&self) -> Self {
        Self {
            input: MaybeOwnedParseBuffer::Owned(self.input.fork()),
            tes_offset: self.tes_offset,
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
        input: TokenStream,
        tes_offset: usize,
        f: impl FnOnce(&mut ParseStreamEx) -> Result<T>,
    ) -> Result<T> {
        (|input: ParseStream| f(&mut ParseStreamEx::new(input, tes_offset))).parse2(input)
    }

    pub fn advance_to(&mut self, fork: &ParseStreamEx) {
        self.tes_offset = fork.tes_offset;
        self.input.advance_to(&fork.input);
    }
}

#[derive(Debug)]
struct TokenOffsets(Vec<Option<usize>>);

impl TokenOffsets {
    fn new(tes: &[TokenEntry], text: &Text) -> Self {
        let mut items = Vec::new();
        let mut end_lc = LineColumn { line: 1, column: 0 };
        let mut end = 0;
        for tf in tes {
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
    tes: Vec<TokenEntry>,
    offsets: TokenOffsets,
}

impl<'a> Source<'a> {
    pub fn from_str(str: &'a str) -> Result<(Self, TokenStream)> {
        let tokens: TokenStream = parse_str(str)?;
        let text = Text::new(str);
        let tes = TokenEntry::build(tokens.clone());
        let offsets = TokenOffsets::new(&tes, &text);
        Ok((Self { text, tes, offsets }, tokens))
    }

    fn get(&self, tes_range: Range<usize>) -> (&[TokenEntry], &str, &[TokenEntry]) {
        let mut start = tes_range.start;
        let mut end = tes_range.end;
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
        (
            &self.tes[tes_range.start..start],
            s,
            &self.tes[end..tes_range.end],
        )
    }
    pub fn unchanged_tes_range_in(&self, tes_range: Range<usize>) -> Range<usize> {
        let mut start = tes_range.start;
        let mut end = tes_range.end;
        while start < end && self.offsets.0[start].is_none() {
            start += 1;
        }
        while start < end && self.offsets.0[end].is_none() {
            end -= 1;
        }
        start..end
    }

    pub fn get_source(&self, tes_range: Range<usize>) -> &str {
        let s = self.get(tes_range);
        assert!(s.0.is_empty());
        assert!(s.2.is_empty());
        s.1
    }
}

pub struct TokenStringBuilder<'a> {
    source: &'a Source<'a>,
    pub s: String,
}

impl<'a> TokenStringBuilder<'a> {
    pub fn new(source: &'a Source<'a>) -> Self {
        Self {
            source,
            s: String::new(),
        }
    }
    pub fn push_tes(&mut self, tes_range: Range<usize>) {
        let (fs0, s, fs1) = self.source.get(tes_range);
        self.push_tokens_by(fs0);
        self.push_str(s);
        self.push_tokens_by(fs1);
    }
    fn push_tokens_by(&mut self, fs: &[TokenEntry]) {
        let mut ts = TokenStream::new();
        for f in fs {
            match f {
                TokenEntry::Token(t) => t.to_tokens(&mut ts),
                TokenEntry::GroupOpen { delimiter, .. } => {
                    self.push_tokens(&take(&mut ts));
                    self.push_str(to_open_str(*delimiter));
                }
                TokenEntry::GroupClose { delimiter, .. } => {
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
fn is_code_space(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r')
}

pub struct FindAllStringBuilder<'a, 'b> {
    pub b: &'a mut TokenStringBuilder<'b>,
    no_match_start: usize,
    no_match_end: usize,
}

impl<'a, 'b> FindAllStringBuilder<'a, 'b> {
    pub fn new(b: &'a mut TokenStringBuilder<'b>, offset: usize) -> Self {
        Self {
            b,
            no_match_start: offset,
            no_match_end: offset,
        }
    }
    pub fn push_no_match(&mut self, tes_len: usize) {
        self.no_match_end += tes_len;
    }
    pub fn commit_no_match(&mut self, tes_len: usize) {
        self.b.push_tes(self.no_match_start..self.no_match_end);
        self.no_match_end += tes_len;
        self.no_match_start = self.no_match_end;
    }
}
