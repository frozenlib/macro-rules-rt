use proc_macro2::{Delimiter, TokenTree};
use quote::ToTokens;
use std::{
    cmp::{max, min},
    fmt::{Display, Formatter},
    mem::take,
    ops::Range,
};
use structmeta::{Parse, ToTokens};
use syn::{
    parse::{discouraged::Speculative, ParseStream},
    token, Expr, MacroDelimiter, Result, Stmt, Token,
};

macro_rules! bail {
    ($span:expr, $message:literal $(,)?) => {
        return std::result::Result::Err(syn::Error::new($span, $message))
    };
    ($span:expr, $err:expr $(,)?) => {
        return std::result::Result::Err(syn::Error::new($span, $err))
    };
    ($span:expr, $fmt:expr, $($arg:tt)*) => {
        return std::result::Result::Err(syn::Error::new($span, std::format!($fmt, $($arg)*)))
    };
}

pub fn parse_macro_stmt(input: ParseStream) -> Result<Stmt> {
    let fork = input.fork();
    if let Ok(expr) = fork.parse::<Expr>() {
        input.advance_to(&fork);
        Ok(Stmt::Expr(expr, Some(Default::default())))
    } else {
        input.parse::<Stmt>()
    }
}

pub fn to_delimiter(value: &MacroDelimiter) -> Delimiter {
    match value {
        MacroDelimiter::Paren(token::Paren { .. }) => Delimiter::Parenthesis,
        MacroDelimiter::Brace(token::Brace { .. }) => Delimiter::Brace,
        MacroDelimiter::Bracket(token::Bracket { .. }) => Delimiter::Bracket,
    }
}

pub struct RangeBuilder(Option<Range<usize>>);

impl RangeBuilder {
    pub fn new() -> Self {
        Self(None)
    }
    pub fn push(&mut self, value: Range<usize>) {
        self.0 = if let Some(range) = &self.0 {
            Some(min(range.start, value.start)..max(range.end, value.end))
        } else {
            Some(value)
        }
    }
    pub fn take(&mut self) -> Option<Range<usize>> {
        take(&mut self.0)
    }
}

pub fn to_open_str(d: Delimiter) -> &'static str {
    match d {
        Delimiter::Parenthesis => "(",
        Delimiter::Brace => "{",
        Delimiter::Bracket => "[",
        Delimiter::None => "",
    }
}
pub fn to_close_str(d: Delimiter) -> &'static str {
    match d {
        Delimiter::Parenthesis => ")",
        Delimiter::Brace => "}",
        Delimiter::Bracket => "]",
        Delimiter::None => "",
    }
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
pub enum LongTokenTree {
    LongPunct(LongPunct),
    Token(TokenTree),
}
impl LongTokenTree {
    pub fn len(&self) -> usize {
        match self {
            LongTokenTree::LongPunct(p) => p.len(),
            LongTokenTree::Token(_) => 1,
        }
    }
}
impl Display for LongTokenTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_token_stream())
    }
}
