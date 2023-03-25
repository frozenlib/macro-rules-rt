use proc_macro2::Delimiter;
use std::{
    cmp::{max, min},
    mem::take,
    ops::Range,
};
use syn::{
    parse::{discouraged::Speculative, ParseStream},
    token, Expr, MacroDelimiter, Result, Stmt,
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
