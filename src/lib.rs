#![warn(clippy::redundant_pub_crate)]

use proc_macro2::TokenStream;
use syn::{parse_str, Result};
use token_fragment::{ParseStreamEx, ResultStringBuilder, Source};

pub use matcher::Matcher;
pub use transcriber::Transcriber;

#[macro_use]
mod utils;

mod matcher;
mod text;
mod token_fragment;
mod transcriber;

/// Pair [`Matcher`] and [`Transcriber`].
pub struct Rule {
    from: Matcher,
    to: Transcriber,
}

impl Rule {
    pub fn new(from: Matcher, mut to: Transcriber) -> Result<Self> {
        to.attach(&from.0)?;
        Ok(Rule { from, to })
    }

    /// Replaces all non-overlapping matches in `input` with the provided transcriber.
    pub fn replace_all(&self, input: TokenStream) -> TokenStream {
        self.from
            .find_all(input.clone())
            .apply_tokens(&mut 0, &self.to, input)
    }

    /// Replaces all non-overlapping matches in input with the provided transcriber.
    ///
    /// Unlike creating `TokenStream` from `str` and then calling [`Rule::replace_all`],
    /// the original string is preserved for the parts that are not replaced.
    pub fn replace_all_str(&self, input: &str) -> Result<String> {
        let input = Source::new(input, parse_str(input)?);
        let mut b = ResultStringBuilder::new(&input);
        self.from
            .find_all(input.tokens.clone())
            .apply_string(&self.to, &mut b);
        Ok(b.build())
    }

    /// If the entire `input` matches the entire `from`, do the conversion. Otherwise, return an error.
    pub fn apply(&self, input: TokenStream) -> Result<TokenStream> {
        ParseStreamEx::parse_from_tokens(input, 0, |input: &mut ParseStreamEx| {
            self.apply_parser(input)
        })
    }
    fn apply_parser(&self, input: &mut ParseStreamEx) -> Result<TokenStream> {
        let m = self.from.try_match(input)?;
        Ok(self.to.apply_tokens(&m))
    }
}
