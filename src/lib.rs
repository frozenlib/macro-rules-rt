#![warn(clippy::redundant_pub_crate)]

use matcher::MatchTokensBuilder;
use proc_macro2::TokenStream;
use syn::Result;
use token_entry::{FindAllStringBuilder, ParseStreamEx, Source, TokenStringBuilder};

pub use matcher::Matcher;
pub use transcriber::Transcriber;

#[macro_use]
mod utils;

mod matcher;
mod text;
mod token_entry;
mod transcriber;

/// Pair [`Matcher`] and [`Transcriber`].
#[derive(Clone, Debug)]
pub struct Rule {
    from: Matcher,
    to: Transcriber,
    nest: bool,
}

impl Rule {
    /// Create a new `Rule` from `Matcher` and `Transcriber`.
    ///
    /// Returns an error if the meta-variables of `Matcher` and `Transcriber` do not match.
    pub fn new(from: Matcher, mut to: Transcriber) -> Result<Self> {
        to.attach(&from.0)?;
        Ok(Rule {
            from,
            to,
            nest: false,
        })
    }
    /// Specifies whether to apply replacements to metavariable matches. (default is false.)
    ///
    /// If false, only the outermost matched range is replaced.
    ///
    /// If true, further substitutions are made for the range matched by meta-variables such as `$e:expr`.
    ///
    /// ```rust
    /// use macro_rules_rt::Rule;
    ///
    /// let from = "a + $e:expr".parse()?;
    /// let to   = "b + $e".parse()?;
    /// let input = "a + a + x";
    /// let rule = Rule::new(from, to)?;
    /// let r_nest_no = rule.clone().replace_all_str(input)?;
    /// let r_nest_yes = rule.nest(true).replace_all_str(input)?;
    /// assert_eq!(r_nest_no,  "b + a + x");
    /// assert_eq!(r_nest_yes, "b + b + x");
    /// # Ok::<(), syn::Error>(())
    /// ```
    pub fn nest(self, yes: bool) -> Self {
        Self { nest: yes, ..self }
    }

    /// Replaces all non-overlapping matches in `input` with the provided transcriber.
    pub fn replace_all(&self, input: TokenStream) -> TokenStream {
        self.from
            .find_all(input.clone(), 0)
            .apply_tokens(&mut 0, input, self)
    }

    /// Replaces all non-overlapping matches in input with the provided transcriber.
    ///
    /// Unlike creating `TokenStream` from `str` and then calling [`Rule::replace_all`],
    /// the original string is preserved as much as possible.
    pub fn replace_all_str(&self, input: &str) -> Result<String> {
        let (source, input) = Source::from_str(input)?;
        let mut b = TokenStringBuilder::new(&source);
        self.from
            .find_all(input, 0)
            .apply_string(self, &mut FindAllStringBuilder::new(&mut b, 0));
        Ok(b.s)
    }

    /// If the entire `input` matches the entire `from`, do the conversion. Otherwise, return an error.
    pub fn apply(&self, input: TokenStream) -> Result<TokenStream> {
        ParseStreamEx::parse_from_tokens(input, 0, |input: &mut ParseStreamEx| {
            self.apply_parser(input)
        })
    }
    fn apply_parser(&self, input: &mut ParseStreamEx) -> Result<TokenStream> {
        let m = self.from.try_match(input)?;
        let mut tokens = TokenStream::new();
        let mut b = MatchTokensBuilder {
            tokens: &mut tokens,
            rule: self,
            tes_len: 0,
        };
        self.to.apply_tokens_to(&m, &mut b);
        Ok(tokens)
    }
}
