use std::collections::HashMap;

use crate::{
    BindPattern, GroupPattern, MacroBindRef, PatternItem, PatternItems, RepPattern,
    TranscriberBind, TranscriberGroup, TranscriberItem, TranscriberItems, TranscriberRep,
};
use proc_macro2::{Delimiter, TokenStream, TokenTree};
use quote::ToTokens;
use structmeta::{Parse, ToTokens};
use syn::{
    ext::IdentExt,
    parse::{Parse, ParseStream},
    spanned::Spanned,
    token::{self},
    Ident, MacroDelimiter, Result, Token,
};

pub(crate) use self::macro_flag_spec::MacroFlagSpec;

pub(crate) struct MacroMatches(Vec<MacroMatch>);

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
    pub(crate) fn to_pattern(&self) -> Result<PatternItems> {
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
    Token(TokenTree),
    Matcher(MacroMatcher),
    Bind(MacroBind),
    Rep(MacroRep),
}
impl MacroMatch {
    fn to_pattern(&self) -> Result<PatternItem> {
        Ok(match self {
            Self::Token(t) => PatternItem::Token(t.to_string()),
            Self::Matcher(m) => m.to_pattern()?,
            Self::Bind(f) => PatternItem::Bind(f.to_pattern()?),
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
                    return Ok(Self::Bind(input.parse()?));
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
        if content.contains_bind_or_rep() {
            Ok(PatternItem::Group(GroupPattern { delimiter, content }))
        } else {
            Ok(PatternItem::Token(
                self.content.to_token_stream().to_string(),
            ))
        }
    }
}

#[derive(Parse, ToTokens)]
struct MacroBind {
    dollar_token: Token![$],
    name: MacroBindName,
    colon_token: Token![:],
    flag: MacroFlagSpec,
}
impl MacroBind {
    fn to_pattern(&self) -> Result<BindPattern> {
        Ok(BindPattern {
            name: self.name.to_option_string(),
            name_span: self.name.span(),
            flag: self.flag,
        })
    }
}

#[derive(Clone, Debug, Parse, ToTokens)]
pub(crate) enum MacroBindName {
    Underscore(#[parse(peek)] token::Underscore),
    Ident(#[parse(peek, any)] Ident),
}
impl MacroBindName {
    pub(crate) fn to_option_string(&self) -> Option<String> {
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
    pub(crate) enum MacroFlagSpec {
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
            sep: self.sep.0.as_ref().map(|t| t.to_string()),
            op: self.op,
        })
    }
}

#[derive(ToTokens, Debug)]
struct MacroRepSep(Option<TokenTree>);

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
pub(crate) enum MacroRepOp {
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

#[derive(Debug)]
pub(crate) struct MacroTranscriberItems(Vec<MacroTranscriberItem>);

impl Parse for MacroTranscriberItems {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut items = Vec::new();
        while !input.is_empty() {
            items.push(input.parse()?);
        }
        Ok(Self(items))
    }
}
impl ToTokens for MacroTranscriberItems {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for item in &self.0 {
            item.to_tokens(tokens);
        }
    }
}
impl MacroTranscriberItems {
    pub(crate) fn to_transcriber(
        &self,
        binds: &HashMap<String, MacroBindRef>,
    ) -> Result<TranscriberItems> {
        let mut items = Vec::new();
        for i in &self.0 {
            i.to_transcriber(binds, &mut items)?;
        }
        Ok(TranscriberItems { items })
    }
}

#[derive(ToTokens, Debug)]
enum MacroTranscriberItem {
    Token(TokenTree),
    Group(MacroTranscriberGroup),
    Bind(MacroTranscriberBind),
    Rep(MacroTranscriberRep),
}
impl MacroTranscriberItem {
    fn to_transcriber(
        &self,
        binds: &HashMap<String, MacroBindRef>,
        items: &mut Vec<TranscriberItem>,
    ) -> Result<()> {
        match self {
            Self::Token(t) => items.push(TranscriberItem::Token(t.clone())),
            Self::Group(g) => items.push(g.to_transcriber(binds)?),
            Self::Bind(b) => b.to_transcriber(binds, items),
            Self::Rep(r) => items.push(TranscriberItem::Rep(r.to_transcriber(binds)?)),
        }
        Ok(())
    }
}
impl Parse for MacroTranscriberItem {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(token::Bracket) || input.peek(token::Brace) || input.peek(token::Paren) {
            Ok(Self::Group(input.parse()?))
        } else {
            if input.peek(Token![$]) {
                if input.peek2(Ident::peek_any) {
                    return Ok(Self::Bind(input.parse()?));
                } else if input.peek2(token::Paren) {
                    return Ok(Self::Rep(input.parse()?));
                }
            }
            Ok(Self::Token(input.parse()?))
        }
    }
}

#[derive(Parse, ToTokens, Debug)]
struct MacroTranscriberGroup {
    #[to_tokens("(")]
    delimiter: MacroDelimiter,
    content: MacroTranscriberItems,
}
impl MacroTranscriberGroup {
    fn to_transcriber(&self, binds: &HashMap<String, MacroBindRef>) -> Result<TranscriberItem> {
        let content = self.content.to_transcriber(binds)?;
        let delimiter = to_delimiter(&self.delimiter);
        Ok(if content.get_bind().is_some() {
            TranscriberItem::Group(TranscriberGroup { delimiter, content })
        } else {
            TranscriberItem::Token(TokenTree::Group(proc_macro2::Group::new(
                delimiter,
                self.content.to_token_stream(),
            )))
        })
    }
}

#[derive(Parse, ToTokens, Debug)]
struct MacroTranscriberBind {
    dollar_token: Token![$],
    #[parse(any)]
    name: Ident,
}
impl MacroTranscriberBind {
    fn to_transcriber(
        &self,
        binds: &HashMap<String, MacroBindRef>,
        items: &mut Vec<TranscriberItem>,
    ) {
        let name = self.name.to_string();
        if binds.contains_key(&name) {
            items.push(TranscriberItem::Bind(TranscriberBind {
                name,
                span: self.span(),
                bind_index: usize::MAX,
            }));
        } else {
            for token in self.to_token_stream() {
                items.push(TranscriberItem::Token(token));
            }
        }
    }
}

#[derive(Parse, ToTokens, Debug)]
struct MacroTranscriberRep {
    dollar_token: Token![$],
    #[to_tokens("(")]
    paren_token: token::Paren,
    content: MacroTranscriberItems,
    #[to_tokens(")")]
    sep: MacroRepSep,
    op: MacroRepOp,
}
impl MacroTranscriberRep {
    fn to_transcriber(&self, binds: &HashMap<String, MacroBindRef>) -> Result<TranscriberRep> {
        let content = self.content.to_transcriber(binds)?;
        if let Some((bind_name, bind_span)) = content.get_bind() {
            Ok(TranscriberRep {
                rep_index: usize::MAX,
                content,
                sep: self.sep.0.as_ref().cloned(),
                op: self.op,
                span: self.span(),
                bind_name,
                bind_span,
            })
        } else {
            bail!(
                self.content.span(),
                "attempted to repeat an expression containing no syntax variables"
            );
        }
    }
}

fn to_delimiter(value: &MacroDelimiter) -> Delimiter {
    match value {
        MacroDelimiter::Paren(token::Paren { .. }) => Delimiter::Parenthesis,
        MacroDelimiter::Brace(token::Brace { .. }) => Delimiter::Brace,
        MacroDelimiter::Bracket(token::Bracket { .. }) => Delimiter::Bracket,
    }
}
