use std::{
    env::current_dir,
    fs,
    process::Command,
    str::{from_utf8, FromStr},
};

use macro_rules_rt::{Matcher, Rule, Transcriber};
use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};
use proptest::{
    collection::{self},
    prelude::Arbitrary,
    prop_oneof,
    sample::select,
    strategy::{BoxedStrategy, Strategy},
};
use quote::{quote, ToTokens};
use syn::parse2;
use test_strategy::{proptest, Arbitrary};

#[ignore]
#[proptest(cases = 4096)]

fn proptest_apply(arg: TestArg) {
    let _ = check(arg);
}
fn tts() -> impl Strategy<Value = TokenStream> {
    token_stream_strategy(10, 100, 20, 20)
}

#[ignore]
#[test]
fn test_0() {
    let _ = check(TestArg::new(quote!(A), quote!(A), quote!(...)).unwrap());
}

#[derive(Arbitrary)]

struct TokenStreamEx(#[strategy(tts())] TokenStream);

impl std::fmt::Debug for TokenStreamEx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}

struct TestArg {
    from: TokenStream,
    to: TokenStream,
    rule: Rule,
    input: TokenStream,
}

impl TestArg {
    fn new(from: TokenStream, to: TokenStream, input: TokenStream) -> syn::Result<Self> {
        let rule = Rule::new(parse2(from.clone())?, parse2(to.clone())?)?;
        Ok(Self {
            from,
            to,
            rule,
            input,
        })
    }
}
impl Arbitrary for TestArg {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        let from = tts().prop_filter_map("matcher", |tokens| {
            if let Ok(m) = parse2::<Matcher>(tokens.clone()) {
                Some((m, tokens))
            } else {
                None
            }
        });
        let to = tts().prop_filter_map("transcriber", |tokens| {
            if let Ok(t) = parse2::<Transcriber>(tokens.clone()) {
                Some((t, tokens))
            } else {
                None
            }
        });
        let input = tts();
        (from, to, input)
            .prop_filter_map("rule", |((from, from_tokens), (to, to_tokens), input)| {
                if let Ok(rule) = Rule::new(from, to) {
                    Some(TestArg {
                        from: from_tokens,
                        to: to_tokens,
                        rule,
                        input,
                    })
                } else {
                    None
                }
            })
            .boxed()
    }
}
impl std::fmt::Debug for TestArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            from, to, input, ..
        } = self;
        writeln!(f)?;
        writeln!(f, "from  = {from}")?;
        writeln!(f, "to    = {to}")?;
        writeln!(f, "input = {input}")?;
        Ok(())
    }
}

fn check(arg: TestArg) -> anyhow::Result<()> {
    let mut p = current_dir()?;
    p.push("tests");
    p.push("proptest");
    if !p.is_dir() {
        return Ok(());
    }
    let TestArg {
        from,
        to,
        input,
        rule,
    } = arg;
    let result = rule.apply(input.clone());

    let mut main_src = p.clone();
    main_src.push("src");
    main_src.push("main.rs");
    let code = format!(
        r"
#[macro_use]
mod utils;
fn main() {{
    apply!(
        ( {from} ),
        ( {to} ),
        ( {input} ),
    );
}}",
    );
    fs::write(main_src, code)?;
    let output = Command::new("cargo").arg("run").current_dir(p).output()?;

    let stdout = from_utf8(&output.stdout)?;
    // let stderr = from_utf8(&output.stderr)?;

    let expect = if let Ok(tokens) = TokenStream::from_str(stdout) {
        tokens
    } else {
        anyhow::bail!("failed to parse stdout");
    };
    match (output.status.success(), result) {
        (true, Ok(result)) => {
            let result = unraw_tokens(result).to_string();
            let expect = unraw_tokens(expect).to_string();
            if result != expect {
                panic!("\n==========\nmismatch result\nresult: {result}\nexpect: {expect}\n")
            }
        }
        (true, Err(e)) => {
            panic!("\n==========\nexpected success, but got error.\nexpect: {expect}\nerror:  {e}")
        }
        // (false, Ok(result)) => {
        //     panic!("\n==========\nexpected error, but got success.\nstderr: {stderr}\nresult: {result}\n");
        // }
        // (false, Err(_)) => {}
        (false, _) => {}
    }
    Ok(())
}

fn token_stream_strategy(
    depth: u32,
    desired_size: u32,
    expected_branch_size: u32,
    items_size: usize,
) -> impl Strategy<Value = TokenStream> {
    token_leaf_strategy()
        .prop_recursive(depth, desired_size, expected_branch_size, move |s| {
            token_list_strategy(items_size, s)
        })
        .prop_map(expand_none_group)
}

fn token_leaf_strategy() -> impl Strategy<Value = TokenTree> {
    prop_oneof![
        ident_strategy().prop_map(TokenTree::Ident),
        keyword_strategy().prop_map(TokenTree::Ident),
        punct_strategy(),
        literal_strategy(),
    ]
}
fn token_list_strategy(
    items_size: usize,
    s: impl Strategy<Value = TokenTree>,
) -> impl Strategy<Value = TokenTree> {
    static DELIMITERS: &[Delimiter] = &[
        Delimiter::Parenthesis,
        Delimiter::Brace,
        Delimiter::Bracket,
        Delimiter::None,
    ];

    (select(DELIMITERS), collection::vec(s, 0..items_size + 1)).prop_map(|(delimiter, items)| {
        TokenTree::Group(Group::new(delimiter, items.into_iter().collect()))
    })
}

fn ident_strategy() -> impl Strategy<Value = Ident> {
    ("[_a-zA-Z][_a-zA-Z0-9]{0,5}").prop_map(|s| Ident::new(&s, Span::call_site()))
}
fn keyword_strategy() -> impl Strategy<Value = Ident> {
    let keywords: &[&str] = &[
        "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn",
        "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref",
        "return", "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe",
        "use", "where", "while", "async", "await", "dyn", "abstract", "become", "box", "do",
        "final", "macro", "override", "priv", "typeof", "unsized", "virtual", "yield", "try",
        "union", "static", "dyn",
    ];
    select(keywords).prop_map(|s| Ident::new(s, Span::call_site()))
}

fn punct_strategy() -> impl Strategy<Value = TokenTree> {
    let mut s = Vec::new();
    s.push(quote!(+));
    s.push(quote!(+=));
    s.push(quote!(&));
    s.push(quote!(&&));
    s.push(quote!(&=));
    s.push(quote!(@));
    s.push(quote!(!));
    s.push(quote!(^));
    s.push(quote!(^=));
    s.push(quote!(:));
    s.push(quote!(::));
    s.push(quote!(,));
    s.push(quote!(/));
    s.push(quote!(/=));
    s.push(quote!($));
    s.push(quote!(.));
    s.push(quote!(..));
    s.push(quote!(...));
    s.push(quote!(..=));
    s.push(quote!(=));
    s.push(quote!(==));
    s.push(quote!(>=));
    s.push(quote!(>));
    s.push(quote!(<=));
    s.push(quote!(<));
    s.push(quote!(*=));
    s.push(quote!(!=));
    s.push(quote!(|));
    s.push(quote!(|=));
    s.push(quote!(||));
    s.push(quote!(#));
    s.push(quote!(?));
    s.push(quote!(->));
    s.push(quote!(<-));
    s.push(quote!(%));
    s.push(quote!(%=));
    s.push(quote!(=>));
    s.push(quote!(;));
    s.push(quote!(<<));
    s.push(quote!(<<=));
    s.push(quote!(>>));
    s.push(quote!(>>=));
    s.push(quote!(*));
    s.push(quote!(-));
    s.push(quote!(-=));
    s.push(quote!(~));
    select(s).prop_map(|s| TokenTree::Group(Group::new(Delimiter::None, s)))
}
fn literal_strategy() -> impl Strategy<Value = TokenTree> {
    let mut lits = Vec::new();
    lits.push(quote!('a'));
    lits.push(quote!("abc"));
    lits.push(quote!(r"abc"));
    lits.push(quote!(b'a'));
    lits.push(quote!(b"abc"));
    lits.push(quote!(br"abc"));
    lits.push(quote!(10));
    lits.push(quote!(10usize));
    lits.push(quote!(0b01));
    lits.push(quote!(0o11));
    lits.push(quote!(0xff));
    lits.push(quote!(1.5));
    lits.push(quote!(1.5f32));
    lits.push(quote!(12E+99_f64));
    lits.push(quote!(true));
    lits.push(quote!(false));
    select(lits).prop_map(|s| TokenTree::Group(Group::new(Delimiter::None, s)))
}

fn expand_none_group(tree: TokenTree) -> TokenStream {
    match tree {
        TokenTree::Group(group) if group.delimiter() == Delimiter::None => group.stream(),
        TokenTree::Group(group) => {
            let mut stream = TokenStream::new();
            for tree in group.stream() {
                stream.extend(expand_none_group(tree));
            }
            let delimiter = group.delimiter();
            TokenTree::Group(Group::new(delimiter, stream)).into()
        }
        tree => tree.into(),
    }
}

fn unraw_tokens(tokens: TokenStream) -> TokenStream {
    let mut ts = TokenStream::new();
    for tree in tokens {
        unraw_tree(tree).to_tokens(&mut ts);
    }
    ts
}

fn unraw_tree(tree: TokenTree) -> TokenTree {
    match tree {
        TokenTree::Ident(ident) => unraw_ident(ident).into(),
        TokenTree::Group(group) => {
            let delimiter = group.delimiter();
            let stream = unraw_tokens(group.stream());
            TokenTree::Group(Group::new(delimiter, stream))
        }
        tree => tree,
    }
}
fn unraw_ident(ident: Ident) -> Ident {
    let mut s = ident.to_string();
    if s.starts_with("r#") {
        s = s[2..].to_string();
    }
    Ident::new(&s, ident.span())
}
