use macro_rules_rt::{Matcher, Rule, Transcriber};
use proc_macro2::TokenStream;
use quote::quote;
use syn::parse2;

fn check(from: TokenStream, to: TokenStream, input: &str, expect: &str) {
    let from: Matcher = parse2(from).unwrap();
    let to: Transcriber = parse2(to).unwrap();
    let rule = Rule::new(from, to).unwrap();
    let actual = rule.replace_all_str(input).unwrap();
    assert_eq!(actual, expect);
}

#[test]
fn simple() {
    check(quote!(+), quote!(-), "1+2+3", "1-2-3");
    check(quote!(+), quote!(-), "+2+3", "-2-3");
    check(
        quote!($($e:ident)*),
        quote!($($e @)*),
        "1+2 a b c 3+4",
        "1+2 a @ b @ c @ 3+4",
    );
}
