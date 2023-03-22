use macro_rules_rt::{Matcher, Rule, Transcriber};
use proc_macro2::TokenStream;
use quote::quote;
use syn::parse2;

macro_rules! check { {
        { $($from:tt)* },
        { $($to:tt)* },
        { $($input:tt)* },
    } => {
        macro_rules! mr {
            { $($from)* } => { quote!($($to)*) };
        }
        check_eq(quote!($($from)*), quote!($($to)*), quote!($($input)*), mr! { $($input)* })
    };
}
#[track_caller]
fn check_eq(from: TokenStream, to: TokenStream, input: TokenStream, expected: TokenStream) {
    let from_str = from.to_string();
    let to_str = to.to_string();
    let input_str = input.to_string();
    let from = parse2::<Matcher>(from).expect("invalid matcher.");
    let to = parse2::<Transcriber>(to).expect("invalid transcriber.");
    let rule = Rule::new(from, to).expect("new rule failed.");
    let actual = rule.replace_all(input).to_string();
    let expected = expected.to_string();
    assert_eq!(
        actual, expected,
        "
from  = {from_str},
to    = {to_str}
input = {input_str}"
    );
}

#[test]
fn simple() {
    check! {
        { a },
        { b },
        { a },
    };
    check! {
        { $a:expr },
        { $a },
        { 1 },
    };
    check! {
        { [$a:expr] },
        { $a @@ },
        { [1] },
    };
}

#[test]
fn metavariable_item() {
    check! {
        { $a:item },
        { $a @@ },
        { fn func() {} },
    };
}

#[test]
fn metavariable_block() {
    check! {
        { $a:block },
        { $a @@ },
        { { 1 } },
    };
}

#[test]
fn metavariable_stmt() {
    check! {
        { $a:stmt },
        { $a @@ },
        { let x = 10 },
    };
}

#[test]
fn metavariable_pat_param() {
    check! {
        { $a:pat_param },
        { $a @@ },
        { (_, x) },
    };
}

#[test]
fn metavariable_pat() {
    check! {
        { $a:pat },
        { $a @@ },
        { (_, x) },
    };

    check! {
        { $a:pat },
        { $a @@ },
        { (_, x) | y },
    };
}

#[test]
fn metavariable_expr() {
    check! {
        { $a:expr },
        { $a @@ },
        { 1 },
    };
}

#[test]
fn metavariable_ty() {
    check! {
        { $a:ty },
        { $a @@ },
        { Option<u8> },
    };
}

#[test]
fn metavariable_ident() {
    check! {
        { $a:ident },
        { $a @@ },
        { x },
    };
}

#[test]
fn metavariable_path() {
    check! {
        { $a:path },
        { $a @@ },
        { std::option::Option },
    };
}

#[test]
fn metavariable_tt() {
    check! {
        { $a:tt },
        { $a @@ },
        { @ },
    };
}

#[test]
fn metavariable_meta() {
    check! {
        { $a:meta },
        { $a @@ },
        { derive(Debug) },
    };
}

#[test]
fn metavariable_lifetime() {
    check! {
        { $a:lifetime },
        { $a @@ },
        { 'a },
    };
}

#[test]
fn metavariable_vis() {
    check! {
        { $a:vis },
        { $a @@ },
        { pub },
    };
}

#[test]
fn metavariable_literal() {
    check! {
        { $a:literal },
        { $a @@ },
        { "abc" },
    };
}

#[test]
fn repeat_zero_or_more() {
    check! {
        { $($a:ident)* },
        { $($a),* @@ },
        { a b c },
    };
    check! {
        { $($a:ident),* },
        { $($a)* @@ },
        { a, b, c },
    };
}
#[test]
fn repeat_zero_or_more_fail() {
    check_eq(
        quote!($($a:ident)*@),
        quote!($($a)*@@),
        quote!(@),
        quote!(@@),
    );
    check_eq(
        quote!($($a:ident)*@),
        quote!($($a)*@@),
        quote!(a@),
        quote!(a@@),
    );
    check_eq(
        quote!($($a:ident)*@),
        quote!($($a)*@@),
        quote!(a a@),
        quote!(a a@@),
    );
}

#[test]
fn repeat_one_or_more() {
    check! {
        { $($a:ident)+ },
        { $($a),+ @@ },
        { a b c },
    };
    check! {
        { $($a:ident),+ },
        { $($a)+ @@ },
        { a, b, c },
    };
}

#[test]
fn repeat_one_or_more_fail() {
    check_eq(
        quote!($($a:ident)+@),
        quote!($($a)+@@),
        quote!(@),
        quote!(@),
    );
    check_eq(
        quote!($($a:ident)+@),
        quote!($($a)+@@),
        quote!(a@),
        quote!(a@@),
    );
    check_eq(
        quote!($($a:ident)*@),
        quote!($($a)*@@),
        quote!(a a@),
        quote!(a a@@),
    );
}

#[test]
fn repeat_zero_or_one() {
    check! {
        { $($a:ident)? },
        { $($a)? @@ },
        { a },
    };
}

#[test]
fn repeat_zero_or_one_fail() {
    check_eq(
        quote!($($a:ident)?@),
        quote!($($a)?@@),
        quote!(@),
        quote!(@@),
    );
    check_eq(
        quote!($($a:ident)?@),
        quote!($($a)?@@),
        quote!(a@),
        quote!(a@@),
    );
    check_eq(
        quote!($($a:ident)?@),
        quote!($($a)?@@),
        quote!(a a@),
        quote!(a a@@),
    );
}

#[test]
fn repeat_and_token_first() {
    check! {
        { $($a:ident)* },
        { $(@ $a)* @},
        { a b },
    };
}

#[test]
fn repeat_metavariable_2() {
    check! {
        { $($a:ident : $b:ty),+ },
        { $($a: $b),+ @@ },
        { a: u8, b: u16 },
    };
}

#[test]
fn group() {
    check! {
        { ($a:ident) },
        { ($a) @@ },
        { (a) },
    };
}

#[test]
fn empty_group() {
    check! {
        { () @ },
        { @@ },
        { () @ },
    };
}

#[test]
fn non_empty_group() {
    check! {
        { (a) @ },
        { @@ },
        { (a) @ },
    };
}

#[test]
fn repeat_empty_group() {
    check! {
        { $(())* @ },
        { @@ },
        { ()() @ },
    };
}

#[test]
fn to_token() {
    check! {
        { $a:ident },
        { $a @@ },
        { a },
    };
}

#[test]
fn nested_rep() {
    check! {
        { $($($a:ident),+)@+ },
        { $($($a),+)@+ @@ },
        { a,b@c,d },
    };
}
