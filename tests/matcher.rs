use macro_rules_rt::Matcher;
use proc_macro2::TokenStream;
use quote::quote;

macro_rules! check_matcher {
    { $($input:tt)* } => {
        check_matcher_raw(quote!($($input)*))
    };
}
fn check_matcher_raw(ts: TokenStream) {
    Matcher::from_token_stream(ts.clone()).unwrap_or_else(|_| panic!("{}", ts.to_string()));
}

#[test]
fn matcher_new() {
    check_matcher!(1);
    check_matcher!($t:expr);
}
