use macro_rules_rt::Pattern;
use proc_macro2::TokenStream;
use quote::quote;

macro_rules! check_pattern {
    { $($input:tt)* } => {
        check_pattern_raw(quote!($($input)*))
    };
}
fn check_pattern_raw(ts: TokenStream) {
    Pattern::new(ts.clone()).unwrap_or_else(|_| panic!("{}", ts.to_string()));
}

#[test]
fn pattern_new() {
    check_pattern!(1);
    check_pattern!($t:expr);
}
