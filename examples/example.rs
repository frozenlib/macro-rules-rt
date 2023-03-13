fn main() -> syn::Result<()> {
    use macro_rules_rt::{Matcher, Rule, Transcriber};
    use quote::quote;

    let from = Matcher::from_token_stream(quote!($e:literal))?;
    let to = Transcriber::from_token_stream(quote!($e * 5))?;
    let rule = Rule::new(from, to)?;
    let source = quote!(1 + 2);
    let result = rule.replace_all(source);
    assert_eq!(result.to_string(), quote!(1 * 5 + 2 * 5).to_string());
    Ok(())
}
