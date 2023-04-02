fn main() -> syn::Result<()> {
    use macro_rules_rt::{Matcher, Rule, Transcriber};
    use quote::quote;
    use syn::parse2;

    let from: Matcher = parse2(quote!($e:literal))?;
    let to: Transcriber = parse2(quote!($e * 5))?;
    let rule = Rule::new(from, to)?;
    let source = quote!(1 + 2);
    let expect = quote!(1 * 5 + 2 * 5);
    let result = rule.replace_all_tokens(source);
    assert_eq!(result.to_string(), expect.to_string());
    Ok(())
}
