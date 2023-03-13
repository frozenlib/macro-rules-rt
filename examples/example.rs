fn main() -> syn::Result<()> {
    use macro_rules_rt::{Pattern, Rule};
    use quote::quote;

    let from = Pattern::new(quote!($e:literal))?;
    let to = Rule::new(from, quote!($e * 5))?;
    let source = quote!(1 + 2);
    let result = to.replace_all(source);
    assert_eq!(result.to_string(), quote!(1 * 5 + 2 * 5).to_string());
    Ok(())
}
