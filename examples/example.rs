fn main() -> syn::Result<()> {
    use macro_rules_rt::Rule;

    let from = "$e:literal".parse()?;
    let to = "$e * 5".parse()?;
    let rule = Rule::new(from, to)?;
    let source = "1 + 2";
    let expect = "1 * 5 + 2 * 5";
    let result = rule.replace_all(source)?;
    assert_eq!(result, expect);
    Ok(())
}
