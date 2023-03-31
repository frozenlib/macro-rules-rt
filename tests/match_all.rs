use macro_rules_rt::{match_all, Rule};
use pretty_assertions::assert_eq;
use syn::{parse_str, Result};

#[derive(Clone, Debug, PartialEq)]
enum Part {
    Unchanged(String),
    Changed(Changed),
}
impl Part {
    fn from(value: match_all::Part) -> Self {
        match value {
            match_all::Part::Unchanged(u) => Self::Unchanged(u.source_str().to_string()),
            match_all::Part::Changed(c) => Self::Changed(Changed::from(c)),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Changed {
    source_str: String,
    replacement_str: String,
    replacements: Vec<Replacement>,
}
impl Changed {
    fn from(value: match_all::Changed) -> Self {
        let source_str = value.source_str().to_string();
        let replacement_str = value.replacement_str();
        let replacements = value.replacements().map(Replacement::from).collect();
        Self {
            source_str,
            replacement_str,
            replacements,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Replacement {
    Match(String),
    Transform(String),
}
impl Replacement {
    fn from(value: match_all::Replacement) -> Self {
        match value {
            match_all::Replacement::Match(m) => Self::Match(m.to_string()),
            match_all::Replacement::Transform(t) => Self::Transform(t.to_string()),
        }
    }
}

fn unchanged(source: &str) -> Part {
    Part::Unchanged(source.to_string())
}
fn changed(source: &str, replacement: &str, replacements: &[Replacement]) -> Part {
    Part::Changed(Changed {
        source_str: source.to_string(),
        replacement_str: replacement.to_string(),
        replacements: replacements.to_vec(),
    })
}
fn m(s: &str) -> Replacement {
    Replacement::Match(s.to_string())
}
fn t(s: &str) -> Replacement {
    Replacement::Transform(s.to_string())
}

#[track_caller]
fn check_raw(from: &str, to: &str, input: &str, expect: &[Part]) -> Result<()> {
    let rule = Rule::new(parse_str(from)?, parse_str(to)?)?;
    let m = rule.match_all(input)?;
    let actual: Vec<_> = m.iter().map(Part::from).collect();
    assert_eq!(actual, expect);
    Ok(())
}
#[track_caller]
fn check(from: &str, to: &str, input: &str, expect: &[Part]) {
    check_raw(from, to, input, expect).unwrap()
}

#[test]
fn test_changed() {
    let e = [changed("1+2", "@", &[m("@")])];
    check("$e:expr", "@", "1+2", &e)
}

#[test]
fn test_unchanged_and_changed() {
    let e = [
        unchanged("@@"),
        changed("1+2", "x", &[m("x")]),
        unchanged("@@"),
    ];
    check("$e:expr", "x", "@@1+2@@", &e)
}

#[test]
fn test_transform() {
    let e = [changed(
        "/// xxx",
        "# [ docx = \" xxx\" ]",
        &[t("# ["), m("docx ="), t("\" xxx\" ]")],
    )];
    check("doc = ", "docx = ", "/// xxx", &e)
}

#[test]
fn test_unchanged_and_transform() {
    let e = [
        unchanged("@@\n"),
        changed(
            "/// xxx",
            "# [ docx = \" xxx\" ]",
            &[t("# ["), m("docx ="), t("\" xxx\" ]")],
        ),
    ];
    check("doc = ", "docx = ", "@@\n/// xxx", &e)
}
