use macro_rules_rt::{Matcher, Rule, Transcriber};
use proc_macro2::TokenStream;
use syn::{parse::Parser, parse2, parse_str};

#[track_caller]
fn check(from: &str, to: &str, input: &str, expect: &str) {
    check_as(from, to, input, expect, false);
}

#[track_caller]
fn check_as(from: &str, to: &str, input: &str, expect: &str, nest: bool) {
    let msg = format!(
        "
from  = {from},
to    = {to}
input = {input}"
    );
    {
        let from: Matcher = from.parse().unwrap();
        let to: Transcriber = to.parse().unwrap();
        let rule = Rule::new(from, to).unwrap();
        let rule = rule.nest(nest);
        let actual = rule.replace_all(input).unwrap();
        assert_eq!(actual, expect, "replace_all str {msg}");
    }
    {
        let from: TokenStream = parse_str(from).unwrap();
        let from: Matcher = parse2(from).unwrap();
        let to: TokenStream = parse_str(to).unwrap();
        let to = Transcriber::parse.parse2(to).unwrap();
        let rule = Rule::new(from, to).unwrap();
        let rule = rule.nest(nest);
        let actual = rule.replace_all(input).unwrap();
        assert_eq!(actual, expect, "replace_all tokens {msg}");

        let input: TokenStream = input.parse().unwrap();
        let expect: TokenStream = expect.parse().unwrap();
        let actual = rule.replace_all_tokens(input);
        assert_eq!(
            actual.to_string(),
            expect.to_string(),
            "replace_all_tokens {msg}"
        );
    }
}

#[test]
fn single_line_1() {
    check("+", "-", "1 + 2 + 3", "1 - 2 - 3");
    check("+", "-", "+2+3", "- 2 - 3");
}

#[test]
fn single_line_2() {
    check(
        "$($e:ident)*",
        "$($e @)*",
        "1+2 a b c 3+4",
        "1+2 a @ b @ c @ 3+4",
    );
}

#[test]
fn var() {
    check("$x:ident", "b", "a", "b");
}

#[test]
fn var_to() {
    check("$x:ident", "@ $x @", "a", "@ a @");
    check("$x:ident", "@ $x @", "1 a", "1 @ a @");
}

#[test]
fn lf() {
    check("+", "-", "1\n2+3;\n4", "1\n2 - 3;\n4");
}

#[test]
fn cr() {
    check("+", "-", "1\r2+3;\r4", "1\r2 - 3;\r4");
}

#[test]
fn cr_lf() {
    check("+", "-", "1\r\n2+3\r\n4", "1\r\n2 - 3\r\n4");
}

#[test]
fn non_ascii() {
    check("+", "-", "\"あ\"+2+3", "\"あ\" - 2 - 3");
}

#[test]
fn replace_middle() {
    check("+", "-", "1+2", "1 - 2");
}

#[test]
fn replace_end() {
    check("+", "-", "1+", "1 -");
}

#[test]
fn var_expr() {
    check("$e:expr", "$e @", "1", "1 @");
    check("$e:expr", "$e @", "1 + 2", "1 + 2 @");
    check("$e:expr", "$e @", "(1 + 2)", "(1 + 2) @");
    check(
        "$e:expr",
        "$e @",
        "(1 + 2) + (3 + 4)",
        "(1 + 2) + (3 + 4) @",
    );
}

#[test]
fn tt() {
    check("$x:tt", "a", "1 2", "a a");
}

#[test]
fn doc_comment_no_panic() {
    check("$x:tt", "", "///\nstruct X;", "");
}
#[test]
fn doc_comment_save() {
    check("a", "a", "/// xxx\nstruct X;", "/// xxx\nstruct X;");
}
#[test]
fn doc_comment_attr() {
    check("doc =", "xxx =", "/// abc", "# [ xxx = \" abc\" ]");
}

#[test]
fn group() {
    check("(a)", "a @@", "(a)", "a @@");
    check("[a]", "a @@", "[a]", "a @@");
    check("{a}", "a @@", "{a}", "a @@");
}

#[test]
fn group_mismatch_delimiter() {
    check("(a)", "a @@", "[a]", "[a]");
    check("[a]", "a @@", "(a)", "(a)");
}

#[test]
fn group_with_var() {
    check("($x:ident)", "$x @@", "(a)", "a @@");
    check("[$x:ident]", "$x @@", "[a]", "a @@");
    check("{$x:ident}", "$x @@", "{a}", "a @@");
}

#[test]
fn group_with_var_mismatch_delimiter() {
    check("($x:ident)", "$x @@", "[a]", "[a]");
    check("[$x:ident]", "$x @@", "(a)", "(a)");
}

#[test]
fn in_group() {
    check("a", "@@", "[a]", "[ @@ ]");
}

#[test]
fn in_group_with_var() {
    check("$x:ident", "$x @@", "[a]", "[ a @@ ]");
}

#[test]
fn keep_spacing_no_match() {
    check("a", "b", "1+2 a 3 + 4", "1+2 b 3 + 4");
}

#[test]
fn keep_spacing_var() {
    check("$e:expr", "$e @@", "1+2", "1+2 @@");
}
#[test]
fn spacing_mismatch() {
    check("=", "@@", "==", "==");
}
#[test]
fn spacing_match() {
    check("==", "@@", "==", "@@");
}

#[test]
fn tt_joint() {
    check("$x:tt", "@@", "==", "@@");
}

#[test]
fn repeat_separator() {
    check("$(a),*", "@", "a,a,a", "@");
}

#[test]
fn repeat_separator_joint() {
    check("$(a)==*", "@", "a==a==a", "@");
}

#[test]
fn rep_empty_content_and_some_sep() {
    check("$(),*", "@", ",,", "@");
}

#[test]
fn keyword_self() {
    check("self", "this", "self", "this");
}

#[test]
fn nest_no() {
    check_as(
        "a + $e:expr",
        "b + $e",
        "a + ( a + x )",
        "b + ( a + x )",
        false,
    );
}

#[test]

fn nest_yes() {
    check_as(
        "a + $e:expr",
        "b + $e",
        "a + ( a + x )",
        "b + ( b + x )",
        true,
    );
}
