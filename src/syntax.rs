use syn::{
    parse::{discouraged::Speculative, ParseStream},
    punctuated::Punctuated,
    token::Semi,
    Expr, Pat, PatOr, Result, Stmt, Token,
};

pub fn parse_macro_pat(input: ParseStream) -> Result<Pat> {
    let mut cases = Punctuated::new();
    let leading_vert;
    if let Ok(p) = input.parse::<Token![|]>() {
        leading_vert = Some(p);
    } else {
        let pat = input.parse::<Pat>()?;
        let Ok(p) = input.parse::<Token![|]>() else {
            return Ok(pat);
        };
        leading_vert = None;
        cases.push_value(pat);
        cases.push_punct(p);
    }
    loop {
        let pat = input.parse::<Pat>()?;
        cases.push_value(pat);
        if let Ok(p) = input.parse::<Token![|]>() {
            cases.push_punct(p);
        } else {
            break;
        }
    }
    Ok(Pat::Or(PatOr {
        attrs: Vec::new(),
        leading_vert,
        cases,
    }))
}

pub fn parse_macro_stmt(input: ParseStream) -> Result<Stmt> {
    let fork = input.fork();
    if let Ok(expr) = fork.parse::<Expr>() {
        input.advance_to(&fork);
        Ok(Stmt::Semi(expr, Semi::default()))
    } else {
        input.parse::<Stmt>()
    }
}
