use syn::{
    parse::{discouraged::Speculative, ParseStream},
    Expr, Result, Stmt,
};

pub fn parse_macro_stmt(input: ParseStream) -> Result<Stmt> {
    let fork = input.fork();
    if let Ok(expr) = fork.parse::<Expr>() {
        input.advance_to(&fork);
        Ok(Stmt::Expr(expr, Some(Default::default())))
    } else {
        input.parse::<Stmt>()
    }
}
