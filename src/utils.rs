use super::lexer::Token;
use super::parser::Ast;

pub struct FmtToken<'a>(pub &'a Vec<Token>);

impl<'a> std::fmt::Display for FmtToken<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for t in self.0 {
            writeln!(f, "{:?}", t).unwrap();
        }
        return Ok(())
    }
}

pub struct FmtAst<'a>(pub &'a Ast);

impl<'a> std::fmt::Display for FmtAst<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for t in self.0.inner.iter() {
            writeln!(f, "{:?}", t).unwrap();
        }
        return Ok(())
    }
}