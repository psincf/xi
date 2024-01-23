use super::lexer::Token;
use super::parser::Ast;

use std::io::Read;

pub struct FmtToken<'a>(pub &'a Vec<Token>);

impl<'a> std::fmt::Display for FmtToken<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for t in self.0 {
            writeln!(f, "{:?}", t).unwrap();
        }
        return Ok(())
    }
}

impl<'a> std::fmt::Debug for FmtToken<'a> {
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

pub fn get_file_string(path: &str) -> String {
    let mut file = std::fs::File::open(path).unwrap();

    let mut file_string = String::new();
    file.read_to_string(&mut file_string).unwrap();
    
    return file_string
}