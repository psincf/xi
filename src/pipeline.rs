use std::io::Read;
use std::io::Write;

use crate::lexer::Token;
use super::lexer::Lexer;
use super::parser::AstParser;

use super::utils;

#[derive(Default)]
pub struct PipelineOptions {
    pub write_tokens: bool,
    pub write_ast: bool,
}

pub struct Pipeline {
    lex_tokens: Vec<Token>,
    file_path: String,
    options: PipelineOptions,
}

impl Pipeline {
    pub fn new(file_path: String, options: PipelineOptions) -> Self {
        Self {
            lex_tokens: Vec::new(),
            file_path,
            options
        }
    }

    pub fn solve(&mut self) {
        let file_string = self.get_file_string();
        self.lex(&file_string);
        self.parse();
    }


    pub fn get_file_string(&mut self) -> String {
        let mut file = std::fs::File::open(&self.file_path).unwrap();

        let mut file_string = String::new();
        file.read_to_string(&mut file_string).unwrap();
        
        return file_string
    }

    pub fn lex(&mut self, file_string: &String) {
        let mut lexer = Lexer::new(file_string);
        let tokens = lexer.lex();

        if self.options.write_tokens {
            let _dir = self.read_or_create_output_dir();
            let mut file = std::fs::File::create("xi_output/tokens.txt").unwrap();
            let mut string = Vec::new();

            let list_tokens = utils::FmtToken(&tokens);
            write!(&mut string, "{}", list_tokens).unwrap();
            file.write(&string).unwrap();
        }

        self.lex_tokens = lexer.tokens;
    }

    fn parse(&mut self) {
        let mut parser = AstParser::new(&self.lex_tokens);
        let result = parser.parse();
        if result.is_err() { panic!("{:?}", result); }

        if self.options.write_ast {
            let _dir = self.read_or_create_output_dir();
            let mut file = std::fs::File::create("xi_output/ast.txt").unwrap();
            let mut string = Vec::new();

            let list_tokens = utils::FmtAst(&parser.ast);
            write!(&mut string, "{}", list_tokens).unwrap();
            file.write(&string).unwrap();
        }
    }

    fn read_or_create_output_dir(&self) -> std::fs::DirEntry {
        let dirs = std::fs::read_dir("").unwrap();
        for dir in dirs {
            if dir.as_ref().unwrap().file_name() == "xi_output" { return dir.unwrap() }
        }

        std::fs::create_dir("xi_output").unwrap();

        let dirs = std::fs::read_dir("").unwrap();
        for dir in dirs {
            if dir.as_ref().unwrap().file_name() == "xi_output" { return dir.unwrap() }
        }

        panic!("Bug in pipeline")

    }
}