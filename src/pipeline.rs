use std::io::Read;
use std::io::Write;

use crate::lexer::Token;
use super::lexer::Lexer;
use super::parser::Ast;
use super::parser::AstParser;
use super::sema::SemanticAnalizer;

use super::utils;

#[derive(Default)]
pub struct PipelineOptions {
    pub write_tokens: bool,
    pub write_ast: bool,
    pub write_sym_table: bool,
    pub write_types_sema: bool,
}

pub struct Pipeline {
    lex_tokens: Vec<Token>,
    ast: Option<Ast>,
    file_path: String,
    options: PipelineOptions,
}

impl Pipeline {
    pub fn new(file_path: String, options: PipelineOptions) -> Self {
        Self {
            lex_tokens: Vec::new(),
            ast: None,
            file_path,
            options
        }
    }

    pub fn solve(&mut self) {
        let file_string = self.get_file_string();
        self.lex(&file_string);
        self.parse();
        self.sema();
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
            self.write_debug_to_file(utils::FmtToken(&tokens), "xi_output/tokens.txt");
        }

        self.lex_tokens = lexer.tokens;
    }

    fn parse(&mut self) {
        let mut parser = AstParser::new(&self.lex_tokens);
        let result = parser.parse();
        if result.is_err() { panic!("{:?}", result); }

        if self.options.write_ast {
            self.write_debug_to_file(&parser.ast, "xi_output/ast.txt");
        }

        self.ast = Some(parser.ast);
    }

    fn sema(&mut self) {
        let mut sema = SemanticAnalizer::new(self.ast.as_ref().unwrap());
        sema.sema();

        if !sema.err.is_empty() {
            panic!("{:#?}", sema.err);
        }

        if self.options.write_sym_table {
            self.write_debug_to_file(&sema.sym_tables, "xi_output/sym_table.txt");
        }

        if self.options.write_types_sema {
            self.write_debug_to_file(&sema.types, "xi_output/types_sema.txt");
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

    fn write_debug_to_file(&self, to_write: impl std::fmt::Debug, path: impl Into<String>) {
        let _dir = self.read_or_create_output_dir();
        let mut file = std::fs::File::create(path.into()).unwrap();
        let mut string = Vec::new();

        write!(&mut string, "{:#?}", to_write).unwrap();
        file.write(&string).unwrap();
    }
}
