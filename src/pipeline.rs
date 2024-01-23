use std::io::Write;

use std::collections::HashMap;

use crate::lexer::Token;
use super::lexer::Lexer;
use super::parser::Ast;
use super::parser::AstParser;
use super::sema::SemanticAnalizer;
use super::sema::SemaIr;

use super::utils;

#[derive(Default)]
pub struct PipelineOptions {
    pub location_debug_info: String,
    pub write_tokens: bool,
    pub write_ast: bool,
    pub write_ir: bool,
    pub write_sym_table: bool,
    pub write_types_sema: bool,
    pub write_sym_sema: bool,
    pub write_profiling: bool,
    pub kind: CompilationKind,
}

impl PipelineOptions {
    fn debug_info(&mut self, debug: bool) {
        self.write_tokens = debug;
        self.write_ast = debug;
        self.write_ir = debug;
        self.write_sym_table = debug;
        self.write_types_sema = debug;
        self.write_sym_sema = debug;
        self.write_profiling = debug;
    }
}

#[derive(Debug, Default)]
pub struct Profiling {
    lex: std::time::Duration,
    parse: std::time::Duration,
    sema: std::time::Duration,
}

pub struct Library {
    pub name: String,
    pub lex_tokens: Vec<Token>,
    pub ast: Option<Ast>,
    pub sema_ir: Option<SemaIr>,
    pub file_path: String,
}

#[derive(PartialEq)]
pub enum CompilationKind {
    Binary,
    Package
}

impl Default for CompilationKind {
    fn default() -> Self {
        return Self::Binary
    }
}

pub struct Pipeline {
    lex_tokens: Vec<Token>,
    ast: Option<Ast>,
    sema_ir: Option<SemaIr>,
    file_path: String,
    options: PipelineOptions,
    profiling: Profiling,
    libraries: Vec<Library>,
    libraries_hash: HashMap<String, usize>
}

impl Pipeline {
    pub fn new(file_path: String, options: PipelineOptions) -> Self {
        Self {
            lex_tokens: Vec::new(),
            ast: None,
            sema_ir: None,
            file_path,
            options,
            profiling: Profiling::default(),
            libraries: Vec::new(),
            libraries_hash: HashMap::new()
        }
    }
    
    pub fn solve_library(&mut self, name: &str, file_path: &str) {
        let mut pipeline_options = PipelineOptions::default();
        pipeline_options.debug_info(true);
        pipeline_options.kind = CompilationKind::Package;
        pipeline_options.location_debug_info = "xi_output/".to_string() + name;

        let mut pipeline = Self::new(file_path.to_string(), pipeline_options);
        pipeline.solve();

        let lib = Library {
            name: name.to_string(),
            lex_tokens: pipeline.lex_tokens,
            ast: pipeline.ast,
            sema_ir: pipeline.sema_ir,
            file_path: file_path.to_string()
        };

        self.libraries.push(lib);
        self.libraries_hash.insert(name.to_string(), self.libraries.len() - 1);
    }

    pub fn solve(&mut self) {
        if self.options.kind == CompilationKind::Binary {
            self.solve_library("std", "lib/std/lib.xi");
        }

        let file_string = self.get_file_string();
        self.lex(&file_string);
        self.parse();
        self.sema();
        
        if self.options.write_profiling {
            self.write_debug_to_file(&self.profiling, self.options.location_debug_info.clone() + "/profiling.txt");
        }

        if self.options.kind == CompilationKind::Binary {
            self.cgen();
        }
    }


    pub fn get_file_string(&mut self) -> String {
        return utils::get_file_string(&self.file_path);
    }

    pub fn lex(&mut self, file_string: &String) {
        let t = std::time::Instant::now();

        let mut lexer = Lexer::new(file_string);
        let tokens = lexer.lex();

        self.profiling.lex = t.elapsed();

        if self.options.write_tokens {
            self.write_debug_to_file(utils::FmtToken(&tokens), self.options.location_debug_info.clone() +  "/tokens.txt");
        }

        self.lex_tokens = lexer.tokens;
    }

    fn parse(&mut self) {
        let t = std::time::Instant::now();

        let mut parser = AstParser::new(&self.lex_tokens);
        let result = parser.parse();
        if result.is_err() { panic!("{:?}", result); }

        self.profiling.parse = t.elapsed();

        if self.options.write_ast {
            self.write_debug_to_file(&parser.ast, self.options.location_debug_info.clone() + "/ast.txt");
        }

        self.ast = Some(parser.ast);
    }

    fn sema(&mut self) {
        let t = std::time::Instant::now();

        let mut sema = SemanticAnalizer::new(self.ast.as_ref().unwrap(), &self.libraries, &self.libraries_hash);
        let sema_ir = sema.sema();

        if !sema.err.is_empty() {
            panic!("{:#?}", sema.err);
        }

        self.profiling.sema = t.elapsed();

        if self.options.write_sym_table {
            self.write_debug_to_file(&sema_ir.sym_tables, self.options.location_debug_info.clone() + "/sym_table.txt");
        }

        if self.options.write_types_sema {
            self.write_debug_to_file(&sema_ir.types, self.options.location_debug_info.clone() + "/types_sema.txt");
        }

        if self.options.write_sym_sema {
            self.write_debug_to_file(&sema_ir.symbols, self.options.location_debug_info.clone() + "/sym_sema.txt");
        }

        if self.options.write_ir {
            self.write_debug_to_file(&sema_ir.ir, self.options.location_debug_info.clone() + "/ir.txt");
        }

        self.sema_ir = Some(sema_ir);
    }

    fn cgen(&mut self) {
        let c_gen_options = crate::cgen::c_gen::CGeneratorOptions {
            out_path: "xi_output/".to_string(),
            bin_file_name: "gen.c".to_string()
        };

        let mut c_gen = crate::cgen::c_gen::CCodeGenerator::new(c_gen_options);
        c_gen.gen();
    }

    fn read_or_create_output_dir(&self) {
        std::fs::create_dir_all(self.options.location_debug_info.clone());
        std::fs::read_dir(self.options.location_debug_info.clone()).unwrap();

    }

    fn write_debug_to_file(&self, to_write: impl std::fmt::Debug, path: impl Into<String>) {
        self.read_or_create_output_dir();
        let mut file = std::fs::File::create(path.into()).unwrap();
        let mut string = Vec::new();

        write!(&mut string, "{:#?}", to_write).unwrap();
        file.write(&string).unwrap();
    }
}
