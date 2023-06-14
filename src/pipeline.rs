use std::io::Read;

use super::Lexer;

pub struct Pipeline {
    file_path: String,
}

impl Pipeline {
    pub fn new(file_path: String) -> Self {
        Self {
            file_path,
        }
    }

    pub fn solve(&mut self) {
        let file_string = self.get_file_string();
        self.lex(&file_string);
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

    }
}