use std::str::Chars;

#[derive(Clone, Debug)]
pub struct Span {
    line: i32,
    column: i32
}

#[derive(Debug)]
pub struct Token {
    span: Span,
    kind: TokenKind
}

#[derive(Debug)]
pub enum Integer {
    Int(isize),
    I32(i32),
    U32(u32)
}

#[derive(Debug)]
pub enum Float {
    Float(f64),
    F32(f32),
    F64(f64)
}

#[derive(Debug)]
pub enum TokenKind {
    Ident(String),
    Integer(Integer),
    Float(Float),
    String(String),

    KeywordLet,
    KeywordEnum,
    KeywordStruct,
    KeywordFn,

    NewLine,

    OpEqual,
    OpMinus,
    OpPlus,
}

#[derive(Clone)]
pub struct CharInfo {
    c: char,
    span: Span,
}

pub struct CharIterator<'a> {
    actual_char_info: Option<CharInfo>,
    chars: Chars<'a>,
    column: i32,
    line: i32,
    reverse: bool,
}

impl<'a> CharIterator<'a> {
    pub fn new(source: &'a String) -> Self {
        Self {
            actual_char_info: None,
            chars: source.chars(),
            column: 0,
            line: 0,
            reverse: false,
        }
    }

    pub fn char_info(&self) -> CharInfo {
        self.actual_char_info.as_ref().unwrap().clone()
    }

    pub fn span(&self) -> Span {
        Span {
            line: self.line,
            column: self.column
        }
    }

    pub fn next(&mut self) -> Option<CharInfo> {
        if self.reverse == true { 
            self.reverse = false;
            assert!(self.actual_char_info.is_some());
            return self.actual_char_info.clone();
        }

        let c_option = self.chars.next();
        if c_option.is_none() { return None }
        let c = c_option.unwrap();
        let span = self.span();

        if c == '\u{d}' {
            return self.next()
        }

        if c == '\u{a}' {
            self.column = 0;
            self.line += 1;
        } else {
            self.column += 1;
        }

        self.actual_char_info = Some(CharInfo { c, span });

        return self.actual_char_info.clone();

    }

    pub fn reverse(&mut self) {
        self.reverse = true;
    }
}

pub struct Lexer<'a> {
    tokens: Vec<Token>,
    source: &'a String,
    chars_iterator: CharIterator<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a String) -> Self {
        let chars_iterator = CharIterator::new(source);
        Self {
            tokens: Vec::new(),
            source,
            chars_iterator
        }
    }

    pub fn lex(&mut self) -> &Vec<Token> {
        loop {
            let char_info_option = self.chars_iterator.next();
            if char_info_option.is_none() { break }
            let c_info = char_info_option.unwrap();
            let c = c_info.c;

            if c == '\u{d}' {
                continue
            }

            if c == '\u{a}' {
                continue
            }

            if c.is_alphabetic() {
                self.parse_word();
                continue
            }

            if c.is_numeric() {
                self.parse_number();
                continue
            }
        }

        return  &self.tokens;
    }

    fn parse_word(&mut self) {
        let first_char = self.chars_iterator.char_info();
        let mut word = String::new();
        word.push(first_char.c);
        let span = first_char.span;

        loop {
            let c_option = self.chars_iterator.next();
            if c_option.is_none() { break }
            let c = c_option.unwrap().c;

            if c.is_alphanumeric() || c == '_' {
                word.push(c);
                continue;
            }

            if c.is_whitespace() || c == '\u{a}' || c.is_ascii_punctuation() {
                self.chars_iterator.reverse();
                break;
            }

            panic!("Incorect word! {:?}", span);
        }

        if word == "let" { self.tokens.push(Token { span, kind: TokenKind::KeywordLet }) }
        else if word == "enum"  { self.tokens.push(Token { span, kind: TokenKind::KeywordLet }) }
        else if word == "struct"  { self.tokens.push(Token { span, kind: TokenKind::KeywordStruct }) }
        else if word == "fn"  { self.tokens.push(Token { span, kind: TokenKind::KeywordFn }) }

        else { self.tokens.push(Token { span, kind: TokenKind::Ident(word) }) }

    }

    fn parse_number(&mut self) {
        let first_num = self.chars_iterator.char_info();
        let mut num = String::new();
        num.push(first_num.c);
        let span = first_num.span;

        loop {
            let c_option = self.chars_iterator.next();
            if c_option.is_none() { break }
            let c = c_option.unwrap().c;

            if c.is_numeric() {
                num.push(c);
                continue
            }

            if c == '.' {
                num.push(c);
                continue
            }

            if c.is_whitespace() || c == '\u{a}' || c.is_ascii_punctuation() {
                self.chars_iterator.reverse();
                break;
            }

            panic!("Incorect num! {:?}", span);
        }

        if num.parse::<i32>().is_ok() {
            self.tokens.push(Token { span, kind: TokenKind::Integer(Integer::I32(num.parse::<i32>().unwrap())) })
        } else if num.parse::<f32>().is_ok() {
            self.tokens.push(Token { span, kind: TokenKind::Float(Float::F32(num.parse::<f32>().unwrap())) })
        } else {
            panic!("Incorect num! {:?}", span);
        }

    }
}