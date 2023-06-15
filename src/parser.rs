use super::lexer::Token;
use super::lexer::TokenKind;

#[derive(Debug)]
pub struct Ast {
    pub inner: Vec<Declaration>,
}

#[derive(Debug)]
pub struct StructDecl {
    
}

#[derive(Debug)]
pub struct EnumDecl {
    
}

#[derive(Debug)]
pub enum Type {
    Ident(String),
    None
}

#[derive(Debug)]
pub struct Param {
    name: String,
    type_: Type,
}

#[derive(Debug)]
pub struct FnDecl {
    name: String,
    paramaters: Vec<Param>,
    return_type: Type,
    body: Vec<Node>
}

#[derive(Debug)]
pub struct VarDecl {
    mutable: bool,
    ident: String,
    expr: Expr,
}

#[derive(Debug)]
pub enum Declaration {
    StructDecl(StructDecl),
    EnumDecl(EnumDecl),
    FnDecl(FnDecl),
    VarDecl(VarDecl),
}

#[derive(Debug)]
pub enum Operator {
    Equal,
    Add,
    Minus,
}

#[derive(Debug)]
pub struct Operation {
    lhs: Box<Expr>,
    rhs: Box<Expr>,
    operator: Operator
}

#[derive(Debug)]
pub enum Lit {
    Int(i32),
    Float(f32),
    String(String)
}

#[derive(Debug)]
pub enum Expr {
    Ident(String),
    Literal(Lit),
    Operation(Operation),
    Paren(Box<Expr>),
}

#[derive(Debug)]
pub enum Node {
    Expr(Expr),
    Declaration(Declaration)
}

pub struct TokenIterator<'a> {
    actual_token: Option<Token>,
    tokens: std::slice::Iter<'a, Token>,
    reverse: bool,
}

impl<'a> TokenIterator<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            actual_token: None,
            tokens: tokens.iter(),
            reverse: false
        }
    }

    pub fn actual_token(&self) -> Token {
        return self.actual_token.as_ref().unwrap().clone()
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.reverse {
            self.reverse = false;
            return self.actual_token.clone()
        }
        let token_option = self.tokens.next();
        if token_option.is_none() { return None }

        let token: &Token = token_option.unwrap();
        self.actual_token = Some(token.clone());
        
        return Some(token.clone())
    }

    pub fn next_nowh(&mut self) -> Option<Token> {
        loop {
            let next_option = self.next();
            if next_option.is_none() { return None }
            let next = next_option.unwrap();

            if next.kind == TokenKind::WhiteSpace { continue }

            return Some(next)
        }
    }
    

    pub fn next_nowh_nonl(&mut self) -> Option<Token> {
        loop {
            let next_option = self.next();
            if next_option.is_none() { return None }
            let next = next_option.unwrap();

            if next.kind == TokenKind::WhiteSpace { continue }
            if next.kind == TokenKind::NewLine { continue }

            return Some(next)
        }
    }

    pub fn next_nowh_nonl_notake(&mut self) -> Option<Token> {
        let token = self.next_nowh_nonl();
        self.reverse();
        return token
    }

    pub fn reverse(&mut self) {
        self.reverse = true;
    }
}

pub struct AstParser<'a> {
    pub ast: Ast,
    tokens: TokenIterator<'a>,
    err: Vec<String>
}

impl<'a> AstParser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            ast: Ast { inner: Vec::new() },
            tokens: TokenIterator::new(tokens),
            err: Vec::new()
        }
    }

    pub fn parse(&mut self) -> Result<(), String> {
        loop {
            let token_option = self.tokens.next();
            if token_option.is_none() { return Ok(()) }

            let token = token_option.unwrap();

            match token.kind {
                TokenKind::KeywordFn => { self.parse_fndecl()? }
                _ => {}
            }
        }
    }

    fn parse_exact_nowh_nonl(&mut self, token_kind: TokenKind) -> Result<(), String> {
        let token = self.tokens.next_nowh_nonl().unwrap();

        if token.kind != token_kind {
            return Err(format!("Wrong token: {:?} \nexpected: {:?}, found: {:?}", token.span, token_kind, token.kind))
        }

        Ok(())
    }

    fn parse_ident_nowh_nonl(&mut self) -> Result<String, String> {
        let token = self.tokens.next_nowh_nonl().unwrap();
        match token.kind {
            TokenKind::Ident(ident) => { return Ok(ident) },
            _ => { return Err("No ident found".to_string()) }
        }
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let token = self.tokens.next_nowh_nonl().unwrap();
        match token.kind {
            TokenKind::Ident(ident) => { return Ok(Type::Ident(ident)) }
            _ => { return Err("No type found".to_string()); }
        }
    }

    fn parse_fndecl(&mut self) -> Result<(), String> {
        let mut fn_decl = FnDecl {
            name: String::new(),
            paramaters: Vec::new(),
            return_type: Type::None,
            body: Vec::new()
        };

        let token_name = self.parse_ident_nowh_nonl()?;
        fn_decl.name = token_name;

        self.parse_exact_nowh_nonl(TokenKind::LeftParen)?;

        let mut params = Vec::new();
        loop {
            let next_token = self.tokens.next_nowh_nonl_notake();
            if next_token.is_none() { return Err("Err".to_string()); }
            if next_token.unwrap().kind == TokenKind::RightParen { break }

            let name_param = self.parse_ident_nowh_nonl()?;
            self.parse_exact_nowh_nonl(TokenKind::Colon)?;
            let type_ = self.parse_type()?;

            let param = Param {
                name: name_param,
                type_: type_
            };
            params.push(param);

            let next_token = self.tokens.next_nowh_nonl_notake();
            if next_token.is_none() { return Err("Err".to_string()); }
            if next_token.unwrap().kind == TokenKind::Comma { continue }
            else { break }
        }

        self.parse_exact_nowh_nonl(TokenKind::RightParen)?;
        self.parse_exact_nowh_nonl(TokenKind::LeftCurly)?;
        loop {
            //TODO
            break
        }
        self.parse_exact_nowh_nonl(TokenKind::RightCurly)?;




        Ok(())


    }
}