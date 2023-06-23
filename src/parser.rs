use super::lexer::Token;
use super::lexer::TokenKind;

#[derive(Debug)]
pub struct Ast {
    pub inner: Vec<Node>,
}

#[derive(Debug)]
pub struct StructField {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<StructField>
}

#[derive(Debug)]
pub struct EnumDecl {
    pub name: String,
    pub variants: Vec<String>,
}

#[derive(Debug)]
pub enum Type {
    Ident(String),
    Inferred,
    None
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub type_: Type,
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
    ty: Type,
    expr: Expr,
}

#[derive(Debug)]
pub struct ModeDecl {
    name: String
}

#[derive(Debug)]
pub enum Statement {
    StructDecl(StructDecl),
    EnumDecl(EnumDecl),
    FnDecl(FnDecl),
    VarDecl(VarDecl),
    ModDecl(ModeDecl),
    Expr(Expr)
}

#[derive(Debug)]
pub enum Operator {
    Equal,
    Add,
    Minus,
    Multiply,
    Divide,
    Dot,
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
pub struct FnCall {
    ident: Box<Expr>,
    arguments: Vec<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Ident(String),
    Literal(Lit),
    FnCall(FnCall),
    Operation(Operation),
    Paren(Box<Expr>),
}

#[derive(Debug)]
pub enum Node {
    Expr(Expr),
    Statement(Statement),
    None
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

        match token.kind {
            TokenKind::CommentLine(_) => { return self.next() }
            TokenKind::CommentMultiLine(_) => {return self.next() }
            _ => {}
        }
        
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

    pub fn take(&mut self) {
        self.next().unwrap();
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
            let next_token = self.tokens.next_nowh_nonl_notake();
            if next_token.is_none() { return Ok(()) }

            let node = self.parse_node()?;
            self.ast.inner.push(node);
        }
    }

    fn parse_node(&mut self) -> Result<Node, String> {
        let token = self.tokens.next_nowh_nonl_notake().unwrap();
        match token.kind {
            TokenKind::Semicolon => { self.tokens.take(); return Ok(Node::None) }

            TokenKind::KeywordFn => { return Ok(Node::Statement(Statement::FnDecl(self.parse_fndecl()?))) }
            TokenKind::KeywordMod => { return Ok(Node::Statement(Statement::ModDecl(self.parse_mod()?))) }
            TokenKind::KeywordEnum => { return Ok(Node::Statement(Statement::EnumDecl(self.parse_enum_decl()?))) }
            TokenKind::KeywordStruct => { return Ok(Node::Statement(Statement::StructDecl(self.parse_struct_decl()?))) }


            TokenKind::Ident(_) | TokenKind::Float(_) | TokenKind::Integer(_) => {
                return Ok(
                    Node::Statement(self.parse_expr_stmt()?)
                );
            },
            TokenKind::KeywordLet => { return Ok(Node::Statement(Statement::VarDecl(self.parse_let_stmt()?))) }
            _ => { Err(format!("Node : {:?}", token)) }
        }

    }

    fn parse_enum_decl(&mut self) -> Result<EnumDecl, String> {
        self.parse_exact_nowh_nonl(TokenKind::KeywordEnum)?;
        let name = self.parse_ident_nowh_nonl()?;
        self.parse_exact_nowh_nonl(TokenKind::LeftCurly)?;
        let mut variants = Vec::new();
        loop {
            let next_token = self.parse_ident_nowh_nonl();
            if next_token.is_err() { self.tokens.reverse(); break }
            let variant_name = next_token.unwrap();
            variants.push(variant_name);

            let maybe_comma = self.parse_exact_nowh_nonl(TokenKind::Comma);
            if maybe_comma.is_err() { self.tokens.reverse(); break }
        }
        self.parse_exact_nowh_nonl(TokenKind::RightCurly)?;

        return Ok(
            EnumDecl {
                name,
                variants
            }
        )
    }

    fn parse_struct_decl(&mut self) -> Result<StructDecl, String> {
        self.parse_exact_nowh_nonl(TokenKind::KeywordStruct)?;
        let name = self.parse_ident_nowh_nonl()?;
        self.parse_exact_nowh_nonl(TokenKind::LeftCurly)?;
        let mut fields = Vec::new();
        loop {
            let next_token = self.parse_ident_nowh_nonl();
            if next_token.is_err() { self.tokens.reverse(); break }
            let field_name = next_token.unwrap();
            self.parse_exact_nowh_nonl(TokenKind::Colon)?;
            let type_ = self.parse_type()?;
            fields.push(StructField {
                name: field_name,
                type_
            });

            let maybe_comma = self.parse_exact_nowh_nonl(TokenKind::Comma);
            if maybe_comma.is_err() { self.tokens.reverse(); break }
        }
        self.parse_exact_nowh_nonl(TokenKind::RightCurly)?;

        return Ok(
            StructDecl {
                name,
                fields
            }
        )
    }

    fn parse_mod(&mut self) -> Result<ModeDecl, String> {
        self.parse_exact_nowh_nonl(TokenKind::KeywordMod)?;
        let name = self.parse_ident_nowh_nonl()?;
        self.parse_exact_nowh_nonl(TokenKind::Semicolon)?;
        
        return Ok(
            ModeDecl { name }
        )
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        let token = self.tokens.next_nowh_nonl().unwrap();
        let expr;
        match token.kind {
            TokenKind::Ident(ident) => { expr = self.parse_expr_with_lhs(Expr::Ident(ident))? }
            TokenKind::Float(f) => {
                let lit;
                match f {
                    crate::lexer::Float::F32(f) => { lit = Lit::Float(f)}
                    //crate::lexer::Float::F64(f) => { lit = Lit::Float(f as f32)}
                    //crate::lexer::Float::Float(f) => { lit = Lit::Float(f as f32)}
                }
                expr = self.parse_expr_with_lhs(Expr::Literal(lit))?
            }
            TokenKind::Integer(i) => {
                let lit;
                match i {
                    crate::lexer::Integer::I32(i) => { lit = Lit::Int(i)}
                    //crate::lexer::Integer::U32(i) => { lit = Lit::Int(i as i32)}
                    //crate::lexer::Integer::Int(i) => { lit = Lit::Int(i as i32)}
                }
                expr = self.parse_expr_with_lhs(Expr::Literal(lit))?
            }
            TokenKind::Semicolon => { panic!() }
            TokenKind::String(s) => { expr = self.parse_expr_with_lhs(Expr::Literal(Lit::String(s)))? }
            _ => { panic!("{:?}", token) }
        }

        return Ok(expr)
    }

    fn parse_expr_stmt(&mut self) -> Result<Statement, String> {
        let expr = self.parse_expr()?;
        self.parse_exact_nowh_nonl(TokenKind::Semicolon)?;
        return Ok(
            Statement::Expr(expr)
        )
    }

    fn parse_expr_with_lhs(&mut self, lhs: Expr) -> Result<Expr, String> {
        let next_token = self.tokens.next_nowh_nonl().unwrap();
        if next_token.kind == TokenKind::Semicolon || next_token.kind == TokenKind::Colon || next_token.kind == TokenKind::RightParen {
            self.tokens.reverse();
            return Ok( lhs );
        }
        if next_token.kind.is_operator() {
            self.tokens.reverse();

            return Ok(Expr::Operation(self.parse_expr_operation(lhs)?))
        }
        if next_token.kind == TokenKind::LeftParen {
            let args = self.parse_args_fncall()?;
            let lhs = Expr::FnCall( FnCall { ident: Box::new(lhs), arguments: args } );
            return self.parse_expr_with_lhs(lhs);
        }

        panic!("{:?}, {:?}", next_token, lhs);
    }

    fn parse_args_fncall(&mut self) -> Result<Vec<Expr>, String> {
        let mut arguments = Vec::new();
        loop {
            let next_token = self.tokens.next_nowh_nonl().unwrap();
            if next_token.kind == TokenKind::RightParen { return Ok(arguments) }
            self.tokens.reverse();

            let arg = self.parse_expr()?;
            arguments.push(arg);

            
            let next_token = self.tokens.next_nowh_nonl().unwrap();
            if next_token.kind == TokenKind::Comma { continue }
            if next_token.kind == TokenKind::RightParen { return Ok(arguments) }
        }
    }

    fn parse_let_stmt(&mut self) -> Result<VarDecl, String> {
        self.parse_exact_nowh_nonl(TokenKind::KeywordLet)?;
        let ident = self.parse_ident_nowh_nonl()?;
        
        self.parse_exact_nowh_nonl(TokenKind::Equal)?;
        let expr = self.parse_expr()?;
        self.parse_exact_nowh_nonl(TokenKind::Semicolon)?;

        let ty = Type::Inferred;

        let decl = VarDecl {
            mutable: true,
            ty,
            ident,
            expr
        };

        return Ok(decl)
    }

    fn parse_expr_operation(&mut self, lhs: Expr) -> Result<Operation, String> {
        let next_token = self.tokens.next_nowh_nonl().unwrap();
        let operator;
        match next_token.kind {
            TokenKind::Equal => { operator = Operator::Equal }
            TokenKind::Minus => { operator = Operator::Minus }
            TokenKind::Plus => { operator = Operator::Add }
            TokenKind::Divide => { operator = Operator::Divide }
            TokenKind::Multiply => { operator = Operator::Multiply }
            TokenKind::Dot => { operator = Operator::Dot }
            _ => { return Err(format!("Err expr_operation, {:?}", next_token)) }
        }

        let rhs = self.parse_expr()?;

        return Ok(Operation { lhs: Box::new(lhs), rhs: Box::new(rhs), operator })
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

    fn parse_fndecl(&mut self) -> Result<(FnDecl), String> {
        let mut fn_decl = FnDecl {
            name: String::new(),
            paramaters: Vec::new(),
            return_type: Type::None,
            body: Vec::new()
        };

        self.parse_exact_nowh_nonl(TokenKind::KeywordFn)?;

        let token_name = self.parse_ident_nowh_nonl()?;
        fn_decl.name = token_name;

        self.parse_exact_nowh_nonl(TokenKind::LeftParen)?;

        let mut params = Vec::new();
        loop {
            let next_token = self.tokens.next_nowh_nonl_notake();
            if next_token.is_none() { return Err("Params".to_string()); }
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
            if next_token.is_none() { return Err("Params2".to_string()); }
            if next_token.unwrap().kind == TokenKind::Comma { continue }
            else { break }
        }

        self.parse_exact_nowh_nonl(TokenKind::RightParen)?;
        self.parse_exact_nowh_nonl(TokenKind::LeftCurly)?;
        let mut nodes = Vec::new();
        loop {
            let next_token = self.tokens.next_nowh_nonl_notake();
            if next_token.is_none() { return Err("BodyFn".to_string()); }
            if next_token.unwrap().kind == TokenKind::RightCurly { break }

            let node = self.parse_node()?;
            nodes.push(node);
        }
        self.parse_exact_nowh_nonl(TokenKind::RightCurly)?;

        fn_decl.body = nodes;

        Ok(fn_decl)


    }
}