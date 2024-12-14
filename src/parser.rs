
use crate::lexer::Lex;
use crate::lexer::Token;
use crate::lexer::TokenKind;

use std::fmt;

#[derive(Clone, Debug)]
pub enum NodeKind {
    NDAdd,         // +
    NDSub,         // -
    NDMul,         // *
    NDDiv,         // /
    NDNeg,         // unary -
    NDEq,          // ==
    NDNe,          // !=
    NDLt,          // <
    NDLe,          // <=
    NDGt,          // >
    NDGe,          // >=
    NDExprStmt,    // Expression statement
    NDNum(i64),    // Integer
}

// AST node type
#[derive(Clone)]
pub struct Node {
    pub kind: NodeKind,              // Node kind
    pub lhs: Option<Box<Node>>,      // Left-hand side
    pub rhs: Option<Box<Node>>,      // Right-hand side
}

pub struct Parser<'a> {
    pub lexer: &'a Lex<'a>,
    pub idx_tokens: usize, // Each time we consume a token from the parser, we add 1 to this index.
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a Lex<'a>) -> Self {
        Self {
            lexer,
            idx_tokens: 0,
        }
    }

    pub fn current_token_string(&self) -> &str {
        let curr_tok: &Token = &self.lexer.tokens[self.idx_tokens];
        curr_tok.string
    }

    pub fn current_token_kind(&self) -> &TokenKind {
        let curr_tok: &Token = &self.lexer.tokens[self.idx_tokens];
        &curr_tok.kind
    }

    pub fn skip(&mut self, op: &str) {
        if self.current_token_string() != op {
            eprintln!("Error: expected {}", op);
            std::process::exit(1);
        }
        self.idx_tokens += 1;
    }

    // program = stmt*
    pub fn parse(&mut self) -> Vec<Box<Node>> {
        let mut ast_nodes: Vec<Box<Node>> = Vec::new();

        while self.idx_tokens < self.lexer.tokens.len() - 1 {
            if self.lexer.tokens[self.idx_tokens].kind != TokenKind::TkEOF {
                ast_nodes.push(self.parse_stmt());
            }
        }
        ast_nodes
    }

    // stmt = expr-stmt
    pub fn parse_stmt(&mut self) -> Box<Node> {
        self.parse_expr_stmt()
    }
  
    // expr-stmt = expr ";"
    pub fn parse_expr_stmt(&mut self) -> Box<Node> {
        let node: Box<Node> = Box::new(Node::new_unary(NodeKind::NDExprStmt, self.parse_expr()));

        self.skip(";");

        return node;
    }

    // expr = equality
    pub fn parse_expr(&mut self) -> Box<Node> {
        self.parse_equality()
    }

    // primary = "(" expr ")" | num
    pub fn parse_primary(&mut self) -> Box<Node> {
        if self.current_token_string() == "(" {
            // The current token is then a '(', so we need to start parsing from the next token, to do that we increment the index:
            self.idx_tokens += 1;
            let node: Box<Node> = self.parse_expr();
            self.skip(")");
            return node;
        }

        //println!("self.current_token_kind() idx: {} ESSSSSS {}", self.idx_tokens, self.current_token_kind());

        match self.current_token_kind() {
            TokenKind::TkNum(val) => {
                let node: Box<Node> = Box::new(Node::new_num(*val));
                // We have just consumed the Token corresponding to a number, so we increment the index, to move to the next token:
                self.idx_tokens += 1;
                return node;
            }

            _ => {
                eprintln!("Error: expected an expression. Got {:?}", self.current_token_kind());
                std::process::exit(1);
            }
        }
    }

    // unary = ("+" | "-") unary
    //          | primary
    pub fn parse_unary(&mut self) -> Box<Node> {
        match self.current_token_string() {
            "+" => {
                // The current token is then a '+', so we need to start parsing from the next token, to do that we increment the index:
                self.idx_tokens += 1;
                return self.parse_unary();
            }
            "-" => {
                // Same idea, we just consumed the '-':
                self.idx_tokens += 1;
                let node: Box<Node> = Box::new(Node::new_unary(NodeKind::NDNeg, self.parse_unary()));
                return node;
            }
            _ => {
                return self.parse_primary();
            }
        }
    }

    // mul = unary ("*" unary | "/" unary)*
    pub fn parse_mul(&mut self) -> Box<Node> {
        let mut node: Box<Node> = self.parse_unary();

        loop {
            match self.current_token_string() {
                "*" => { 
                    // The current token is the '*', so to start parsing the 'primary' rule, we need to go to the next token:
                    self.idx_tokens += 1;
                    node = Box::new(Node::new_binary(NodeKind::NDMul, node, self.parse_unary()));
                    continue;
                }
                "/" => { 
                    // Same idea as before:
                    self.idx_tokens += 1;
                    node = Box::new(Node::new_binary(NodeKind::NDDiv, node, self.parse_unary()));
                    continue;
                }
                _ => {
                    break;
                }
            }
        }
        return node;
    }

    // add = mul ("+" mul | "-" mul)*
    pub fn parse_add(&mut self) -> Box<Node> {
        let mut node: Box<Node> = self.parse_mul();

        loop {
            match self.current_token_string() {
                "+" => {
                    // The current token is the '+', so to start parsing the 'mul' rule, we need to go to the next token:
                    self.idx_tokens += 1;
                    node = Box::new(Node::new_binary(NodeKind::NDAdd, node, self.parse_mul()));
                    continue;
                }
                "-" => {
                    // Same idea as before:
                    self.idx_tokens += 1;
                    node = Box::new(Node::new_binary(NodeKind::NDSub, node, self.parse_mul()));
                    continue;
                }
                _ => {
                    break;
                }
            }
        }
        return node;
    }

    // equality = relational ("==" relational | "!=" relational)*
    pub fn parse_equality(&mut self) -> Box<Node> {
        let mut node: Box<Node> = self.parse_relational();

        loop {
            match self.current_token_string() {
                "==" => {
                    // The current token is the '+', so to start parsing the 'mul' rule, we need to go to the next token:
                    self.idx_tokens += 1;
                    node = Box::new(Node::new_binary(NodeKind::NDEq, node, self.parse_relational()));
                    continue;
                }
                "!=" => {
                    // Same idea as before:
                    self.idx_tokens += 1;
                    node = Box::new(Node::new_binary(NodeKind::NDNe, node, self.parse_relational()));
                    continue;
                }
                _ => {
                    break;
                }
            }
        }
        return node;
    }

    // relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    pub fn parse_relational(&mut self) -> Box<Node> {
        let mut node: Box<Node> = self.parse_add();

        loop {
            match self.current_token_string() {
                "<" => {
                    // The current token is the '+', so to start parsing the 'mul' rule, we need to go to the next token:
                    self.idx_tokens += 1;
                    node = Box::new(Node::new_binary(NodeKind::NDLt, node, self.parse_add()));
                    continue;
                }
                "<=" => {
                    // Same idea as before:
                    self.idx_tokens += 1;
                    node = Box::new(Node::new_binary(NodeKind::NDLe, node, self.parse_add()));
                    continue;
                }
                ">" => {
                    // Same idea as before:
                    self.idx_tokens += 1;
                    node = Box::new(Node::new_binary(NodeKind::NDGt, node, self.parse_add()));
                    continue;
                }
                ">=" => {
                    // Same idea as before:
                    self.idx_tokens += 1;
                    node = Box::new(Node::new_binary(NodeKind::NDGe, node, self.parse_add()));
                    continue;
                }
                _ => {
                    break;
                }
            }
        }
        return node;
    }

}

impl Node {
    pub fn new(kind: NodeKind) -> Self {
        Self {
            kind,
            lhs: None,
            rhs: None,
        }
    }

    pub fn new_binary(kind: NodeKind, lhs: Box<Node>, rhs: Box<Node>) -> Self {
        Self {
            kind,
            lhs: Some(lhs),
            rhs: Some(rhs),
        }
    }

    pub fn new_num(val: i64) -> Self {
        Self {
            kind: NodeKind::NDNum(val),
            lhs: None,
            rhs: None,
        }
    }

    pub fn new_unary(kind: NodeKind, expr: Box<Node>) -> Self {
        Self {
            kind,
            lhs: Some(expr),
            rhs: None,
        }
    }

}


impl fmt::Display for NodeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeKind::NDAdd => write!(f, "AST Node Add"),
            NodeKind::NDSub => write!(f, "AST Node Sub"),
            NodeKind::NDMul => write!(f, "AST Node Mul"),
            NodeKind::NDDiv => write!(f, "AST Node Div"),
            NodeKind::NDNum(val) => write!(f, "AST Node Num: {}", val),
            NodeKind::NDNeg => write!(f, "AST Node Neg"),
            NodeKind::NDEq => write!(f, "AST Node Eq"),
            NodeKind::NDNe => write!(f, "AST Node Not Equal"),          
            NodeKind::NDLt => write!(f, "AST Node LessThan"),          
            NodeKind::NDLe => write!(f, "AST Node Less or Equal"), 
            NodeKind::NDGe => write!(f, "AST Node GreaterThan"), 
            NodeKind::NDGt => write!(f, "AST Node Greater or Equal"),    
            NodeKind::NDExprStmt => write!(f, "AST Node Expression Statement")      
        }
    }
}


impl fmt::Debug for Node {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Node")
           .field("kind", &self.kind) // We add `bar` field.
           .field("lhs", &self.lhs) // We add `another` field.
           // We even add a field which doesn't exist (because why not?).
           .field("rhs", &self.rhs)
           //.field("\n", &1)
           .finish() // We're good to go!
    }
}

/*
Node { 
    kind: ND_Add,
    lhs: Some(Node{ 
        kind: ND_Num(5),
        lhs: None,
        rhs: None }),
    rhs: Some(Node {
        kind: ND_Sub,
        lhs: Some(Node { 
            kind: ND_Num(24),
            lhs: None,
            rhs: None }),
        rhs: Some(Node {
            kind: ND_Num(1),
            lhs: None,
            rhs: None }) }) 
}
*/
#[test]
fn test_parser() {
    test_parse(String::from("5+(24-1)"),
                String::from("Node { kind: NDAdd, lhs: Some(Node { kind: NDNum(5), lhs: None, rhs: None }), rhs: Some(Node { kind: NDSub, lhs: Some(Node { kind: NDNum(24), lhs: None, rhs: None }), rhs: Some(Node { kind: NDNum(1), lhs: None, rhs: None }) }) }")
                )
}

#[cfg(test)]
fn test_parse(input: String, expect: String) {
    let mut binding = &input[..];
    let mut lex: Lex = Lex::new(&mut binding);
    lex.tokenize();
    let mut parser: Parser = Parser::new(&lex);
    assert_eq!(format!("{:?}", parser.parse_expr()), expect);
}
