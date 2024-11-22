use std::env;
use std::fmt;

#[derive(Debug)]
pub enum Op {
    Plus,
    Sub,
    Mult,
    Div,
    LParen,
    RParen,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Plus => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mult => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::LParen => write!(f, "("),
            Op::RParen => write!(f, ")"),
        }
    }
}

#[derive(Debug)]
pub enum TokenKind {
    TkPunct(Op),     // Punctuators
    TkNum(i64),      // Numeric literals
    TkEOF,           // End-of-file markers
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::TkPunct(op) => write!(f, "TkPunct: {}", op),
            TokenKind::TkNum(val) => write!(f, "TkNum: {}", val),
            TokenKind::TkEOF => write!(f, "TkEOF"),
        }
    }
}

#[derive(Debug)]
pub struct Token<'a> {
    kind: TokenKind,
    string: &'a str,
}


impl<'b> Token<'b> {
    pub fn new(kind: TokenKind, string: &'b str) -> Token<'b> {
        Token {
            kind,
            string,
        }
    }

    pub fn get_number(&self) -> i64 {
        match self.kind {
            TokenKind::TkNum(val) => return val,
            _ => {
                eprintln!("Error: expected a number");
                std::process::exit(1);
            }
        }
    }



}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} . str: {}", self.kind, self.string)
    }
}



// I don't really want to implement the tokens as a linked-list like chibicc so I'll use a Vec to store them
pub struct Lex<'a> {
    program: &'a mut &'a str,
    tokens: Vec<Token<'a>>,
}


impl<'a> Lex<'a> {
    pub fn new(program: &'a mut &'a str) -> Self {
        Self {
            tokens: Vec::new(),
            program,
        }
    }

    pub fn add_punctuator_tok(&mut self, token_kind: TokenKind, program_copy: &mut &str, start_idx: &mut usize) {
        self.tokens.push(Token::new(token_kind, &self.program[*start_idx..*start_idx+1]));
        *program_copy = &(*program_copy)[1..];    // program++
        *start_idx = *start_idx + 1;
    }

    fn tokenize(&mut self) {
        // Fill in the tokens vector

        let mut program_copy_to_modify = self.program.clone();
        let mut start = 0;

        while let Some(c) = program_copy_to_modify.chars().next() {

            // Skip whitespace characters
            if c.is_whitespace() {
                program_copy_to_modify = &program_copy_to_modify[1..];  // program++
                start += 1;
                continue;
            }

            if c.is_digit(10) {
                let (val, len) = strtol(&mut program_copy_to_modify);
                self.tokens.push(Token::new(TokenKind::TkNum(val), &self.program[start..start+len]));
                start = start + len;
                continue;
            }

            // Punctuator
            match c {
                '+' => { 
                    self.add_punctuator_tok(TokenKind::TkPunct(Op::Plus),
                                        &mut program_copy_to_modify, &mut start);
                    continue;
                }
                '-' => {
                    self.add_punctuator_tok(TokenKind::TkPunct(Op::Sub),
                                        &mut program_copy_to_modify, &mut start);
                    continue;
                }
                '(' => {
                    self.add_punctuator_tok(TokenKind::TkPunct(Op::LParen),
                                        &mut program_copy_to_modify, &mut start);
                    continue;
                }
                ')' => {
                    self.add_punctuator_tok(TokenKind::TkPunct(Op::RParen),
                                        &mut program_copy_to_modify, &mut start);
                    continue;
                }
                '*' => {
                    self.add_punctuator_tok(TokenKind::TkPunct(Op::Mult),
                                        &mut program_copy_to_modify, &mut start);
                    continue;
                }
                '/' => {
                    self.add_punctuator_tok(TokenKind::TkPunct(Op::Div),
                                        &mut program_copy_to_modify, &mut start);
                    continue;
                }
                _ => {
                    eprintln!("Error: unsupported operation");
                    std::process::exit(1);
                }
            }
        }

        self.tokens.push(Token::new(TokenKind::TkEOF, ""));
    }
}

impl<'a> fmt::Display for Lex<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut it = self.tokens.iter();
        while let Some(tok) = it.next() {
            write!(f, "{}\n", tok)?;
        }
        Ok(())
    }
}

fn strtol(p: &mut &str) -> (i64, usize) {
    
    let digits_count = p.chars().take_while(|&c| c.is_digit(10)).count();
    //println!("DIGITS COUNT: {}", digits_count);
    let (num, rest) = p.split_at(digits_count);
    /*
    let (num, rest) = p.split_at(p.chars().take_while(|&c| c.is_digit(10)).count());

    split_at:
        Divides one string slice into two at an index.
        The argument, mid, should be a byte offset from the start of the string. It must also be on the boundary of a UTF-8 code point.
        The two slices returned go from the start of the string slice to mid, and from mid to the end of the string slice.
    chars()
        Returns an iterator over the chars of a string slice.
        As a string slice consists of valid UTF-8, we can iterate through a string slice by char. This method returns such an iterator.
    take_while()
        Creates an iterator that yields elements based on a predicate.
        take_while() takes a closure as an argument. It will call this closure on each element of the iterator,
         and yield elements while it returns true.
        After false is returned, take_while()’s job is over, and the rest of the elements are ignored.
    */
    *p = rest;

    match num.parse::<i64>() {
        Ok(v) => (v, digits_count),
        Err(e) => {
            eprintln!("Error: parsing value in strtol {}", e);
            std::process::exit(1);
        }
    }
}


////////////////// PARSER

#[derive(Clone, Debug)]
pub enum NodeKind {
    NDAdd,         // +
    NDSub,         // -
    NDMul,         // *
    NDDiv,         // /
    NDNeg,         // unary -
    NDNum(i64),    // Integer
}

// AST node type
#[derive(Clone)]
pub struct Node {
    kind: NodeKind,              // Node kind
    lhs: Option<Box<Node>>,      // Left-hand side
    rhs: Option<Box<Node>>,      // Right-hand side
}

pub struct Parser<'a> {
    lexer: &'a Lex<'a>,
    idx_tokens: usize, // Each time we consume a token from the parser, we add 1 to this index.
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

    // primary = "(" expr ")" | num
    pub fn parse_primary(&mut self) -> Box<Node> {
        if self.current_token_string() == "(" {
            // The current token is then a '(', so we need to start parsing from the next token, to do that we increment the index:
            self.idx_tokens += 1;
            let node: Box<Node> = self.parse_expr();
            self.skip(")");
            return node;
        }

        match self.current_token_kind() {
            TokenKind::TkNum(val) => {
                let node: Box<Node> = Box::new(Node::new_num(*val));
                // We have just consumed the Token corresponding to a number, so we increment the index, to move to the next token:
                self.idx_tokens += 1;
                return node;
            }

            _ => {
                eprintln!("Error: expected an expression");
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
                let node: Box<Node> = Box::new(Node::new_unary(self.parse_unary()));
                // We have just consumed the Token corresponding to a number, so we increment the index, to move to the next token:
                //self.idx_tokens += 1;
                //???????????????????????????????????????????
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

    // expr = mul ("+" mul | "-" mul)*
    pub fn parse_expr(&mut self) -> Box<Node> {
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

    pub fn new_unary(expr: Box<Node>) -> Self {
        Self {
            kind: NodeKind::NDNeg,
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



////////////////// CODE GENERATOR
/// 
/// RAX ==> x0
/// RDI ==> x1

pub fn push() {
    //println!("push %rax");
    println!("  stp x0, x0, [sp, #-16]!");
}

pub fn pop(arg: &str) {
    //println!("pop {}", arg);
    println!("  ldp {}, x29, [sp], #16", arg);
}

pub fn gen_binary_op(node: &Node) {
    if let Some(rhs) = &node.rhs {
        gen_expr(&(*rhs));
    } else {
        eprintln!("Error: gen_binary_op");
        std::process::exit(1);
    }

    push();
   
    if let Some(lhs) = &node.lhs {
        gen_expr(&(*lhs));
    } else {
        eprintln!("Error: gen_binary_op");
        std::process::exit(1);
    }

    //pop("%rdi");
    pop("x1");
}

pub fn gen_expr(node: &Node) {
    match node.kind {
        NodeKind::NDAdd => {
                gen_binary_op(node);
                //println!("add %rdi, %rax");
                println!("  add x0, x0, x1");
        }
        NodeKind::NDSub => {
                gen_binary_op(node);
                //println!("sub %rdi, %rax");
                println!("  sub x0, x0, x1");
        }
        NodeKind::NDMul => {
                gen_binary_op(node);
                //println!("imul %rdi, %rax");
                println!("  mul x0, x0, x1");
        }
        NodeKind::NDDiv => {
            gen_binary_op(node);
            //println!("cqo");
            //println!("idiv %rdi");
            println!("  sdiv x0, x0, x1");
        }
        NodeKind::NDNum(val) => {
            //println!("mov {}, %rax", val);
            println!("  mov x0, #{}", val);
        }
        NodeKind::NDNeg => {
            //println!("neg %rax");
            if let Some(lhs) = &node.lhs {
                gen_expr(&(*lhs));
            } else {
                eprintln!("Error: gen_expr NDNeg");
                std::process::exit(1);
            }
            println!("  NEG x0, x0");
        }
    }
    
}

fn main() {
    
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Error: invalid number of arguments");
        std::process::exit(1);
    }

    let mut p = args[1].as_str();


    let mut lex: Lex = Lex::new(&mut p);
    lex.tokenize();
    //println!("{}", lex);    // For debugging

    let mut parser: Parser = Parser::new(&lex);

    //let node: Box<Node> = Parser::parse_expr(&mut parser);
    let node: Box<Node> = parser.parse_expr();


    //println!("{:?}", node);    // For debugging

    //println!("  .globl main");
    //println!("main:");
    
    println!("  .globl _start");
    println!("_start:");
    println!("stp x19, x30, [sp, #-16]!"); // Keep x19 and x30 (link register), so we can use them as scratch registers

    // Traverse the AST to emit assembly:
    gen_expr(&node);

    println!("ldp x19, x30, [sp], #16");  // Restore x19 and x30 (link register)
    println!("  ret");
    

    
}
