
use std::fmt;

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub string: &'a str,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    TkPunct(Op),     // Punctuators
    TkNum(i64),      // Numeric literals
    TkEOF,           // End-of-file markers
}

#[derive(Debug, PartialEq)]
pub enum Op {
    Plus,
    Sub,
    Mult,
    Div,
    LParen,
    RParen,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Assign,
    Semicolon,
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


// I don't really want to implement the tokens as a linked-list like chibicc so I'll use a Vec to store them
pub struct Lex<'a> {
    pub program: &'a mut &'a str,
    pub tokens: Vec<Token<'a>>,
}


impl<'a> Lex<'a> {
    pub fn new(program: &'a mut &'a str) -> Self {
        Self {
            tokens: Vec::new(),
            program,
        }
    }

    fn add_punctuator_tok(&mut self, token_kind: TokenKind, program_copy: &mut &str, start_idx: &mut usize, len: usize) {
        self.tokens.push(Token::new(token_kind, &self.program[*start_idx..*start_idx+len]));
        *program_copy = &(*program_copy)[len..];    // program++
        *start_idx = *start_idx + len;
    }

   
    pub fn tokenize(&mut self) {
        // Fill in the tokens vector

        let mut program_copy_to_modify = self.program.clone();
        let mut start = 0;

        let mut iter_peek = program_copy_to_modify.chars().clone().peekable();
        let mut c_len = 0;

        while let Some(c) = program_copy_to_modify.chars().next() {

            _ = iter_peek.next();

            // Skip whitespace characters
            if c.is_whitespace() {
                program_copy_to_modify = &program_copy_to_modify[1..];  // program++
                start += 1;
                continue;
            }

            if c.is_digit(10) {
                let (val, len) = strtol(&mut program_copy_to_modify);
                c_len = len as i64;     // We save it here because we'll be using it later
                self.tokens.push(Token::new(TokenKind::TkNum(val), &self.program[start..start+len]));
                start = start + len;
                continue;
            }

            // Punctuator
            match c {
                '+' => { 
                    self.add_punctuator_tok(TokenKind::TkPunct(Op::Plus),
                                        &mut program_copy_to_modify, &mut start, 1);
                    continue;
                }
                '-' => {
                    self.add_punctuator_tok(TokenKind::TkPunct(Op::Sub),
                                        &mut program_copy_to_modify, &mut start, 1);
                    continue;
                }
                '(' => {
                    self.add_punctuator_tok(TokenKind::TkPunct(Op::LParen),
                                        &mut program_copy_to_modify, &mut start, 1);
                    continue;
                }
                ')' => {
                    self.add_punctuator_tok(TokenKind::TkPunct(Op::RParen),
                                        &mut program_copy_to_modify, &mut start, 1);
                    continue;
                }
                '*' => {
                    self.add_punctuator_tok(TokenKind::TkPunct(Op::Mult),
                                        &mut program_copy_to_modify, &mut start, 1);
                    continue;
                }
                '/' => {
                    self.add_punctuator_tok(TokenKind::TkPunct(Op::Div),
                                        &mut program_copy_to_modify, &mut start, 1);
                    continue;
                }
                '=' => {
                    for _ in 1..c_len {
                        iter_peek.next();
                    }

                    if let Some(another) = iter_peek.peek() {
                        if *another == '=' {
                            self.add_punctuator_tok(TokenKind::TkPunct(Op::Eq),
                                        &mut program_copy_to_modify, &mut start, 2);
                        }
                    } else {
                        self.add_punctuator_tok(TokenKind::TkPunct(Op::Assign),
                                        &mut program_copy_to_modify, &mut start, 1);
                    }
                    continue;
                }
                '!' => {
                    for _ in 1..c_len {
                        iter_peek.next();
                    }

                    if let Some(another) = iter_peek.peek() {
                        if *another == '=' {
                            self.add_punctuator_tok(TokenKind::TkPunct(Op::Ne), &mut program_copy_to_modify, &mut start, 2);
                        } else {
                            eprintln!("Error: unsupported operation '!' {} {}", *another, c);
                            std::process::exit(1);
                        }
                    } 
                    continue;
                }
                '<' => {
                    for _ in 1..c_len {
                        iter_peek.next();
                    }

                    if let Some(another) = iter_peek.peek() {
                        if *another == '=' {
                            self.add_punctuator_tok(TokenKind::TkPunct(Op::Le), &mut program_copy_to_modify, &mut start, 2);
                        } else {
                            self.add_punctuator_tok(TokenKind::TkPunct(Op::Lt),
                                        &mut program_copy_to_modify, &mut start, 1);
                        }
                    } 
                    continue;
                }

                '>' => {
                    for _ in 1..c_len {
                        iter_peek.next();
                    }

                    if let Some(another) = iter_peek.peek() {
                        if *another == '=' {    
                            self.add_punctuator_tok(TokenKind::TkPunct(Op::Ge), &mut program_copy_to_modify, &mut start, 2);
                        } else {
                            self.add_punctuator_tok(TokenKind::TkPunct(Op::Gt),
                                        &mut program_copy_to_modify, &mut start, 1);
                        }
                    } 
                    continue;
                }

                ';' => {
                    self.add_punctuator_tok(TokenKind::TkPunct(Op::Semicolon),
                                        &mut program_copy_to_modify, &mut start, 1);
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


impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Plus => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mult => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::LParen => write!(f, "("),
            Op::RParen => write!(f, ")"),
            Op::Eq => write!(f, "=="),
            Op::Ne => write!(f, "!="),
            Op::Lt => write!(f, "<"),
            Op::Le => write!(f, "<="),
            Op::Gt => write!(f, ">"),
            Op::Ge => write!(f, ">="),
            Op::Assign => write!(f, "="),
            Op::Semicolon => write!(f, ";"),
        }
    }
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

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} . str: {}", self.kind, self.string)
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