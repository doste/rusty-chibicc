
use crate::code_gen::CodeGenerator;
use crate::lexer::Lex;
use crate::parser::Parser;
use crate::parser::Node;


mod code_gen;
mod lexer;
mod parser;



use std::env;



pub fn main() {
    
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

    let node: Box<Node> = parser.parse_expr();

    //println!("{:?}", node);    // For debugging

    let code_gen: CodeGenerator = CodeGenerator::new(&parser);
    
    println!("  .globl _start");
    println!("_start:");
    println!("stp x19, x30, [sp, #-16]!"); // Keep x19 and x30 (link register), so we can use them as scratch registers

    // Traverse the AST to emit assembly:
    code_gen.gen_expr(&node);

    println!("ldp x19, x30, [sp], #16");  // Restore x19 and x30 (link register)
    println!("  ret");
    

    
}
