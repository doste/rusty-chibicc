
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
    //println!("TOKENS: \n {}", lex);    // For debugging

    let mut parser: Parser = Parser::new(&lex);

    let ast_nodes: Vec<Box<Node>> = parser.parse();
    
    //println!("AST NODES:");       // For debugging
    //for node in &ast_nodes {
    //    println!("{:?}", node);
    //}
    

    let code_gen: CodeGenerator = CodeGenerator::new(&parser);
    // Traverse the AST to emit assembly:
    code_gen.codegen(&ast_nodes);

    
}
