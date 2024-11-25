
use crate::parser::Parser;
use crate::parser::Node;
use crate::parser::NodeKind;

/// RAX ==> x0
/// RDI ==> x1


pub struct CodeGenerator<'a> {
    pub parser: &'a Parser<'a>,
}


impl<'a> CodeGenerator<'a> {

    pub fn new(parser: &'a Parser<'a>) -> Self {
        Self {
            parser,
        }
    }

    pub fn push(&self) {
        //println!("push %rax");
        println!("  stp x0, x0, [sp, #-16]!");
    }

    pub fn pop(&self, arg: &str) {
        //println!("pop {}", arg);
        println!("  ldp {}, x29, [sp], #16", arg);
    }

    pub fn gen_binary_op(&self, node: &Node) {
        if let Some(rhs) = &node.rhs {
            self.gen_expr(&(*rhs));
        } else {
            eprintln!("Error: gen_binary_op");
            std::process::exit(1);
        }

        self.push();
    
        if let Some(lhs) = &node.lhs {
            self.gen_expr(&(*lhs));
        } else {
            eprintln!("Error: gen_binary_op");
            std::process::exit(1);
        }

        //pop("%rdi");
        self.pop("x1");
    }

    pub fn gen_expr(&self, node: &Node) {
        match node.kind {
            NodeKind::NDAdd => {
                    self.gen_binary_op(node);
                    //println!("add %rdi, %rax");
                    println!("  add x0, x0, x1");
            }
            NodeKind::NDSub => {
                    self.gen_binary_op(node);
                    //println!("sub %rdi, %rax");
                    println!("  sub x0, x0, x1");
            }
            NodeKind::NDMul => {
                    self.gen_binary_op(node);
                    //println!("imul %rdi, %rax");
                    println!("  mul x0, x0, x1");
            }
            NodeKind::NDDiv => {
                self.gen_binary_op(node);
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
                    self.gen_expr(&(*lhs));
                } else {
                    eprintln!("Error: gen_expr NDNeg");
                    std::process::exit(1);
                }
                println!("  neg x0, x0");
            }
            NodeKind::NDEq => {                                     /// 1 = true . 0 = false
                self.gen_binary_op(node);
                //printf("  cmp %%rdi, %%rax\n");
                //printf("  sete %%al\n");
                //printf("  movzb %%al, %%rax\n");
                println!("  cmp x0, x1");
                println!("  cset x0, eq");
            }
            NodeKind::NDNe => {
                self.gen_binary_op(node);
                //printf("  cmp %%rdi, %%rax\n");
                //printf("  setne %%al\n");
                //printf("  movzb %%al, %%rax\n");
                println!("  cmp x0, x1");
                println!("  cset x0, ne");
            }
            NodeKind::NDLt => {
                self.gen_binary_op(node);
                //printf("  cmp %%rdi, %%rax\n");
                //printf("  setl %%al\n");
                //printf("  movzb %%al, %%rax\n");
                println!("  cmp x0, x1");
                println!("  cset x0, lt");
            }
            NodeKind::NDLe => {
                self.gen_binary_op(node);
                //printf("  cmp %%rdi, %%rax\n");   
                //printf("  setle %%al\n");
                //printf("  movzb %%al, %%rax\n");
                println!("  cmp x0, x1");
                println!("  cset x0, le");
            }
            NodeKind::NDGt => {
                self.gen_binary_op(node);
                println!("  cmp x0, x1");
                println!("  cset x0, gt");
            }
            NodeKind::NDGe => {
                self.gen_binary_op(node);
                println!("  cmp x0, x1");
                println!("  cset x0, ge");
            }
        }
        
    }
}