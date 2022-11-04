use std::boxed::Box;
use logos::Logos;
use statrs::function::gamma;
use serde_derive::{Serialize, Deserialize};
use std::option::Option;
use crate::parsers::*;

pub mod parsers;

trait Object {
    fn get_val(&self) -> Expr;
}
#[derive(Serialize, Deserialize, Debug)]
struct Variable{
    name: String,
    val: Expr
}

impl Object for Variable{
    fn get_val(&self) -> Expr {
        self.val.clone()
    }
}

// struct Function{
//     name: String,
//     body: Expr,
//     env: Environment
// }

// impl Object for Function{
//     fn get_val(&self) -> Expr {
//         // replace LocalVars with parameter List
//     }
// }


#[derive(Serialize, Deserialize, Debug)]
pub struct Environment{
    run: bool,
    variables: Vec<Variable>,
    input: Option<String>,
    last_result: Option<f64>
}

#[derive(Logos, Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Token{

    #[token(":")]
    Command,

    #[token("ans")]
    Ans,

    #[regex("[0-9]+", |lex| lex.slice().parse())]
    #[regex("[0-9]*\\.[0-9]+", |lex| lex.slice().parse())]
    #[token("PI", |_lex| 3.14159265358979323)]
    #[token("e", |_lex| 2.7141)]
    Num(f64),
    

    #[regex("[A-Za-z_]+[a-zA-Z0-9_]*", |lex| String::from(lex.slice()))]
    Name(String),

    #[token("+")]
    Add,

    #[token("-")]
    Sub,

    #[token("*")]
    Mul,

    #[token("/")]
    Div,

    #[token("^")]
    Pow,

    #[token("!")]
    Fac,

    #[token("==")]
    Equal,

    #[token("<")]
    Less,

    #[token(">")]
    Greater,

    #[token("=")]
    Assign,

    #[token(",")]
    Comma,

    #[token("(")]
    PrenticesOpen,

    #[token(")")]
    PrenticesClosed,

    #[error]
    #[regex(r"[ \t\n\f]", logos::skip)]
    Error,

    // Internal States
    EOF,
    LocalVar(String),
    Var(String)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Expr{
    a: Option<Box<Expr>>,
    b: Option<Box<Expr>>,
    operation: Token
}

impl Expr{
    fn eval(&self, env: &Environment) -> Result<f64, HandlerResult>{

        // if token is a number
        match &self.operation {
            Token::Num(n) => return Ok(n.clone()),
            Token::Var(name) => {
                let var = env.variables.iter().find(|&var| var.name==*name);
                if var.is_none(){
                    return Err(HandlerResult::Error(String::from(format!("Variable '{}' not defined!", name))));
                }
                return var.unwrap().val.eval(env);
            },
            Token::Ans => {
                if env.last_result.is_none(){
                    return Err(HandlerResult::Error(String::from("No previous Answer.")));
                }
                return Ok(env.last_result.unwrap());
            },
            _ => ()

        };

        // Evaluate expressions recursivly
        let a = self.a.as_ref().unwrap().eval(env)?;

        if self.operation == Token::Fac{
            return Ok(gamma::gamma(a+1.0));
        }

        let b = self.b.as_ref().unwrap().eval(env)?;

        match &self.operation {
            Token::Add => Ok(a + b),
            Token::Sub => Ok(a - b),
            Token::Mul => Ok(a * b),
            Token::Div => {
                if b < -0.00001 || b > 0.000001{
                    Ok(a / b)
                }
                else {
                    Err(HandlerResult::Error(String::from("Divison by Zero")))
                }
            },
            Token::Pow => Ok(a.powf(b)),
            t @ _ => Err(HandlerResult::Error(String::from(format!("Unexpected Token '{:?}' as operation.", t))))
                
        }
    }
}




fn main() {
    let mut input: String = String::new();
    println!("Calc 0.5");
    let mut env = Environment{run: true, variables: Vec::new(), input: None, last_result: None};
    let parsers: &[Box<dyn ParseHandler>] = &[Box::new(VarHandler{}), Box::new(CommandHandler{}), Box::new(ExprHandler{})];
    while env.run {
        input.clear();
        // get Input
        match std::io::stdin().read_line(&mut input){
            Ok(size) => if size==0{continue;},
            Err(e) =>{
                println!("[ERROR] ReadInput: {}", e);
                continue;
            }
        }

        env.input=Some(input.clone());

        // Lex input
        let lex = Token::lexer(input.trim_end());
        
        for parser in parsers {
            match parser.handle(&mut lex.clone(), &mut env) {
                HandlerResult::Ok => break,
                HandlerResult::Pass => continue,
                HandlerResult::Error(reason) => {
                    println!("[ERROR] {reason}");
                    break;
                },
                HandlerResult::Exit => return
            };
        }
    }
}
