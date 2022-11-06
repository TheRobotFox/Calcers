use std::boxed::Box;
use logos::Logos;
use statrs::function::gamma;
use serde_derive::{Serialize, Deserialize};
use std::option::Option;
use crate::parsers::*;

pub mod parsers;

type Number = f64;

trait Object {
    fn get_val(&self) -> Expr;
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Variable{
    name: String,
    val: Expr,
    argnames: Vec<String>
}

impl Variable{
    fn eval(&self, arglist: &Vec<Arg>) -> Result<Expr, HandlerResult> {

        let mut vars = Vec::new();

        if arglist.len()<self.argnames.len(){
            let mut error = format!("Function {} takes parameters: ", self.name.clone());
            for name in &self.argnames{
                error+=(name.clone() + " ").as_str();
            }
            return Err(HandlerResult::Error(error));
        }

        let mut index = 0;

        // ordered
        while index<self.argnames.len() {
            match &arglist[index] {
                Arg::Ordered(expr) => vars.push(Variable{ name: self.argnames[index].clone(), val: expr.clone(), argnames: vec![]}),
                Arg::Named(var) =>{
                    vars.push(var.clone());
                    break;
                }
            };
            index+=1;
        }
        while index<arglist.len() {
            match &arglist[index] {
                Arg::Named(var) => vars.push(var.clone()),
                _ => return Err(HandlerResult::Error(String::from(String::from(format!("Too many parameters! Try named mapping")))))
            };
            index+=1;
        }

        let mut expr = self.val.clone();
        match self.compile_expr(&mut expr, &vars){
            Ok(()) => (),
            Err(reason) => return Err(HandlerResult::Error(reason))
        };
        Ok(expr)

    }
    fn compile_expr(&self, expr: &mut Expr, args: &Vec<Variable>) -> Result<(),String>{
        match expr.operation.clone(){
            Token::Var(name) => {
                for var in args{
                    if *name==var.name{
                        *expr=var.val.clone();
                        return Ok(());
                    }
                }
                if self.argnames.contains(&name){
                    return Err(String::from(format!("Local variable \"{name}\" is not defined!")))
                }
            }
            _ => {
                if expr.a.is_some(){
                    self.compile_expr(&mut expr.a.as_mut().unwrap(), args)?;
                };
                if expr.b.is_some(){
                    self.compile_expr(&mut expr.b.as_mut().unwrap(), args)?;
                };
            }
        };
        Ok(())
    }
}
        
        

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct FunctionCall{
    name: String,
    arglist: Vec<Arg>
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum Arg{
    Ordered(Expr),
    Named(Variable),
}
        
        
#[derive(Serialize, Deserialize, Debug)]
pub struct Environment{
    run: bool,
    variables: Vec<Variable>,
    input: String,
    last_input: Option<String>,
    last_result: Option<Number>
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
    Num(Number),
    

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
    Call(FunctionCall),
    Var(String)
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Expr{
    a: Option<Box<Expr>>,
    b: Option<Box<Expr>>,
    operation: Token
}

impl Expr{
    fn eval(&self, env: &Environment) -> Result<Number, HandlerResult>{

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
                    return Err(HandlerResult::Error(String::from("No previous Result.")));
                }
                return Ok(env.last_result.unwrap());
            },
            Token::Call(func) => {
                let var = env.variables.iter().find(|&var| var.name==*func.name);
                if var.is_none(){
                    return Err(HandlerResult::Error(String::from(format!("Function '{}' not defined!", func.name))));
                }
                return var.unwrap().eval(&func.arglist)?.eval(env);
            },
            _ => ()

        };

        // Evaluate expressions recursivly
        let a = self.a.as_ref().unwrap().eval(env)?;

        if self.operation == Token::Fac{
            //return Err(HandlerResult::Error(String::from("Not supported by Big Float")));
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

// TODO: refactor ParseHandlers
// TODO: use defined functions
// TODO: map all operators to functions
// TODO: Serialize{
//      Only Serialize important components
//      Save strings not expressions -> parse at deserialisation
//      
// }
// TODO: Refactor Commands eg. help, description
// TODO: Improve Errors
// TODO: general cleaness
// TODO: port to actual Calculator
// TODO: Arbitrary precision Integers


fn main() {
    println!("Calc 0.6.1");
    let mut input: String = String::new();
    let mut env = Environment{run: true, variables: Vec::new(), input: String::new(), last_input: None, last_result: None};
    let parsers: &[Box<dyn ParseHandler>] = &[Box::new(AssignHandler), Box::new(CommandHandler), Box::new(ExprHandler)];
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
        input = input.trim_end().to_string();
        
        // if no input, use last valid input or skip
        if input.len()==0{
            if env.last_input.is_some(){
                input = env.last_input.clone().unwrap();
            }else{
                continue;
            }
        }

        env.input = input.clone();
        env.last_input = None;

        // Lex input
        let lex = Token::lexer(input.as_str());
        
        for parser in parsers {
            match parser.handle(&mut lex.clone(), &mut env) {
                HandlerResult::Ok => break,
                HandlerResult::Pass => {
                    //println!("Pass");
                    continue
                },
                HandlerResult::Error(reason) => {
                    println!("[ERROR] {reason}");
                    break;
                },
                HandlerResult::Exit => return
            };
        }
    }
}
