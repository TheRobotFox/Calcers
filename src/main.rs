use std::boxed::Box;
use std::path::Path;
use logos::Logos;
use statrs::function::gamma;
use std::option::Option;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::env;
use crate::parsers::*;

pub mod parsers;

type Number = f64;

trait Object {
    fn get_val(&self) -> Expr;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable{
    name: String,
    val: Expr,
    argnames: Vec<String>,
    def_string: Option<String>
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
                Arg::Ordered(expr) => vars.push(Variable{ name: self.argnames[index].clone(), val: expr.clone(), argnames: vec![], def_string: None}),
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
                _ => return Err(HandlerResult::Error(String::from(format!("Too many parameters! Try named mapping"))))
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
        match &mut expr.operation {
            Token::Call(func) => {
                for mut arg in &mut func.arglist {
                    match &mut arg {
                        Arg::Ordered(expr) =>{
                            self.compile_expr(expr, args)?;
                        }
                        Arg::Named(var) =>{
                            self.compile_expr(&mut var.val, args)?;
                        }
                    };
                }
            }
            _ => ()
        };
        match expr.operation.clone(){
            Token::Var(name) => {
                for var in args{
                    if *name==var.name{
                        *expr=var.val.clone();
                        return Ok(());
                    }
                }
                if self.argnames.contains(&name){
                    return Err(format!("Local variable \"{name}\" is not defined!"))
                }
            },
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
    fn obj_type(&self) -> &str{
        if self.argnames.len()==0 {"Variable"}else{"Function"}
    }
}
        
        

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall{
    name: String,
    arglist: Vec<Arg>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Arg{
    Ordered(Expr),
    Named(Variable),
}

#[derive(Clone)]
pub enum MacroVal{
    User(Vec<String>),
    Internal(fn(&mut logos::Lexer<Token>, &mut Environment) -> HandlerResult)
}
#[derive(Clone)]
pub struct Macro{
    name: String,
    val: MacroVal
}

pub struct Function{
    name: String,
    val: Box<dyn Fn(&Vec<Arg>, &Environment) -> Result<Number, HandlerResult>>
}

pub struct Environment{
    run: bool,
    variables: Vec<Variable>,
    defined_functions: Vec<Function>,
    macros: Vec<Macro>,
    last_input: Option<String>,
    last_result: Option<Number>
}

#[derive(Logos, PartialEq, Debug, Clone)]
pub enum Token{
    // Comments
    #[regex("\"[^\"]*\"", logos::skip)]
    Comment,

    #[token(":")]
    Macro,

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

    #[token("%")]
    Mod,

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

    #[token("[")]
    EvalOpen,

    #[token("]")]
    EvalClosed,

    #[error]
    // whitespaces
    #[regex(r"[ \t\n\f]", logos::skip)]
    Unknown,

    // Internal States
    EOF,
    Call(FunctionCall),
    Var(String)
}

#[derive(Debug, PartialEq, Clone)]
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
                    return Err(HandlerResult::Error(format!("Variable '{}' not defined!", name)));
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
                    let var = env.defined_functions.iter().find(|&var| var.name==*func.name);
                    if var.is_none(){
                        return Err(HandlerResult::Error(format!("Function '{}' not defined!", func.name)));
                    }
                    return (var.unwrap().val)(&func.arglist, env);
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
            Token::Mod => {
                if b < -0.00001 || b > 0.000001{
                    Ok(a % b)
                }
                else {
                    Err(HandlerResult::Error(String::from("Modules by Zero")))
                }
            }
            Token::Pow => Ok(a.powf(b)),
            Token::Greater => Ok(Into::<i8>::into(a>b).into()),
            Token::Less => Ok(Into::<i8>::into(a<b).into()),
            Token::Equal => Ok(Into::<i8>::into(a==b).into()),
            t @ _ => Err(HandlerResult::Error(format!("Unexpected Token '{:?}' as operation.", t)))
                
        }
    }
}

// TODO: Disallow self reference Variables
// TODO: add eval modifier
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
// TODO: Calc Class with env generator
// TODO: Env mod (...)

pub fn calc_parse(input: &String, env: &mut Environment){

    let parsers: &[Box<dyn ParseHandler>] = &[Box::new(CommentHandler),Box::new(AssignHandler), Box::new(MacroHandler), Box::new(ExprHandler)];
    let mut input = input.clone();

    // if no input, use last valid input or skip
    if input.len()==0{
        if env.last_input.is_some(){
            input = env.last_input.as_ref().unwrap().clone();
        }else{
            return;
        }
    }
    
    env.last_input = None;

    // Lex input
    let lex = Token::lexer(input.as_str());

    for parser in parsers {
        match parser.handle(&mut lex.clone(), env) {
            HandlerResult::Ok => break,
            HandlerResult::Pass => {
                //println!("Pass");
                continue;
            },
            HandlerResult::Error(reason) => {
                println!("[ERROR] {reason}");
                break;
            },
            HandlerResult::Exit => return
        };
    }
}

pub fn calc_parse_file<T: AsRef<Path> + std::fmt::Debug>(path: T, env: &mut Environment) -> HandlerResult{

    let reader = match File::open(&path){
        Ok(file) => BufReader::new(file),
        Err(error) => {
        return HandlerResult::Error(format!("Could not open file \"{:?}\": {error}", path));
        }
    };

    let lines = reader.lines();

    for line in lines{
        match line {
            Ok(str) => calc_parse(&str, env),
            Err(error) => return HandlerResult::Error(format!("Could not read file: {}",error.to_string()))
        };
    }
    HandlerResult::Ok
}
pub const VERSION: &str = "0.8.4";

fn main() {
    println!("Calc v{VERSION}");

    let mut env = Environment{run: true,
                                           variables: Vec::new(),
                                           last_input: None,
                                           last_result: None,
                                           macros: vec![Macro{name: String::from("save"), val: MacroVal::Internal(MacroHandler::parse_save)},
                                                        Macro{name: String::from("load"), val: MacroVal::Internal(MacroHandler::parse_load)},
                                                        Macro{name: String::from("clear"), val: MacroVal::Internal(MacroHandler::parse_load)},
                                                        Macro{name: String::from("exit"), val: MacroVal::Internal(MacroHandler::parse_exit)}
                                                    ],
                                           defined_functions: vec![
                                                        Function{ name: String::from("ceil"), val: Box::new(|arg_list: &Vec<Arg>, env: &Environment| Ok(f64::ceil(match &arg_list[0] {Arg::Named(var) => var.val.clone(), Arg::Ordered(val) => val.clone()}.eval(env)?)))},
                                                        Function{ name: String::from("floor"), val: Box::new(|arg_list: &Vec<Arg>, env: &Environment| Ok(f64::floor(match &arg_list[0] {Arg::Named(var) => var.val.clone(), Arg::Ordered(val) => val.clone()}.eval(env)?)))}
                                                    ]
                                          };

    for path in env::args().skip(1){
        match calc_parse_file(&path, &mut env){
            HandlerResult::Ok => (),
            HandlerResult::Error(reason) => println!("[ERROR] While loading file \"{path}\": {reason}"),
            _ => println!("Unexpected Error! 357")
        }
    }

    let mut input: String = String::new();
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
        calc_parse(&input, &mut env);
    }
}
