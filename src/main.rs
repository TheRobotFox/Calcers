use core::fmt;
use std::{boxed::Box, io::{Write, Read}};
use logos::Logos;
use statrs::function::gamma;
use std::option::Option;
use serde_derive::{Serialize, Deserialize};
use std::fs::File;

#[derive(Debug)]
struct MathError{
    error: String
}

impl fmt::Display for MathError{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "MathError: {}", self.error)
    }
}
struct ParseError{
    error: String
}
impl fmt::Display for ParseError{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {}", self.error)
    }
}

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
struct Environment{
    variables: Vec<Variable>
}

#[derive(Logos, Debug, PartialEq, Clone, Serialize, Deserialize)]
enum Token{

    #[token("save")]
    Save,
    #[token("exit")]
    Exit,

    #[token("load")]
    Load,

    #[token("clear")]
    Clear,

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
struct Expr{
    a: Option<Box<Expr>>,
    b: Option<Box<Expr>>,
    operation: Token
}

impl Expr{
    fn eval(&self, env: &Environment) -> Result<f64, MathError>{

        // if token is a number
        match self.operation.clone() {
            Token::Num(n) => return Ok(n),
            Token::Var(name) => {
                let var = env.variables.iter().find(|&var| var.name==name);
                if var.is_none(){
                    return Err(MathError{error: String::from(format!("Variable '{}' not defined!", name))});
                }
                return var.unwrap().val.eval(env);
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
                    Err(MathError { error: String::from("Divison by Zero") })
                }
            },
            Token::Pow => Ok(a.powf(b)),
            _ => panic!("Unexpected Token")
                
        }
    }
}


// () -> DEF | EXPR

// DEF -> VARDEF | FUNCDEF
// VARDEF -> NAME ASSIGN EXPR
// FUNCDEF -> NAME ARGLIST ASSIGN EXPR
// ARGLIST -> (ARGS)
// ARGS -> ARG | ARG COMMA ARGS

// EXPR1 -> EXPR2 + EXPR1 | EXPR2
// EXPR2 -> EXPR3 - EXPR2 | EXPR3
// EXPR3 -> EXPR4 * EXPR3 | EXPR4
// EXPR4 -> EXPR5 / EXPR4 | EXPR5
// EXPR5 -> EXPR6 ^ EXPR5 | EXPR6
// EXPR6 -> EXPR6! | EXPR7
// EXPR7 -> NUM | (EXPR1) | VAR | FUNC ARGLIST

fn parse_val(lex: &mut logos::Lexer<Token>) -> Result<Expr, ParseError> {
    match lex.next() {
        Some(Token::Num(n)) =>{
            Ok(Expr { a: None, b: None, operation: Token::Num(n.clone()) })
        },
        Some(Token::Name(name)) =>{
            Ok(Expr { a: None, b: None, operation: Token::Var(name) })
        },
        Some(Token::PrenticesOpen) => {
            let expr = parse_expr_add(lex)?;

            let prentices_closed:Option<Token> = lex.next();
            if prentices_closed != Some(Token::PrenticesClosed) {
                return Err(ParseError { error: String::from(format!("Expected ')' got '{:?}' (\"{}\") at {:?}", prentices_closed.unwrap_or(Token::EOF), lex.slice(), lex.span())) });
            }

            Ok(expr)
        },
        None => Err(ParseError { error: String::from("Unexpected EOF!") }),
        t @ _ => Err(ParseError { error: String::from(format!("Expected '(' or 'Num' got '{:?}' (\"{}\") at {:?}. ", t.unwrap(), lex.slice(), lex.span())) })
    }
}

fn parse_expr_fac(lex: &mut logos::Lexer<Token>) -> Result<Expr, ParseError> {
    let mut expr5 = parse_val(lex)?;

    loop{
        match lex.clone().peekable().peek() {
            Some(Token::Fac) => {
                // Consume Element
                lex.next();
                expr5 = Expr { a: Some(Box::new(expr5)), b: None, operation: Token::Fac };
            },
            _ => return Ok(expr5)
        };
    }
}

fn parse_level<F>(lex: &mut logos::Lexer<Token>, expect: &[Token], next_func: F) -> Result<Expr, ParseError>
    where F: Fn(&mut logos::Lexer<Token>) -> Result<Expr,ParseError>
{
    let next_expr = next_func(lex)?;

    let mut peeker = lex.clone().peekable();
    let next_token: Option<&Token> = peeker.peek();

    if next_token.is_none(){
        return Ok(next_expr);
    }
    
    for token in expect{
        if Some(token)==next_token {
            // Consume Element
            lex.next();
            let expr = parse_level(lex, expect, next_func)?;

            return Ok(Expr{ a: Some(Box::new(next_expr)), b: Some(Box::new(expr)), operation: token.clone()});
        }
    }
    Ok(next_expr)
    //Err(ParseError { error: String::from(format!("Expected {:?} or 'EOF' got '{:?}' (\"{}\") at {:?}.", expect.as_slice(), next_token.unwrap(), lex.slice(), lex.span())) })
}

fn parse_expr_pow(lex: &mut logos::Lexer<Token>) -> Result<Expr, ParseError> {
    parse_level(lex, &[Token::Pow], parse_expr_fac)
}
fn parse_expr_div(lex: &mut logos::Lexer<Token>) -> Result<Expr, ParseError> {
    parse_level(lex, &[Token::Div], parse_expr_pow)
}
fn parse_expr_mul(lex: &mut logos::Lexer<Token>) -> Result<Expr, ParseError> {
    parse_level(lex, &[Token::Mul], parse_expr_div)
}
fn parse_expr_sub(lex: &mut logos::Lexer<Token>) -> Result<Expr, ParseError> {

    parse_level(lex, &[Token::Sub], parse_expr_mul)
}
fn parse_expr_add(lex: &mut logos::Lexer<Token>) -> Result<Expr, ParseError> {

    parse_level(lex, &[Token::Add], parse_expr_sub)
}

// fn parse_signature(lex: &mut logos::Lexer<Token>, sequence: &[Token]) -> bool {
//     for token in sequence{
//         if lex.next().unwrap_or(Token::EOF)!=*token{
//             return false;
//         }
//     }
//     return true;
// }
fn parse_var(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> Result<(),ParseError>
{
    let name = match lex.next() {
        Some(Token::Name(name)) => name,
        _ => return Err(ParseError{error: String::from("Wrong Syntax")})
    };

    match lex.next() {
        Some(Token::Assign) => (),
        _ => return Err(ParseError{error: String::from("Wrong Syntax")})
    };

    let expr = parse_expr(lex)?;

    let var = env.variables.iter_mut().find(|var| var.name==name);
    if var.is_none() {
        env.variables.push(Variable{name: name, val: expr});
    }else{
        var.unwrap().val=expr;
    }

    Ok(())
}

fn parse_expr(lex: &mut logos::Lexer<Token>) -> Result<Expr, ParseError>{
    let expr = parse_expr_add(lex)?;
    let token = lex.next();
    if token.is_some() {
        return Err(ParseError { error: String::from(format!("Expected 'EOF' got '{:?}' (\"{}\") at {:?}", token.unwrap(), lex.slice(), lex.span())) });
    }
    return Ok(expr);
}

fn main() {
    let mut input: String = String::new();
    println!("Calc 0.4");
    let mut env = Environment{variables: Vec::new()};
    loop{
        input.clear();
        // get Input
        match std::io::stdin().read_line(&mut input){
            Ok(size) => if size==0{continue;},
            Err(e) =>{
                println!("[ERROR] ReadInput: {}", e);
                continue;
            }
        }
        // Lex input
        let mut lex = Token::lexer(input.trim_end());
        let mut lex_tmp = lex.clone();
        match lex_tmp.next() {
            // Save Command
            Some(Token::Save) =>{
                let path = match lex_tmp.next(){
                    Some(Token::Name(path)) => path,
                    t @ _ => {
                        println!("[ERROR] Syntax error! Expected 'Name' got '{:?}'.\nuse: save [Path]", t.unwrap_or(Token::EOF));
                        continue;
                    }
                };

                let save_str = match serde_json::to_string_pretty(&env){
                    Ok(str) => str,
                    Err(error) => {
                        println!("[ERROR] Could not serialize environment: {error}");
                        continue;
                    }
                };

                let mut file = match File::create(&path){
                    Ok(file) => file,
                    Err(error) => {
                        println!("[ERROR] Could not open file \"{path}\": {error}");
                        continue;
                    }
                };

                match file.write_all(save_str.as_bytes()) {
                    Ok(()) => println!("Saved!"),
                    Err(error) => println!("[ERROR] Could not write to file \"{path}\": {error}")
                };
                
            },
            // Load Command
            Some(Token::Load) => {
                let path = match lex_tmp.next() {
                    Some(Token::Name(path)) => path,
                    _ => {
                        println!("[ERROR] Syntax error! use: load [Path]");
                        continue;
                    }
                };
                let mut file = match File::open(&path) {
                    Ok(file) => file,
                    Err(error) => {
                        println!("[ERROR] Could not open file \"{path}\": {error}");
                        continue;
                    }
                };
                let mut load_str = String::new();
                match file.read_to_string(&mut load_str) {
                    Ok(_length) => (),
                    Err(error) => {
                        println!("[ERROR] Could not read file \"{path}\": {error}");
                        continue;
                    }
                }

                env =  match serde_json::from_str(&load_str) {
                    Ok(env) => env,
                    Err(error) => {
                        println!("[ERROR] Could not parse file: {error}");
                        continue;
                    }
                }
            },
            Some(Token::Clear) => {
                env = Environment{variables: Vec::new()};
            },
            // Exit Command
            Some(Token::Exit) => {
                println!("ByeBye!");
                break;
            },
            // Unknown Token
            Some(Token::Error) => {println!("Unknown identifiere \"{}\"", input.trim_end())}

            // Nothing
            None => {}

            // Pass to other Parsers
            _ =>{
                lex_tmp = lex.clone();
                // if assign signature -> parse Var define
                match parse_var(&mut lex_tmp, &mut env) {
                    Ok(()) => (),
                    Err(_error) => {
                        // else parse expression
                        let expr = match parse_expr(&mut lex) {
                            Ok(expr) => expr,
                            Err(error) => {
                                println!("[ERROR] {}", error.error);
                                continue;
                            }
                        };
                        // Eval input
                        match expr.eval(&env) {
                            Ok(val) => println!("= {}", val),
                            Err(error) => println!("[ERROR] {}", error.error)
                        };
                    }
                };
            }
        }

    }
}
