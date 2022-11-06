use std::boxed::Box;
use std::option::Option;
use std::fs::File;
use std::io::{Read, Write};
use serde_json;
use crate::{Expr, Token, Environment, Variable, FunctionCall, Arg};

pub trait ParseHandler{
    fn handle(&self, lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult;
}

pub enum HandlerResult{
    Error(String),
    Pass,
    Ok,
    Exit
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


fn parse_arglist(lex: &mut logos::Lexer<Token>) -> Result<Vec<Arg>, HandlerResult> {
    // let mut lex_tmp = lex.clone();
    // match ExprHandler::parse_val(&mut lex_tmp) {
    //     Ok(expr) => {
    //         *lex=lex_tmp;
    //         vec![Arg::Ordered(expr)]
    //     }
    //     Err(reason) => return Err(HandlerResult::Error(reason))
    // };

    match lex.next() {
        Some(Token::PrenticesOpen) => {
            let mut arglist = Vec::new();
            let mut lex_tmp;

            // Ordered Args
            loop {

                lex_tmp = lex.clone();
                match AssignHandler::parse_assign(&mut lex_tmp) {
                    Ok(var) => {
                        arglist.push(Arg::Named(var));
                        *lex=lex_tmp;
                        break;
                    },
                    Err(HandlerResult::Pass) => {
                        let expr = match ExprHandler::parse_expr(lex) {
                            Ok(expr) => expr,
                            Err(reason) => return Err(HandlerResult::Error(reason))
                        };
                        arglist.push(Arg::Ordered(expr));
                    },
                    Err(HandlerResult::Error(reason)) => return Err(HandlerResult::Error(reason)),
                    _ => return Err(HandlerResult::Error(String::from("Unexpected error! 70")))
                };

                match lex.next() {
                    Some(Token::Comma) => (),
                    Some(Token::PrenticesClosed) => return Ok(arglist),
                    t @ _ => return Err(HandlerResult::Error(String::from(format!("Expected 'Comma' or ')' got '{:?}' (\"{}\") at {:?}. ", t.unwrap_or(Token::EOF), lex.slice(), lex.span()))))
                };
            };
            // Named args
            loop {
                
                match lex.next() {
                    Some(Token::Comma) => (),
                    Some(Token::PrenticesClosed) => return Ok(arglist),
                    t @ _ => return Err(HandlerResult::Error(String::from(format!("Expected 'Comma' or ')' got '{:?}' (\"{}\") at {:?}. ", t.unwrap_or(Token::EOF), lex.slice(), lex.span()))))
                };

                match AssignHandler::parse_assign(lex) {
                    Ok(var) => {
                        arglist.push(Arg::Named(var));
                    },
                    Err(HandlerResult::Error(reason)) => return Err(HandlerResult::Error(reason)),
                    _ => return Err(HandlerResult::Error(String::from("Unexpected error! 93")))
                };
            };
        },
        _ => return Err(HandlerResult::Pass)
    };
}

pub struct ExprHandler;

impl ExprHandler{

    fn parse_val(lex: &mut logos::Lexer<Token>) -> Result<Expr, String> {
        match lex.next() {
            Some(Token::Num(n)) => Ok(Expr { a: None, b: None, operation: Token::Num(n.clone()) }) ,
            Some(Token::Name(name)) =>{
                let mut lex_tmp = lex.clone();
                match parse_arglist(&mut lex_tmp) {
                    Ok(arg) => {
                        *lex=lex_tmp;
                        return Ok(Expr { a: None, b: None, operation: Token::Call(FunctionCall{name: name, arglist: arg}) });
                    },
                    Err(HandlerResult::Pass) => Ok(Expr { a: None, b: None, operation: Token::Var(name) }),

                    Err(HandlerResult::Error(reason)) => return Err(reason),
                    _ => return Err(String::from("Unexpected Error! 118"))
                }
            },
            Some(Token::PrenticesOpen) => {
                let expr = Self::parse_expr_add(lex)?;

                let prentices_closed:Option<Token> = lex.next();
                if prentices_closed != Some(Token::PrenticesClosed) {
                    return Err(String::from(format!("Expected ')' got '{:?}' (\"{}\") at {:?}", prentices_closed.unwrap_or(Token::EOF), lex.slice(), lex.span())));
                }

                Ok(expr)
            },
            Some(Token::Ans) => Ok(Expr { a: None, b: None, operation: Token::Ans }),
            None => Err(String::from("Unexpected EOF!")),
            t @ _ => Err(String::from(format!("Expected '(' or 'Num' got '{:?}' (\"{}\") at {:?}. ", t.unwrap(), lex.slice(), lex.span())))
        }
    }

    fn parse_expr_fac(lex: &mut logos::Lexer<Token>) -> Result<Expr, String> {
        let mut expr5 = Self::parse_val(lex)?;

        loop{
            match lex.clone().next() {
                Some(Token::Fac) => {
                    // Consume Element
                    lex.next();
                    expr5 = Expr { a: Some(Box::new(expr5)), b: None, operation: Token::Fac };
                },
                _ => return Ok(expr5)
            };
        }
    }

    fn parse_level<F>(lex: &mut logos::Lexer<Token>, expect: &[Token], next_func: F) -> Result<Expr, String>
        where F: Fn(&mut logos::Lexer<Token>) -> Result<Expr, String>
    {
        let next_expr = next_func(lex)?;

        let next_token: Option<Token> = lex.clone().next();

        if next_token.is_none(){
            return Ok(next_expr);
        }
        
        for token in expect{
            if Some(token)==next_token.as_ref() {
                // Consume Element
                lex.next();
                let expr = Self::parse_level(lex, expect, next_func)?;

                return Ok(Expr{ a: Some(Box::new(next_expr)), b: Some(Box::new(expr)), operation: token.clone()});
            }
        }
        Ok(next_expr)
        //Err(ParseError { error: String::from(format!("Expected {:?} or 'EOF' got '{:?}' (\"{}\") at {:?}.", expect.as_slice(), next_token.unwrap(), lex.slice(), lex.span())) })
    }

    fn parse_expr_pow(lex: &mut logos::Lexer<Token>) -> Result<Expr, String> {
        Self::parse_level(lex, &[Token::Pow], Self::parse_expr_fac)
    }
    fn parse_expr_div(lex: &mut logos::Lexer<Token>) -> Result<Expr, String> {
        Self::parse_level(lex, &[Token::Div], Self::parse_expr_pow)
    }
    fn parse_expr_mul(lex: &mut logos::Lexer<Token>) -> Result<Expr, String> {
        Self::parse_level(lex, &[Token::Mul], Self::parse_expr_div)
    }
    fn parse_expr_sub(lex: &mut logos::Lexer<Token>) -> Result<Expr, String> {

        Self::parse_level(lex, &[Token::Sub], Self::parse_expr_mul)
    }
    fn parse_expr_add(lex: &mut logos::Lexer<Token>) -> Result<Expr, String> {

        Self::parse_level(lex, &[Token::Add], Self::parse_expr_sub)
    }
    pub fn parse_expr(lex: &mut logos::Lexer<Token>) -> Result<Expr, String>{
        let expr = Self::parse_expr_add(lex)?;
        return Ok(expr);
    }
}

impl ParseHandler for ExprHandler{
    fn handle(&self, lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult {
        let expr = match Self::parse_expr(lex) {
            Ok(expr) => expr,
            Err(reason) =>{return HandlerResult::Error(reason);}
        };

        let token = lex.next();
        if token.is_some() {
            return HandlerResult::Error(String::from(format!("Expected 'EOF' got '{:?}' (\"{}\") at {:?}", token.unwrap(), lex.slice(), lex.span())));
        }

        let val = match expr.eval(env) {
            Ok(val) => val,
            Err(error) => {return error;}
        };
        println!("= {}", val);

        env.last_result=Some(val);
        env.last_input=Some(env.input.clone());
        HandlerResult::Ok
    }
}
// fn parse_signature(lex: &mut logos::Lexer<Token>, sequence: &[Token]) -> bool {
//     for token in sequence{
//         if lex.next().unwrap_or(Token::EOF)!=*token{
//             return false;
//         }
//     }
//     return true;
// }

pub struct AssignHandler;

impl AssignHandler{
    fn parse_argnames(lex: &mut logos::Lexer<Token>) -> Result<Vec<String>, HandlerResult> {
        match lex.next() {
            Some(Token::Name(n)) => Ok(vec!(n)),
            Some(Token::PrenticesOpen) => {
                let mut argnames = Vec::new();

                loop{

                    match lex.next() {
                        Some(Token::Name(n)) => argnames.push(n),
                        _ => return Err(HandlerResult::Pass)
                    };

                    match lex.next() {
                        Some(Token::Comma) => (),
                        Some(Token::PrenticesClosed) => return Ok(argnames),
                        Some(Token::Assign) => return Err(HandlerResult::Pass),
                        t @ _ => return Err(HandlerResult::Error(String::from(format!("Expected 'COMMA' or ')' got '{:?}' (\"{}\") at {:?}. ", t.unwrap_or(Token::EOF), lex.slice(), lex.span()))))
                    };
                }
            },
            _ => Err(HandlerResult::Pass)
        }

    }

    fn parse_assign(lex: &mut logos::Lexer<Token>) -> Result<Variable, HandlerResult>{
        
        let name = match lex.next() {
            Some(Token::Name(name)) => name,
            _ => return Err(HandlerResult::Pass)
        };

        let mut lex_tmp = lex.clone();

        let argnames = match Self::parse_argnames(&mut lex_tmp) {
            Ok(argnames) =>{
                *lex=lex_tmp;
                argnames
            },
            Err(HandlerResult::Pass) => vec![],
            Err(HandlerResult::Error(reason)) => return Err(HandlerResult::Error(reason)),
            _ => return Err(HandlerResult::Error(String::from("Unexpected error! 276")))
        };
        match lex.next() {
            Some(Token::Assign) => (),
            // Wrong Syntax, pass to next Parser
            _ => return Err(HandlerResult::Pass)
        };

        let expr = match ExprHandler::parse_expr(lex) {
            Ok(expr) => expr,
            Err(reason) => return Err(HandlerResult::Error(reason))
        };
        Ok(Variable{name: name, val: expr, argnames: argnames})
    }
}

impl ParseHandler for AssignHandler{
    fn handle(&self, lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult
    {
        let def = match Self::parse_assign(lex){
            Ok(var) => var,
            Err(e) => return e
        };

        match env.variables.iter_mut().find(|var| var.name==def.name) {
            None => {env.variables.push(def);},
            Some(var) => {
                *var=def;
            }
        };
        
        HandlerResult::Ok
    }
}

pub struct CommandHandler;

impl CommandHandler{
	fn parse_save(lex: &mut logos::Lexer<Token>, env: &Environment) -> HandlerResult
	{

		let path = match lex.next(){
			Some(Token::Name(path)) => path,
			t @ _ => {
			return HandlerResult::Error(String::from(format!("Expected 'Name' got '{:?}'.", t.unwrap_or(Token::EOF))));
			}
		};

		let save_str = match serde_json::to_string_pretty(&env){
			Ok(str) => str,
			Err(error) => {
			return HandlerResult::Error(String::from(format!("Could not serialize environment: {error}")));
			}
		};

		let mut file = match File::create(&path){
			Ok(file) => file,
			Err(error) => {
			return HandlerResult::Error(String::from(format!("Could not open file \"{path}\": {error}")));
			}
		};

		match file.write_all(save_str.as_bytes()) {
			Ok(()) => HandlerResult::Ok,
			Err(error) => HandlerResult::Error(String::from(format!("Could not write to file \"{path}\": {error}")))
		}
			
	}
	fn parse_load(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult
	{

                let path = match lex.next() {
                    Some(Token::Name(path)) => path,
                    t @ _ => {
                        return HandlerResult::Error(format!("Expected 'Name' got '{:?}'. \nUsage: load [Path]", t.unwrap_or(Token::EOF)));
                    }
                };

                let mut file = match File::open(&path) {
                    Ok(file) => file,
                    Err(error) => {
                        return HandlerResult::Error(format!("Could not open file \"{path}\": {error}"));
                    }
                };
                let mut load_str = String::new();
                match file.read_to_string(&mut load_str) {
                    Ok(_length) => (),
                    Err(error) => {
                        return HandlerResult::Error(format!("Could not read file \"{path}\": {error}"));
                    }
                }

                *env =  match serde_json::from_str(&load_str) {
                    Ok(env) => env,
                    Err(error) => {
                        return HandlerResult::Error(format!("Could not parse file: {error}"));
                    }
                };
		println!("Successfully loaded environment");
		HandlerResult::Ok
	}
}

impl ParseHandler for CommandHandler{
    fn handle(&self, lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult
    {
	if lex.next()!=Some(Token::Command){
		return HandlerResult::Pass;
	}
	let command = match lex.next() {
		Some(Token::Name(command)) => command,
		t @ _ => { return HandlerResult::Error(String::from(format!("Expected 'Name' got '{:?}' (\"{}\") at {:?}", t.unwrap_or(Token::EOF), lex.slice(), lex.span())))}
	};
	match command.as_str(){
		"save" => Self::parse_save(lex, env),
		"load" => Self::parse_load(lex, env),
		"clear" => {
			*env = Environment{run: true, variables: Vec::new(), input: String::new(), last_input: None, last_result: None};
			HandlerResult::Ok
		},
		"exit" => {
			println!("ByeBye!");
			HandlerResult::Exit
		}
		_ => {
			HandlerResult::Error(String::from(format!("Unknown Command \"{command}\"")))
		}
	}
    }
}