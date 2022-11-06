use std::boxed::Box;
use std::option::Option;
use std::fs::File;
use std::io::Write;
use crate::{Expr, Token, Environment, Variable, FunctionCall, Arg, VERSION, calc_parse_file, MacroVal, calc_parse, Macro};

pub trait ParseHandler{
    fn handle(&self, lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult;
}

#[derive(Clone)]
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

fn check_eof(lex: &mut logos::Lexer<Token>, success: HandlerResult) -> HandlerResult{

    match lex.next(){
        None => success,
        Some(token) => HandlerResult::Error(format!("Expected 'EOF' got '{:?}' (\"{}\") at {:?}", token, lex.slice(), lex.span()))
    }
}
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
                match AssignHandler::parse_variable(&mut lex_tmp) {
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
                    t @ _ => return Err(HandlerResult::Error(format!("Expected 'Comma' or ')' got '{:?}' (\"{}\") at {:?}. ", t.unwrap_or(Token::EOF), lex.slice(), lex.span())))
                };
            };
            // Named args
            loop {
                
                match lex.next() {
                    Some(Token::Comma) => (),
                    Some(Token::PrenticesClosed) => return Ok(arglist),
                    t @ _ => return Err(HandlerResult::Error(format!("Expected 'Comma' or ')' got '{:?}' (\"{}\") at {:?}. ", t.unwrap_or(Token::EOF), lex.slice(), lex.span())))
                };

                match AssignHandler::parse_variable(lex) {
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
            Some(Token::Add) => return Self::parse_val(lex),
            Some(Token::Sub) => {
                let expr = match Self::parse_val(lex){
                    Ok(expr) => expr,
                    Err(reason) => return Err(reason)
                };
                Ok(Expr { a: Some(Box::new(Expr{a: None, b: None, operation: Token::Num(-1.0)})), b: Some(Box::new(expr)), operation: Token::Mul })
            }
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
                let expr = Self::parse_expr(lex)?;

                let prentices_closed:Option<Token> = lex.next();
                if prentices_closed != Some(Token::PrenticesClosed) {
                    return Err(format!("Expected ')' got '{:?}' (\"{}\") at {:?}", prentices_closed.unwrap_or(Token::EOF), lex.slice(), lex.span()));
                }

                Ok(expr)
            },
            Some(Token::Ans) => Ok(Expr { a: None, b: None, operation: Token::Ans }),
            None => Err(String::from("Unexpected EOF!")),
            t @ _ => Err(format!("Expected '(' or 'Num' got '{:?}' (\"{}\") at {:?}. ", t.unwrap(), lex.slice(), lex.span()))
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
        //Err(ParseError { error: format!("Expected {:?} or 'EOF' got '{:?}' (\"{}\") at {:?}.", expect.as_slice(), next_token.unwrap(), lex.slice(), lex.span())) })
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
    fn parse_expr_cmp(lex: &mut logos::Lexer<Token>) -> Result<Expr, String> {

        Self::parse_level(lex, &[Token::Less, Token::Greater, Token::Equal], Self::parse_expr_add)
    }
    pub fn parse_expr(lex: &mut logos::Lexer<Token>) -> Result<Expr, String>{
        return Ok(Self::parse_expr_cmp(lex)?);
    }
}

impl ParseHandler for ExprHandler{
    fn handle(&self, lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult {
        let expr = match Self::parse_expr(lex) {
            Ok(expr) => expr,
            Err(reason) =>{return HandlerResult::Error(reason);}
        };

        match lex.next() {
            None =>(),
            Some(e@_) => return HandlerResult::Error(format!("Expected 'EOF' got '{:?}' (\"{:?}\") at {:?}", e, lex.slice(), lex.span()))
        };

        let val = match expr.eval(env) {
            Ok(val) => val,
            Err(error) => {return error;}
        };
        println!("= {}", val);

        env.last_result=Some(val);
        env.last_input=Some(lex.source().to_string());
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
                        t @ _ => return Err(HandlerResult::Error(format!("Expected 'COMMA' or ')' got '{:?}' (\"{}\") at {:?}. ", t.unwrap_or(Token::EOF), lex.slice(), lex.span())))
                    };
                }
            },
            _ => Err(HandlerResult::Pass)
        }

    }

    fn parse_variable(lex: &mut logos::Lexer<Token>) -> Result<Variable, HandlerResult>{
        
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
            _ => return Err(HandlerResult::Pass)
        };

        let expr = match ExprHandler::parse_expr(lex) {
            Ok(expr) => expr,
            Err(reason) => return Err(HandlerResult::Error(reason))
        };


        Ok(Variable{name: name, val: expr, argnames: argnames, def_string: None})
    }
    fn parse_macro(lex: &mut logos::Lexer<Token>) -> Result<Macro, HandlerResult>{

        let name = match lex.next() {
            Some(Token::Name(name)) => name,
            _ => return Err(HandlerResult::Pass)
        };

        let mut lex_tmp = lex.clone();
        let argnames = match Self::parse_argnames(&mut lex_tmp) {
            Ok(argnames) => {
                *lex=lex_tmp;
                argnames
            },
            Err(HandlerResult::Pass) => vec![],
            Err(HandlerResult::Error(reason)) => return Err(HandlerResult::Error(reason)),
            _ => return Err(HandlerResult::Error(String::from("Unexpected error! 319")))
        };

        match lex.next() {
            Some(Token::Macro) => (),
            _ => return Err(HandlerResult::Pass)
        };

        // Parse Commands
        let commands: Vec<String> = lex.remainder().split("|").map(str::to_string).collect();
        

        Ok(Macro { name: name, val: MacroVal::User(commands) })
    }

}

impl ParseHandler for AssignHandler{
    fn handle(&self, lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult
    {
        let mut lex_tmp = lex.clone();
        match Self::parse_variable(&mut lex_tmp){
            Ok(mut var) => {
                var.def_string=Some(lex.source().to_string());

                // check self reference
                for token in lex.into_iter().skip(2){
                    match token {
                        Token::Name(n) => {
                            if n==var.name{
                                // fix self reference
                                let prev_val = match var.val.eval(env) {
                                    Ok(val) => val,
                                    Err(error) => return error
                                };

                                let mut expr = var.val.clone();
                                match var.compile_expr(&mut expr,
                                            &vec![Variable{argnames: vec![], def_string: None, name: var.name.clone(),
                                                val: Expr { a: None, b: None, operation: Token::Num(prev_val)}}])
                                {
                                    Ok(()) => (),
                                    Err(error) => return HandlerResult::Error(error)
                                }
                                var.val=expr;
                                break;
                            }
                        },
                        _ => ()
                    }
                }
                *lex=lex_tmp;
                match env.variables.iter_mut().find(|e| e.name==var.name) {
                    None => {env.variables.push(var);},
                    Some(e) => {
                        *e=var;
                    }
                };
                check_eof(lex, HandlerResult::Ok)
            },
            // Assign Macro
            Err(HandlerResult::Pass) => match Self::parse_macro(lex){
                Ok(mac) =>{
                    match env.macros.iter_mut().find(|e| e.name==mac.name) {
                        None => {env.macros.push(mac);},
                        Some(e) => {
                            *e=mac;
                        }
                    };
                    HandlerResult::Ok
                },
                Err(e) => return e
            }
            Err(e) => return e
        }
    }
}

pub struct MacroHandler;

impl MacroHandler{
	pub fn parse_save(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult
	{

		let path = match lex.next(){
			Some(Token::Name(path)) => path,
			t @ _ => {
			return HandlerResult::Error(format!("Expected 'Name' got '{:?}'.", t.unwrap_or(Token::EOF)));
			}
		};

        let mut buff: String = format!("\"Calcers v{VERSION} Environment\"\n\n\n");

        for var in &env.variables{
            match &var.def_string{
                Some(str) => buff+=format!("\"Define {} {}\"\n{}\n\n", var.obj_type(), &var.name, str).as_str(),
                None => return HandlerResult::Error(format!("Definition of {} \"{}\" not known!", var.obj_type(), var.name))
            }
        }

		let mut file = match File::create(&path){
			Ok(file) => file,
			Err(error) => {
			return HandlerResult::Error(format!("Could not open file \"{path}\": {error}"));
			}
		};

		match file.write_all(buff.as_bytes()) {
			Ok(()) => HandlerResult::Ok,
			Err(error) => HandlerResult::Error(format!("Could not write to file \"{path}\": {error}"))
		}
			
	}
	pub fn parse_load(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult
	{

        let path = match lex.next() {
            Some(Token::Name(path)) => path,
            t @ _ => {
                return HandlerResult::Error(format!("Expected 'Name' got '{:?}'. \nUsage: load [Path]", t.unwrap_or(Token::EOF)));
            }
        };
        match calc_parse_file(path, env){
            HandlerResult::Ok => (),
            HandlerResult::Error(reason) => return HandlerResult::Error(reason),
            _ => return HandlerResult::Error(String::from("Unexpected error! 378"))
        }
		println!("Successfully loaded environment");
		HandlerResult::Ok
	}
	pub fn parse_clear(_lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult
    {
        *env = Environment{run: true,
                                    variables: Vec::new(),
                                    last_input: None,
                                    last_result: None,
                                    macros: vec![Macro{name: String::from("save"), val: MacroVal::Internal(MacroHandler::parse_save)},
                                                Macro{name: String::from("load"), val: MacroVal::Internal(MacroHandler::parse_load)},
                                                Macro{name: String::from("clear"), val: MacroVal::Internal(MacroHandler::parse_load)}
                                            ]
                                    };
        HandlerResult::Ok
    }
	pub fn parse_exit(_lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult
    {
        env.run=false;
        HandlerResult::Ok
    }
}


impl ParseHandler for MacroHandler{
    fn handle(&self, lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult
    {
        if lex.next()!=Some(Token::Macro){
            return HandlerResult::Pass;
        }
        let name = match lex.next() {
            Some(Token::Name(command)) => command,
            t @ _ => { return HandlerResult::Error(format!("Expected 'Name' got '{:?}' (\"{}\") at {:?}", t.unwrap_or(Token::EOF), lex.slice(), lex.span()))}
        };

        let mac = match env.macros.iter().find(|e| e.name==name) {
            Some(mac) => mac.clone(),
            None => return HandlerResult::Error(format!("Unknown Macro \"{name}\"!"))
        };

        let res = match &mac.val {
            MacroVal::Internal(func) => func(lex, env),
            MacroVal::User(commands) =>{
                for command in commands{
                    calc_parse(&command, env);
                }
                HandlerResult::Ok
            }
        };

        check_eof(lex, res)
    }
}

pub struct CommentHandler;
impl ParseHandler for CommentHandler{
    fn handle(&self, lex: &mut logos::Lexer<Token>, _env: &mut Environment) -> HandlerResult{
        match lex.next() {
            None => HandlerResult::Ok,
            Some(Token::Comment) => HandlerResult::Ok,
            _ => HandlerResult::Pass
        }

    }
}