use std::boxed::Box;
use std::option::Option;
use std::fs::File;
use std::io::Write;
use crate::{Expr, Token, Environment, Variable, FunctionCall, Arg, VERSION, calc_parse_file, MacroVal, calc_parse, Macro, Function};

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
fn parse_arglist(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> Result<Vec<Arg>, HandlerResult> {
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
                match AssignHandler::parse_variable(&mut lex_tmp, env) {
                    Ok(var) => {
                        arglist.push(Arg::Named(var));
                        *lex=lex_tmp;
                        break;
                    },
                    Err(HandlerResult::Pass) => {
                        let expr = match ExprHandler::parse_expr(lex, env) {
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

                match AssignHandler::parse_variable(lex, env) {
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

    fn parse_val(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> Result<Expr, String> {
        match lex.next() {
            Some(Token::Add) => return Self::parse_val(lex, env),
            Some(Token::Sub) => {
                let expr = match Self::parse_val(lex, env){
                    Ok(expr) => expr,
                    Err(reason) => return Err(reason)
                };
                Ok(Expr { a: Some(Box::new(Expr{a: None, b: None, operation: Token::Num(-1.0)})), b: Some(Box::new(expr)), operation: Token::Mul })
            }
            Some(Token::Num(n)) => Ok(Expr { a: None, b: None, operation: Token::Num(n.clone()) }) ,
            Some(Token::Name(name)) =>{
                let mut lex_tmp = lex.clone();
                match parse_arglist(&mut lex_tmp, env) {
                    Ok(arg) => {
                        *lex=lex_tmp;
                        return Ok(Expr { a: None, b: None, operation: Token::Call(FunctionCall{name: name, arglist: arg}) });
                    },
                    Err(HandlerResult::Pass) =>{
                        if name.starts_with("log") {
                            if let Ok(base) = name[3..].to_string().parse::<f64>() {
                                match lex.next() {
                                    Some(Token::Num(n)) => {
                                        return Ok(Expr { a: None, b: None, operation: Token::Call(FunctionCall{name: "log".to_string(), arglist: vec![Arg::Ordered(Expr{a: None, b: None, operation: Token::Num(base)}), Arg::Ordered(Expr{a: None, b: None, operation: Token::Num(n)})]})})
                                    },
                                    Some(Token::Name(var)) => {
                                        return Ok(Expr { a: None, b: None, operation: Token::Call(FunctionCall{name: "log".to_string(), arglist: vec![Arg::Ordered(Expr{a: None, b: None, operation: Token::Num(base)}) , Arg::Ordered(Expr{a: None, b: None, operation: Token::Var(var)})]})})
                                    },
                                    _ => return Err(String::from("Unexpected Error! 118"))
                                };
                            };
                        }
                        Ok(Expr { a: None, b: None, operation: Token::Var(name) })
                    },

                    Err(HandlerResult::Error(reason)) => return Err(reason),
                    _ => return Err(String::from("Unexpected Error! 118"))
                }
            },
            Some(Token::PrenticesOpen) => {
                let expr = Self::parse_expr(lex, env)?;

                let prentices_closed:Option<Token> = lex.next();
                if prentices_closed != Some(Token::PrenticesClosed) {
                    return Err(format!("Expected ')' got '{:?}' (\"{}\") at {:?}", prentices_closed.unwrap_or(Token::EOF), lex.slice(), lex.span()));
                }

                Ok(expr)
            },
            Some(Token::EvalOpen) => {
                let val = match Self::parse_expr(lex, env)?.eval(env) {
                    Ok(val) => val,
                    Err(HandlerResult::Error(res)) => return Err(res),
                    _ => return Err(String::from("Unexpected 150"))
                };

                let prentices_closed:Option<Token> = lex.next();
                if prentices_closed != Some(Token::EvalClosed) {
                    return Err(format!("Expected ']' got '{:?}' (\"{}\") at {:?}", prentices_closed.unwrap_or(Token::EOF), lex.slice(), lex.span()));
                }

                Ok(Expr{a: None, b: None, operation: Token::Num(val)})
            }
            Some(Token::Ans) => Ok(Expr { a: None, b: None, operation: Token::Ans }),
            None => Err(String::from("Unexpected EOF!")),
            t @ _ => Err(format!("Expected '(' or 'Num' got '{:?}' (\"{}\") at {:?}. ", t.unwrap(), lex.slice(), lex.span()))
        }
    }

    fn parse_expr_fac(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> Result<Expr, String> {
        let mut expr5 = Self::parse_val(lex, env)?;

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

    fn parse_level<FL,FR>(lex: &mut logos::Lexer<Token>, env: &mut Environment, expect: &[Token], left_func: FL, right_func: FR) -> Result<Expr, String>
        where FL: Fn(&mut logos::Lexer<Token>, &mut Environment) -> Result<Expr, String>,
              FR: Fn(&mut logos::Lexer<Token>, &mut Environment) -> Result<Expr, String>
    {
        let next_expr = right_func(lex, env)?;

        let next_token: Option<Token> = lex.clone().next();

        if next_token.is_none(){
            return Ok(next_expr);
        }
        
        for token in expect{
            if Some(token)==next_token.as_ref() {
                // Consume Element
                lex.next();
                let expr = left_func(lex, env)?;

                return Ok(Expr{ a: Some(Box::new(next_expr)), b: Some(Box::new(expr)), operation: token.clone()});
            }
        }
        Ok(next_expr)
        //Err(ParseError { error: format!("Expected {:?} or 'EOF' got '{:?}' (\"{}\") at {:?}.", expect.as_slice(), next_token.unwrap(), lex.slice(), lex.span())) })
    }

    fn parse_expr_pow(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> Result<Expr, String> {
        Self::parse_level(lex, env, &[Token::Pow], Self::parse_expr_pow, Self::parse_expr_fac)
    }
    fn parse_expr_div(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> Result<Expr, String> {
        Self::parse_level(lex, env, &[Token::Div], Self::parse_expr_div, Self::parse_expr_pow)
    }
    fn parse_expr_mul(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> Result<Expr, String> {
        Self::parse_level(lex, env, &[Token::Mul], Self::parse_expr_mul, Self::parse_expr_div)
    }
    fn parse_expr_mod(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> Result<Expr, String> {
        Self::parse_level(lex, env, &[Token::Mod], Self::parse_expr_mod, Self::parse_expr_mul)
    }
    fn parse_expr_sub(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> Result<Expr, String> {

        Self::parse_level(lex, env, &[Token::Sub], Self::parse_expr_sub, Self::parse_expr_mod)
    }
    fn parse_expr_add(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> Result<Expr, String> {

        Self::parse_level(lex, env, &[Token::Add], Self::parse_expr_add, Self::parse_expr_sub)
    }
    fn parse_expr_cmp(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> Result<Expr, String> {

        Self::parse_level(lex, env, &[Token::Less, Token::Greater, Token::Equal], Self::parse_expr_add, Self::parse_expr_add)
    }
    pub fn parse_expr(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> Result<Expr, String>{
        return Ok(Self::parse_expr_cmp(lex, env)?);
    }
}

impl ParseHandler for ExprHandler{
    fn handle(&self, lex: &mut logos::Lexer<Token>, env: &mut Environment) -> HandlerResult {
        let expr = match Self::parse_expr(lex, env) {
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

    fn parse_variable(lex: &mut logos::Lexer<Token>, env: &mut Environment) -> Result<Variable, HandlerResult>{
        
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

        let expr = match ExprHandler::parse_expr(lex, env) {
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
        let _argnames = match Self::parse_argnames(&mut lex_tmp) {
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
        match Self::parse_variable(&mut lex_tmp, env){
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
                                                        Macro{name: String::from("clear"), val: MacroVal::Internal(MacroHandler::parse_load)},
                                                        Macro{name: String::from("exit"), val: MacroVal::Internal(MacroHandler::parse_exit)}
                                                    ],
                                           defined_functions: vec![
                                                        Function{ name: String::from("ceil"), val: Box::new(|arg_list: &Vec<Arg>, env: &Environment| Ok(f64::ceil(match &arg_list[0] {Arg::Named(var) => var.val.clone(), Arg::Ordered(val) => val.clone()}.eval(env)?)))},
                                                        Function{ name: String::from("floor"), val: Box::new(|arg_list: &Vec<Arg>, env: &Environment| Ok(f64::floor(match &arg_list[0] {Arg::Named(var) => var.val.clone(), Arg::Ordered(val) => val.clone()}.eval(env)?)))},
                                                        Function{ name: String::from("log"), val: Box::new(|arg_list: &Vec<Arg>, env: &Environment| {
                                                                                                           let mut base = None;
                                                                                                           let mut x = None;
                                                                                                           for arg in arg_list {
                                                                                                               match arg {
                                                                                                                   Arg::Named(var) => {
                                                                                                                           match  var.name.as_str() {
                                                                                                                               "base" | "b" => {base=Some(var.val.clone())},
                                                                                                                               "x" => {x=Some(var.val.clone())},
                                                                                                                               _ => {}
                                                                                                                           }
                                                                                                                       },
                                                                                                                   Arg::Ordered(val) => {
                                                                                                                       if base == None {
                                                                                                                           base=Some(val.clone());
                                                                                                                       } else {
                                                                                                                           x = Some(val.clone());
                                                                                                                       }
                                                                                                                   }
                                                                                                               }
                                                                                                           }
                                                                                                        if x == None {
                                                                                                            return Err(HandlerResult::Error("no value specified".to_string()));
                                                                                                        }
                                                                                                        if base == None {
                                                                                                            return Err(HandlerResult::Error("no base specified".to_string()));
                                                                                                        }
                                                            Ok(f64::log(x.unwrap().eval(env)?, base.unwrap().eval(env)?))})},
                                                        Function{ name: String::from("ln"), val: Box::new(|arg_list: &Vec<Arg>, env: &Environment| Ok(f64::log(match &arg_list[0] {Arg::Named(var) => var.val.clone(), Arg::Ordered(val) => val.clone()}.eval(env)?, std::f64::consts::E)))},
                                                        Function{ name: String::from("lg"), val: Box::new(|arg_list: &Vec<Arg>, env: &Environment| Ok(f64::log10(match &arg_list[0] {Arg::Named(var) => var.val.clone(), Arg::Ordered(val) => val.clone()}.eval(env)?)))},
                                                        Function{ name: String::from("sin"), val: Box::new(|arg_list: &Vec<Arg>, env: &Environment| Ok(f64::sin(match &arg_list[0] {Arg::Named(var) => var.val.clone(), Arg::Ordered(val) => val.clone()}.eval(env)?)))},
                                                        Function{ name: String::from("cos"), val: Box::new(|arg_list: &Vec<Arg>, env: &Environment| Ok(f64::cos(match &arg_list[0] {Arg::Named(var) => var.val.clone(), Arg::Ordered(val) => val.clone()}.eval(env)?)))},
                                                        Function{ name: String::from("tan"), val: Box::new(|arg_list: &Vec<Arg>, env: &Environment| Ok(f64::tan(match &arg_list[0] {Arg::Named(var) => var.val.clone(), Arg::Ordered(val) => val.clone()}.eval(env)?)))}
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
                let tmp = String::from(lex.source());
                for command in commands{
                    calc_parse(&command, env);
                }
                env.last_input=Some(tmp);
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
