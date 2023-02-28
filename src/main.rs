

use std::borrow::Borrow;
use std::io::BufReader;
use std::io::Read;
use std::file;
use std::fmt;
use std::collections::HashMap;
use std::str::FromStr;

enum LispValue{
    Cons(Box<(LispValue, LispValue)>),
    Nil,
    String(String),
    Rational(f64),
    Integer(i64),
    Symbol(i64)
}

impl LispValue {
    
    fn cons(car: LispValue, cdr: LispValue) -> LispValue {
        return LispValue::Cons(Box::new((car, cdr)));
    }
}

impl From<i64> for LispValue {
    fn from(item: i64) -> Self {
        LispValue::Integer(item)
    }
}
impl From<String> for LispValue {
    fn from(item: String) -> Self {
        LispValue::String(item)
    }
}
impl From<&str> for LispValue {
    fn from(item: &str) -> Self {
        LispValue::String(item.to_string())
    }
}

impl TryInto<i64> for LispValue{
    type Error = ();
    fn try_into(self) -> Result<i64, Self::Error>{
        if let LispValue::Integer(v) = self {
            return Ok(v);
        }
        return Err(())
    }
}

impl fmt::Display for LispValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self {
            LispValue::Cons (bx)=>{
                
                write!(f, "(").unwrap();
                write!(f, "{}", bx.0).unwrap();
                if let LispValue::Nil = bx.1 {
                    return write!(f, ")");    
                }
                write!(f, " ").unwrap();
                write!(f, "{}", bx.1).unwrap();
                return write!(f, ")");
                
            }
            LispValue::Nil => {
                return write!(f, "()");
            }
            LispValue::String(str) => {
                return write!(f, "{}", str);
            }
            LispValue::Symbol(id) => {
                return write!(f, "Symbol({})", id)
            }
            &LispValue::Rational(x) => {write!(f, "{0}", x)}
            LispValue::Integer(x) => {write!(f, "{0}", x)}

        }
    
    }
}    

struct LispContext{
    symbols: HashMap<String, i64>,
    id_gen: i64,
}

impl<'a> LispContext{
    fn new() -> LispContext{
        let x  = LispContext{symbols: HashMap::new(), id_gen: 1};
        return x
    }

    fn new2() -> HashMap<String, i64> {
        HashMap::new()
    }

    fn NewSymbol(& mut self, name: &str) -> LispValue{
        

        if let Option::Some(id) = self.symbols.get(name) {
            return LispValue::Symbol(*id);
        }
        let id2  = self.id_gen;
        self.id_gen += 1;

        self.symbols.insert(name.into(), id2);
        return LispValue::Symbol(id2)
    }
}

fn parse_integer<'a>(code0: &'a [u8], val: &mut i64) -> Option<&'a [u8]>{
    *val = 0;
    let mut code = code0;
    while code.len() > 0 &&  code[0] >= b'0' && code[0] <= b'9' {
        *val = *val * 10;
        let charcode = code[0] - b'0';
        *val = *val + i64::from(charcode);
        code = &code[1..]

    }
    if code.len() > 0 && code[0] != b')' && code[0] != b' '{
        return None
    }
    return Option::Some(code);
}
fn skip_whitespace(code: &[u8]) -> &[u8]{
    let mut code2 = code;
    while(code2.len() > 0 && code2[0] == b' '){
        code2 = &code2[1..];
    }
    return code2;
}

fn parse_symbol<'a>(ctx: &mut LispContext, code0: &'a [u8], value: &mut LispValue) ->  Option<&'a[u8]>{
    
    let mut code = code0;
    let mut len = 0;
    
    while code.len() > 0 &&  code[0] != b')' && code[0] != b' ' {
        code = &code[1..];
        len += 1;
    }
    
    if code.len() > 0 && code[0] != b')' && code[0] != b' '{
        return None
    }

    if let Ok(str) = String::from_utf8(Vec::from(&code0[..len])){
        *value = ctx.NewSymbol(&str.to_string()) ;
    
    }
    
    return Option::Some(code);

}


fn parse<'a>(ctx: &mut LispContext, code: &'a [u8], value: &mut LispValue) ->  Option<&'a[u8]>{
    let mut code2 = code;
    code2 = skip_whitespace(code2);

    let mut integerval: i64 = 0;
    if let Some(code3) = parse_integer(code2, &mut integerval)  {
        *value = LispValue::Integer(integerval);
        return Option::Some(code3);
    }
    if code2.len() == 0 {
        return None;
    }

    if(code2[0] == b'('){
        code2 = &code2[1..];
        
        code2 = skip_whitespace(code2);
        let mut sub = Vec::new();
        while code2.len() > 0 && code2[0] != b')'{
            

            println!("{}", char::from(code2[0]));
            let mut v2 = LispValue::Nil;
            if let Some(x) = parse(ctx, code2, &mut v2) {
                sub.push(v2);
                code2 = x;
            }
            
            code2 = skip_whitespace(code2);

        }

        let mut v = LispValue::Nil;
        while sub.len() > 0{
            v = LispValue::Cons( Box::new((sub.pop().unwrap(), v)))
        }
        *value = v;
        return Some(code2);
    }

    if code2[0] == b'"'{
        code2 = &code2[1..];
        return None;
    }

    {
        let mut v = LispValue::Nil;
        
        if let Some(sym) = parse_symbol(ctx, code2, value) {
            println!("Parsed  symbol!");
            return Some(sym)
        }
    }

    return Some(code2);
}

fn main() {
    let mut ctx = LispContext::new();println!("Hello, world!");
    let nil = LispValue::Nil;
    println!("{}", nil);
    let X = "Hej";
    let str1 = LispValue::from("Hej");
    let str2 = LispValue::from("123");
    let x2 : i64 = LispValue::from(123).try_into().unwrap();
    let v1 = LispValue::cons(str1, LispValue::cons(str2, LispValue::Nil));
    println!("{}", v1);
    
    
    let a = ctx.NewSymbol("1234");
    let b = ctx.NewSymbol("123");

{
    let alice = String::from("I like dogs");
    let d = ctx.NewSymbol(&alice);

}

    let c = ctx.NewSymbol("123");

    println!("{} {} {}", a, b, c);

    let code = "(111 222 333 asd)";//"(+ 1 2)";
    let mut out : LispValue = LispValue::Nil;
    let code2 = parse(&mut ctx, code.as_bytes(), &mut out);
    println!("Parsed: {}", out);

}
