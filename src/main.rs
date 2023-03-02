
use std::fmt;
use std::collections::HashMap;
use num::{self, FromPrimitive, BigRational, BigInt, ToPrimitive};
mod lisp;
use lisp::*;
mod math;
use math::*;

enum CallableType{
    F1(fn(f64, f64) -> f64),
    F2(fn(i64, i64) -> i64),
    F3(fn(num::BigInt, num::BigInt) -> num::BigInt),
    F4(fn(num::BigRational, num::BigRational) -> num::BigRational),
}



struct Callable {

}

struct DynamicDispatch{

}



#[derive(Clone)]
pub enum LispValue{
    Cons(Box<(LispValue, LispValue)>),
    Nil,
    String(String),
    Rational(f64),
    Integer(i64),
    Symbol(i64),
    BigInt(num::BigInt),
    BigRational(num::BigRational),
    Function1(fn(LispValue) -> LispValue),
    Function2(fn(LispValue, LispValue) -> LispValue),
    FunctionN(fn(Vec<LispValue>) -> LispValue)
}

impl LispValue {
    
}

impl PartialEq for LispValue{
    fn eq(&self, other: &Self) -> bool 
    {
        match self {
            LispValue::Cons(c) => {
                if let LispValue::Cons(c2) = other {
                    return c2 == c;
                }
                return false;
            },
            LispValue::Nil =>  {
                if let LispValue::Nil = other {
                    return true;
                }
                return false;
            },
            LispValue::String(v1) =>  {
                if let LispValue::String(v2) = other {
                    return v1 == v2;
                }
                return false;
            },
            LispValue::Rational(v1) => {
                if let LispValue::Rational(v2) = other {
                    return v1 == v2;
                }
                return false;
            },
            LispValue::Integer(v1) => {
                if let LispValue::Integer(v2) = other {
                    return v1 == v2;
                }
                return false;
            },
            LispValue::Symbol(v1) => {
                if let LispValue::Symbol(v2) = other {
                    return v1 == v2;
                }
                return false;
            },
            LispValue::BigInt(v1) =>{
                if let LispValue::BigInt(v2) = other {
                    return v1 == v2;
                }
                return false;
            },
            LispValue::BigRational(v1) => {
                if let LispValue::BigRational(v2) = other {
                    return v1 == v2;
                }
                return false;
            },
            LispValue::Function1(_) => {
                
                return false;
            },
            LispValue::Function2(_) => {
                
                return false;
            },
            LispValue::FunctionN(_) => {
                
                return false;
            },
        }
    
    }
}

impl From<i64> for LispValue {
    fn from(item: i64) -> Self {
        LispValue::Integer(item)
    }
}
impl From<num::BigInt> for LispValue {
    fn from(item: num::BigInt) -> Self {
        LispValue::BigInt(item)
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
                let mut it = &*self;
                write!(f, "(").unwrap();
                let mut first = true;

                while let LispValue::Cons(bx2) = &it  {
                    if first {
                        first = false
                    }else{
                        write!(f, " ").unwrap();
                        
                    }
                    write!(f, "{}", bx2.0).unwrap();
                    it = &bx2.1;
                    
                    
                }
                
                if let LispValue::Nil = it {
                    //
                }else{
                    write!(f, " . ").unwrap();
                    write!(f, "{}", it).unwrap();

                }
                
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
            LispValue::Rational(x) => {write!(f, "{0}", x)}
            LispValue::Integer(x) => {write!(f, "{0}", x)}
            LispValue::BigInt(x) => {write!(f, "{0}", x)}
            LispValue::BigRational(x) => {write!(f, "{0}", x)}
            LispValue::Function1(_) => todo!(),
            LispValue::Function2(_) => todo!(),
            LispValue::FunctionN(_) => todo!()
        }
    
    }
}    

struct LispContext{
    symbols: HashMap<String, i64>,
    id_gen: i64,
    globals: Vec<LispValue>,
    global_names: HashMap<i64, usize>
}

impl<'a> LispContext{
    fn new() -> LispContext{
        return LispContext{symbols: HashMap::new(), id_gen: 1, globals: Vec::new(), global_names: HashMap::new()};
    }

    fn get_symbol_id(& mut self, name: &str) -> i64{
        

        if let Option::Some(id) = self.symbols.get(name) {
            return *id;
        }
        let id2  = self.id_gen;
        self.id_gen += 1;

        self.symbols.insert(name.into(), id2);
        return id2
    }

    fn get_symbol(& mut self, name: &str) -> LispValue{
        return LispValue::Symbol(self.get_symbol_id(name))
    }

    fn set_global(&mut self, symbol_name: i64, value: LispValue){
        
        if let Option::Some(index) = self.global_names.get(&symbol_name) {
            self.globals[*index] = value;
        }else{
            let new_index = self.globals.len();

            self.global_names.insert(symbol_name, new_index);
            self.globals.insert(new_index, value);
        }
    }

    fn get_global(&self, symbol_name: i64) -> Option<&LispValue>{
        if let Option::Some(index) = self.global_names.get(&symbol_name) {
            return Some(&self.globals[*index]);
        }
        return None;
        
    }

    fn set_global_str(& mut self, name: &str, value: LispValue){
        let id = self.get_symbol_id(name);
        self.set_global(id, value);
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
    while code2.len() > 0 && code2[0] == b' '{
        code2 = &code2[1..];
    }
    return code2;
}

fn parse_rational<'a>(code0: &'a [u8], val: &mut f64) -> Option<&'a [u8]> {
    *val = 0.0;
    let mut code = code0;
    while code.len() > 0 &&  code[0] >= b'0' && code[0] <= b'9' {
        *val = *val * 10.0;
        let charcode = code[0] - b'0';
        *val = *val + f64::from(charcode);
        code = &code[1..]
    }
    
    
    if code.len() > 0 && code[0] == b'.' {
        code = &code[1..];
        let mut subscale = 0.1;
        while code.len() > 0 &&  code[0] >= b'0' && code[0] <= b'9' {
            let charcode = f64::from(code[0] - b'0');
            *val += subscale * charcode;
            subscale *= 0.1;
            code = &code[1..]
    
        }
        return Some(code);    
    }else{
        // there is no '.' -> not a rational.
    }
    
    return None;

    

}

fn parse_symbol<'a>(ctx: &mut LispContext, code0: &'a [u8], value: &mut LispValue) ->  Option<&'a[u8]>{
    
    let mut code = code0;
    let mut len = 0;
    
    while code.len() > 0 &&  code[0] != b')' && code[0] != b'(' && code[0] != b' ' {
        code = &code[1..];
        len += 1;
    }
    
    if code.len() > 0 && code[0] != b')' && code[0] != b' ' && code[0] != b'('{
        return None
    }

    if let Ok(str) = String::from_utf8(Vec::from(&code0[..len])){
        *value = ctx.get_symbol(&str.to_string()) ;
    
    }
    
    return Option::Some(code);

}


fn parse<'a>(ctx: &mut LispContext, code: &'a [u8], value: &mut LispValue) ->  Option<&'a[u8]>{
    let mut code2 = code;
    code2 = skip_whitespace(code2);

    let mut integer_value: i64 = 0;
    if let Some(code3) = parse_integer(code2, &mut integer_value)  {
        *value = LispValue::Integer(integer_value);
        return Option::Some(code3);
    }
    let mut rational_value: f64 = 0.0;
    if let Some(code3) = parse_rational(code2, &mut rational_value)  {
        *value = LispValue::Rational(rational_value);
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
            
            let mut v2 = LispValue::Nil;
            if let Some(x) = parse(ctx, code2, &mut v2) {
                sub.push(v2);
                code2 = x;
            }
            
            code2 = skip_whitespace(code2);

        }
        code2 = &code2[1..]; 

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
            return Some(sym)
        }
    }

    return Some(code2);
}

fn parse_string(ctx: &mut LispContext, code: &str) -> LispValue{
    let mut c = code.as_bytes();
    let mut v = LispValue::Nil;
    parse(ctx, c, &mut v);
    return v;
}



fn lisp_print(v: LispValue) -> LispValue{
    println!("{}", v);
    return v;
}



fn lisp_conss(v: Vec<LispValue>) -> LispValue {
    let mut v0 = LispValue::Nil;
    for i in v.iter().rev() {
        v0 = LispValue::Cons(Box::new((i.clone(), v0)));
    }
    return v0;
}

fn lisp_raise_error(ctx: &mut LispContext, error: LispValue){

}

fn cons_count(v: &LispValue ) -> i64 {
    let mut it = v;
    let mut count = 0;
    while let LispValue::Cons(c) = it {
        count += 1;
        it = &c.1;
    }
    return count;
}

fn lisp_invoken<'a>(ctx :&mut LispContext, argsl: &'a LispValue, fcn : fn(Vec<LispValue>) -> LispValue) -> LispValue{
    let mut args : Vec<LispValue> = Vec::new();
    let mut it = argsl;
    while let LispValue::Cons(c) = it {
        let r = lisp_eval(ctx, &c.0);
        args.push(r);
        it = &c.1;
    }
    let args2 = args;
    let r2 = fcn(args2);
    return r2;
}

fn lisp_eval<'a>(ctx :&mut LispContext , v: &'a LispValue) -> LispValue{
    match v {
        LispValue::Cons (bx)=>{
            
            if let LispValue::Symbol(id) = bx.0{
                if let Some(value) = ctx.get_global(id) {
                    if let LispValue::FunctionN(f) = value {
                        return lisp_invoken(ctx, &bx.1, *f);
                    }

                }
                 
            }
            lisp_raise_error(ctx, "123".into());
            
            return LispValue::Nil;
            
        }
        LispValue::Nil => {
            return LispValue::Nil
        }
        LispValue::String(ref str) => {
            return v.clone();
        }
        LispValue::Symbol(id) => {
            if let Some(value) = ctx.get_global(*id) {
                return LispValue::Nil;//value;

            }else{
                //lisp_raise_error(ctx, "123".into());
                return LispValue::Nil;
            }
        }
        LispValue::Rational(x) => {return v.clone()},
        LispValue::Integer(x) => {return v.clone()},
        LispValue::BigInt(ref x) => {return v.clone()},
        LispValue::BigRational(ref x) => {return v.clone()},
        LispValue::Function1(_) => {return v.clone()},
        LispValue::Function2(_) => {return v.clone()},
        LispValue::FunctionN(_) => {return v.clone()}
    }
}



fn main() {
    let mut ctx = Box::new(LispContext::new());
    
        
    ctx.set_global_str("print", LispValue::Function1(lisp_print));
    ctx.set_global_str("car", LispValue::Function1(car2));
    //ctx.set_global_str("caddr", LispValue::Function1(caddr));
    ctx.set_global_str("+", LispValue::FunctionN(lisp_add));
    ctx.set_global_str("-", LispValue::FunctionN(lisp_sub));
    ctx.set_global_str("*", LispValue::FunctionN(lisp_mul));
    ctx.set_global_str("/", LispValue::FunctionN(lisp_div));
    ctx.set_global_str("cons", LispValue::FunctionN(lisp_conss));
    let code = "(111 222  333 asd asd asdd asddd asdd asd 1.1 2.2 3.3 (x y z) (1.0 2.0 3.0) 3.14)";
    let mut out : LispValue = LispValue::Nil;
    parse(&mut ctx, code.as_bytes(), &mut out);
    println!("Parsed: {}", out);

    println!("BigInt: {}", LispValue::from(num::BigInt::from(10000)));

    assert!(eq(&LispValue::Integer(222), car(cdr(&out))));
    assert!(eq(&ctx.get_symbol("asd"), cadddr(&out)));
    assert!(!eq(&ctx.get_symbol("asdd"), cadddr(&out)));
    let code2 = parse_string(&mut ctx, "(cons (+ 1 2 3.14) (cons (* 1000 (+ 1234 9999 4321 1111 (- 1000 1000 1000)))))");
    let result = lisp_eval(&mut ctx, &code2);
    println!("Code2: {}", result);

}
