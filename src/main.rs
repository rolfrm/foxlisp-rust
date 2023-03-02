use std::fmt;
use std::collections::HashMap;
use num::{self};
mod lisp;
use lisp::*;
mod math;
use math::*;

mod parser;
use parser::*;

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
            LispValue::Cons (_)=>{
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

pub struct LispContext{
    symbols: HashMap<String, i64>,
    id_gen: i64,
    globals: Vec<LispValue>,
    global_names: HashMap<i64, usize>
}

impl<'a> LispContext{
    fn new() -> LispContext{
        return LispContext{symbols: HashMap::new(), id_gen: 1, globals: Vec::new(), global_names: HashMap::new()};
    }

    pub fn get_symbol_id(& mut self, name: &str) -> i64{
        

        if let Option::Some(id) = self.symbols.get(name) {
            return *id;
        }
        let id2  = self.id_gen;
        self.id_gen += 1;

        self.symbols.insert(name.into(), id2);
        return id2
    }

    pub fn get_symbol(& mut self, name: &str) -> LispValue{
        return LispValue::Symbol(self.get_symbol_id(name))
    }

    pub fn set_global(&mut self, symbol_name: i64, value: LispValue){
        
        if let Option::Some(index) = self.global_names.get(&symbol_name) {
            self.globals[*index] = value;
        }else{
            let new_index = self.globals.len();

            self.global_names.insert(symbol_name, new_index);
            self.globals.insert(new_index, value);
        }
    }

    pub fn get_global(&self, symbol_name: i64) -> Option<&LispValue>{
        if let Option::Some(index) = self.global_names.get(&symbol_name) {
            return Some(&self.globals[*index]);
        }
        return None;
        
    }

    pub fn set_global_str(& mut self, name: &str, value: LispValue){
        let id = self.get_symbol_id(name);
        self.set_global(id, value);
    }
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
