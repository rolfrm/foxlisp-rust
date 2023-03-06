use num::{self};
use std::collections::HashMap;
use std::fmt::{self, Pointer};
use std::sync::Arc;
mod lisp;
use lisp::*;
mod math;
use math::*;
use nohash_hasher;

mod parser;
use parser::*;

#[derive(Clone)]
pub enum NativeFunc {
    Function1(fn(LispValue) -> LispValue),
    Function2(fn(LispValue, LispValue) -> LispValue),
    Function1r(fn(&LispValue) -> &LispValue),
    Function2r(for<'a> fn(&'a LispValue, &'a LispValue) -> &'a LispValue),
    FunctionN(fn(Vec<LispValue>) -> LispValue),
}

impl fmt::Debug for NativeFunc{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Nativefunc")
    }
}

#[derive(Debug)]
pub struct LispFunc {
    code: Box<LispValue>,
    args_names: Vec<i32>,
}

pub enum LispValue {
    Cons(Box<(LispValue, LispValue)>),
    Consr(Arc<(LispValue, LispValue)>),
    Nil,
    String(String),
    Rational(f64),
    Integer(i64),
    Symbol(i32),
    BigInt(num::BigInt),
    BigRational(num::BigRational),
    NativeFunction(NativeFunc),
    Macro(fn(&mut Stack, &LispValue) -> LispValue),
    LispFunction(Arc<LispFunc>),
}

impl fmt::Debug for LispValue{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self)
    }
}
impl Default for LispValue {
    fn default() -> Self {
        LispValue::Nil
    }
}

impl Clone for LispValue {
    fn clone(&self) -> Self {
        match self {
            LispValue::Cons(v) => LispValue::Consr(Arc::new((v.0.clone(), v.1.clone()))),
            LispValue::Consr(v) => LispValue::Consr(v.clone()),
            LispValue::Nil => LispValue::Nil,
            LispValue::String(s) => LispValue::String(s.clone()),
            LispValue::BigInt(b) => LispValue::BigInt(b.clone()),
            LispValue::BigRational(b) => LispValue::BigRational(b.clone()),
            LispValue::Integer(i) => LispValue::Integer(*i),
            LispValue::Rational(r) => LispValue::Rational(*r),
            LispValue::Symbol(s) => LispValue::Symbol(*s),
            LispValue::NativeFunction(f) => LispValue::NativeFunction(f.clone()),
            LispValue::Macro(m) => LispValue::Macro(m.clone()),
            LispValue::LispFunction(f) => LispValue::LispFunction(f.clone()),
        }
    }
}

impl LispValue {
    pub fn from_2(item: fn(LispValue, LispValue) -> LispValue) -> Self {
        LispValue::NativeFunction(NativeFunc::Function2(item))
    }
    pub fn from_1r(item: fn(&LispValue) -> &LispValue) -> Self {
        LispValue::NativeFunction(NativeFunc::Function1r(item))
    }
    pub fn from_1(item: fn(LispValue) -> LispValue) -> Self {
        LispValue::NativeFunction(NativeFunc::Function1(item))
    }
    pub fn from_2r(item: for<'a> fn(&'a LispValue, &'a LispValue) -> &'a LispValue) -> Self {
        LispValue::NativeFunction(NativeFunc::Function2r(item))
    }
    pub fn from_n(item: fn(Vec<LispValue>) -> LispValue) -> Self {
        LispValue::NativeFunction(NativeFunc::FunctionN(item))
    }
    pub fn from_macro(item: fn(&mut Stack, &LispValue) -> LispValue) -> Self {
        LispValue::Macro(item)
    }
}

impl PartialEq for LispValue {
    fn eq(&self, other: &Self) -> bool {
        match self {
            LispValue::Cons(c) => {
                if let LispValue::Cons(c2) = other {
                    return c2 == c;
                }
                return false;
            }
            LispValue::Consr(ar) => {
                if let LispValue::Consr(ar2) = other {
                    return ar2 == ar;
                }
                return false;
            }
            LispValue::Nil => {
                if let LispValue::Nil = other {
                    return true;
                }
                return false;
            }
            LispValue::String(v1) => {
                if let LispValue::String(v2) = other {
                    return v1 == v2;
                }
                return false;
            }
            LispValue::Rational(v1) => {
                if let LispValue::Rational(v2) = other {
                    return v1 == v2;
                }
                return false;
            }
            LispValue::Integer(v1) => {
                if let LispValue::Integer(v2) = other {
                    return v1 == v2;
                }
                return false;
            }
            LispValue::Symbol(v1) => {
                if let LispValue::Symbol(v2) = other {
                    return v1 == v2;
                }
                return false;
            }
            LispValue::BigInt(v1) => {
                if let LispValue::BigInt(v2) = other {
                    return v1 == v2;
                }
                return false;
            }
            LispValue::BigRational(v1) => {
                if let LispValue::BigRational(v2) = other {
                    return v1 == v2;
                }
                return false;
            }
            _ => false,
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

impl From<fn(Vec<LispValue>) -> LispValue> for LispValue {
    fn from(item: fn(Vec<LispValue>) -> LispValue) -> Self {
        LispValue::NativeFunction(NativeFunc::FunctionN(item))
    }
}

impl From<fn(&LispValue) -> &LispValue> for LispValue {
    fn from(item: fn(&LispValue) -> &LispValue) -> Self {
        LispValue::NativeFunction(NativeFunc::Function1r(item))
    }
}

impl From<fn(LispValue) -> LispValue> for LispValue {
    fn from(item: fn(LispValue) -> LispValue) -> Self {
        LispValue::NativeFunction(NativeFunc::Function1(item))
    }
}

impl From<for<'a> fn(&'a LispValue, &'a LispValue) -> &'a LispValue> for LispValue {
    fn from(item: for<'a> fn(&'a LispValue, &'a LispValue) -> &'a LispValue) -> Self {
        LispValue::NativeFunction(NativeFunc::Function2r(item))
    }
}

impl TryInto<i64> for LispValue {
    type Error = ();
    fn try_into(self) -> Result<i64, Self::Error> {
        if let LispValue::Integer(v) = self {
            return Ok(v);
        }
        return Err(());
    }
}

impl fmt::Display for LispValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self {
            LispValue::Cons(_) | LispValue::Consr(_) => {
                let mut it = &*self;
                write!(f, "(").unwrap();
                let mut first = true;

                while is_cons(it) {
                    if first {
                        first = false
                    } else {
                        write!(f, " ").unwrap();
                    }
                    write!(f, "{}", car(it)).unwrap();
                    it = cdr(it);
                }

                if let LispValue::Nil = it {
                    //
                } else {
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
            LispValue::Symbol(id) => return write!(f, "Symbol({})", id),
            LispValue::Rational(x) => {
                write!(f, "{0}", x)
            }
            LispValue::Integer(x) => {
                write!(f, "{0}", x)
            }
            LispValue::BigInt(x) => {
                write!(f, "{0}", x)
            }
            LispValue::BigRational(x) => write!(f, "{0}", x),
            LispValue::NativeFunction(_) => write!(f, "Native Function"),
            LispValue::Macro(_) => write!(f, "Macro"),
            LispValue::LispFunction(_) => write!(f, "LispFunction"),
        }
    }
}

pub trait Scope {
    fn get_value(&self, symbol: i32) -> Option<&LispValue>;
    fn set_value(&mut self, symbol_name: i32, value: &LispValue);
}
#[derive(Debug)]
pub struct LispContext {
    symbols: HashMap<String, i32>,
    id_gen: i32,
    globals: Vec<LispValue>,
    global_names: HashMap<i32, usize, nohash_hasher::BuildNoHashHasher<i32>>,
}

#[derive(Debug)]
pub struct LispScope<'a> {
    id: &'a [i32],
    values: &'a mut [LispValue],
    parent: &'a Option<Box<LispScope<'a>>>,
}

#[derive(Debug)]
pub struct Stack<'a> {
    local_scope: Option<Box<LispScope<'a>>>,
    global_scope: &'a mut LispContext,
}

impl<'a> Stack<'a> {
    fn get_value(&self, symbol_name: i32) -> Option<&LispValue> {
        if let Some(l) = &self.local_scope {
            if let Some(r) = l.get_value(symbol_name) {
                return Some(r);
            }
        }
        return self.global_scope.get_value(symbol_name);
    }

    fn set_value(&mut self, symbol_name: i32, value: &LispValue) {
        if let Some(l) = &mut self.local_scope {
            if l.set_value(symbol_name, value) {
                return;
            }
        }
        self.global_scope.set_value(symbol_name, value)
    }
}

impl<'a> LispScope<'a> {
    fn get_value(&self, symbol_name: i32) -> Option<&LispValue> {
        for i in 0..self.id.len() {
                if self.id[i] == symbol_name {
                return Some(&self.values[i]);
            }
        }

        if let Some(p) = &self.parent {
            p.get_value(symbol_name)
        } else {
            None
        }
    }
    fn set_value(&mut self, symbol_name: i32, value: &LispValue) -> bool {
        for i in 0..self.id.len() {
            if self.id[i] == symbol_name {
                self.values[i] = value.clone();
                return true;
            }
        }
        return false;
    }
}

impl<'a> LispContext {
    fn new() -> LispContext {
        return LispContext {
            symbols: HashMap::new(),
            id_gen: 1,
            globals: Vec::new(),
            global_names: HashMap::with_capacity_and_hasher(8, nohash_hasher::BuildNoHashHasher::default()),
        };
    }

    pub fn get_symbol_id(&mut self, name: &str) -> i32 {
        if let Option::Some(id) = self.symbols.get(name) {
            return *id;
        }
        let id2 = self.id_gen;
        self.id_gen += 1;

        self.symbols.insert(name.into(), id2);
        return id2;
    }

    pub fn get_symbol(&mut self, name: &str) -> LispValue {
        return LispValue::Symbol(self.get_symbol_id(name));
    }

    pub fn set_global(&mut self, symbol_name: i32, value: LispValue) {
        if let Option::Some(index) = self.global_names.get(&symbol_name) {
            self.globals[*index] = value;
        } else {
            let new_index = self.globals.len();

            self.global_names.insert(symbol_name, new_index);
            self.globals.insert(new_index, value);
        }
    }

    pub fn get_global(&self, symbol_name: i32) -> Option<&LispValue> {
        if let Option::Some(index) = self.global_names.get(&symbol_name) {
            return Some(&self.globals[*index]);
        }
        return None;
    }

    pub fn set_global_str(&mut self, name: &str, value: LispValue) {
        let id = self.get_symbol_id(name);
        self.set_global(id, value);
    }

    fn get_value(&self, symbol_name: i32) -> Option<&LispValue> {
        let r= self.get_global(symbol_name);
        if let Some(x) = r {
            return Some(x);
        }
        return None;
        
    }
    fn set_value(&mut self, symbol_name: i32, value: &LispValue) {
        self.set_global(symbol_name, value.clone())
    }
}

fn eq(a: &LispValue, b: &LispValue) -> bool {
    return a == b;
}

fn lisp_raise_error(ctx: &Stack, error: LispValue) {
    println!("ERROR {}", error);
}

fn cons_count(v: &LispValue) -> i64 {
    let mut it = v;
    let mut cnt = 0;
    while !is_nil(it) {
        it = cdr(it);
        cnt += 1;
    }
    return cnt;
}

fn lisp_invoken<'a>(
    ctx: &mut Stack,
    argsl: &'a LispValue,
    fcn: fn(Vec<LispValue>) -> LispValue,
) -> LispValue {
    let mut args: Vec<LispValue> = Vec::with_capacity(cons_count(argsl) as usize);
    let mut it = argsl;
    
    while !is_nil(it) {
        let r = lisp_eval(ctx, car(it));
        args.push(r);
        it = cdr(it);
    }
    let args2 = args;
    let r2 = fcn(args2);
    return r2;
}

fn lisp_invoke2<'a>(
    ctx: &mut Stack,
    v: &'a LispValue,
    fcn: fn(LispValue, LispValue) -> LispValue,
) -> LispValue {
    let a1 = lisp_eval(ctx, car(v));
    let a2 = lisp_eval(ctx, cadr(v));
    return (fcn)(a1, a2);
}

fn lisp_invoke1<'a>(
    ctx: &mut Stack,
    v: &'a LispValue,
    fcn: fn(LispValue) -> LispValue,
) -> LispValue {
    let a1 = lisp_eval(ctx, car(v));
    return (fcn)(a1);
}

fn lisp_invoke2r<'a>(
    ctx: &mut Stack,
    v: &'a LispValue,
    fcn: for<'b> fn(&'b LispValue, &'b LispValue) -> &'b LispValue,
) -> LispValue {
    let a1 = lisp_eval(ctx, car(v));
    let a2 = lisp_eval(ctx, cadr(v));
    return (fcn)(&a1, &a2).clone();
}

fn lisp_invoke1r<'a>(
    ctx: &mut Stack,
    v: &'a LispValue,
    fcn: fn(&LispValue) -> &LispValue,
) -> LispValue {
    let a1 = lisp_eval(ctx, car(v));
    return (fcn)(&a1).clone();
}

fn lisp_eval_lisp_function<'a>(ctx: &mut Stack, func: &LispFunc, args: &'a LispValue) -> LispValue {
    let mut i = 0;
    let mut it = args;
    let mut arg_veca = Vec::new();
    let mut arg_id = Vec::new();
    while let LispValue::Cons(_) = it {
        arg_veca.push(lisp_eval(ctx, car(it)));
        arg_id.push(func.args_names[i]);
        i += 1;
        it = cdr(it);
    }
    let scope = LispScope{
        id: arg_id.as_slice(),
        values: arg_veca.as_mut_slice(),
        parent: &None 
    };
     
    let mut stack2 = Stack {
        global_scope: ctx.global_scope,
        local_scope: Some(Box::new(scope)),
    };
    let mut it = func.code.as_ref();   
    let mut result = LispValue::Nil; 
    while !is_nil(it) {
        result = lisp_eval(&mut stack2, car(it));
        it = cdr(it);
    }
        
    result
}

fn lisp_eval<'a>(ctx: &mut Stack, v: &'a LispValue) -> LispValue {
    match v {
        LispValue::Cons(_) | LispValue::Consr(_) => {
            let value = lisp_eval(ctx, car(v));
            match value {

            LispValue::NativeFunction(n) => {
                return match n {
                    NativeFunc::Function1(f) 
                            => lisp_invoke1(ctx, cdr(v), f),
                    NativeFunc::Function2(f) 
                            => lisp_invoke2(ctx, cdr(v), f),
                    NativeFunc::Function1r(f) 
                            =>  lisp_invoke1r(ctx, cdr(v), f),
                    NativeFunc::Function2r(f) 
                            =>  lisp_invoke2r(ctx, cdr(v), f),
                    NativeFunc::FunctionN(f) 
                            => lisp_invoken(ctx, cdr(v), f)
                }
            },

            LispValue::Macro(mf) => {
                return (mf)(ctx, cdr(v));
            },
            LispValue::LispFunction(lf) => {
                return lisp_eval_lisp_function(ctx, &lf, cdr(v));
            },
            
            _ => lisp_raise_error(ctx, "no such function!".into())
    }
            return LispValue::Nil;
        }
        LispValue::Symbol(id) => {
            if let Some(value) = ctx.get_value(*id) {
                return value.clone();
            } else {
                lisp_raise_error(ctx, "No such symbol!".into());
                return LispValue::Nil;
            }
        }
        _ => v.clone(),
    }
}

fn lisp_eval_str(ctx: &mut Stack, code: &str) -> LispValue {
    let code2 = parse_string(ctx.global_scope, code);
    return lisp_eval(ctx, &code2);
}

fn main() {
    let mut ctx = Box::new(LispContext::new());

    lisp_load_lisp(&mut ctx);
    lisp_math_load(&mut ctx);

    let code = "(111 222  333 asd asd asdd asddd asdd asd 1.1 2.2 3.3 (x y z) (1.0 2.0 3.0) 3.14)";
    let mut out: LispValue = LispValue::Nil;
    parse(&mut ctx, code.as_bytes(), &mut out);
    println!("Parsed: {}", out);

    println!("BigInt: {}", LispValue::from(num::BigInt::from(10000)));

    assert!(eq(&LispValue::Integer(222), car(cdr(&out))));
    assert!(eq(&ctx.get_symbol("asd"), cadddr(&out)));
    assert!(!eq(&ctx.get_symbol("asdd"), cadddr(&out)));
    let code2 = parse_string(&mut ctx, "(cdr (println  (cons (+ 1 2 3.14) (cons (* 1000 (+ 1234 9999 4321 1111 (- 1000 1000 1000))) 1))))");
    let mut stk = Stack {
        global_scope: &mut ctx,
        local_scope: None,
    };

    let result = lisp_eval(&mut stk, &code2);
    let result2 = lisp_eval_str(&mut stk, "(println (list 1 2 3))");

    println!("Code2: {}", result);

    lisp_eval_str(&mut stk, "(let ((x 10)) (println (list 1 2 3 x)))");
    lisp_eval_str(
        &mut stk,
        "(println (let ((x 10)) (println x) (set! x 5) (println x) (not (not (eq x 10)))))",
    );
    lisp_eval_str(
        &mut stk,
        "(println (let ((x 5)) (loop (not (eq x 0)) (println (set! x (- x 1))))))",
    );
    lisp_eval_str(&mut stk, "(println (* (big-rational 10000) 10000 10000 10000 100000 10000000 100000 1000000 100000 10000))");
    lisp_eval_str(
        &mut stk,
        "(println (car (println (cdr (cdr (list 1 2 3 4 5 6 7 8))))))",
    );
    lisp_eval_str(&mut stk, "(println (list 1 2 3 4 5 6 7 (cons 8 9)))");
    lisp_eval_str(&mut stk, "(println (if 1 2 3))");
    lisp_eval_str(&mut stk, "(println (if () 2 3))");

    lisp_eval_str(
        &mut stk,
        "(println (let ((a 1)) (let ((b 2)) (println (+ a b)))))",
    );
    lisp_eval_str(&mut stk, "(println (let ((a 2) (b 3) (c 5)) (* a b c)))");
    lisp_eval_str(
        &mut stk,
        "(println (let ((x 10000)) (loop (not (eq x 0)) (set! x (- x 1))) x))",
    );
    println!("go");
    
    lisp_eval_str(
        &mut stk,
        "(defun print2 (a) (println (+ 5 a)))",
    );
    lisp_eval_str(&mut stk,
        "(println (print2 123))");
    
    
}
