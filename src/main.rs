use num::{self, ToPrimitive};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{self};
use std::rc::Rc;
use std::{env, i128, u128};
mod lisp;
use lisp::*;
mod math;
use math::*;
use std::cell::RefCell;
use std::fs;

mod bytecode;
use bytecode::*;

mod parser;
use parser::*;

mod code_reader_writer;
use code_reader_writer::*;

mod compile;

#[derive(Clone)]
pub enum NativeFunc {
    Function1(fn(LispValue) -> LispValue),
    Function2(fn(LispValue, LispValue) -> LispValue),
    Function1r(fn(&LispValue) -> &LispValue),
    Function2r(for<'a> fn(&'a LispValue, &'a LispValue) -> &'a LispValue),
    FunctionN(fn(Vec<LispValue>) -> LispValue),
    FunctionNr(for<'a> fn(&[LispValue]) -> LispValue),
}

impl fmt::Debug for NativeFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Nativefunc")
    }
}

#[derive(Debug)]
pub struct LispFunc {
    code: Box<LispValue>,
    args_names: Vec<i32>,
    magic: bool,
    variadic: bool,
}

struct TempIndex {
    value: LispValue,
}

pub enum LispValue {
    Cons(Rc<(LispValue, LispValue)>),
    Nil,
    T,
    Rest,
    String(String),
    Rational(f64),
    Integer(i64),
    Symbol(i32),
    BigInt(Rc<num::BigInt>),
    BigRational(Rc<num::BigRational>),
    NativeFunction(NativeFunc),
    Macro(fn(&mut Stack, &LispValue) -> LispValue),
    LispFunction(Rc<LispFunc>),
}

impl fmt::Debug for LispValue {
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
            LispValue::Cons(a) => LispValue::Cons(a.clone()),
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
            LispValue::T => LispValue::T,
            LispValue::Rest => LispValue::Rest,
        }
    }
}

trait LispEvalable {
    fn to_evalable(&self, ctx: &mut LispContext) -> Option<LispValue>;
}

impl LispEvalable for str {
    fn to_evalable(&self, ctx: &mut LispContext) -> Option<LispValue> {
        let mut code = self.as_bytes();
        return parse_bytes(ctx, &mut code);
    }
}

impl LispEvalable for LispValue {
    fn to_evalable(&self, _: &mut LispContext) -> Option<LispValue> {
        Some(self.clone())
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
    pub fn from_nr(item: fn(&[LispValue]) -> LispValue) -> Self {
        LispValue::NativeFunction(NativeFunc::FunctionNr(item))
    }
    pub fn from_macro(item: fn(&mut Stack, &LispValue) -> LispValue) -> Self {
        LispValue::Macro(item)
    }
    pub fn cons(a: LispValue, b: LispValue) -> LispValue {
        LispValue::Cons(Rc::new((a,b)))
    }
    pub fn to_iter<'a>(&'a self) -> ConsIter<'a> {
        ConsIter { current: self }
    }
}

impl PartialEq for LispValue {
    fn eq(&self, other: &Self) -> bool {
        match self {
            LispValue::Cons(a0) => {
                if let LispValue::Cons(a) = other {
                    return a == a0;
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
                    return v1.eq(v2);
                }
                return false;
            }
            LispValue::BigRational(v1) => {
                if let LispValue::BigRational(v2) = other {
                    return v1.eq(v2);
                }
                return false;
            }
            LispValue::T => {
                if let LispValue::T = other {
                    return true;
                }
                return false;
            }
            _ => false,
        }
    }
}

impl PartialOrd for LispValue {
    fn partial_cmp(&self, other: &LispValue) -> Option<Ordering> {
        match self {
            LispValue::Cons(a) => {
                if let LispValue::Cons(a2) = other {
                    let c1 = a.partial_cmp(&a2); 
                    
                    return c1;
                }
                return None;
            }

            LispValue::Nil => {
                if let LispValue::Nil = other {
                    return Some(Ordering::Equal);
                }
                return None;
            }
            LispValue::String(v1) => {
                if let LispValue::String(v2) = other {
                    return v1.partial_cmp(v2);
                }
                return None;
            }
            LispValue::Rational(v1) => {
                if let LispValue::Rational(v2) = other {
                    return v1.partial_cmp(v2);
                }

                return None;
            }
            LispValue::Integer(v1) => {
                if let LispValue::Integer(v2) = other {
                    return v1.partial_cmp(v2);
                }
                return None;
            }
            LispValue::Symbol(v1) => {
                if let LispValue::Symbol(v2) = other {
                    return v1.partial_cmp(v2);
                }
                return None;
            }
            LispValue::BigInt(v1) => {
                if let LispValue::BigInt(v2) = other {
                    return v1.partial_cmp(v2);
                }
                return None;
            }
            LispValue::BigRational(v1) => {
                if let LispValue::BigRational(v2) = other {
                    return v1.partial_cmp(v2);
                }
                return None;
            }
            _ => match self.eq(other) {
                true => return Some(Ordering::Equal),
                false => return None,
            },
        }
    }
}

impl LispValue {
    fn equals(&self, other: &Self) -> bool {
        match self {
            LispValue::Cons(a) => {
                return match other {
                    LispValue::Cons(a2) => a.0.equals(&a2.0) && a.1.equals(&a2.1),
                    _ => false,
                }
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
                if let LispValue::BigInt(v2) = other {
                    if let Some(v2_2) = v2.to_i64() {
                        return v2_2 == *v1;
                    }
                }
                if let LispValue::BigRational(v2) = other {
                    if let Some(i2) = v2.to_i64() {
                        return i2 == *v1;
                    }
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
                    return v1.eq(v2);
                }
                if let LispValue::Integer(v2) = other {
                    if let Some(v1_2) = v1.to_i64() {
                        return v1_2 == *v2;
                    }
                }
                return false;
            }
            LispValue::BigRational(v1) => {
                if let LispValue::BigRational(v2) = other {
                    return v1.eq(v2);
                }
                if let LispValue::Integer(i) = other {
                    if let Some(i2) = v1.to_i64() {
                        return i2 == *i;
                    }
                }
                return false;
            }
            LispValue::Rest => false,
            LispValue::T => {
                if let LispValue::T = other {
                    return true;
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
        LispValue::BigInt(Rc::new(item))
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
            LispValue::Cons(a) => {
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
            LispValue::Symbol(id) => {
                return CURRENT_NAMES.with(|v| {
                    let v2 = v.borrow();
                    if v2.len() > *id as usize {
                        write!(f, "{}", v2[*id as usize])
                    } else {
                        write!(f, "Symbol({})", id)
                    }
                });
            }
            LispValue::Rational(x) => {
                write!(f, "{}", x)
            }
            LispValue::Integer(x) => {
                write!(f, "{}", x)
            }
            LispValue::BigInt(x) => {
                write!(f, "{}", x)
            }
            LispValue::BigRational(x) => write!(f, "{}", x.to_string()),
            LispValue::NativeFunction(_) => write!(f, "Native Function"),
            LispValue::Macro(_) => write!(f, "Macro"),
            LispValue::LispFunction(_) => write!(f, "LispFunction"),
            LispValue::Rest => write!(f, "&REST"),
            LispValue::T => write!(f, "T"),
        }
    }
}

#[derive(Debug)]
pub struct LispContext {
    symbols: HashMap<String, i32>,
    symbol_name_lookup: Vec<String>,
    globals: Vec<LispValue>,
    global_names: Vec<usize>,
    arg_stack: Vec<LispValue>,
}

impl LispContext {
    fn load(&mut self, code: &str) -> Option<LispValue> {
        let mut stk = Stack::new_root(self);
        lisp_load_str(&mut stk, code);
        return stk.error;
    }

    fn eval(&mut self, code: &dyn LispEvalable) -> Option<LispValue> {
        let mut stk = Stack::new_root(self);
        stk.eval(code);
        stk.error
    }
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
    error: Option<LispValue>,
}

pub struct ConsIter<'a> {
    current: &'a LispValue,
}

impl<'a> Iterator for ConsIter<'a> {
    type Item = &'a LispValue;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current;
        match current {
            LispValue::Cons(a) => {
                let res = &a.0;
                self.current = &a.1;
                return Some(res);
            }
            _ => return None,
        }
    }
}

thread_local! {
    pub static CURRENT_NAMES: RefCell<Vec<String>> = RefCell::new(Vec::new());
}

impl<'a> Stack<'a> {
    fn new_root(ctx: &'a mut LispContext) -> Self {
        Stack {
            local_scope: None,
            global_scope: ctx,
            error: None,
        }
    }
    fn new_local(ctx: &'a mut LispContext, local: LispScope<'a>) -> Self {
        Stack {
            local_scope: Some(Box::new(local)),
            global_scope: ctx,
            error: None,
        }
    }

    fn eval<T>(&mut self, code: &T) -> LispValue
    where
        T: LispEvalable + ?Sized,
    {
        let code = code.to_evalable(self.global_scope);
        let src = &self.global_scope.symbol_name_lookup;

        CURRENT_NAMES.with(|v| {
            let mut v = v.borrow_mut();
            if v.len() != src.len() {
                v.clear();
                for s in src.iter() {
                    v.push(s.clone());
                }
            }
        });
        if let Some(c) = code {
            return lisp_eval(self, &c);
        } else {
            lisp_raise_error(self, LispValue::String("Unable to parse code".to_string()));
        }
        LispValue::Nil
    }

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
            symbol_name_lookup: Vec::new(),
            globals: Vec::new(),
            global_names: Vec::with_capacity(10),
            arg_stack: Vec::new().into(),
        };
    }
    pub fn find_symbol(&self, name: &str) -> LispValue {
        if let Option::Some(id) = self.symbols.get(name) {
            return LispValue::Symbol(*id);
        }
        return LispValue::Nil;
    }

    pub fn get_symbol_id(&mut self, name: &str) -> i32 {
        if let Option::Some(id) = self.symbols.get(name) {
            return *id;
        }
        let lookup = &mut self.symbol_name_lookup;
        let id2 = lookup.len() as i32;
        lookup.push(name.to_string());
        self.symbols.insert(name.into(), id2);
        return id2;
    }

    pub fn get_symbol(&mut self, name: &str) -> LispValue {
        LispValue::Symbol(self.get_symbol_id(name))
    }

    pub fn set_global(&mut self, symbol_name: i32, value: LispValue) {
        if let Option::Some(index) = self.global_names.get(symbol_name as usize) {
            self.globals[*index] = value;
        } else {
            let new_index = self.globals.len();
            while self.global_names.len() <= symbol_name as usize {
                self.global_names.push(0);
            }
            self.global_names[symbol_name as usize] = new_index;
            self.globals.insert(new_index, value);
        }
    }

    pub fn get_global(&self, symbol_name: i32) -> Option<&LispValue> {
        if let Option::Some(index) = self.global_names.get(symbol_name as usize) {
            let x = Some(&self.globals[*index]);
            return x;
        }
        return None;
    }

    pub fn set_global_str(&mut self, name: &str, value: LispValue) {
        let id = self.get_symbol_id(name);
        self.set_global(id, value);
    }

    fn get_value(&self, symbol_name: i32) -> Option<&LispValue> {
        let r = self.get_global(symbol_name);
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
    a == b
}

fn lisp_raise_error(ctx: &mut Stack, error: LispValue) {
    ctx.error = Some(error);
}

fn cons_count(v: &LispValue) -> i64 {
    v.to_iter().count() as i64
}

fn lisp_invoken<'a>(
    ctx: &mut Stack,
    argsl: &'a LispValue,
    fcn: fn(Vec<LispValue>) -> LispValue,
) -> LispValue {
    let mut args: Vec<LispValue> = Vec::with_capacity(cons_count(argsl) as usize);

    for it in argsl.to_iter() {
        let r = lisp_eval(ctx, it);
        args.push(r);
    }
    let args2 = args;
    let r2 = fcn(args2);
    return r2;
}
fn lisp_invokenr<'a>(
    ctx: &mut Stack,
    argsl: &'a LispValue,
    fcn: fn(&[LispValue]) -> LispValue,
) -> LispValue {
    let mut args = [
        LispValue::Nil,
        LispValue::Nil,
        LispValue::Nil,
        LispValue::Nil,
        LispValue::Nil,
        LispValue::Nil,
        LispValue::Nil,
        LispValue::Nil,
        LispValue::Nil,
        LispValue::Nil,
    ];
    let mut itv = 0;
    for it in argsl.to_iter() {
        let r = lisp_eval(ctx, it);
        args[itv] = r;
        itv += 1;
    }

    let r2 = fcn(&args[0..itv]);
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
    let mut i: usize = 0;
    let mut it = args;

    let mut argcnt = func.args_names.len();
    let mut cap = argcnt;
    if func.variadic {
        argcnt = argcnt - 1;
        cap = 2;
    }

    let mut arg_veca = Vec::with_capacity(cap);

    while is_cons(it) && i < argcnt {
        arg_veca.push(lisp_eval(ctx, car(it)));
        i += 1;
        it = cdr(it);
    }
    if i < argcnt {
        lisp_raise_error(
            ctx,
            "Not enough required arguments to satisfy function.".into(),
        );
        return LispValue::Nil;
    }
    if func.variadic {
        fn listeval(ctx: &mut Stack, v: &LispValue) -> LispValue {
            let v0 = car(v);
            let rest = cdr(v);
            if is_nil(rest) {
                return LispValue::cons(lisp_eval(ctx, v0), LispValue::Nil);
            }
            LispValue::cons(lisp_eval(ctx, v0), listeval(ctx, rest))
        }
        let lst = listeval(ctx, it);
        arg_veca.push(lst);
    } else if is_cons(it) {
        lisp_raise_error(ctx, "Too many arguments for function.".into());
        return LispValue::Nil;
    }

    let scope = LispScope {
        id: func.args_names.as_slice(),
        values: arg_veca.as_mut_slice(),
        parent: match func.magic {
            true => &ctx.local_scope,
            false => &None,
        },
    };

    let mut stack2 = Stack::new_local(ctx.global_scope, scope);

    let mut result = LispValue::Nil;
    for form in func.code.to_iter() {
        result = lisp_eval(&mut stack2, form);
    }
    ctx.error = stack2.error;

    result
}


fn lisp_eval<'a>(ctx: &'a mut Stack, v: &'a LispValue) -> LispValue {
    if let Some(e) = &ctx.error {
        ctx.error = Some(LispValue::String(format!("{}\nat {}", e, v)));
        return LispValue::Nil;
    }
    match v {
        LispValue::Cons(a) => {
            let value = lisp_eval(ctx, &a.0);

            match value {
                LispValue::NativeFunction(n) => {
                    let prev_count = ctx.global_scope.arg_stack.len();

                    for it in v.to_iter().skip(1) {
                        let r = lisp_eval(ctx, it);

                        ctx.global_scope.arg_stack.push(r);
                    }
                    let slice = &ctx.global_scope.arg_stack[prev_count..];

                    let result: (i32, LispValue) = match n {
                        NativeFunc::Function1(f) => (1, (f)(slice[0].clone())),
                        NativeFunc::Function2(f) => (2, (f)(slice[0].clone(), slice[1].clone())),
                        NativeFunc::Function1r(f) => (1, (f)(&slice[0]).clone()).clone(),
                        NativeFunc::Function2r(f) => (2, (f)(&slice[0], &slice[1]).clone()),
                        NativeFunc::FunctionN(f) => (slice.len() as i32, (f)(slice.to_vec())),
                        NativeFunc::FunctionNr(f) => (slice.len() as i32, (f)(slice)),
                    };
                    ctx.global_scope.arg_stack.truncate(prev_count);
                    return result.1;
                }

                LispValue::Macro(mf) => return (mf)(ctx, cdr(v)),
                LispValue::LispFunction(lf) => {
                    return lisp_eval_lisp_function(ctx, &lf, cdr(v));
                }

                _ => lisp_raise_error(ctx, format!("no such function: {}", car(v)).into()),
            }
            return LispValue::Nil;
        }
        LispValue::Symbol(id) => {
            if let Some(value) = ctx.get_value(*id) {
                return value.clone();
            } else {
                lisp_raise_error(ctx, format!("No such symbol! {} {:#?}", v, ctx).into());
                return LispValue::Nil;
            }
        }
        _ => v.clone(),
    }
}

fn lisp_eval_str(ctx: &mut Stack, code: &str) -> LispValue {
    let code2 = parse_from_string(ctx.global_scope, code);
    return lisp_eval(ctx, &code2);
}

fn lisp_load_str(ctx: &mut Stack, code: &str) -> Option<LispValue> {
    let mut bytes = code.as_bytes();
    while let Some(c) = parse_bytes(ctx.global_scope, &mut bytes) {
        ctx.eval(&c);
        if ctx.error.is_some() {
            return ctx.error.clone();
        }
    }
    None
}

fn lisp_eval_file(ctx: &mut Stack, code: &str) {
    let str = fs::read_to_string(code);
    match str {
        Ok(r) => {
            lisp_load_str(ctx, r.as_str());
        }
        Err(_) => {
            lisp_raise_error(ctx, LispValue::String("eerr".to_string()));
        }
    }
}

fn main() {
    let mut ctx = lisp_load_basic();
    let mut stk = Stack::new_root(&mut ctx);

    let args: Vec<String> = env::args().collect();
    for arg in args.iter().skip(1) {
        lisp_eval_file(&mut stk, arg.as_str());
        if let Some(err) = stk.error.clone() {
            println!("ERROR: {}", err);
            break;
        }
    }
}

fn lisp_load_basic() -> LispContext {
    let mut ctx = LispContext::new();
    ctx.set_global_str("<<<RESERVED>>>", LispValue::Nil);
    lisp_load_lisp(&mut ctx);
    lisp_math_load(&mut ctx);
    //lisp_advanced_load(&mut ctx);
    return ctx;
}

#[cfg(test)]
mod test {

    use crate::*;

    #[test]
    fn bignum_test() {
        let mut ctx = lisp_load_basic();

        let mut stk = Stack::new_root(&mut ctx);
        lisp_eval_str(&mut stk, "(defvar big1 1000000)");
        lisp_eval_str(&mut stk, "(defvar big2 (* big1 big1 big1 big1 big1 big1))");
        lisp_eval_str(&mut stk, "(println big2)");
        lisp_eval_str(&mut stk, "(defvar big3 (/ big2 big1 big1 big1 big1 big1))");
        lisp_eval_str(&mut stk, "(println (list big3 big2 big1))");
        lisp_eval_str(&mut stk, "(assert (println (equals big3 big1)))");
        lisp_eval_str(&mut stk, "(assert (not (equals big2 big1)))");

        assert!(stk.error.is_none());
    }

    #[test]
    fn eval_test() {
        let mut ctx = lisp_load_basic();

        let mut stk = Stack::new_root(&mut ctx);
        stk.eval("(println assert)");
        stk.eval("(assert (eq 1 0))");

        assert!(stk.error.is_some());
    }

    #[test]
    fn eq_test() {
        let mut ctx = lisp_load_basic();

        let err = ctx.load("(assert (println (eq 1 1)))");
        assert!(err.is_none());

        let mut stk = Stack::new_root(&mut ctx);
        stk.eval("(assert (not (eq 1 0)))");
        stk.eval("(assert (not (eq (println (quote a)) (quote b))))");

        assert!(stk.error.is_none());
    }

    #[test]
    fn if_test() {
        let mut ctx = lisp_load_basic();
        let err = ctx.load("(if 1 () (raise (quote error)))");
        assert!(err.is_none());
        let err = ctx.load("(if () (raise (quote error)) 1)");
        assert!(err.is_none());
    }

    #[test]
    fn raise_test() {
        let mut ctx = lisp_load_basic();
        let err = ctx.load("(raise (quote error))");
        assert!(err.is_some());
    }

    #[test]
    fn iter_test() {
        let i = LispValue::cons(1.into(), LispValue::cons(2.into(), LispValue::Nil));
        let mut it = i.to_iter();
        assert!(eq(it.next().unwrap(), &LispValue::Integer(1)));
        assert!(eq(it.next().unwrap(), &LispValue::Integer(2)));
        assert!(it.next().is_none());
    }

    #[test]
    fn mega_test() {
        let mut ctx = lisp_load_basic();

        let code =
            "(111 222  333 asd asd asdd asddd asdd asd 1.1 2.2 3.3 (x y z) (1.0 2.0 3.0) 3.14)";
        let mut out: LispValue = LispValue::Nil;
        parse(&mut ctx, code.as_bytes(), &mut out);
        println!("Parsed: {}", out);

        println!("BigInt: {}", LispValue::from(num::BigInt::from(10000)));

        assert!(eq(&LispValue::Integer(222), car(cdr(&out))));
        assert!(eq(&ctx.get_symbol("asd"), cadddr(&out)));
        assert!(!eq(&ctx.get_symbol("asdd"), cadddr(&out)));
        let code2 = parse_from_string(&mut ctx, "(cdr (println  (cons (+ 1 2 3.14) (cons (* 1000 (+ 1234 9999 4321 1111 (- 1000 1000 1000))) 1))))");

        let mut stk = Stack::new_root(&mut ctx);

        lisp_eval(&mut stk, &code2);
        stk.eval("(println (list 1 2 3))");

        stk.eval("(let ((x 10)) (println (list 1 2 3 x)))");
        stk.eval(
            "(println (let ((x 10)) (println x) (set! x 5) (println x) (not (not (eq x 10)))))",
        );
        stk.eval("(println (let ((x 5)) (loop (not (eq x 0)) (println (set! x (- x 1))))))");
        stk.eval( "(println (* (big-rational 10000) 10000 10000 10000 100000 10000000 100000 1000000 100000 10000))");
        stk.eval("(println (car (println (cdr (cdr (list 1 2 3 4 5 6 7 8))))))");
        stk.eval("(println (list 1 2 3 4 5 6 7 (cons 8 9)))");
        stk.eval("(println (if 1 2 3))");
        stk.eval("(println (if () 2 3))");

        stk.eval("(println (let ((a 1)) (let ((b 2)) (println (+ a b)))))");
        stk.eval("(println (let ((a 2) (b 3) (c 5)) (* a b c)))");
        stk.eval("(println (let ((x 10000)) (loop (not (eq x 0)) (set! x (- x 1))) x))");

        stk.eval("(defun print2 (a) (println ;;hello world\n (+ 5 a 1.3)))");
        stk.eval("(assert (println (print2 123)))");
        stk.eval("(defun pow2 (a) (* a a))");
        println!("{}", stk.eval("(pow2 10)"));
        assert!(stk.error.is_none());
    }

    #[test]
    fn test_code_builder() {
        let ctx = lisp_load_basic();

        let mut wd = CodeWriter::new(ctx);
        let bignumber :u128 = 111222333444555666777888999000111222333;
        let bignumber2 :i128 = -111222333444555666777888999000111222333;
        wd.emit(ByteCode::LdSym);
        wd.emit_uleb(bignumber);
        wd.emit(ByteCode::LdSym);
        wd.emit_sleb(bignumber2);
        wd.emit(ByteCode::SetSym);
        wd.emit_uleb(303);
        println!("{:?}", wd.bytes);

        let mut reader = wd.to_reader();
        let sym1 = reader.read_u8();
        let val1 = reader.read_uleb_u128();
        let sym2 = reader.read_u8();
        let val2 = reader.read_sleb_i128();
        assert_eq!(bignumber,val1);
        println!("{} {}   {} {}", sym1, val1, sym2, val2);
    }
}
//#[cfg(test)]
//#[test]
//fn tmpIndex_test() {
//let mut v :Rc<RwLock<Vec<LispValue>>> = Arc::new(RwLock::new(vec![LispValue::Nil]));
//{
//    let vw = v.try_write();
//    vw.unwrap()[0] = LispValue::T;
//}
//println!("{:#?}", v);
//}
