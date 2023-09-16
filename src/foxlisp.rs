use num::{self, ToPrimitive};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{self};
use std::rc::Rc;
use std::{env, u128};
mod lisp;
use lisp::*;
use num::BigInt;
mod math;
use math::*;
use std::cell::RefCell;
use std::fs;

mod bytecode;
use bytecode::*;
mod bytecode_optimizer;
use bytecode_optimizer::*;
mod parser;
use parser::*;

mod code_reader_writer;
use code_reader_writer::*;

mod compile;
use compile::*;

#[derive(Clone)]
pub enum NativeFunc {
    Function1(fn(LispValue) -> LispValue),
    Function2(fn(LispValue, LispValue) -> LispValue),
    Function1r(fn(&LispValue) -> &LispValue),
    Function2r(fn(&[LispValue]) -> &LispValue),
    FunctionN(fn(&[LispValue]) -> LispValue),
    FunctionMacroLike(fn(&mut LispContext, &[LispValue]) -> LispValue),
}

impl fmt::Debug for NativeFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Nativefunc")
    }
}

#[derive(Debug)]
pub struct LispFunc {
    code: LispValue,
    compiled_code: Vec<u8>,
    args_names: Vec<i32>,
    magic: bool,
    variadic: bool,
}

impl LispFunc {
    pub fn with_compled_code(&self, code: Vec<u8>) -> LispFunc {
        LispFunc {
            code: self.code.clone(),
            compiled_code: code,
            args_names: self.args_names.clone(),
            magic: self.magic,
            variadic: self.variadic,
        }
    }
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
    pub fn from_2r(item: fn(&[LispValue]) -> &LispValue) -> Self {
        LispValue::NativeFunction(NativeFunc::Function2r(item))
    }
    pub fn from_n(item: fn(&[LispValue]) -> LispValue) -> Self {
        LispValue::NativeFunction(NativeFunc::FunctionN(item))
    }
    pub fn from_n_macrolike(item: fn(&mut LispContext, &[LispValue]) -> LispValue) -> Self {
        LispValue::NativeFunction(NativeFunc::FunctionMacroLike(item))
    }

    pub fn cons(a: LispValue, b: LispValue) -> LispValue {
        LispValue::Cons(Rc::new((a, b)))
    }
    pub fn to_iter<'a>(&'a self) -> ConsIter<'a> {
        ConsIter { current: self }
    }

    pub fn to_integer(&self) -> Option<i64> {
        match self {
            LispValue::Rational(x) => x.to_i64(),
            LispValue::Integer(x) => Some(*x),
            LispValue::BigInt(x) => x.to_i64(),
            LispValue::BigRational(x) => x.to_i64(),
            _ => None,
        }
    }
    pub fn to_bigint(&self) -> Option<Rc<BigInt>> {
        match self {
            LispValue::Rational(_x) => None,
            LispValue::Integer(_x) => None,
            LispValue::BigInt(x) => Some(x.clone()),
            LispValue::BigRational(_x) => None,
            _ => None,
        }
    }
    pub fn to_lisp_func(&self) -> Option<&LispFunc> {
        match self {
            LispValue::LispFunction(f) => Some(f.as_ref()),
            _ => None,
        }
    }
    pub fn to_symbol_id(&self) -> Result<i32, String> {
        match self {
            LispValue::Symbol(s) => Ok(*s),
            _ => Err("!!".into()),
        }
    }

    fn is_nil(&self) -> bool {
        match self {
            LispValue::Nil => true,
            _ => false,
        }
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

    pub fn as_car(self, cdr: LispValue) -> LispValue {
        lisp_cons(self, cdr)
    }
}

trait ToLisp {
    fn to_lisp(&self) -> LispValue;
}

impl ToLisp for i64 {
    fn to_lisp(&self) -> LispValue {
        LispValue::Integer(*self)
    }
}

impl ToLisp for i32 {
    fn to_lisp(&self) -> LispValue {
        LispValue::Integer(*self as i64)
    }
}

impl ToLisp for u64 {
    fn to_lisp(&self) -> LispValue {
        LispValue::Integer(*self as i64)
    }
}

impl ToLisp for f64 {
    fn to_lisp(&self) -> LispValue {
        LispValue::Rational(*self)
    }
}

impl ToLisp for String {
    fn to_lisp(&self) -> LispValue {
        LispValue::String(self.clone())
    }
}

impl ToLisp for str {
    fn to_lisp(&self) -> LispValue {
        LispValue::String(self.into())
    }
}

impl ToLisp for LispValue {
    fn to_lisp(&self) -> LispValue {
        self.clone()
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

impl From<fn(&[LispValue]) -> LispValue> for LispValue {
    fn from(item: fn(&[LispValue]) -> LispValue) -> Self {
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
            LispValue::Cons(_) => {
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
            LispValue::LispFunction(_) => write!(f, "LispFunction"),
            LispValue::Rest => write!(f, "&REST"),
            LispValue::T => write!(f, "T"),
        }
    }
}

#[derive(Debug)]
pub enum ScopeType {
    FunctionScope(LispScope2),
    LetScope(LetScope),
}

#[derive(Debug)]
pub struct LetScope {
    sym: i32,
    argoffset: usize,
}

impl LetScope {
    pub fn get_value<'a>(&self, ctx: &'a LispContext, symid: i32) -> Option<&'a LispValue> {
        if self.sym == symid {
            return Some(&ctx.arg_stack[self.argoffset]);
        }
        return None;
    }
    pub fn set_value<'a>(&self, ctx: &mut LispContext, symid: i32, v: LispValue) -> Option<()> {
        if self.sym == symid {
            ctx.arg_stack[self.argoffset] = v;
            return Some(());
        }
        return None;
    }
    pub fn get_arg_offset(&self, symid: i32) -> Option<usize> {
        if self.sym == symid {
            return Some(self.argoffset);
        }
        return None;
    }
}

#[derive(Debug)]
pub struct LispScope2 {
    func: Rc<LispFunc>,
    argoffset: usize,
    reader: CodeReader,
    local_vars: Vec<LetScope>,
}

impl LispScope2 {
    pub fn new(func: Rc<LispFunc>, argoffset: usize, reader: CodeReader) -> LispScope2 {
        LispScope2 {
            func,
            argoffset,
            reader,
            local_vars: Vec::new(),
        }
    }
    pub fn get_arg_offset(&self, symid: i32) -> Option<usize> {
        for i in 0..self.func.args_names.len() {
            if self.func.args_names[i] == symid {
                return Some(self.argoffset + i);
            }
        }
        return None;
    }

    pub fn get_value<'a>(&self, ctx: &'a LispContext, symid: i32) -> Option<&'a LispValue> {
        for i in 0..self.func.args_names.len() {
            if self.func.args_names[i] == symid {
                return Some(&ctx.arg_stack[self.argoffset + i]);
            }
        }
        return None;
    }
    pub fn set_value<'a>(&self, ctx: &mut LispContext, symid: i32, v: LispValue) -> Option<()> {
        for i in 0..self.func.args_names.len() {
            if self.func.args_names[i] == symid {
                ctx.arg_stack[self.argoffset + i] = v;
                return Some(());
            }
        }
        return None;
    }
}

#[derive(Debug)]
pub struct LispContext {
    symbols: HashMap<String, i32>,
    symbol_name_lookup: Vec<String>,
    globals: Vec<LispValue>,
    global_names: Vec<usize>,

    arg_stack: Vec<LispValue>,
    current_scope: Vec<LispScope2>,
    current_error: LispValue,

    quote_store: Vec<LispValue>,
    panic_on_error: bool,
}

impl LispContext {
    pub fn push_local_var(&mut self, scope: LetScope) {
        self.current_scope
            .last_mut()
            .unwrap()
            .local_vars
            .push(scope);
    }

    pub fn panic_if_error(&self) {
        println!("panic ? {}", self.current_error);
        if self.current_error.is_nil() {
            return;
        }
        panic!("{}", self.current_error);
    }

    pub fn get_quote_store(&mut self, v: &LispValue) -> i32 {
        let id = self.quote_store.len() as i32;
        self.quote_store.push(v.clone());
        return id;
    }

    pub fn lookup_quote(&self, id: i32) -> &LispValue {
        self.quote_store.get(id as usize).unwrap()
    }

    fn load(&mut self, code: &str) -> LispValue {
        lisp_load_str(self, code)
    }

    fn eval(&mut self, code: &LispValue) -> LispValue {
        lisp_compile_and_eval(self, code)
    }

    fn eval_str(&mut self, code: &str) -> LispValue {
        lisp_compile_and_eval_string(self, code)
    }

    fn parse(&mut self, code: &str) -> Option<LispValue> {
        let mut code_bytes = code.as_bytes();
        let c1 = parse_bytes(self, &mut code_bytes);
        return c1;
    }

    fn get_reader(&self) -> Option<&CodeReader> {
        for x in self.current_scope.iter().rev() {
            return Some(&x.reader);
        }
        return None;
    }

    fn get_reader_mut(&mut self) -> &mut CodeReader {
        let len = self.current_scope.len();
        return &mut self.current_scope[len - 1].reader;
    }

    fn reader_end(&self) -> bool {
        self.get_reader().unwrap().end()
    }
    fn read_u8(&mut self) -> u8 {
        self.get_reader_mut().read_u8()
    }
    fn read_uleb(&mut self) -> u64 {
        self.get_reader_mut().read_uleb()
    }
    fn read_sleb(&mut self) -> i64 {
        self.get_reader_mut().read_sleb()
    }
    fn jmp(&mut self, offset: i64) {
        self.get_reader_mut().jmp(offset)
    }

    fn get_symbol_name(&self, value: &LispValue) -> Option<String> {
        value.symbol_name(self)
    }
}

pub trait LispSymbolName {
    fn symbol_name(&self, ctx: &LispContext) -> Option<String>;
}

impl LispSymbolName for i32 {
    fn symbol_name(&self, ctx: &LispContext) -> Option<String> {
        ctx.symbol_name_lookup
            .get(*self as usize)
            .and_then(|x| Some(x.clone()))
    }
}

impl LispSymbolName for LispValue {
    fn symbol_name(&self, ctx: &LispContext) -> Option<String> {
        if let Ok(x) = self.to_symbol_id() {
            return x.symbol_name(ctx).clone();
        }
        return None;
    }
}

pub trait IntoSymbol {
    fn to_symbol(&self) -> LispValue;
}

impl IntoSymbol for i32 {
    fn to_symbol(&self) -> LispValue {
        LispValue::Symbol(*self)
    }
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
pub fn update_symbol_names(ctx: &LispContext) {
    let src = &ctx.symbol_name_lookup;
    CURRENT_NAMES.with(|v| {
        let mut v = v.borrow_mut();
        if v.len() != src.len() {
            v.clear();
            for s in src.iter() {
                v.push(s.clone());
            }
        }
    });
}

impl LispContext {
    fn new() -> LispContext {
        return LispContext {
            symbols: HashMap::new(),
            symbol_name_lookup: Vec::new(),
            globals: Vec::new(),
            global_names: Vec::with_capacity(10),
            arg_stack: Vec::new(),
            current_scope: Vec::new(),
            current_error: LispValue::Nil,
            quote_store: Vec::new(),
            panic_on_error: false,
        };
    }

    pub fn raise_error(&mut self, error: LispValue) {
        if self.panic_on_error {
            panic!("{}", error);
        } else {
            self.current_error = error;
        }
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

    pub fn get_global_str(&mut self, name: &str) -> Option<&LispValue> {
        let id = self.get_symbol_id(name);
        self.get_global(id)
    }

    pub fn set_global_str(&mut self, name: &str, value: LispValue) {
        let id = self.get_symbol_id(name);
        self.set_global(id, value);
    }

    fn get_value(&self, symid: i32) -> Option<&LispValue> {
        let mut place: Option<usize> = None;
        if let Some(scope) = self.current_scope.last() {
            for let_var in scope.local_vars.iter().rev() {
                if let_var.sym == symid {
                    place = Some(let_var.argoffset);
                    break;
                }
            }
            if place.is_none() {
                place = scope.get_arg_offset(symid)
            }
        }

        if let Some(i) = place {
            return Some(&self.arg_stack[i]);
        }
        return self.get_global(symid);
    }

    fn set_value(&mut self, symid: i32, v: LispValue) -> bool {
        let mut place: Option<usize> = None;
        if let Some(scope) = self.current_scope.last() {
            for let_var in scope.local_vars.iter().rev() {
                if let_var.sym == symid {
                    place = Some(let_var.argoffset);
                    break;
                }
            }
            if place.is_none() {
                place = scope.get_arg_offset(symid)
            }
        }
        if let Some(i) = place {
            self.arg_stack[i] = v;
            return true;
        }
        self.set_global(symid, v);
        return true;
    }
}

fn eq(a: &LispValue, b: &LispValue) -> bool {
    a == b
}

fn cons_count(v: &LispValue) -> i64 {
    v.to_iter().count() as i64
}

fn lisp_load_str(ctx: &mut LispContext, code: &str) -> LispValue {
    let mut bytes = code.as_bytes();
    let mut result = LispValue::Nil;
    while let Some(c) = parse_bytes(ctx, &mut bytes) {
        update_symbol_names(&ctx);
        let mut wd = CodeWriter::new();

        lisp_compile(ctx, &c, &mut wd).unwrap();

        let mut bytecode = bytecode_to_lisp(&mut wd.bytes.as_slice(), ctx);
        bytecode = optimize_bytecode(bytecode);
        println!("Optimized bytecode: {}", bytecode);
        //    lisp_bytecode_print(&mut CodeReader::new(wd.bytes.clone()), ctx);

        let code_reader = CodeReader::new(wd.bytes.clone());

        let lf = LispFunc {
            code: c.clone(),
            compiled_code: wd.bytes.clone(),
            args_names: Vec::new(),
            magic: false,
            variadic: false,
        };

        let s2 = LispScope2::new(Rc::new(lf), 0, code_reader);

        ctx.current_scope.push(s2);

        lisp_eval_bytecode(ctx);
        ctx.current_scope.pop();
        result = ctx.arg_stack.pop().unwrap();
    }
    return result;
}

fn lisp_eval_file(ctx: &mut LispContext, code: &str) {
    let str = fs::read_to_string(code);
    match str {
        Ok(r) => {
            lisp_load_str(ctx, r.as_str());
        }
        Err(_) => {
            lisp_raise(ctx, &[LispValue::String("eerr".to_string())]);
        }
    }
}

fn main() {
    let mut ctx = lisp_load_basic();
    let args: Vec<String> = env::args().collect();
    for arg in args.iter().skip(1) {
        lisp_eval_file(&mut ctx, arg.as_str());
        //if let Some(err) = stk.error.clone() {
        //    println!("ERROR: {}", err);
        //    break;
        //}
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

        ctx.eval_str("(defvar big1 1000000)");
        ctx.eval_str("(defvar big2 (* big1 big1 big1 big1 big1 big1))");
        ctx.eval_str("(println big2)");
        ctx.eval_str("(defvar big3 (/ big2 big1 big1 big1 big1 big1))");
        ctx.eval_str("(println (list big3 big2 big1))");
        ctx.eval_str("(assert (println (equals big3 big1)))");
        ctx.eval_str("(assert (not (equals big2 big1)))");
        update_symbol_names(&mut ctx);
        assert!(ctx.current_error.is_nil());
    }

    #[test]
    fn test_error() {
        let mut ctx = lisp_load_basic();
        ctx.eval_str("(assert nil)");
        assert!(ctx.current_error.is_nil() == false);
    }
    #[test]
    fn eval_test() {
        let mut ctx = lisp_load_basic();

        ctx.eval_str("(println assert)");
        ctx.eval_str("(assert (eq 1 1))");
        ctx.panic_if_error();
    }

    #[test]
    fn eq_test() {
        let mut ctx = lisp_load_basic();

        ctx.load("(assert (println (eq 1 1)))");
        assert!(ctx.current_error.is_nil());

        ctx.eval_str("(assert (not (eq 1 0)))");
        ctx.eval_str("(assert (not (eq (println (quote a)) (quote b))))");

        assert!(ctx.current_error.is_nil());
    }

    #[test]
    fn if_test() {
        let mut ctx = lisp_load_basic();
        ctx.load("(if 1 () (raise (quote error)))");
        assert!(ctx.current_error.is_nil());
        let err = ctx.load("(if () (raise (quote error)) 1)");
        assert!(ctx.current_error.is_nil());
    }

    #[test]
    fn raise_test() {
        let mut ctx = lisp_load_basic();
        ctx.load("(raise (quote error))");
        assert!(ctx.current_error.is_nil() == false);
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

        ctx.eval(&code2);
        ctx.eval_str("(println (list 1 2 3))");

        ctx.eval_str("(let ((x 10)) (println (list 1 2 3 x)))");
        ctx.eval_str(
            "(println (let ((x 10)) (println x) (set! x 5) (println x) (not (not (eq x 10)))))",
        );
        ctx.eval_str("(println (let ((x 5)) (loop (not (eq x 0)) (println (set! x (- x 1))))))");
        ctx.eval_str( "(println (* (big-rational 10000) 10000 10000 10000 100000 10000000 100000 1000000 100000 10000))");
        ctx.eval_str("(println (car (println (cdr (cdr (list 1 2 3 4 5 6 7 8))))))");
        ctx.eval_str("(println (list 1 2 3 4 5 6 7 (cons 8 9)))");
        ctx.eval_str("(println (if 1 2 3))");
        ctx.eval_str("(println (if () 2 3))");

        ctx.eval_str("(println (let ((a 1)) (let ((b 2)) (println (+ a b)))))");
        ctx.eval_str("(println (let ((a 2) (b 3) (c 5)) (* a b c)))");
        ctx.eval_str("(println (let ((x 10000)) (loop (not (eq x 0)) (set! x (- x 1))) x))");

        ctx.eval_str("(defun print2 (a) (println ;;hello world\n (+ 5 a 1.3)))");
        ctx.eval_str("(assert (println (print2 123)))");
        ctx.eval_str("(defun pow2 (a) (* a a))");
        println!("{}", ctx.eval_str("(pow2 10)"));
        assert!(ctx.current_error.is_nil());
    }
}
