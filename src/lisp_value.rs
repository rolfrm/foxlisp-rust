use std::cmp::Ordering;
use std::rc::*;
use std::fmt::*;
use std::result::Result;
use crate::*;

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
    fn to_evalable(&self, ctx: &mut Lisp) -> Option<LispValue>;
}

impl LispEvalable for str {
    fn to_evalable(&self, ctx: &mut Lisp) -> Option<LispValue> {
        let mut code = self.as_bytes();
        return parse_bytes(ctx, &mut code);
    }
}

impl LispEvalable for LispValue {
    fn to_evalable(&self, _: &mut Lisp) -> Option<LispValue> {
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
    pub fn from_n_macrolike(item: fn(&mut Lisp, &[LispValue]) -> LispValue) -> Self {
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

    pub fn is_nil(&self) -> bool {
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
    pub fn equals(&self, other: &Self) -> bool {
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

pub trait ToLisp {
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

pub trait ToSym{
     fn to_sym(&self, lisp: &mut Lisp) -> LispValue;
}

impl ToSym for &str {
    fn to_sym(&self, lisp: &mut Lisp) -> LispValue {
        lisp.get_symbol(self)
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

#[derive(Clone)]
pub enum NativeFunc {
    Function1(fn(LispValue) -> LispValue),
    Function2(fn(LispValue, LispValue) -> LispValue),
    Function1r(fn(&LispValue) -> &LispValue),
    Function2r(fn(&[LispValue]) -> &LispValue),
    FunctionN(fn(&[LispValue]) -> LispValue),
    FunctionMacroLike(fn(&mut Lisp, &[LispValue]) -> LispValue),
}

impl fmt::Debug for NativeFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Nativefunc")
    }
}

#[derive(Debug)]
pub struct LispFunc {
    pub code: LispValue,
    pub compiled_code: Vec<u8>,
    pub labels: Vec<u32>,
    pub args_names: Vec<i32>,
    pub magic: bool,
    pub variadic: bool,
}

impl LispFunc {
    pub fn with_compled_code(&self, code: CodeWriter) -> LispFunc {
        LispFunc {
            code: self.code.clone(),
            compiled_code: code.bytes,
            labels: code.labels,
            args_names: self.args_names.clone(),
            magic: self.magic,
            variadic: self.variadic,
        }
    }
}
