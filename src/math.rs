use num::{self, FromPrimitive, BigRational, BigInt, ToPrimitive};

use crate::LispValue;

struct NumericFunc{
    f_f64: fn(f64, f64) -> f64,
    f_int: fn(i64, i64) -> Option<i64>,
    f_bigint: fn(&num::BigInt, &num::BigInt) -> num::BigInt,
    f_bigrational: fn(&num::BigRational, &num::BigRational) -> num::BigRational
}

static ADD_OP: NumericFunc = NumericFunc{
    f_f64: |x,y|{ x + y},
    f_int: |x,y|{x.checked_add(y)},
    f_bigint: |x,y|{ x + y},
    f_bigrational: |x,y|{ x + y},
};

static SUB_OP: NumericFunc = NumericFunc{
    f_f64: |x,y|{ x - y},
    f_int: |x,y|{ x.checked_sub(y)},
    f_bigint: |x,y|{ x - y},
    f_bigrational: |x,y|{ x - y},
};
static MUL_OP: NumericFunc = NumericFunc{
    f_f64: |x,y|{ x * y},
    f_int: |x,y|{ x.checked_mul(y)},
    f_bigint: |x,y|{ x * y},
    f_bigrational: |x,y|{ x * y},
};
static DIV_OP: NumericFunc = NumericFunc{
    f_f64: |x,y|{ x / y},
    f_int: |x,y|{ x.checked_div(y)},
    f_bigint: |x,y|{ x / y},
    f_bigrational: |x,y|{ x / y},
};

fn handler_underflow(lv : LispValue) -> LispValue {
    if let LispValue::BigInt(v) = &lv {
        if let Some(v2) = v.to_i64() {
            return LispValue::Integer(v2);
        }
    }
    return lv;
}

fn apply_number_func(func: &NumericFunc, a: LispValue, b: LispValue) -> LispValue
{
    if let LispValue::Integer(x) = a {
        if let LispValue::Integer(y) = b {
            if let Some(x) = (func.f_int)(x, y){
                return LispValue::Integer(x);
            }
            return LispValue::BigInt((func.f_bigint)(&BigInt::from_i64(x).unwrap(), &BigInt::from_i64(y).unwrap()));
            
        }
        if let LispValue::Rational(y) = b {
            return LispValue::Rational((func.f_f64)(x as f64, y));
        }
        if let LispValue::BigInt(y) = &b {
            return handler_underflow(LispValue::BigInt((func.f_bigint)(&num::BigInt::from(x), y)));
        }
        if let LispValue::BigRational(y) = &b {
            return LispValue::BigRational((func.f_bigrational)(&num::BigRational::from_i64(x).unwrap(), y));
        }
    }
    if let LispValue::Rational(x) = a {
        if let LispValue::Integer(y) = b {
            return LispValue::Rational((func.f_f64)(x, y as f64));
        }
        if let LispValue::Rational(y) = b {
            return LispValue::Rational((func.f_f64)(x, y));
        }
        if let LispValue::BigInt(y) = &b {
            return LispValue::BigRational((func.f_bigrational)(&num::BigRational::from_f64(x).unwrap(), &num::BigRational::from(y.clone())));
        }
        if let LispValue::BigRational(y) = &b {
            return LispValue::BigRational((func.f_bigrational)(&num::BigRational::from_f64(x).unwrap(), y));
        }
    }
    if let LispValue::BigInt(x) = &a {
        if let LispValue::Integer(y) = b {
            return LispValue::BigRational((func.f_bigrational)(&BigRational::from(x.clone()), &BigRational::from_i64(y).unwrap()));
        }
        if let LispValue::Rational(y) = b {
            return LispValue::BigRational((func.f_bigrational)(&BigRational::from(x.clone()), &BigRational::from_f64(y).unwrap()));
        }
        if let LispValue::BigInt(y) = &b {
            return handler_underflow(LispValue::BigInt((func.f_bigint)(x, y)));
        }
        if let LispValue::BigRational(y) = &b {
            return LispValue::BigRational((func.f_bigrational)(&num::BigRational::from(x.clone()), y));
        }
    }

    if let LispValue::BigRational(x) = &a {
        if let LispValue::Integer(y) = b {
            return LispValue::BigRational((func.f_bigrational)(x, &BigRational::from_i64(y).unwrap()));
        }
        if let LispValue::Rational(y) = b {
            return LispValue::BigRational((func.f_bigrational)(x, &BigRational::from_f64(y).unwrap()));
        }
        if let LispValue::BigInt(y) = &b {
            return LispValue::BigRational((func.f_bigrational)(x, &BigRational::from(y.clone())));
        }
        if let LispValue::BigRational(y) = &b {
            return LispValue::BigRational((func.f_bigrational)(x, y));
        }
    }
    return LispValue::Nil;

}

fn lisp_apply_numbers(v: &Vec<LispValue>, func: &NumericFunc,) -> LispValue{
    let first = v.first();
    if let Some(firstv) = first {
        let mut acc = firstv.clone();
        for i in v.iter().skip(1) {
            acc = apply_number_func(func, acc, i.clone());
        }
        return acc;
    }
    return LispValue::Nil;
}

pub fn lisp_add(v: Vec<LispValue>) -> LispValue {
    return lisp_apply_numbers(&v, &ADD_OP);
}

pub fn lisp_sub(v: Vec<LispValue>) -> LispValue {
    return lisp_apply_numbers(&v, &SUB_OP);
}

pub fn lisp_mul(v: Vec<LispValue>) -> LispValue {
    return lisp_apply_numbers(&v, &MUL_OP);
}

pub fn lisp_div(v: Vec<LispValue>) -> LispValue {
    return lisp_apply_numbers(&v, &DIV_OP);
}

