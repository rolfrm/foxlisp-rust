use crate::{LispContext, LispValue, car, cadr, Stack, is_nil, lisp_raise_error};
use num::{self, BigInt, BigRational, FromPrimitive, ToPrimitive};

struct NumericFunc {
    f_f64: fn(f64, f64) -> f64,
    f_int: fn(i64, i64) -> Option<i64>,
    f_bigint: fn(&num::BigInt, &num::BigInt) -> num::BigInt,
    f_bigrational: fn(&num::BigRational, &num::BigRational) -> num::BigRational,
}

static ADD_OP: NumericFunc = NumericFunc {
    f_f64: |x, y| x + y,
    f_int: |x, y| x.checked_add(y),
    f_bigint: |x, y| x + y,
    f_bigrational: |x, y| x + y,
};

static SUB_OP: NumericFunc = NumericFunc {
    f_f64: |x, y| x - y,
    f_int: |x, y| x.checked_sub(y),
    f_bigint: |x, y| x - y,
    f_bigrational: |x, y| x - y,
};

static MUL_OP: NumericFunc = NumericFunc {
    f_f64: |x, y| x * y,
    f_int: |x, y| x.checked_mul(y),
    f_bigint: |x, y| x * y,
    f_bigrational: |x, y| x * y,
};

static DIV_OP: NumericFunc = NumericFunc {
    f_f64: |x, y| x / y,
    f_int: |x, y| x.checked_div(y),
    f_bigint: |x, y| x / y,
    f_bigrational: |x, y| x / y,
};

fn handler_underflow(lv: LispValue) -> LispValue {
    if let LispValue::BigInt(v) = &lv {
        if let Some(v2) = v.to_i64() {
            return LispValue::Integer(v2);
        }
    }
    return lv;
}

fn apply_number_func(func: &NumericFunc, a: &LispValue, b: &LispValue) -> LispValue {
    if let LispValue::Integer(x) = a {
        if let LispValue::Integer(y) = b {
            if let Some(x) = (func.f_int)(*x, *y) {
                return LispValue::Integer(x);
            }
            return LispValue::BigInt((func.f_bigint)(
                &BigInt::from_i64(*x).unwrap(),
                &BigInt::from_i64(*y).unwrap(),
            ));
        }
        if let LispValue::Rational(y) = b {
            return LispValue::Rational((func.f_f64)(*x as f64, *y));
        }
        if let LispValue::BigInt(y) = &b {
            return handler_underflow(LispValue::BigInt((func.f_bigint)(
                &num::BigInt::from(*x),
                y,
            )));
        }
        if let LispValue::BigRational(y) = &b {
            return LispValue::BigRational((func.f_bigrational)(
                &num::BigRational::from_i64(*x).unwrap(),
                y,
            ));
        }
    }
    if let LispValue::Rational(x) = a {
        if let LispValue::Integer(y) = b {
            return LispValue::Rational((func.f_f64)(*x, *y as f64));
        }
        if let LispValue::Rational(y) = b {
            return LispValue::Rational((func.f_f64)(*x, *y));
        }
        if let LispValue::BigInt(y) = &b {
            return LispValue::BigRational((func.f_bigrational)(
                &num::BigRational::from_f64(*x).unwrap(),
                &num::BigRational::from(y.clone()),
            ));
        }
        if let LispValue::BigRational(y) = &b {
            return LispValue::BigRational((func.f_bigrational)(
                &num::BigRational::from_f64(*x).unwrap(),
                y,
            ));
        }
    }
    if let LispValue::BigInt(x) = &a {
        if let LispValue::Integer(y) = b {
            return LispValue::BigRational((func.f_bigrational)(
                &BigRational::from(x.clone()),
                &BigRational::from_i64(*y).unwrap(),
            ));
        }
        if let LispValue::Rational(y) = b {
            return LispValue::BigRational((func.f_bigrational)(
                &BigRational::from(x.clone()),
                &BigRational::from_f64(*y).unwrap(),
            ));
        }
        if let LispValue::BigInt(y) = &b {
            return handler_underflow(LispValue::BigInt((func.f_bigint)(x, y)));
        }
        if let LispValue::BigRational(y) = &b {
            return LispValue::BigRational((func.f_bigrational)(
                &num::BigRational::from(x.clone()),
                y,
            ));
        }
    }

    if let LispValue::BigRational(x) = &a {
        if let LispValue::Integer(y) = b {
            return LispValue::BigRational((func.f_bigrational)(
                x,
                &BigRational::from_i64(*y).unwrap(),
            ));
        }
        if let LispValue::Rational(y) = b {
            return LispValue::BigRational((func.f_bigrational)(
                x,
                &BigRational::from_f64(*y).unwrap(),
            ));
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

fn lisp_apply_numbers(v: &[LispValue], func: &NumericFunc) -> LispValue {
    let mut it = v.iter();
    if let Some(firstv) = it.next() {
        if let Some(nextv) = it.next() {
            let mut acc = apply_number_func(func, firstv, nextv);
            while let Some(itv) = it.next() {
                acc = apply_number_func(func, &acc, itv);
            }
            return acc;
        } else {
            return firstv.clone();
        }
    }
    LispValue::Nil
}

fn lisp_add(v: &[LispValue]) -> LispValue {
    lisp_apply_numbers(&v, &ADD_OP)
}

fn lisp_sub(v: &[LispValue]) -> LispValue {
    lisp_apply_numbers(&v, &SUB_OP)
}

fn lisp_mul(v: &[LispValue]) -> LispValue {
    lisp_apply_numbers(&v, &MUL_OP)
}

fn lisp_div(v: &[LispValue]) -> LispValue {
    lisp_apply_numbers(&v, &DIV_OP)
}

fn lisp_bigrational(v: LispValue) -> LispValue {
    match v {
        LispValue::Integer(i) => LispValue::BigRational(num::BigRational::from_i64(i).unwrap()),
        LispValue::Rational(r) => LispValue::BigRational(num::BigRational::from_f64(r).unwrap()),
        LispValue::BigRational(_) => v,
        LispValue::BigInt(i) => LispValue::BigRational(num::BigRational::from(i)),
        _ => LispValue::Nil,
    }
}

fn lisp_decf(scope: &mut Stack, v: &LispValue) -> LispValue {
    let location = car(v);
    let amount = cadr(v);
    let mut amount2 = &LispValue::Integer(1);
    if !is_nil(amount) { 
        amount2 = amount;
    }
    
    if let LispValue::Symbol(loc) = location {
        let v = scope.get_value(*loc);
        if let Some(r) = v {
            let args = [r.clone(), amount2.clone()];
            let result = lisp_sub(&args);
            scope.set_value(*loc, &result);
            return result;
        }
    }
    
    lisp_raise_error(scope, "error".into());
    
    return LispValue::Nil;
}


pub fn lisp_math_load(ctx: &mut LispContext) {
    ctx.set_global_str("+", LispValue::from_nr(lisp_add));
    ctx.set_global_str("-", LispValue::from_nr(lisp_sub));
    ctx.set_global_str("*", LispValue::from_nr(lisp_mul));
    ctx.set_global_str("/", LispValue::from_nr(lisp_div));
    ctx.set_global_str("big-rational", LispValue::from_1(lisp_bigrational));
    ctx.set_global_str("decf", LispValue::from_macro(lisp_decf));

}
