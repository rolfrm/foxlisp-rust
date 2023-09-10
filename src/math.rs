use std::{f64::consts::PI, rc::Rc, ops::Neg};

use crate::{cadr, car, is_nil, LispContext, LispValue};
use num::{
    self, integer::Roots, rational::Ratio, traits::AsPrimitive, BigInt, BigRational, FromPrimitive,
    ToPrimitive,
};

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

static POW_OP: NumericFunc = NumericFunc {
    f_f64: |x, y| f64::powf(x, y),
    f_int: |x, y| x.checked_pow(y.as_()),
    f_bigint: |x, y| x.pow(y.to_u32().unwrap()),
    f_bigrational: |x, y| x.pow(y.to_i32().unwrap()),
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
            return LispValue::BigInt(Rc::new((func.f_bigint)(
                &BigInt::from_i64(*x).unwrap(),
                &BigInt::from_i64(*y).unwrap(),
            )));
        }
        if let LispValue::Rational(y) = b {
            return LispValue::Rational((func.f_f64)(*x as f64, *y));
        }
        if let LispValue::BigInt(y) = &b {
            return handler_underflow(LispValue::BigInt(Rc::new((func.f_bigint)(
                &num::BigInt::from(*x),
                y,
            ))));
        }
        if let LispValue::BigRational(y) = &b {
            return LispValue::BigRational(Rc::new((func.f_bigrational)(
                &num::BigRational::from_i64(*x).unwrap(),
                y,
            )));
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
            return LispValue::BigRational(Rc::new((func.f_bigrational)(
                &num::BigRational::from_f64(*x).unwrap(),
                &num::BigRational::from(y.as_ref().clone()),
            )));
        }
        if let LispValue::BigRational(y) = &b {
            return LispValue::BigRational(Rc::new((func.f_bigrational)(
                &num::BigRational::from_f64(*x).unwrap(),
                y,
            )));
        }
    }
    if let LispValue::BigInt(x) = &a {
        if let LispValue::Integer(y) = b {
            return LispValue::BigRational(Rc::new((func.f_bigrational)(
                &BigRational::from(x.as_ref().clone()),
                &BigRational::from_i64(*y).unwrap(),
            )));
        }
        if let LispValue::Rational(y) = b {
            return LispValue::BigRational(Rc::new((func.f_bigrational)(
                &BigRational::from(x.as_ref().clone()),
                &BigRational::from_f64(*y).unwrap(),
            )));
        }
        if let LispValue::BigInt(y) = &b {
            return handler_underflow(LispValue::BigInt(Rc::new((func.f_bigint)(
                x.as_ref(),
                y.as_ref(),
            ))));
        }
        if let LispValue::BigRational(y) = &b {
            return LispValue::BigRational(Rc::new((func.f_bigrational)(
                &num::BigRational::from(x.as_ref().clone()),
                y,
            )));
        }
    }

    if let LispValue::BigRational(x) = &a {
        if let LispValue::Integer(y) = b {
            return LispValue::BigRational(Rc::new((func.f_bigrational)(
                x,
                &BigRational::from_i64(*y).unwrap(),
            )));
        }
        if let LispValue::Rational(y) = b {
            return LispValue::BigRational(Rc::new((func.f_bigrational)(
                x,
                &BigRational::from_f64(*y).unwrap(),
            )));
        }
        if let LispValue::BigInt(y) = &b {
            return LispValue::BigRational(Rc::new((func.f_bigrational)(
                x,
                &BigRational::from(y.as_ref().clone()),
            )));
        }
        if let LispValue::BigRational(y) = &b {
            return LispValue::BigRational(Rc::new((func.f_bigrational)(x, y)));
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

pub fn lisp_add(v: &[LispValue]) -> LispValue {
    lisp_apply_numbers(&v, &ADD_OP)
}

pub fn lisp_sub(v: &[LispValue]) -> LispValue {
    if v.len() == 1 {
        match &v[0] {
            LispValue::BigInt(v) => LispValue::BigInt(Rc::new(v.as_ref().neg())),
            LispValue::BigRational(v) => LispValue::BigRational(Rc::new(v.as_ref().neg())),
            LispValue::Integer(v) => LispValue::Integer(v.neg()),
            LispValue::Rational(v) => LispValue::Rational(v.neg()),
            _ => LispValue::Nil
            
        }
    }else{
        lisp_apply_numbers(&v, &SUB_OP)
    }
}

pub fn lisp_mul(v: &[LispValue]) -> LispValue {
    lisp_apply_numbers(&v, &MUL_OP)
}

pub fn lisp_div(v: &[LispValue]) -> LispValue {
    lisp_apply_numbers(&v, &DIV_OP)
}

pub fn lisp_pow(v: &[LispValue]) -> LispValue {
    lisp_apply_numbers(&v, &POW_OP)
}
fn lisp_sqrt(v: LispValue) -> LispValue {
    match v {
        LispValue::Integer(i) => LispValue::Integer(i.sqrt()),
        LispValue::Rational(r) => LispValue::Rational(r.sqrt()),
        LispValue::BigRational(br) => {
            let half = Ratio::new(BigInt::from(1), BigInt::from(2));
            let r = br.as_ref().clone();
            let mut x = r.clone();
            for _ in 0..10 {
                let xn = (&x + &r / &x) * &half;
                x = xn;
            }

            return LispValue::BigRational(Rc::new(x));
        }
        LispValue::BigInt(i) => LispValue::BigInt(Rc::new(i.sqrt())),
        _ => LispValue::Nil,
    }
}
fn lisp_bigrational(v: LispValue) -> LispValue {
    match v {
        LispValue::Integer(i) => {
            LispValue::BigRational(Rc::new(num::BigRational::from_i64(i).unwrap()))
        }
        LispValue::Rational(r) => {
            LispValue::BigRational(Rc::new(num::BigRational::from_f64(r).unwrap()))
        }
        LispValue::BigRational(_) => v,
        LispValue::BigInt(i) => {
            LispValue::BigRational(Rc::new(num::BigRational::from(i.as_ref().clone())))
        }
        _ => LispValue::Nil,
    }
}
fn lisp_float(v: LispValue) -> LispValue {
    match v {
        LispValue::Integer(i) => LispValue::Rational(i as f64),
        LispValue::Rational(r) => LispValue::Rational(r),
        LispValue::BigRational(r) => LispValue::Rational(r.to_f64().unwrap()),
        LispValue::BigInt(i) => LispValue::Rational(i.to_f64().unwrap()),
        _ => LispValue::Nil,
    }
}

pub fn lisp_math_load(ctx: &mut LispContext) {
    ctx.set_global_str("+", LispValue::from_n(lisp_add));
    ctx.set_global_str("-", LispValue::from_n(lisp_sub));
    ctx.set_global_str("*", LispValue::from_n(lisp_mul));
    ctx.set_global_str("/", LispValue::from_n(lisp_div));
    ctx.set_global_str("pow", LispValue::from_n(lisp_pow));
    ctx.set_global_str("sqrt", LispValue::from_1(lisp_sqrt));

    ctx.set_global_str("big-rational", LispValue::from_1(lisp_bigrational));
    ctx.set_global_str("float", LispValue::from_1(lisp_float));
    
    ctx.set_global_str("pi", LispValue::Rational(PI));
}

#[cfg(test)]
mod test {

    use crate::*;

    #[test]
    fn test_basic_math() {
        let mut ctx = lisp_load_basic();
        ctx.panic_on_error = true;
        ctx.eval_str("(assert (eq (- 0 1) -1))");
        ctx.eval_str("(assert (eq (- 1) -1))");
        ctx.eval_str("(assert (eq (- 1 5) -4))");
        

    }
    
    
}
