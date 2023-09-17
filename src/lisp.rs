use crate::*;

pub fn caddr(lisp: &LispValue) -> &LispValue {
    return car(cddr(lisp));
}

pub fn lisp_eq(a: &[LispValue]) -> &LispValue {
    if eq(&a[0], &a[1]) {
        return &LispValue::Integer(1);
    }
    return &LispValue::Nil;
}

pub fn lisp_neq(a: &[LispValue]) -> &LispValue {
    if !eq(&a[0], &a[1]) {
        return &LispValue::Integer(1);
    }
    return &LispValue::Nil;
}
pub fn lisp_equals(a: &[LispValue]) -> &LispValue {
    if a[0].equals(&a[1]) {
        return &LispValue::Integer(1);
    }
    return &LispValue::Nil;
}

pub fn lisp_not(a: &LispValue) -> &LispValue {
    if eq(a, &LispValue::Nil) {
        return &LispValue::Integer(1);
    }
    return &LispValue::Nil;
}

pub fn car(lisp: &LispValue) -> &LispValue {
    match lisp {
        LispValue::Cons(a) => &a.0,
        _ => &LispValue::Nil,
    }
}

pub fn cdr(lisp: &LispValue) -> &LispValue {
    match lisp {
        LispValue::Cons(a) => &a.1,
        _ => &LispValue::Nil,
    }
}
pub fn cadr(lisp: &LispValue) -> &LispValue {
    return car(cdr(lisp));
}
pub fn cddr(lisp: &LispValue) -> &LispValue {
    return cdr(cdr(lisp));
}

pub fn cdddr(lisp: &LispValue) -> &LispValue {
    return cdr(cddr(lisp));
}

pub fn cddddr(lisp: &LispValue) -> &LispValue {
    return cdr(cdddr(lisp));
}

pub fn cadddr(lisp: &LispValue) -> &LispValue {
    return car(cdddr(lisp));
}

fn lisp_print(v: &[LispValue]) -> LispValue {
    for x in v {
        print!("{} ", x);
    }
    println!("");
    if v.len() == 0 {
        return LispValue::Nil;
    }
    return v[0].clone();
}

fn lisp_conss(v: &[LispValue]) -> LispValue {
    let mut v0 = LispValue::Nil;
    for i in v.iter().rev() {
        v0 = LispValue::cons(i.clone(), v0);
    }
    return v0;
}

pub fn lisp_cons(a: LispValue, b: LispValue) -> LispValue {
    LispValue::cons(a, b)
}

#[macro_export]
macro_rules! list {
        // Case for when there are no more elements
        () => {
            Nil
        };
        // Case for one or more elements
        ($head:expr, $($tail:expr),+) => {
            lisp_cons($head.to_lisp(), list!($($tail),*))
        };
        // Case for a single element
        ($elem:expr) => {
            lisp_cons($elem.to_lisp(), LispValue::Nil)
        };
}

pub fn is_nil(a: &LispValue) -> bool {
    match a {
        &LispValue::Nil => true,
        _ => false,
    }
}

pub fn is_cons(a: &LispValue) -> bool {
    match a {
        LispValue::Cons(_) => true,
        _ => false,
    }
}

pub fn lisp_reverse(v: LispValue) -> LispValue {
    let mut out = Vec::new();
    let mut v2 = v;
    while v2.is_nil() == false {
        out.push(car(&v2).clone());
        v2 = cdr(&v2).clone();
    }

    for x in out {
        v2 = lisp_cons(x.clone(), v2);
    }
    return v2;
}

fn lisp_gt(a: &[LispValue]) -> &LispValue {
    if let Some(x) = a[0].partial_cmp(&a[1]) {
        if x.is_gt() {
            return &LispValue::T;
        }
    }
    return &LispValue::Nil;
}

fn lisp_lt(a: &[LispValue]) -> &LispValue {
    if let Some(x) = a[0].partial_cmp(&a[1]) {
        if x.is_lt() {
            return &LispValue::T;
        }
    }
    return &LispValue::Nil;
}

fn lisp_type_of(ctx: &mut Lisp, a: &[LispValue]) -> LispValue {
    match a[0] {
        LispValue::Integer(_) => ctx.get_symbol("I64"),
        LispValue::Rational(_) => ctx.get_symbol("F64"),
        LispValue::BigInt(_) => ctx.get_symbol("BigInteger"),
        LispValue::BigRational(_) => ctx.get_symbol("BigRational"),
        LispValue::T => ctx.get_symbol("T"),
        LispValue::String(_) => ctx.get_symbol("String"),
        LispValue::Nil => LispValue::Nil,
        _ => panic!("Unknown type!"),
    }
}

pub fn lisp_raise(ctx: &mut Lisp, error: &[LispValue]) -> LispValue {
    if ctx.panic_on_error {
        panic!("panic: {}", error[0]);
    }
    ctx.current_error = error[0].clone();
    return LispValue::Nil;
}

pub fn lisp_load_lisp(ctx: &mut Lisp) {
    ctx.set_global_str(
        "println",
        LispValue::NativeFunction(NativeFunc::FunctionN(lisp_print)),
    );
    ctx.set_global_str("cons", LispValue::from_2(lisp_cons));
    ctx.set_global_str("car", LispValue::from_1r(car));
    ctx.set_global_str("cdr", LispValue::from_1r(cdr));
    ctx.set_global_str("cadr", LispValue::from_1r(cadr));
    ctx.set_global_str("cddr", LispValue::from_1r(cddr));
    ctx.set_global_str("caddr", LispValue::from_1r(caddr));
    ctx.set_global_str("cdddr", LispValue::from_1r(cdddr));
    ctx.set_global_str("cadddr", LispValue::from_1r(cadddr));
    ctx.set_global_str("cddddr", LispValue::from_1r(cddddr));
    ctx.set_global_str("eq", LispValue::from_2r(lisp_eq));
    ctx.set_global_str("neq", LispValue::from_2r(lisp_neq));
    ctx.set_global_str("<", LispValue::from_2r(lisp_lt));
    ctx.set_global_str(">", LispValue::from_2r(lisp_gt));
    ctx.set_global_str("equals", LispValue::from_2r(lisp_equals));
    ctx.set_global_str("not", LispValue::from_1r(lisp_not));
    ctx.set_global_str("list", LispValue::from_n(lisp_conss));
    ctx.set_global_str("type-of", LispValue::from_n_macrolike(lisp_type_of));
    ctx.set_global_str("raise", LispValue::from_n_macrolike(lisp_raise));

    ctx.eval_str("(defun assert (cond) (if cond 1 (raise '(assert failed))))");

    ctx.eval_str("(let ((values '(123.0 123 \"aaa\"))) (loop values (set! values (cdr values)) (println (type-of (car values)))))");
    ctx.eval_str("(let ((values '(BigInteger BigRational))))");
}

#[cfg(test)]
mod test {

    use crate::*;

    #[test]
    fn type_of_test() {
        let mut ctx = Lisp::new();

        let tests = [
            ("(type-of 123)", "I64"),
            ("(type-of 123.0)", "F64"),
            ("(type-of \"asd\")", "String"),
            ("(type-of 123)", "I64"),
        ];
        for x in tests {
            let result = ctx.eval_str(x.0);
            assert_eq!(result, ctx.get_symbol(x.1));
        }
    }
}
