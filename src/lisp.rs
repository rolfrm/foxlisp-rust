use crate::*;
pub fn caddr(lisp: &LispValue) -> &LispValue {
    return car(cddr(lisp));
}

pub fn car(lisp: &LispValue) -> &LispValue{
    if let LispValue::Cons(l) = lisp {
        return &l.0;
    }
    return &LispValue::Nil;
}

pub fn car2(lisp: LispValue) -> LispValue{
    if let LispValue::Cons(l) = lisp {
        return l.0;
    }
    return LispValue::Nil;
}

pub fn cdr(lisp: &LispValue) -> &LispValue{
    if let LispValue::Cons(l) = lisp {
        return &l.1;
    }
    return &LispValue::Nil;
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

fn lisp_conss(v: Vec<LispValue>) -> LispValue {
    let mut v0 = LispValue::Nil;
    for i in v.iter().rev() {
        v0 = LispValue::Cons(Box::new((i.clone(), v0)));
    }
    return v0;
}

fn lisp_cons(a: LispValue, b: LispValue) -> LispValue{
    println!("CONS: {} {}", a, b);
    LispValue::Cons(Box::new((a, b)))
}

pub fn lisp_load_lisp(ctx: &mut LispContext){
    ctx.set_global_str("cons", LispValue::from_2(lisp_cons));
    //ctx.set_global_str("list", LispValue::from(lisp_conss));
    ctx.set_global_str("car", LispValue::from_1r(car));
    ctx.set_global_str("cdr", LispValue::from_1r(cdr));
    ctx.set_global_str("cadr", LispValue::from_1r(cadr));
    ctx.set_global_str("cddr", LispValue::from_1r(cddr));
    ctx.set_global_str("cdddr", LispValue::from_1r(cdddr));
    ctx.set_global_str("cddddr", LispValue::from_1r(cddddr));
    
}
