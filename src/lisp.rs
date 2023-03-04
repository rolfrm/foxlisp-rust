use crate::*;
pub fn caddr(lisp: &LispValue) -> &LispValue {
    return car(cddr(lisp));
}

pub fn lisp_eq<'a>(a: &'a LispValue, b: &'a LispValue) -> &'a LispValue {
    if eq(a, b) {
        return &LispValue::Integer(1);
    }
    return &LispValue::Nil;
}
pub fn lisp_not(a: &LispValue) -> & LispValue {
    if eq(a, &LispValue::Nil) {
        return &LispValue::Integer(1);
    }
    return &LispValue::Nil;
}

pub fn car(lisp: &LispValue) -> &LispValue{
    if let LispValue::Cons(l) = lisp {
        return &l.0;
    }
    if let LispValue::Consr(l) = lisp {
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
    if let LispValue::Consr(l) = lisp {
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
    LispValue::Cons(Box::new((a, b)))
}

pub fn is_nil(a: &LispValue) -> bool {
    return eq(a, &LispValue::Nil);
}

fn lisp_loop(ctx: &mut dyn Scope, body: &LispValue) -> LispValue {
    let cond = car(body);
    let body = cdr(body);
    while !is_nil(&lisp_eval(ctx, cond)){
        let mut it = body;
        while !is_nil(it) {
            lisp_eval(ctx, car(it));
            it = cdr(it);
        }
    }
    return LispValue::Nil;
}

fn lisp_let1(ctx: &mut dyn Scope, args: &LispValue, body: &LispValue) -> LispValue{
    let arg = car(args);
    let mut arga = [LispValue::Nil];
    let mut ids = [0];
    if let LispValue::Symbol(id) = car(arg) {
        ids[0] = *id;
        let v = lisp_eval(ctx, cadr(arg));
        arga[0] = v;
        let mut scope = LispScope { id: &ids, values: &mut arga, parent: ctx };
        let mut it = body;
        let mut result = LispValue::Nil;
        
        while !is_nil(it) {
            result = lisp_eval(&mut scope, car(it));
            it = cdr(it);
        }
        return result;
    }
    return LispValue::Nil;
}

fn lisp_let(ctx: &mut dyn Scope, body: &LispValue) -> LispValue {
    let let_vars = car(body);
    let let_body = cdr(body);
    let mut result = LispValue::Nil;
    if cons_count(let_vars) == 1 {    
        return lisp_let1(ctx, let_vars, let_body);
    }
    let values : Vec<(i64, &LispValue)> = Vec::new();
    // fill the values
    
    return result;
}

fn lisp_set(ctx: &mut dyn Scope, body: &LispValue) -> LispValue {
    let sym = car(body);
    let val = cadr(body);
    if let LispValue::Symbol(s) = sym {
        let val = lisp_eval(ctx, val);
        ctx.set_value(*s, &val);
        return val;
    }
    lisp_raise_error(ctx, LispValue::Integer(5));
    return LispValue::Nil;
    
}


pub fn lisp_load_lisp(ctx: &mut LispContext){
    ctx.set_global_str("cons", LispValue::from_2(lisp_cons));
    ctx.set_global_str("car", LispValue::from_1r(car));
    ctx.set_global_str("cdr", LispValue::from_1r(cdr));
    ctx.set_global_str("cadr", LispValue::from_1r(cadr));
    ctx.set_global_str("cddr", LispValue::from_1r(cddr));
    ctx.set_global_str("caddr", LispValue::from_1r(caddr));
    ctx.set_global_str("cdddr", LispValue::from_1r(cdddr));
    ctx.set_global_str("cddddr", LispValue::from_1r(cddddr));
    ctx.set_global_str("eq", LispValue::from_2r(lisp_eq));
    ctx.set_global_str("not", LispValue::from_1r(lisp_not));
    ctx.set_global_str("list", LispValue::from_n(lisp_conss));
    ctx.set_global_str("loop", LispValue::from_macro(lisp_loop));
    ctx.set_global_str("let", LispValue::from_macro(lisp_let));
    ctx.set_global_str("set!", LispValue::from_macro(lisp_set));
}
