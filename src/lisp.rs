use std::default;

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
pub fn lisp_not(a: &LispValue) -> &LispValue {
    if eq(a, &LispValue::Nil) {
        return &LispValue::Integer(1);
    }
    return &LispValue::Nil;
}

pub fn car(lisp: &LispValue) -> &LispValue {
    if let LispValue::Cons(l) = lisp {
        return &l.0;
    }
    if let LispValue::Consr(l) = lisp {
        return &l.0;
    }
    return &LispValue::Nil;
}

pub fn cdr(lisp: &LispValue) -> &LispValue {
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

fn lisp_print(v: LispValue) -> LispValue {
    println!("{}", v);
    return v;
}

fn lisp_conss(v: Vec<LispValue>) -> LispValue {
    let mut v0 = LispValue::Nil;
    for i in v.iter().rev() {
        v0 = LispValue::Cons(Box::new((i.clone(), v0)));
    }
    return v0;
}

fn lisp_cons(a: LispValue, b: LispValue) -> LispValue {
    LispValue::Cons(Box::new((a, b)))
}

pub fn is_nil(a: &LispValue) -> bool {
    match a {
        &LispValue::Nil => true,
        _ => false
    }
}

pub fn is_cons(a: &LispValue) -> bool {
    match a {
        LispValue::Cons(_) => true,
        LispValue::Consr(_) => true,
        _ => false,
    }
}

fn lisp_loop(ctx: &mut Stack, body: &LispValue) -> LispValue {
    let cond = car(body);
    let body = cdr(body);
    let mut result = LispValue::Nil;
    while !is_nil(&lisp_eval(ctx, cond)) {
        let mut it = body;
        while !is_nil(it) {
            result = lisp_eval(ctx, car(it));
            it = cdr(it);
        }
    }
    return result;
}

fn lisp_let_n<const N: usize>(ctx: &mut Stack, args: &LispValue, body: &LispValue) -> LispValue {
    let mut arga = [0; N].map(|_| LispValue::Nil);
    let mut ids: [i32; N] = [0; N];
    let mut argit = args;
    for i in 0..N {
        let arg = car(argit);
        if let LispValue::Symbol(id) = car(arg) {
            ids[i] = *id;
            let v = lisp_eval(ctx, cadr(arg));
            arga[i] = v;
        }
        argit = cdr(argit);
    }
    let scope = LispScope {
        id: &ids,
        values: &mut arga,
        parent: &ctx.local_scope,
    };

    let mut it = body;
    let mut result = LispValue::Nil;

    let mut stack2 = Stack {
        global_scope: ctx.global_scope,
        local_scope: Some(Box::new(scope)),
    };
    while !is_nil(it) {
        result = lisp_eval(&mut stack2, car(it));
        it = cdr(it);
    }
    return result;
}

fn lisp_let(ctx: &mut Stack, body: &LispValue) -> LispValue {
    let let_vars = car(body);
    let let_body = cdr(body);
    let cnt = cons_count(let_vars);
    match cnt {
        0 => return lisp_let_n::<1>(ctx, let_vars, let_body),
        1 => return lisp_let_n::<1>(ctx, let_vars, let_body),
        2 => return lisp_let_n::<2>(ctx, let_vars, let_body),
        3 => return lisp_let_n::<3>(ctx, let_vars, let_body),
        4 => return lisp_let_n::<4>(ctx, let_vars, let_body),
        5 => return lisp_let_n::<5>(ctx, let_vars, let_body),
        6 => return lisp_let_n::<6>(ctx, let_vars, let_body),
        7 => return lisp_let_n::<7>(ctx, let_vars, let_body),
        _ => todo!(),
    }
}

fn lisp_set(ctx: &mut Stack, body: &LispValue) -> LispValue {
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

fn lisp_if(ctx: &mut Stack, body: &LispValue) -> LispValue {
    let cond_clause = car(body);
    let if_clause = cadr(body);
    let else_clause = caddr(body);
    let val = lisp_eval(ctx, cond_clause);
    if is_nil(&val) {
        return lisp_eval(ctx, if_clause);
    }
    return lisp_eval(ctx, else_clause);
}

fn lisp_defun(ctx: &mut Stack, body: &LispValue) -> LispValue{
    let name = car(body);
    let args = cadr(body);
    let code = cddr(body);
    let mut arg_names = Vec::new();
    let mut it = args;
    while !is_nil(it) {
        if let LispValue::Symbol(id) = car(it) {
            arg_names.push(*id);
        }else{
            panic!("error");
        }
        it = cdr(it);
    }
    
    let f = LispFunc {code: Box::new(code.clone()), args_names: arg_names};
    if let LispValue::Symbol(name) = name{
        
        ctx.global_scope.set_global(*name, LispValue::LispFunction(Arc::new(f)));    
    }else{
        panic!("Not a symbol");
    }
    return LispValue::Nil;
    
}

pub fn lisp_load_lisp(ctx: &mut LispContext) {
    ctx.set_global_str(
        "println",
        LispValue::NativeFunction(NativeFunc::Function1(lisp_print)),
    );
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
    ctx.set_global_str("if", LispValue::from_macro(lisp_if));
    ctx.set_global_str("defun", LispValue::from_macro(lisp_defun));

}
