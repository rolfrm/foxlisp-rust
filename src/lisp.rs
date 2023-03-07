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

pub fn lisp_equals<'a>(a: &'a LispValue, b: &'a LispValue) -> &'a LispValue {
    if a.equals(b) {
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
        LispValue::Cons(c) => &c.0,
        LispValue::Consr(c) => &c.0,
        _ => &LispValue::Nil,
    }
}

pub fn cdr(lisp: &LispValue) -> &LispValue {
    match lisp {
        LispValue::Cons(c) => &c.1,
        LispValue::Consr(c) => &c.1,
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
        _ => false,
    }
}

pub fn is_cons(a: &LispValue) -> bool {
    match a {
        LispValue::Consr(_)| LispValue::Cons(_) => true,
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

fn lisp_progn(ctx: &mut Stack, body: &LispValue) -> LispValue {
    let mut it = body;
    let mut result = LispValue::Nil;

    while !is_nil(it) {
        result = lisp_eval(ctx, car(it));
        if ctx.error.is_some() {
            return LispValue::Nil;
        }
        it = cdr(it);
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

    let mut stack2 = Stack::new_local(ctx.global_scope, scope);

    while !is_nil(it) {
        result = lisp_eval(&mut stack2, car(it));
        it = cdr(it);
    }
    ctx.error = stack2.error;
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
        return lisp_eval(ctx, else_clause);
    } else {
        return lisp_eval(ctx, if_clause);
    }
}

fn lisp_defun(ctx: &mut Stack, body: &LispValue) -> LispValue {
    let name = car(body);
    let args = cadr(body);
    let code = cddr(body);
    let mut arg_names = Vec::new();
    let mut it = args;
    let mut variadic = false;
    while !is_nil(it) {
        if let LispValue::Symbol(id) = car(it) {
            arg_names.push(*id);
        } else if let LispValue::Rest = car(it) {
            it = cdr(it);
             if let LispValue::Symbol(restarg) = car(it) {
                arg_names.push(*restarg);
             }else{
                lisp_raise_error(ctx, "an argument name must follow &rest.".into());
                return LispValue::Nil;     
             }
             variadic = true;
             break;
        }  
        else {
            lisp_raise_error(ctx, "unsupported defun argument.".into());
            return LispValue::Nil;
        }
        it = cdr(it);
    }

    let f = LispFunc {
        code: Box::new(code.clone()),
        args_names: arg_names,
        magic: false,
        variadic: variadic
    };
    if let LispValue::Symbol(name) = name {
        ctx.global_scope
            .set_global(*name, LispValue::LispFunction(Arc::new(f)));
    } else {
        panic!("Not a symbol");
    }
    return LispValue::Nil;
}

fn lisp_defun_magic(ctx: &mut Stack, body: &LispValue) -> LispValue {
    let name = car(body);
    let args = cadr(body);
    let code = cddr(body);
    let mut arg_names = Vec::new();
    let mut it = args;
    while !is_nil(it) {
        if let LispValue::Symbol(id) = car(it) {
            arg_names.push(*id);
        } else {
            panic!("error");
        }
        it = cdr(it);
    }

    let f = LispFunc {
        code: Box::new(code.clone()),
        args_names: arg_names,
        magic: true,
        variadic: true
    };
    if let LispValue::Symbol(name) = name {
        ctx.global_scope
            .set_global(*name, LispValue::LispFunction(Arc::new(f)));
    } else {
        panic!("Not a symbol");
    }
    return LispValue::Nil;
}

fn lisp_defvar(ctx: &mut Stack, body: &LispValue) -> LispValue {
    let name = car(body);
    let args = cadr(body);
    let value = lisp_eval(ctx, args);
    if let LispValue::Symbol(id) = name {
        ctx.global_scope.set_global(*id, value);
    }
    LispValue::Nil
}

fn lisp_quote(ctx: &mut Stack, body: &LispValue) -> LispValue {
    if !is_nil(cdr(body)){
        lisp_raise_error(ctx, "quote may only take one argument!".into());
        return LispValue::Nil;
        
    }
    return car(body).clone();
}

fn lisp_raise(ctx: &mut Stack, body: &LispValue) -> LispValue {
    let error = car(body);
    let err_val = lisp_eval(ctx, error);
    lisp_raise_error(ctx, err_val);
    return LispValue::Nil;
}

fn lisp_load(ctx: &mut Stack, body: &LispValue) -> LispValue {
    let path = car(body);
    if let LispValue::String(path) = path {
        lisp_eval_file(ctx, path);
    } else {
        lisp_raise_error(ctx, LispValue::from("not a file"));
    }
    return LispValue::Nil;
}

pub fn lisp_eval_value(ctx: &mut Stack, body: &LispValue) -> LispValue {
    lisp_eval(ctx, car(body))
}

fn lisp_gt<'a>(a: &'a LispValue,b: &'a LispValue) -> &'a LispValue{
    if let Some(x) = a.partial_cmp(b) {
        if x.is_gt() {
            return &LispValue::Integer(1);
        }
    }
    return &LispValue::Nil;
}

fn lisp_lt<'a>(a: &'a LispValue,b: &'a LispValue) -> &'a LispValue{
    if let Some(x) = a.partial_cmp(b) {
        if x.is_lt() {
            return &LispValue::Integer(1);
        }
    }
    return &LispValue::Nil;
}

fn lisp_handle_error(ctx: &mut Stack, body: &LispValue) -> LispValue{
    let main = car(body);
    let handler = cadr(body);
    let r = lisp_eval(ctx, main);
    if let Some(err) = ctx.error.clone() {
        let handler_name = car(handler);
        let handler_body = cdr(handler);
        if let LispValue::Symbol(id) = handler_name {
            let local = LispScope {
                id: &[*id],
                values: &mut [err],
            parent: &ctx.local_scope,
        };
        ctx.error = None;
        let mut stack = Stack::new_local(ctx.global_scope, local);
        return lisp_progn(&mut stack, handler_body);
        }else{
            lisp_raise_error(ctx, "First handler argument must be a symbol".into());
        }
    }
    return r;
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
    ctx.set_global_str("<", LispValue::from_2r(lisp_lt));
    ctx.set_global_str(">", LispValue::from_2r(lisp_gt));
    ctx.set_global_str("equals", LispValue::from_2r(lisp_equals));
    ctx.set_global_str("not", LispValue::from_1r(lisp_not));
    ctx.set_global_str("list", LispValue::from_n(lisp_conss));
    ctx.set_global_str("loop", LispValue::from_macro(lisp_loop));
    ctx.set_global_str("let", LispValue::from_macro(lisp_let));
    ctx.set_global_str("set!", LispValue::from_macro(lisp_set));
    ctx.set_global_str("if", LispValue::from_macro(lisp_if));
    ctx.set_global_str("defun", LispValue::from_macro(lisp_defun));
    ctx.set_global_str("defun:magic", LispValue::from_macro(lisp_defun_magic));
    ctx.set_global_str("defvar", LispValue::from_macro(lisp_defvar));
    ctx.set_global_str("quote", LispValue::from_macro(lisp_quote));
    ctx.set_global_str("raise", LispValue::from_macro(lisp_raise));
    ctx.set_global_str("load", LispValue::from_macro(lisp_load));
    ctx.set_global_str("progn", LispValue::from_macro(lisp_progn));
    ctx.set_global_str("eval", LispValue::from_macro(lisp_eval_value));
    ctx.set_global_str("handle-error", LispValue::from_macro(lisp_handle_error));
    
    let mut stack = Stack::new_root(ctx);

    stack.eval("(defun assert (cond) (if cond 1 (raise (quote (assert failed)))))");
}
