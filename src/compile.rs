use crate::*;

#[derive(Debug)]
pub enum CompileError {
    InvalidValue,
    ArgumentError(LispValue),
    UnknownSymbol(LispValue),
}

pub fn lisp_compile(
    ctx: &mut LispContext,
    code: &LispValue,
    w: &mut CodeWriter,
) -> Result<(), CompileError> {
    match code {
        LispValue::Cons(a) => {
            if let LispValue::Symbol(s1) = a.0 {
                let name = &ctx.symbol_name_lookup[s1 as usize].clone();

                if name.eq("eval") {
                    for arg in a.1.to_iter().take(1) {
                        lisp_compile(ctx, arg, w)?;
                    }
                    w.emit(ByteCode::Eval);
                    return Ok(());
                }
                if name.eq("quote") {
                    let v = &a.1.clone();
                    let id = ctx.get_quote_store(car(v));

                    w.emit(ByteCode::LdQuote);
                    w.emit_uleb(id);
                    return Ok(());
                }

                if name.eq("set!") {
                    if let LispValue::Symbol(name) = car(&a.1) {
                        let value = cadr(&a.1);
                        lisp_compile(ctx, value, w)?;
                        w.emit(ByteCode::Dup);
                        w.emit(ByteCode::SetSym);
                        w.emit_uleb(name);
                        return Ok(());
                    }
                }

                if name.eq("defvar") {
                    let value = cadr(&a.1);
                    lisp_compile(ctx, value, w)?;
                    w.emit(ByteCode::DefVar);
                    let sym = car(&a.1);
                    if let LispValue::Symbol(symi) = sym {
                        w.emit_uleb(*symi);
                    } else {
                        return Err(CompileError::ArgumentError(code.clone()));
                    }
                    return Ok(());
                }
                if name.eq("defun") {
                    let name = car(&a.1).to_symbol_id();
                    if let Err(_) = name {
                        return Err(CompileError::ArgumentError(code.clone()));
                    }
                    let args = cadr(&a.1);
                    let body = cddr(&a.1);

                    let arg_symids: Result<Vec<i32>, String> =
                        args.to_iter().map(|x| Ok(x.to_symbol_id()?)).collect();
                    if let Err(_) = arg_symids {
                        return Err(CompileError::InvalidValue);
                    }
                    let arg_symids = arg_symids.unwrap();
                    let name_id = name.unwrap();
                    ctx.set_global(name_id, LispValue::Nil);
                    let mut w2 = CodeWriter::new();

                    let mut first = true;
                    for i in body.to_iter() {
                        if first {
                            first = false;
                        } else {
                            w2.emit(ByteCode::Drop);
                        }
                        lisp_compile(ctx, i, &mut w2)?;
                    }

                    if arg_symids.len() > 0 {
                        // after runnning the function, the arguments are still on the stack
                        // and just above the arguments are the return value.
                        // so the first arg is reused as storage for the return value
                        // and the rest are dropped before the function returns.
                        w2.emit(ByteCode::SetSym);
                        w2.emit_uleb(arg_symids[0]);
                        for _ in 1..(arg_symids.len()) {
                            w2.emit(ByteCode::Drop);
                        }
                    }

                    let f = LispFunc {
                        code: body.clone(),
                        args_names: arg_symids,
                        magic: false,
                        variadic: false,
                        compiled_code: w2.bytes,
                    };

                    ctx.set_global(name_id, LispValue::LispFunction(Rc::new(f)));
                    w.emit(ByteCode::LdSym);
                    w.emit_uleb(name_id);
                    return Ok(());
                }
                if name.eq("if") {
                    // fetch all the arguments
                    let check = car(&a.1);
                    let then = cadr(&a.1);
                    let _else = caddr(&a.1);

                    lisp_compile(ctx, check, w)?;
                    // insert the jump to the else clause
                    w.emit(ByteCode::IsNil);
                    w.emit(ByteCode::CondJmp);

                    // the next piece is a bit ugly - we need to calculate the jump sizes
                    // this first the code gets compiled and both clauses gets pushed into reserved vectors
                    // the the jumps before and after 'then' is inserted.
                    let offset1 = w.offset();
                    lisp_compile(ctx, then, w)?;
                    let offset2 = w.offset();
                    lisp_compile(ctx, _else, w)?;

                    let mut reserved2 = w.bytes.drain(offset2..).as_slice().to_vec();

                    w.emit(ByteCode::Jmp); // the jump over 'else' part
                    w.emit_sleb(reserved2.len() as i32);
                    let mut reserved1 = w.bytes.drain(offset1..).as_slice().to_vec();

                    // jump to the 'else' part
                    w.emit_sleb(reserved1.len() as i32);

                    // insert both caluese.
                    w.bytes.append(&mut reserved1);
                    w.bytes.append(&mut reserved2);

                    return Ok(());
                }

                if name.eq("loop") {
                    let offset1 = w.offset();
                    let check = car(&a.1);
                    lisp_compile(ctx, check, w)?;
                    w.emit(ByteCode::IsNil);
                    w.emit(ByteCode::CondJmp);
                    let check_code = w.bytes.drain(offset1..).as_slice().to_vec();

                    let body = cdr(&a.1);

                    for x in body.to_iter() {
                        lisp_compile(ctx, x, w)?;
                        w.emit(ByteCode::Drop);
                    }
                    w.emit(ByteCode::Jmp);
                    let body_code = w.bytes.drain(offset1..).as_slice().to_vec();
                    w.bytes.append(&mut check_code.clone());
                    let jump_from = w.offset();

                    w.emit_sleb(body_code.len() as i32);
                    w.bytes.append(&mut body_code.clone());
                    let offset2 = w.offset();
                    w.emit_sleb(-((offset2 - offset1) as i64));

                    // iterate again.
                    for _ in 0..4 {
                        // at this point, actually the a code has been built
                        // but we need to calculate the exact jump sizes.

                        // -1: I am not sure why the jump calculations wrong.
                        // maybe this is because the jmp itself is included in the code?
                        let jmp_finish = w.offset() - jump_from - 1;

                        w.bytes.truncate(offset1);

                        w.bytes.append(&mut check_code.clone());
                        w.emit_sleb(jmp_finish as i32);
                        w.bytes.append(&mut body_code.clone());
                        let offset2 = w.offset();
                        w.emit_sleb(-((offset2 - offset1) as i64));
                        let offset3 = w.offset();
                        w.bytes.truncate(offset2);
                        w.emit_sleb(-((offset3 - offset1) as i64));
                        let offset3 = w.offset();
                        w.bytes.truncate(offset2);
                        w.emit_sleb(-((offset3 - offset1) as i64));
                    }
                    w.emit(ByteCode::LdNil);

                    return Ok(());
                }

                if name.eq("let") {
                    let without_let = &a.1;
                    let args = car(&without_let);
                    let let_body = cdr(without_let);
                    let mut cnt = 0;
                    println!("let: {}", args);
                    for arg in args.to_iter() {
                        let body = cadr(arg);
                        lisp_compile(ctx, body, w)?;
                        cnt += 1;
                    }

                    w.emit(ByteCode::Let);
                    w.emit_uleb(cnt);
                    for arg in args.to_iter() {
                        if let Ok(symi) = car(arg).to_symbol_id() {
                            w.emit_uleb(symi);
                        } else {
                            return Err(CompileError::ArgumentError(arg.clone()));
                        }
                    }
                    let mut first = true;
                    for i in let_body.to_iter() {
                        if first {
                            first = false;
                        } else {
                            w.emit(ByteCode::Drop);
                        }
                        lisp_compile(ctx, i, w)?;
                    }
                    if let_body.is_nil() {
                        // the body is empty. Just load a NIL.
                        w.emit(ByteCode::LdNil);
                    }

                    if cnt > 0 {
                        let id1 = car(car(args)).to_symbol_id().unwrap();
                        w.emit(ByteCode::SetSym);
                        w.emit_uleb(id1);
                        for _ in 1..cnt {
                            w.emit(ByteCode::Drop);
                        }
                    }

                    w.emit(ByteCode::DropScope);
                    w.emit_uleb(cnt);
                    return Ok(());
                }

                let namecode = ctx.global_names.get(s1 as usize);

                if namecode.is_none() {
                    return Err(CompileError::UnknownSymbol(a.0.clone()));
                }

                let mut argcount: u32 = 0;
                for arg in a.1.to_iter() {
                    lisp_compile(ctx, arg, w)?;
                    argcount += 1;
                }

                if let LispValue::Symbol(name) = a.0 {
                    w.emit(ByteCode::Call);
                    w.emit_uleb(argcount as u128);
                    w.emit_uleb(name);
                    return Ok(());
                }
                return Err(CompileError::UnknownSymbol(code.clone()));
            } else {
                return Err(CompileError::UnknownSymbol(code.clone()));
            }
        }
        LispValue::Nil => {
            w.emit(ByteCode::LdNil);
            Ok(())
        }
        LispValue::Symbol(s) => {
            w.emit(ByteCode::LdSym);
            w.emit_uleb(s);
            Ok(())
        }
        LispValue::Integer(i) => {
            if *i < 128 && *i > -127 {
                w.emit(ByteCode::LdConstI1);
                w.emit_i8(*i as i8);
                return Ok(());
            }
            w.emit(ByteCode::LdConstI);
            w.emit_sleb(*i);
            Ok(())
        }
        LispValue::T => {
            w.emit(ByteCode::LdT);
            Ok(())
        }
        LispValue::String(_) => {
            let c = ctx.get_quote_store(code);
            w.emit(ByteCode::LdQuote);
            w.emit_uleb(c);
            Ok(())
        }
        LispValue::BigRational(_) => {
            let c = ctx.get_quote_store(code);
            w.emit(ByteCode::LdQuote);
            w.emit_uleb(c);
            Ok(())
        }
        LispValue::BigInt(_) => {
            let c = ctx.get_quote_store(code);
            w.emit(ByteCode::LdQuote);
            w.emit_uleb(c);
            Ok(())
        }
        LispValue::Rational(r) => {
            w.emit(ByteCode::LdConstR);
            w.emit_f64(r);
            Ok(())
        }
        _ => {
            panic!("Not implemented for {:?}", code);
            todo!();
        }
    }
}
pub fn lisp_bytecode_print_bytes(code: Vec<u8>, stk: &LispContext) {
    let code = CodeReader::new(code);
    lisp_bytecode_print(&code.bytes[0..], stk);
}

pub fn bytecode_to_lisp<T>(code: &mut T, _stk: &LispContext) -> LispValue
where
    T: CodeReader2,
{
    let mut value = LispValue::Nil;
    loop {
        if code.end() {
            return lisp_reverse(value);
        }

        let upcode = code.read_u8().to_bytecode();

        match upcode {
            ByteCode::LdNil => {
                value = lisp_cons(list!("LdNil"), value);
            }
            ByteCode::Let => {
                let cnt = read_uleb_u32(code) as i32;
                let mut cntvar = cnt.to_lisp();
                for _ in 0..cnt {
                    let s = read_uleb_u32(code) as i32;
                    cntvar = lisp_cons(s.to_symbol(), cntvar);
                }
                value = lisp_cons(lisp_cons("Let".to_lisp(), cntvar), value);
            }
            ByteCode::LdSym => {
                let symi = read_uleb_u32(code) as i32;
                value = lisp_cons(lisp_cons("LdSym".to_lisp(), symi.to_symbol()), value);
            }
            ByteCode::SetSym => {
                let symi = read_uleb_u32(code) as i32;
                value = lisp_cons(lisp_cons("SetSym".to_lisp(), symi.to_symbol()), value);
            }
            ByteCode::DefVar => {
                let symi = read_uleb_u32(code) as i32;
                value = lisp_cons(lisp_cons("DefVar".to_lisp(), symi.to_symbol()), value);
            }
            ByteCode::Call => {
                let _argcnt = read_uleb_u128(code) as usize;
                let symi: i32 = read_uleb_u32(code) as i32;
                value = lisp_cons(list!("Call", symi.to_symbol()), value);
            }
            ByteCode::LdConstI => {
                let integer = read_sleb64(code);

                value = lisp_cons(list!("LdConsti", integer), value);
            }
            ByteCode::IsNil => {
                value = lisp_cons(list!("IsNil"), value);
            }
            ByteCode::CondJmp => {
                let len = read_sleb64(code);
                value = lisp_cons(list!("CondJmp", len), value);
            }
            ByteCode::Jmp => {
                let len = read_sleb64(code);
                value = lisp_cons(list!("Jmp", len), value);
            }
            ByteCode::LdT => value = list!(list!("LdT"), value),
            ByteCode::Drop => value = lisp_cons(list!("Drop"), value),

            ByteCode::Defun => {
                panic!("This should not happen");
            }
            ByteCode::Return => todo!(),
            ByteCode::DropScope => {
                let n = read_sleb64(code);
                value = lisp_cons(list!("DropScope: {}", n), value);
            }
            ByteCode::Dup => value = lisp_cons(list!("Dup"), value),
            ByteCode::LdQuote => {
                let id = read_sleb64(code);
                value = lisp_cons(list!("LdQuote", id), value);
            }
            ByteCode::Eval => value = lisp_cons("Eval".to_lisp(), value),
            ByteCode::LdConstR => {
                let r = read_f64(code);
                value = lisp_cons(list!("LdConstR", r), value);
            }
            ByteCode::LdConstI1 => {
                let v = code.read_u8() as i8;

                value = list!("LdConstI1".to_lisp(), (v as i32).to_lisp()).as_car(value);
            }
        }
    }
}

pub fn lisp_bytecode_print(code: &[u8], stk: &LispContext) {
    let mut c = code;
    let mut r = bytecode_to_lisp(&mut c, stk);
    while r.is_nil() == false {
        println!("  {}", car(&r));
        r = cdr(&r).clone();
    }
}

pub fn lisp_eval_bytecode(stk: &mut LispContext) -> () {
    loop {
        if stk.reader_end() {
            // return from call.
            stk.current_scope.pop();
            if stk.current_scope.len() == 0 {
                return;
            }

            continue;
        }
        let upcode = stk.read_u8().to_bytecode();

        match upcode {
            ByteCode::LdNil => {
                stk.arg_stack.push(LispValue::Nil);
            }
            ByteCode::LdSym => {
                let symi = stk.read_uleb() as i32;
                let r = stk.get_value(symi).clone();

                stk.arg_stack.push(r.unwrap().clone());
            }
            ByteCode::SetSym => {
                let symi = stk.read_uleb() as i32;
                let value = stk.arg_stack.pop().unwrap();
                stk.set_value(symi, value);
            }
            ByteCode::DefVar => {
                let symi = stk.read_uleb() as i32;
                let value = stk.arg_stack.pop().unwrap().clone();
                stk.arg_stack.push(value.clone());
                stk.set_global(symi, value);
            }
            ByteCode::Let => {
                let stk_offset = stk.arg_stack.len();
                let cnt = stk.read_uleb() as u32;
                for i in 0..cnt {
                    let symi = stk.read_uleb() as i32;

                    stk.push_local_var(LetScope {
                        sym: symi,
                        argoffset: (stk_offset as u32 - cnt + i) as usize,
                    });
                }
            }
            ByteCode::Call => {
                let argcnt = stk.read_uleb() as usize;
                //println!("Call! ({}, {})", argcnt, stk.arg_stack.len());
                let symi = stk.read_uleb() as i32;
                let call = stk.get_value(symi).unwrap().clone();
                if let LispValue::NativeFunction(nf) = call {
                    let v = match nf {
                        NativeFunc::Function1(f1) => f1(stk.arg_stack.pop().unwrap()),
                        NativeFunc::Function2(f2) => {
                            let a1 = stk.arg_stack.pop().unwrap();
                            let a2 = stk.arg_stack.pop().unwrap();
                            f2(a1, a2)
                        }
                        NativeFunc::Function1r(f1) => {
                            let l = stk.arg_stack.len();
                            let r = f1(&stk.arg_stack[l - 1]).clone();
                            stk.arg_stack.truncate(l - 1);
                            r
                        }
                        NativeFunc::Function2r(f2) => {
                            let l = stk.arg_stack.len();
                            let r = f2(&stk.arg_stack[l - 2..]).clone();
                            stk.arg_stack.truncate(l - 2);
                            r
                        }
                        NativeFunc::FunctionN(fr) => {
                            let newlen = stk.arg_stack.len() - argcnt;
                            let r = fr(&stk.arg_stack[newlen..]);
                            stk.arg_stack.truncate(newlen);
                            r
                        }
                        NativeFunc::FunctionMacroLike(fr) => {
                            let newlen = stk.arg_stack.len() - argcnt;
                            let args = stk.arg_stack[newlen..].to_vec();
                            let r = fr(stk, args.as_slice());
                            stk.arg_stack.truncate(newlen);
                            r
                        }
                    };
                    stk.arg_stack.push(v);
                } else if let LispValue::LispFunction(func) = call {
                    let newlen = stk.arg_stack.len() - argcnt;
                    let _args = &mut stk.arg_stack[newlen..];

                    if func.compiled_code.len() == 0 {
                        //let mut writer = CodeWriter::new();
                        //lisp_compile(stk, &func.code, &mut writer).unwrap();
                        //let f2 = func.with_compled_code(writer.bytes);

                        panic!("Not supported!");
                    }

                    // todo: this is pretty slow. Find a way of using the func code without copying it.
                    let rd = CodeReader::new(func.compiled_code.clone());

                    let scope = LispScope2::new(func.clone(), newlen as usize, rd);
                    stk.current_scope.push(scope);
                    continue;
                } else {
                    panic!("Not a function!");
                }
            }
            ByteCode::LdConstI => {
                let integer = stk.read_sleb();
                let v = LispValue::Integer(integer);
                //println!("Loading integer {}", integer);
                stk.arg_stack.push(v);
            }
            ByteCode::IsNil => {
                if is_nil(&stk.arg_stack.pop().unwrap()) {
                    stk.arg_stack.push(LispValue::T);
                } else {
                    stk.arg_stack.push(LispValue::Nil);
                }
            }
            ByteCode::CondJmp => {
                let len = stk.read_sleb();
                if is_nil(&stk.arg_stack.pop().unwrap()) {
                    // do nothing.
                } else {
                    stk.jmp(len);
                }
            }
            ByteCode::Jmp => {
                let len = stk.read_sleb();
                stk.jmp(len);
            }
            ByteCode::LdT => stk.arg_stack.push(LispValue::T),
            ByteCode::Drop => {
                stk.arg_stack.pop();
            }
            ByteCode::Return => {
                stk.current_scope.pop();
                if stk.current_scope.len() == 0 {
                    return;
                }
                continue;
            }
            ByteCode::Defun => todo!(),
            ByteCode::DropScope => {
                let n = stk.read_uleb();
                let vars = &mut stk.current_scope.last_mut().unwrap().local_vars;
                for _ in 0..n {
                    vars.pop();
                }
            }
            ByteCode::Dup => {
                stk.arg_stack.push(stk.arg_stack.last().unwrap().clone());
            }
            ByteCode::LdQuote => {
                let id = stk.read_uleb() as i32;
                stk.arg_stack.push(stk.lookup_quote(id).clone());
            }
            ByteCode::Eval => {
                // assume something evalable is on the stack.
                let code = stk.arg_stack.pop().unwrap();
                let mut wd = CodeWriter::new();
                lisp_compile(stk, &code, &mut wd).unwrap();
                let eval_scope = LispScope2 {
                    func: Rc::new(LispFunc {
                        code: code.clone(),
                        compiled_code: wd.bytes.clone(),
                        args_names: Vec::new(),
                        magic: false,
                        variadic: false,
                    }),
                    argoffset: stk.arg_stack.len(),
                    reader: CodeReader::new(wd.bytes),
                    local_vars: Vec::new(),
                };
                stk.current_scope.push(eval_scope);
                continue;
            }
            ByteCode::LdConstR => {
                let r = stk.get_reader_mut().read_f64();
                stk.arg_stack.push(LispValue::Rational(r));
            }
            ByteCode::LdConstI1 => {
                let i = stk.get_reader_mut().read_i8();
                stk.arg_stack.push(LispValue::Integer(i as i64));
            }
        }
    }
}

pub fn lisp_compile_and_eval_string(ctx: &mut LispContext, code: &str) -> LispValue {
    assert_eq!(0, ctx.arg_stack.len());
    let mut code_bytes = code.as_bytes();
    let mut result = LispValue::Nil;

    loop {
        let c1 = parse_bytes(ctx, &mut code_bytes);
        update_symbol_names(ctx);
        if let Some(c) = c1 {
            let mut wd = CodeWriter::new();
            lisp_compile(ctx, &c, &mut wd).unwrap();
            let mut bytes = wd.bytes.as_slice();
            lisp_bytecode_print(&mut bytes, ctx);

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
            //return result;
        } else {
            println!("Result: {}", result);
            if ctx.arg_stack.len() > 0 {
                println!("stack: {:?}", ctx.arg_stack);
            }
            assert_eq!(0, ctx.arg_stack.len());
            return result;
        }
    }
}

pub fn lisp_compile_and_eval(ctx: &mut LispContext, code: &LispValue) -> LispValue {
    assert_eq!(0, ctx.arg_stack.len());

    let mut wd = CodeWriter::new();

    lisp_compile(ctx, &code, &mut wd).unwrap();
    let mut bytes = wd.bytes.as_slice();
    lisp_bytecode_print(&mut bytes, ctx);

    let code_reader = CodeReader::new(wd.bytes.clone());

    let lf = LispFunc {
        code: code.clone(),
        compiled_code: wd.bytes.clone(),
        args_names: Vec::new(),
        magic: false,
        variadic: false,
    };

    let s2 = LispScope2::new(Rc::new(lf), 0, code_reader);

    ctx.current_scope.push(s2);

    lisp_eval_bytecode(ctx);
    ctx.current_scope.pop();
    return ctx.arg_stack.pop().unwrap();
    //return result;
}

#[cfg(test)]
mod test {

    use crate::{
        compile::compile::{lisp_bytecode_print_bytes, lisp_compile_and_eval_string},
        compile::lisp_eval_bytecode,
        *,
    };

    #[test]
    fn test_list_macro() {
        let x = lisp_reverse(list!(1, 2, 3, "4"));
        println!("{}", x);
        assert_eq!(3, cadr(&x).to_integer().unwrap());
    }

    #[test]
    fn test_basic_compile() {
        let mut ctx = lisp_load_basic();
        let result = lisp_compile_and_eval_string(&mut ctx, "(println (+ 123 333 (* 22 1000) 11))");
        println!("Result: {}", result);
        if let LispValue::Integer(i) = result {
            assert_eq!(123 + 333 + (22 * 1000) + 11, i);
        } else {
            panic!("Unexpected value type");
        }

        lisp_compile_and_eval_string(&mut ctx, "(println pi)");

        lisp_compile_and_eval_string(&mut ctx, "(defvar x1 100)");
        lisp_compile_and_eval_string(&mut ctx, "(println (cons x1 x1))");

        lisp_compile_and_eval_string(&mut ctx, "(println (if nil (if nil (* 5 5) 4) 3))");
        let r = lisp_compile_and_eval_string(&mut ctx, "(+ 1 2)");
        assert_eq!(3, r.to_integer().unwrap());
        let r = lisp_compile_and_eval_string(&mut ctx, "(- 1 5)");
        assert_eq!(-4, r.to_integer().unwrap());

        let r = lisp_compile_and_eval_string(&mut ctx, "(* 1 2 3 4 5 6 7)");
        assert_eq!(1 * 2 * 3 * 4 * 5 * 6 * 7, r.to_integer().unwrap());

        lisp_compile_and_eval_string(&mut ctx, "(loop nil 1)");
        lisp_compile_and_eval_string(&mut ctx, "(loop (< x1 150000) (set! x1 (+ x1 1)))");
        lisp_compile_and_eval_string(&mut ctx, "(defun test0 (a b) (+ a 1 2 b))");

        lisp_bytecode_print_bytes(
            ctx.get_global_str("test0")
                .unwrap()
                .to_lisp_func()
                .unwrap()
                .compiled_code
                .clone(),
            &ctx,
        );

        let r = lisp_compile_and_eval_string(&mut ctx, "(test0 4 3)").to_integer();
        assert_eq!(4 + 3 + 2 + 1, r.unwrap());

        let r = lisp_compile_and_eval_string(&mut ctx, "(test0 (test0 10 10) 3)").to_integer();
        assert_eq!(10 + 10 + 2 + 1 + 3 + 2 + 1, r.unwrap());
        let r = lisp_compile_and_eval_string(&mut ctx, "(let ((a 2) (b 3)) (+ a b))").to_integer();
        assert_eq!(5, r.unwrap());

        let r = lisp_compile_and_eval_string(
            &mut ctx,
            "(let ((a 2) (b (let ((x 1) (y 2)) (+ x y)))) (let ((c 4) (d 5)) (+ a b c d)))",
        )
        .to_integer();
        assert_eq!(14, r.unwrap());

        let r = lisp_compile_and_eval_string(
            &mut ctx,
            "(let ((a 2) (b 3)) (set! a 5) (set! b 10) (- a b))",
        )
        .to_integer();
        assert_eq!(-5, r.unwrap());

        lisp_compile_and_eval_string(
            &mut ctx,
            "(defun fib (x) (if (< x 2) 1 (+ (fib (- x 1)) (fib (- x 2))))",
        );

        fn fib(x: i64) -> i64 {
            if x < 2 {
                return 1;
            }
            return fib(x - 1) + fib(x - 2);
        }
        let fib2 = ctx.get_global_str("fib").unwrap();
        let func = fib2.to_lisp_func();
        lisp_bytecode_print_bytes(func.unwrap().compiled_code.clone(), &ctx);
        println!("{:?}", ctx.arg_stack);
        let fib5 = lisp_compile_and_eval_string(&mut ctx, "(fib 10)");
        let fib5_2 = fib(10);
        assert_eq!(fib5_2, fib5.to_integer().unwrap());

        let cdr_quote = lisp_compile_and_eval_string(&mut ctx, "(cdr '(1 2 asd))");

        let code = "(2 asd)".to_evalable(&mut ctx).unwrap();

        assert_eq!(true, cdr_quote.equals(&code));

        let evaled_3 = lisp_compile_and_eval_string(&mut ctx, "(eval '(+ 1 2))");
    }

    #[test]
    fn test_handle_err() {
        let mut ctx = lisp_load_basic();
        ctx.eval_str("(with-handle-error (assert nil) ((e) 5)))");
    }
}
