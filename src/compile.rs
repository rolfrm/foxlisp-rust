use crate::*;

#[derive(Debug)]
pub enum CompileError {
    InvalidValue,
    ArgumentError(LispValue),
    UnknownSymbol(LispValue),
}

pub fn lisp_compile(
    ctx: &mut Lisp,
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
                        labels:  w2.labels
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
                    
                    let exit_label = w.new_label();
                    let else_label = w.new_label();
                    lisp_compile(ctx, check, w)?;
                    // insert the jump to the else clause
                    w.emit(ByteCode::IsNil);
                    w.emit(ByteCode::CondJmp);
                    w.emit_uleb(else_label);
                    lisp_compile(ctx, then, w)?;
                    w.emit(ByteCode::Jmp);
                    w.emit_uleb(exit_label);
                    w.assign_label(else_label);
                    lisp_compile(ctx, _else, w)?;
                    w.assign_label(exit_label);

                    return Ok(());
                }
                if name.eq("with-handle-error") {
                    let main = car (&a.1);
                    let handler = cadr (&a.1);
                    if main.is_nil() || handler.is_nil() {
                        return Err(CompileError::ArgumentError(code.clone()));
                    }
                    let ok_label = w.new_label();
                    let error_label = w.new_label();
                    w.emit(ByteCode::PushHandler);
                    w.emit_uleb(error_label);
                    lisp_compile(ctx, main, w)?;
                    w.emit(ByteCode::Jmp);
                    w.emit_uleb(ok_label);
                    w.assign_label(error_label);
                    w.emit(ByteCode::Drop);
                    lisp_compile(ctx, handler, w)?;
                    w.assign_label(ok_label);
                    w.emit(ByteCode::PopHandler);
                    return Ok(());
                }

                if name.eq("loop") {
                    let check = car(&a.1);
                    
                    let continue_label = w.new_label();
                    let exit_label = w.new_label();
                    
                    w.assign_label(continue_label);
                    lisp_compile(ctx, check, w)?;
                    w.emit(ByteCode::IsNil);
                    w.emit(ByteCode::CondJmp);
                    w.emit_uleb(exit_label);
                    
                    let body = cdr(&a.1);

                    for x in body.to_iter() {
                        lisp_compile(ctx, x, w)?;
                        w.emit(ByteCode::Drop);
                    }
                    w.emit(ByteCode::Jmp);
                    w.emit_uleb(continue_label);
                    w.assign_label(exit_label);
                    
                    w.emit(ByteCode::LdNil);

                    return Ok(());
                }

                if name.eq("let") {
                    let without_let = &a.1;
                    let args = car(&without_let);
                    let let_body = cdr(without_let);
                    let mut cnt = 0;
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
pub fn lisp_bytecode_print_bytes(code: Vec<u8>, stk: &Lisp) {
    let code = CodeReader::new(code, Vec::new());
    lisp_bytecode_print(&code.bytes[0..], stk);
}

pub fn bytecode_to_lisp<T>(code: &mut T, _stk: &Lisp) -> LispValue
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
                value = list!("LdSym".to_lisp(), symi.to_symbol()).as_car(value);
            }
            ByteCode::SetSym => {
                let symi = read_uleb_u32(code) as i32;
                value = list!("SetSym".to_lisp(), symi.to_symbol()).as_car(value);
            }
            ByteCode::DefVar => {
                let symi = read_uleb_u32(code) as i32;
                value = list!("DefVar".to_lisp(), symi.to_symbol()).as_car(value);
            }
            ByteCode::Call => {
                let _argcnt = read_uleb_u128(code) as usize;
                let symi: i32 = read_uleb_u32(code) as i32;
                value = list!("Call", symi.to_symbol()).as_car(value);
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
            ByteCode::PushHandler => {
                let offset = read_uleb_u32(code);
                value = list!("PushHandler".to_lisp(), (offset as i32).to_lisp()).as_car(value);   
            }
            ByteCode::PopHandler => {
                value = list!("PopHandler".to_lisp()).as_car(value);   
            }
        }
    }
}

pub fn lisp_bytecode_print(code: &[u8], stk: &Lisp) {
    let mut c = code;
    let mut r = bytecode_to_lisp(&mut c, stk);
    while r.is_nil() == false {
        println!("  {}", car(&r));
        r = cdr(&r).clone();
    }
}

pub fn lisp_eval_bytecode(stk: &mut Lisp) -> () {
    loop {
        if stk.current_error.is_nil() == false {
            if stk.error_handler_labels.len() > 0 {
                
                let label = stk.error_handler_labels.pop().unwrap();
                while label.1 < stk.current_scope.len() as u32 {
                    
                    let top_scope = stk.current_scope.pop().unwrap();
                    while stk.arg_stack.len() > top_scope.argoffset {
                        stk.arg_stack.pop();
                    }
                }
                println!("jmp to error handling label:{}", stk.get_reader_mut().labels[label.0 as usize]);
                stk.get_reader_mut().jmp(label.0 );
                stk.current_error = LispValue::Nil;
            }
        }
        
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
                    let rd = CodeReader::new(func.compiled_code.clone(), func.labels.clone());

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
                let len = stk.read_uleb();
                if is_nil(&stk.arg_stack.pop().unwrap()) {
                    // do nothing.
                } else {
                    stk.jmp(len as u32);
                }
            }
            ByteCode::Jmp => {
                let len = stk.read_uleb();
                stk.jmp(len as u32);
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
                        labels: wd.labels.clone(),
                        args_names: Vec::new(),
                        magic: false,
                        variadic: false,
                    }),
                    argoffset: stk.arg_stack.len(),
                    reader: CodeReader::from_code_writer(wd),
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
            ByteCode::PushHandler => {
                let label = stk.get_reader_mut().read_uleb() as u32;
                stk.error_handler_labels.push((label, stk.current_scope.len() as u32));
                  
            }
            ByteCode::PopHandler => {
                println!("error handler labels: {:?}", stk.error_handler_labels);
                println!("argstack:{:?}", stk.arg_stack);
                stk.error_handler_labels.pop();
            }
        }
    }
}

pub fn lisp_compile_and_eval_string(ctx: &mut Lisp, code: &str) -> LispValue {
    assert_eq!(0, ctx.arg_stack.len());
    let mut code_bytes = code.as_bytes();
    let mut result = LispValue::Nil;

    loop {
        let c1 = parse_bytes(ctx, &mut code_bytes);
        update_symbol_names(ctx);
        if let Some(c) = c1 {
            println!("code: {}", code);
            let mut wd = CodeWriter::new();
            lisp_compile(ctx, &c, &mut wd).unwrap();
            let mut bytes = wd.bytes.as_slice();
            lisp_bytecode_print(&mut bytes, ctx);

            let code_reader = wd.to_reader();

            let lf = LispFunc {
                code: c.clone(),
                compiled_code: code_reader.bytes.clone(),
                labels: code_reader.labels.clone(),
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

pub fn lisp_compile_and_eval(ctx: &mut Lisp, code: &LispValue) -> LispValue {
    assert_eq!(0, ctx.arg_stack.len());

    let mut wd = CodeWriter::new();

    lisp_compile(ctx, &code, &mut wd).unwrap();
    let mut bytes = wd.bytes.as_slice();
    lisp_bytecode_print(&mut bytes, ctx);

    let code_reader = CodeReader::from_code_writer(wd);

    let lf = LispFunc {
        code: code.clone(),
        compiled_code: code_reader.bytes.clone(),
        labels : code_reader.labels.clone(),
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
        let mut lisp = Lisp::new();
        let result = lisp_compile_and_eval_string(&mut lisp, "(println (+ 123 333 (* 22 1000) 11))");
        println!("Result: {}", result);
        if let LispValue::Integer(i) = result {
            assert_eq!(123 + 333 + (22 * 1000) + 11, i);
        } else {
            panic!("Unexpected value type");
        }

        lisp_compile_and_eval_string(&mut lisp, "(println pi)");

        lisp_compile_and_eval_string(&mut lisp, "(defvar x1 100)");
        lisp_compile_and_eval_string(&mut lisp, "(println (cons x1 x1))");

        lisp_compile_and_eval_string(&mut lisp, "(println (if nil (if nil (* 5 5) 4) 3))");
        let r = lisp_compile_and_eval_string(&mut lisp, "(+ 1 2)");
        assert_eq!(3, r.to_integer().unwrap());
        let r = lisp_compile_and_eval_string(&mut lisp, "(- 1 5)");
        assert_eq!(-4, r.to_integer().unwrap());

        let r = lisp_compile_and_eval_string(&mut lisp, "(* 1 2 3 4 5 6 7)");
        assert_eq!(1 * 2 * 3 * 4 * 5 * 6 * 7, r.to_integer().unwrap());

        lisp_compile_and_eval_string(&mut lisp, "(loop nil 1)");
        lisp_compile_and_eval_string(&mut lisp, "(loop (< x1 150000) (set! x1 (+ x1 1)))");
        lisp_compile_and_eval_string(&mut lisp, "(defun test0 (a b) (+ a 1 2 b))");

        lisp_bytecode_print_bytes(
            lisp.get_global_str("test0")
                .unwrap()
                .to_lisp_func()
                .unwrap()
                .compiled_code
                .clone(),
            &lisp,
        );

        let r = lisp_compile_and_eval_string(&mut lisp, "(test0 4 3)").to_integer();
        assert_eq!(4 + 3 + 2 + 1, r.unwrap());

        let r = lisp_compile_and_eval_string(&mut lisp, "(test0 (test0 10 10) 3)").to_integer();
        assert_eq!(10 + 10 + 2 + 1 + 3 + 2 + 1, r.unwrap());
        let r = lisp_compile_and_eval_string(&mut lisp, "(let ((a 2) (b 3)) (+ a b))").to_integer();
        assert_eq!(5, r.unwrap());

        let r = lisp_compile_and_eval_string(
            &mut lisp,
            "(let ((a 2) (b (let ((x 1) (y 2)) (+ x y)))) (let ((c 4) (d 5)) (+ a b c d)))",
        )
        .to_integer();
        assert_eq!(14, r.unwrap());

        let r = lisp_compile_and_eval_string(
            &mut lisp,
            "(let ((a 2) (b 3)) (set! a 5) (set! b 10) (- a b))",
        )
        .to_integer();
        assert_eq!(-5, r.unwrap());

        lisp_compile_and_eval_string(
            &mut lisp,
            "(defun fib (x) (if (< x 2) 1 (+ (fib (- x 1)) (fib (- x 2))))",
        );

        fn fib(x: i64) -> i64 {
            if x < 2 {
                return 1;
            }
            return fib(x - 1) + fib(x - 2);
        }
        let fib2 = lisp.get_global_str("fib").unwrap();
        let func = fib2.to_lisp_func();
        lisp_bytecode_print_bytes(func.unwrap().compiled_code.clone(), &lisp);
        println!("{:?}", lisp.arg_stack);
        let fib5 = lisp_compile_and_eval_string(&mut lisp, "(fib 10)");
        let fib5_2 = fib(10);
        assert_eq!(fib5_2, fib5.to_integer().unwrap());

        let cdr_quote = lisp_compile_and_eval_string(&mut lisp, "(cdr '(1 2 asd))");

        let code =  lisp.parse("(2 asd)").unwrap();

        assert_eq!(true, cdr_quote.equals(&code));

        let evaled_3 = lisp_compile_and_eval_string(&mut lisp, "(eval '(+ 1 2))");
    }

    #[test]
    fn test_handle_err() {
        let mut lisp = Lisp::new();
        lisp.panic_on_error = true;
        lisp.eval_str("(defun fails-with-args (a) (let ((b 3)) (raise 'est)))");
        let r = lisp.eval_str("(with-handle-error (let ((x 2)) (assert nil)) 5)");
        
        assert_eq!(5, r.to_integer().unwrap());
    }
}
