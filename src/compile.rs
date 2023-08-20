use crate::*;

pub fn lisp_compile(ctx: &mut LispContext, code: &LispValue, w: &mut CodeWriter) {
    match code {
        LispValue::Cons(a) => {
            if let LispValue::Symbol(s1) = a.0 {
                let namecode = ctx.global_names[s1 as usize];
                let name = &ctx.symbol_name_lookup[namecode];
                //println!("Name code: {}", name);
                if name.eq("set!") {
                    if let LispValue::Symbol(name) = car(&a.1) {
                        let value = cadr(&a.1);
                        lisp_compile(ctx, value, w);
                        w.emit(ByteCode::SetSym);
                        w.emit_uleb(name);
                        return;
                    }
                }

                if name.eq("defvar") {
                    let value = cadr(&a.1);
                    lisp_compile(ctx, value, w);
                    w.emit(ByteCode::DefVar);
                    let sym = car(&a.1);
                    if let LispValue::Symbol(symi) = sym {
                        w.emit_uleb(*symi);
                    } else {
                        panic!("defvar arg1 must be a symbol");
                    }
                    return;
                }
                if name.eq("if") {
                    // fetch all the arguments
                    let check = car(&a.1);
                    let then = cadr(&a.1);
                    let _else = caddr(&a.1);

                    lisp_compile(ctx, check, w);
                    // insert the jump to the else clause
                    w.emit(ByteCode::IsNil);
                    w.emit(ByteCode::CondJmp);

                    // the next piece is a bit ugly - we need to calculate the jump sizes
                    // this first the code gets compiled and both clauses gets pushed into reserved vectors
                    // the the jumps before and after 'then' is inserted.
                    let offset1 = w.offset();
                    lisp_compile(ctx, then, w);
                    let offset2 = w.offset();
                    lisp_compile(ctx, _else, w);

                    let mut reserved2 = w.bytes.drain(offset2..).as_slice().to_vec();

                    w.emit(ByteCode::Jmp); // the jump over 'else' part
                    w.emit_sleb(reserved2.len() as i32);
                    let mut reserved1 = w.bytes.drain(offset1..).as_slice().to_vec();

                    // jump to the 'else' part
                    w.emit_sleb(reserved1.len() as i32);

                    // insert both caluese.
                    w.bytes.append(&mut reserved1);
                    w.bytes.append(&mut reserved2);

                    return;
                }

                if name.eq("loop") {
                    let offset1 = w.offset();
                    let check = car(&a.1);
                    lisp_compile(ctx, check, w);
                    w.emit(ByteCode::IsNil);
                    w.emit(ByteCode::CondJmp);
                    let check_code = w.bytes.drain(offset1..).as_slice().to_vec();

                    let body = cdr(&a.1);
                    for x in body.to_iter() {
                        lisp_compile(ctx, x, w);
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

                    return;
                }

                let mut argcount: u32 = 0;
                for arg in a.1.to_iter() {
                    lisp_compile(ctx, arg, w);
                    argcount += 1;
                }

                if let LispValue::Symbol(name) = a.0 {
                    w.emit(ByteCode::Call);
                    w.emit_uleb(argcount as u128);
                    w.emit_uleb(name);
                }
            } else {
                panic!("No such global!");
            }
        }
        LispValue::Nil => {
            w.emit(ByteCode::LdNil);
        }
        LispValue::Symbol(s) => {
            w.emit(ByteCode::LdSym);
            w.emit_uleb(s);
        }
        LispValue::Integer(i) => {
            w.emit(ByteCode::LdConstI);
            w.emit_sleb(*i);
        }
        LispValue::T => {
            w.emit(ByteCode::LdT);
        }
        _ => {
            todo!();
        }
    }
}

fn lisp_bytecode_print(code: &mut CodeReader, stk: &LispContext){
    loop {
        if code.end() {
            return;
        }
        print!("{} ", code.offset);
        let upcode = code.read_u8().to_bytecode();
        
        match upcode {
            ByteCode::NoCode => todo!(),
            ByteCode::LdNil => {
                println!("LdNil");
            }
            ByteCode::LdSym => {
                let symi = code.read_uleb() as i32;
                println!("LdSym ({})", symi.symbol_name(stk));
            }
            ByteCode::SetSym => {
                let symi = code.read_uleb() as i32;
                println!("SetSym ({})", symi.symbol_name(stk));
            }
            ByteCode::DefVar => {
                let symi = code.read_uleb() as i32;
                println!("DefVar ({})" ,symi.symbol_name(stk));
            }
            ByteCode::Call => {
                let argcnt = code.read_uleb() as usize;
                let symi = code.read_uleb() as i32;
                println!("Call ({}, argcnt: {})", symi.symbol_name(stk), argcnt);
               
            }
            ByteCode::LdConstI => {
                let integer = code.read_sleb();
                let v = LispValue::Integer(integer);
                println!("LdConstI64 ({})", v);
            }
            ByteCode::IsNil => {
                println!("IsNil")
            }
            ByteCode::CondJmp => {
                let len = code.read_sleb();
                println!("CondJmp ({} -> {})", len, (code.offset as i64) + len);
            }
            ByteCode::Jmp => {
                let len = code.read_sleb();
                println!("Jmp ({} -> {})", len, (code.offset as i64) + len);
            }
            ByteCode::LdT => println!("LdT"),
            ByteCode::Drop => {
                println!("Drop");
            }
        }
    }
}

fn lisp_eval_bytecode(code: &mut CodeReader, stk: &mut LispContext) {
    loop {
        if code.end() {
            return;
        }
        let upcode = code.read_u8().to_bytecode();

        match upcode {
            ByteCode::NoCode => todo!(),
            ByteCode::LdNil => {
                stk.arg_stack.push(LispValue::Nil);
            }
            ByteCode::LdSym => {
                let symi = code.read_uleb() as i32;
                let r = stk.get_value(symi).clone().unwrap();
                stk.arg_stack.push(r.clone());
            }
            ByteCode::SetSym => {
                let symi = code.read_uleb() as i32;
                let value = stk.arg_stack.pop().unwrap();
                stk.set_value(symi, &value);
            }
            ByteCode::DefVar => {
                let symi = code.read_uleb() as i32;
                let value = stk.arg_stack.pop().unwrap().clone();
                stk.set_global(symi, value);
            }
            ByteCode::Call => {
                let argcnt = code.read_uleb() as usize;
                //println!("Call! ({}, {})", argcnt, stk.arg_stack.len());
                let symi = code.read_uleb() as i32;
                let call = stk.get_value(symi).unwrap().clone();
                if let LispValue::NativeFunction(nf) = call {
                    let v = match nf {
                        NativeFunc::Function1(f1) => f1(stk.arg_stack.pop().unwrap()),
                        NativeFunc::Function2(f2) => {
                            let a1 = stk.arg_stack.pop().unwrap();
                            let a2 = stk.arg_stack.pop().unwrap();
                            f2(a1, a2)
                        }
                        NativeFunc::Function1r(_) => todo!(),
                        NativeFunc::Function2r(f2) => {
                            let l = stk.arg_stack.len();
                            let r = f2(&stk.arg_stack[l - 2], &stk.arg_stack[l - 1]).clone();
                            stk.arg_stack.truncate(l - 2);
                            r
                        }
                        NativeFunc::FunctionN(_) => todo!(),
                        NativeFunc::FunctionNr(fr) => {
                            let newlen = stk.arg_stack.len() - argcnt;
                            let r = fr(&stk.arg_stack[newlen..]);
                            stk.arg_stack.truncate(newlen);
                            r
                        }
                    };
                    stk.arg_stack.push(v);
                } else {
                    panic!("Not a function!");
                }
            }
            ByteCode::LdConstI => {
                let integer = code.read_sleb();
                let v = LispValue::Integer(integer);
                //println!("Loading integer {}", integer);
                stk.arg_stack.push(v);
            }
            ByteCode::IsNil => {
                if (is_nil(&stk.arg_stack.pop().unwrap())) {
                    stk.arg_stack.push(LispValue::T);
                } else {
                    stk.arg_stack.push(LispValue::Nil);
                }
            }
            ByteCode::CondJmp => {
                let len = code.read_sleb();
                if (is_nil(&stk.arg_stack.pop().unwrap())) {
                    // do nothing.
                } else {
                    //println!("jmp from {} by {}", code.offset, len);
                    code.jmp(len);
                }
            }
            ByteCode::Jmp => {
                let len = code.read_sleb();
                //println!("jmp by {}", len);
                code.jmp(len);
            }
            ByteCode::LdT => stk.arg_stack.push(LispValue::T),
            ByteCode::Drop => {
                stk.arg_stack.pop();
            }
        }
    }
}

pub fn lisp_compile_and_eval_string(ctx: &mut LispContext, code: &str) -> LispValue {
    println!("");
    println!("code: {}", code);
    let mut code = code.as_bytes();
    let mut result = LispValue::Nil;

    loop {
        let c1 = parse_bytes(ctx, &mut code);

        if let Some(c) = c1 {
            let mut wd = CodeWriter::new();

            lisp_compile(ctx, &c, &mut wd);
            println!("bytes: {:?}", wd.bytes);
            lisp_bytecode_print(&mut CodeReader::new(wd.bytes.clone()), ctx);

            let mut code_reader = CodeReader::new(wd.bytes);
            lisp_eval_bytecode(&mut code_reader, ctx);
            result = ctx.arg_stack.pop().unwrap();
        } else {
            println!("Result: {}", result);
            return result;
        }
    }
}

#[cfg(test)]
mod test {

    use std::mem::size_of;

    use crate::{compile::lisp_compile_and_eval_string, compile::lisp_eval_bytecode, *};

    use super::lisp_compile;

    #[test]
    fn test_basic_compile() {
        let mut ctx = lisp_load_basic();
        let code = "(println (+ 123 333 (* 22 1000) 11))";
        let code2 = code.to_evalable(&mut ctx).unwrap();
        let mut wd = CodeWriter::new();

        lisp_compile(&mut ctx, &code2, &mut wd);
        println!("{:?}", wd.bytes);
        let mut code_reader = CodeReader::new(wd.bytes);
        lisp_eval_bytecode(&mut code_reader, &mut ctx);
        let result = ctx.arg_stack.pop().unwrap();
        println!("Result: {}", result);
        if let LispValue::Integer(i) = result {
            assert_eq!(123 + 333 + (22 * 1000) + 11, i);
        } else {
            panic!("Unexpected value type");
        }

        let code = "(println pi)";
        let code2 = code.to_evalable(&mut ctx).unwrap();
        let mut wd = CodeWriter::new();

        lisp_compile(&mut ctx, &code2, &mut wd);
        println!("{:?}", wd.bytes);
        let mut code_reader = CodeReader::new(wd.bytes);
        lisp_eval_bytecode(&mut code_reader, &mut ctx);

        let code = "(defvar x1 100)";
        let code2 = code.to_evalable(&mut ctx).unwrap();
        let mut wd = CodeWriter::new();

        lisp_compile(&mut ctx, &code2, &mut wd);
        println!("{:?}", wd.bytes);
        let mut code_reader = CodeReader::new(wd.bytes);
        lisp_eval_bytecode(&mut code_reader, &mut ctx);

        let code = "(println (cons x1 x1))";
        let code2 = code.to_evalable(&mut ctx).unwrap();
        let mut wd = CodeWriter::new();

        lisp_compile(&mut ctx, &code2, &mut wd);
        println!("{:?}", wd.bytes);
        let mut code_reader = CodeReader::new(wd.bytes);
        lisp_eval_bytecode(&mut code_reader, &mut ctx);

        let code = "(println (if nil (if nil (* 5 5) 4) 3))";
        let code2 = code.to_evalable(&mut ctx).unwrap();
        let mut wd = CodeWriter::new();

        lisp_compile(&mut ctx, &code2, &mut wd);
        println!("{:?}", wd.bytes);
        let mut code_reader = CodeReader::new(wd.bytes);
        lisp_eval_bytecode(&mut code_reader, &mut ctx);
        let r = lisp_compile_and_eval_string(&mut ctx, "(+ 1 2)");
        assert_eq!(3, r.to_integer().unwrap());

        let r = lisp_compile_and_eval_string(&mut ctx, "(* 1 2 3 4 5 6 7)");
        assert_eq!(1 * 2 * 3 * 4 * 5 * 6 * 7, r.to_integer().unwrap());
        
        lisp_compile_and_eval_string(&mut ctx, "(loop nil 1)");
        lisp_compile_and_eval_string(&mut ctx, "(loop (< x1 15000000) (set! x1 (+ x1 1)))");

        let s1 = std::mem::size_of_val(&LispValue::Nil);
        println!("Size: {}", s1);
    }
}
