use crate::*;

pub fn lisp_compile(code: &LispValue, w: &mut CodeWriter) {
    
    match code {
        LispValue::Cons(a, b) => {
            if let LispValue::Symbol(s1) = a.as_ref() {

                let namecode = w.ctx.global_names[*s1 as usize];
                let name = &w.ctx.symbol_name_lookup[namecode];
                println!("Name code: {}", name);
                if name.eq("set!") {
                    if let LispValue::Symbol(name) = car(&b) {
                        let value = cadr(&b);
                        lisp_compile(value, w);
                        w.emit(ByteCode::SetSym);
                        w.emit_uleb(name);
                        return;
                    }
                }

                if name.eq("defvar") {

                    let value = cadr(&b);
                    lisp_compile(value, w);
                    w.emit(ByteCode::DefVar);
                    let sym = car(&b);
                    if let LispValue::Symbol(symi) = sym {
                        w.emit_uleb(*symi);
                    }else {
                        panic!("defvar arg1 must be a symbol");
                    }
                    return;
                }
                if name.eq("if") {
                    
                    // fetch all the arguments
                    let check = car(b);
                    let then = cadr(b);
                    let _else = caddr(b);


                    lisp_compile(check, w);
                    // insert the jump to the else clause
                    w.emit(ByteCode::IsNil);
                    w.emit(ByteCode::CondJmp);
                    
                    // the next piece is a bit ugly - we need to calculate the jump sizes
                    // this first the code gets compiled and both clauses gets pushed into reserved vectors
                    // the the jumps before and after 'then' is inserted.
                    let offset1 = w.offset();
                    lisp_compile(then, w);
                    let offset2 = w.offset();
                    lisp_compile(_else, w);

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
                    println!("Compiling loop!");
                    let offset1 = w.offset();
                    let check = car(b);
                    lisp_compile(check, w);
                    w.emit(ByteCode::IsNil);
                    w.emit(ByteCode::CondJmp);
                    let mut check_code = w.bytes.drain(offset1..).as_slice().to_vec();
                    
                    let body = cdr(b);
                    for x in body.to_iter() {
                        lisp_compile(x, w);
                        w.emit(ByteCode::Drop);
                    }
                    w.emit(ByteCode::Jmp); 
                    let mut body_code = w.bytes.drain(offset1..).as_slice().to_vec();
                    w.bytes.append(&mut check_code.clone());
                    let jump_from = w.offset();

                    w.emit_sleb(body_code.len() as i32);
                    w.bytes.append(&mut body_code.clone());
                    let offset2 = w.offset();
                    w.emit_sleb(-((offset2 - offset1) as i64));
                    
                    let jmpl2 = w.offset() - jump_from;
                    
                    w.bytes.truncate(offset1);

                    w.bytes.append(&mut check_code.clone());
                    w.emit_sleb(jmpl2 as i32);
                    w.bytes.append(&mut body_code.clone());
                    let offset2 = w.offset();
                    w.emit_sleb(-((offset2 - offset1) as i64));
                    

                    // iterate again.
                    let jmpl2 = w.offset() - jump_from;
                    
                    w.bytes.truncate(offset1);

                    w.bytes.append(&mut check_code.clone());
                    w.emit_sleb(jmpl2 as i32);
                    w.bytes.append(&mut body_code.clone());
                    let offset2 = w.offset();
                    w.emit_sleb(-((offset2 - offset1) as i64));
                    let offset3 = w.offset();
                    w.bytes.truncate(offset2);
                    w.emit_sleb(-((offset3 - offset1) as i64));
                    let offset3 = w.offset();
                    w.bytes.truncate(offset2);
                    w.emit_sleb(-((offset3 - offset1) as i64));

                    return;
                    
                }

                let mut argcount :u32 = 0;
                for arg in b.to_iter() {
                    lisp_compile(arg, w);
                    argcount += 1;
                    
                }
                
                if let LispValue::Symbol(name) = a.as_ref() {
                    
                    w.emit(ByteCode::Call);
                    w.emit_uleb(argcount as u128);
                    w.emit_uleb(name);
                }

            }else{
                panic!("No such global!");
            }
        }
        LispValue::Nil => {
            w.emit(ByteCode::Nil);
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
            w.emit(ByteCode::T);
        }
        _ => {
            todo!();
        }
    }
}

fn lisp_eval_bytecode(code : &mut CodeReader, stk: &mut LispContext){
    
    loop {
        if code.end() {
            return;
        }
    let upcode = match code.read_u8() {
        1 => ByteCode::Nil,
        2 => ByteCode::LdSym,
        3 => ByteCode::SetSym,
        4 => ByteCode::Call,
        5 => ByteCode::LdConstI,
        6 => ByteCode::DefVar,
        7 => ByteCode::IsNil,
        8 => ByteCode::CondJmp,
        9 => ByteCode::Jmp,
        10 => ByteCode::T,
        11 => ByteCode::Drop,
        _=> panic!("Invalid upcode")
    };
    
    match upcode {
        ByteCode::NoCode => todo!(),
        ByteCode::Nil => {
            stk.arg_stack.push(LispValue::Nil);
        },
        ByteCode::LdSym => {
            let symi = code.read_uleb() as i32;
            let r = stk.get_value(symi).clone().unwrap();
            stk.arg_stack.push(r.clone());
        },
        ByteCode::SetSym => {
            let symi = code.read_uleb() as i32;
            let value = stk.arg_stack.pop().unwrap();
            stk.set_value(symi, &value);
        },
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
                    },
                    NativeFunc::Function1r(_) => todo!(),
                    NativeFunc::Function2r(f2) => {
                        let l = stk.arg_stack.len();
                        let r = f2(&stk.arg_stack[l - 2],&stk.arg_stack[l - 1]).clone();
                        stk.arg_stack.truncate(l - 2);
                        r
                    },
                    NativeFunc::FunctionN(_) => todo!(),
                    NativeFunc::FunctionNr(fr) => {
                        let newlen = stk.arg_stack.len() - argcnt;
                        let r = fr(&stk.arg_stack[newlen..]);
                        stk.arg_stack.truncate(newlen);
                        r
                    },
                };
                stk.arg_stack.push(v);

            }else{
                panic!("Not a function!");
            }
        },
        ByteCode::LdConstI => {
            
            let integer = code.read_sleb();
            let v = LispValue::Integer(integer);
            //println!("Loading integer {}", integer);
            stk.arg_stack.push(v);
        },
        ByteCode::IsNil => {
            if(is_nil(&stk.arg_stack.pop().unwrap())){
                stk.arg_stack.push(LispValue::T);

            }else{
                stk.arg_stack.push(LispValue::Nil);
            }
        },
        ByteCode::CondJmp => {
            let len = code.read_sleb();
            if(is_nil(&stk.arg_stack.pop().unwrap())){
                // do nothing.
                

            }else{
                //println!("jmp from {} by {}", code.offset, len);
                code.jmp(len);

            }
        },
        ByteCode::Jmp => {
            let len = code.read_sleb();
            //println!("jmp by {}", len);
            code.jmp(len);
        },
        ByteCode::T => stk.arg_stack.push(LispValue::T),
        ByteCode::Drop => {
            stk.arg_stack.pop();
        }
    }
}
}

#[cfg(test)]
mod test {

    use std::mem::size_of;

    use crate::{*, compile::lisp_eval_bytecode};

    use super::lisp_compile;

    #[test]
    fn test_basic_compile(){
        let mut ctx = lisp_load_basic();
        let code = "(println (+ 123 333 (* 22 1000) 11))";
        let code2 = code.to_evalable(&mut ctx).unwrap();
        let mut wd = CodeWriter::new(ctx);
        
        lisp_compile(&code2, &mut wd);
        println!("{:?}", wd.bytes);
        let mut code_reader = (CodeReader::new(wd.bytes), wd.ctx);
        lisp_eval_bytecode(&mut code_reader.0,  &mut code_reader.1);
        let result = code_reader.1.arg_stack.pop().unwrap();
        println!("Result: {}", result);
        if let LispValue:: Integer(i) = result {
            assert_eq!(123 + 333 + (22 * 1000) + 11, i);
        }else{
            panic!("Unexpected value type");
        }

        let code = "(println pi)";
        let code2 = code.to_evalable(&mut code_reader.1).unwrap();
        let mut wd = CodeWriter::new(code_reader.1);
        
        lisp_compile(&code2, &mut wd);
        println!("{:?}", wd.bytes);
        let mut code_reader = (CodeReader::new(wd.bytes), wd.ctx);
        lisp_eval_bytecode(&mut code_reader.0,  &mut code_reader.1);
        
        let code = "(defvar x1 100)";
        let code2 = code.to_evalable(&mut code_reader.1).unwrap();
        let mut wd = CodeWriter::new(code_reader.1);
        
        lisp_compile(&code2, &mut wd);
        println!("{:?}", wd.bytes);
        let mut code_reader = (CodeReader::new(wd.bytes), wd.ctx);
        lisp_eval_bytecode(&mut code_reader.0,  &mut code_reader.1);
        
        let code = "(println (cons x1 x1))";
        let code2 = code.to_evalable(&mut code_reader.1).unwrap();
        let mut wd = CodeWriter::new(code_reader.1);
        
        lisp_compile(&code2, &mut wd);
        println!("{:?}", wd.bytes);
        let mut code_reader = (CodeReader::new(wd.bytes), wd.ctx);
        lisp_eval_bytecode(&mut code_reader.0,  &mut code_reader.1);

        let code = "(println (if nil (if nil (* 5 5) 4) 3))";
        let code2 = code.to_evalable(&mut code_reader.1).unwrap();
        let mut wd = CodeWriter::new(code_reader.1);
        
        lisp_compile(&code2, &mut wd);
        println!("{:?}", wd.bytes);
        let mut code_reader = (CodeReader::new(wd.bytes), wd.ctx);
        lisp_eval_bytecode(&mut code_reader.0,  &mut code_reader.1);

        let code = "(loop (< x1 15000000) (set! x1 (+ x1 1)))";
        let code2 = code.to_evalable(&mut code_reader.1).unwrap();
        let mut wd = CodeWriter::new(code_reader.1);
        
        lisp_compile(&code2, &mut wd);
        println!("{:?}", wd.bytes);
        let mut code_reader = (CodeReader::new(wd.bytes), wd.ctx);
        lisp_eval_bytecode(&mut code_reader.0,  &mut code_reader.1);
        

        let s1 = std::mem::size_of_val(&LispValue::Nil);
        println!("Size: {}", s1);
    }

}