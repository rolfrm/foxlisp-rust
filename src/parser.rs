use num::{BigInt, FromPrimitive};

use crate::*;

fn parse_integer<'a>(code0: &'a [u8], val: &mut i64) -> Option<&'a [u8]> {
    *val = 0;
    if code0.len() == 0 {
        return None;
    }
    let mut code = code0;
    while code.len() > 0 && code[0] >= b'0' && code[0] <= b'9' {
        *val = *val * 10;
        let charcode = code[0] - b'0';
        *val = *val + i64::from(charcode);
        code = &code[1..]
    }
    if code.len() > 0 && code[0] != b')' && code[0] != b' ' {
        return None;
    }
    return Option::Some(code);
}

fn parse_integer_lisp_value<'a>(code0: &'a [u8], val: &mut LispValue) -> Option<&'a [u8]> {
    
    if code0.len() == 0 {
        return None;
    }
    
    let mut code = code0;
    if !(code[0] >= b'0' && code[0] <= b'9') {
        return None;
    }
    let mut outvar : i64 = 0;
    while code.len() > 0 && code[0] >= b'0' && code[0] <= b'9' {
        let outvar_checked = outvar.checked_mul(10);
        if let Some(outvar2) = outvar_checked {
            let charcode = code[0] - b'0';
            outvar = outvar2 + (u64::from(charcode) as i64);
            code = &code[1..]
        }else{
            let mut v2 = LispValue::Nil;
            let rest_code = parse_integer_lisp_value(code, &mut v2);
            if rest_code.is_some(){ // this means that the i64 oveflowed so we need to shift it based on log10(i64::max)
                //let shift = i64::MAX.ilog10();
                let offset = code.len() - rest_code.unwrap().len();
                let shift = offset as u32;
                let bigi = BigInt::from_i64(10).unwrap().pow(shift);
                let bigi = LispValue::BigInt(Rc::new(bigi * outvar));
                let vals = [bigi, v2];
                *val = lisp_add(&vals);
                
                return rest_code;
            }else{
                return None;
            }
        }
        
    }
    
    if code.len() > 0 && code[0] != b')' && code[0] != b' ' {
        return None;
    }
    
    *val = LispValue::Integer(outvar);
    return Option::Some(code);
}

fn skip_line(code: &[u8]) -> &[u8] {
    let mut code2 = code;

    while code2.len() > 0 && code2[0] != b'\n' {
        code2 = &code2[1..];
    }
    if code2.len() > 0 {
        code2 = &code2[1..];
    }
    return code2;
}

fn is_whitespace(c: u8) -> bool {
    match c {
        b' ' | b'\n' | b'\t' | b'\r' => true,
        _ => false,
    }
}

fn skip_whitespace_and_comment(code: &[u8]) -> &[u8] {
    let mut code2 = code;
    loop {
        while code2.len() > 0 && is_whitespace(code2[0]) {
            code2 = &code2[1..];
        }
        if code2.len() > 0 && code2[0] == b';' {
            code2 = skip_line(code2);
        } else {
            break;
        }
    }
    return code2;
}

fn parse_rational<'a>(code0: &'a [u8], val: &mut f64) -> Option<&'a [u8]> {
    *val = 0.0;
    let mut code = code0;
    if code.len() == 0 || !(code[0] >= b'0' && code[0] <= b'9' ) {
        return None;
    }
    while code.len() > 0 && code[0] >= b'0' && code[0] <= b'9' {
        *val = *val * 10.0;
        let charcode = code[0] - b'0';
        *val = *val + f64::from(charcode);
        code = &code[1..]
    }

    if code.len() > 0 && code[0] == b'.' {
        code = &code[1..];
        let mut subscale = 0.1;
        while code.len() > 0 && code[0] >= b'0' && code[0] <= b'9' {
            let charcode = f64::from(code[0] - b'0');
            *val += subscale * charcode;
            subscale *= 0.1;
            code = &code[1..]
        }
        return Some(code);
    } else {
        // there is no '.' -> not a rational.
    }

    return None;
}

fn is_token_end(c: u8) -> bool {
    return is_whitespace(c) || c == b'(' || c == b')';
}

fn parse_token<'a>(code0: &'a [u8], token: &str) -> Option<&'a [u8]> {
    let b = token.as_bytes();
    let l = b.len();
    if code0.len() < l {
        return None;
    }
    for i in 0..l {
        if b[i] != code0[i] {
            return None;
        }
    }
    if code0.len() == l || is_token_end(code0[l]) {
        return Some(&code0[l..]);
    }
    return None;
}

fn parse_symbol<'a>(
    ctx: &mut LispContext,
    code0: &'a [u8],
    value: &mut LispValue,
) -> Option<&'a [u8]> {
    let mut code = code0;
    let mut len = 0;

    while code.len() > 0 && code[0] != b')' && code[0] != b'(' && !is_whitespace(code[0]) {
        code = &code[1..];
        len += 1;
    }

    if code.len() > 0 && code[0] != b')' && !is_whitespace(code[0]) && code[0] != b'(' {
        return None;
    }

    if let Ok(str) = String::from_utf8(Vec::from(&code0[..len])) {
        *value = ctx.get_symbol(&str.to_string());
    }

    return Option::Some(code);
}

pub fn parse<'a>(ctx: &mut LispContext, code: &'a [u8], value: &mut LispValue) -> Option<&'a [u8]> {
    let mut code2 = code;
    code2 = skip_whitespace_and_comment(code2);
    let prev_neg = code2;
    let negative = 
        if code2.len() > 0 && code2[0] == b'-' {
          code2 = &code2[1..];
            true
        }else{
          false
      };
    
    if let Some(code3) = parse_integer_lisp_value(code2, value) {
        if negative {
            let v = [value.clone()];
            *value = lisp_sub(&v);
        }
        return Option::Some(code3);
    }
    
    let mut rational_value: f64 = 0.0;
    if let Some(code3) = parse_rational(code2, &mut rational_value) {
        *value = LispValue::Rational(rational_value);
        if negative {
            let v = [value.clone()];
            *value = lisp_sub(&v);
        }
        return Option::Some(code3);
    }
    if negative {
        code2 = prev_neg;
    }

    if code2.len() == 0 {
        return None;
    }
    
    if code2[0] == b'(' {
        code2 = &code2[1..];

        code2 = skip_whitespace_and_comment(code2);
        let mut sub = Vec::new();
        while code2.len() > 0 && code2[0] != b')' {
            let mut v2 = LispValue::Nil;
            if let Some(x) = parse(ctx, code2, &mut v2) {
                sub.push(v2);
                code2 = x;
            }

            code2 = skip_whitespace_and_comment(code2);
        }
        if code2.len() > 0 {
            code2 = &code2[1..];
        }

        let mut v = LispValue::Nil;
        while sub.len() > 0 {
            v = LispValue::Cons(Rc::new((sub.pop().unwrap(), v)))
        }
        *value = v;
        return Some(code2);
    }

    if code2[0] == b'"' {
        let mut str: Vec<u8> = Vec::new();
        let mut code3 = &code2[1..];
        let mut finished = false;
        loop {
            if code3.len() == 0 {
                break;
            }
            if code3[0] == b'"' {
                if code3.len() > 1 {
                    if code3[1] == b'"' {
                        code3 = &code3[2..];
                        str.push(code3[0]);
                        continue;
                    }
                }
                code3 = &code3[1..];
                finished = true;
                break;
            }
            str.push(code3[0]);
            code3 = &code3[1..]
        }
        if finished {
            *value = LispValue::String(String::from_utf8(str).unwrap());
            return Some(code3);
        }

        return None;
    }

    {
        let mut keyword = false;
        if code2.len() > 0 && code2[0] == b':' {
            keyword = true
        }
        
        if code2[0] == b'\'' {
        
            code2 = &code2[1..];
            let mut lv = LispValue::Nil;
            if let Some(c) = parse(ctx, code2, &mut lv) {
                
                let newcons = lisp_cons(ctx.get_symbol("quote"), lisp_cons(lv.clone(), LispValue::Nil));
                *value = newcons;
                return Some(c);
            }
            return None;
        }

        if let Some(next) = parse_token(code2, "t") {
            *value = LispValue::T;
            return Some(next);
        }

        if let Some(next) = parse_token(code2, "nil") {
            *value = LispValue::Nil;
            return Some(next);
        }

        if let Some(next) = parse_token(code2, "&rest") {
            *value = LispValue::Rest;
            return Some(next);
        }
        if let Some(sym) = parse_symbol(ctx, code2, value) {
            
            if keyword {
                if let LispValue::Symbol(x) = value {
                    ctx.set_value(*x, value.clone());
                }
            }
            return Some(sym);
        }
    }

    None
}

pub fn parse_from_string(ctx: &mut LispContext, code: &str) -> LispValue {
    let mut c = code.as_bytes();
    return parse_bytes(ctx, &mut c).unwrap();
}

pub fn parse_bytes(ctx: &mut LispContext, code: &mut &[u8]) -> Option<LispValue> {
    let mut v = LispValue::Nil;

    if let Some(c3) = parse(ctx, code, &mut v) {
        *code = c3;
        Some(v)
    } else {
        None
    }
}

#[cfg(test)]
mod test {

    use crate::*;

    #[test]
    fn test_code_builder() {
        let ctx = lisp_load_basic();

        let mut wd = CodeWriter::new();
        let bignumber: u128 = 111222333444555666777888999000111222333;
        let bignumber2: i128 = -111222333444555666777888999000111222333;
        wd.emit(ByteCode::LdSym);
        wd.emit_uleb(bignumber);
        wd.emit(ByteCode::LdSym);
        wd.emit_sleb(bignumber2);
        wd.emit(ByteCode::SetSym);
        wd.emit_uleb(303);
        println!("{:?}", wd.bytes);

        let mut reader = wd.to_reader();
        let sym1 = reader.read_u8();
        let val1 = reader.read_uleb_u128();
        let sym2 = reader.read_u8();
        let val2 = reader.read_sleb_i128();
        assert_eq!(bignumber, val1);
        println!("{} {}   {} {}", sym1, val1, sym2, val2);
    }
    #[test]
    fn test_parse_symbol() {
        let mut ctx = lisp_load_basic();
        let value = parse_from_string(&mut ctx, "'aaa");
        update_symbol_names(&ctx);
        println!("{:?}", ctx.symbol_name_lookup);
        println!("{}", value);
        let symname = ctx.get_symbol_name(cadr(&value)).unwrap();
        assert_eq!("aaa", symname);
        let qname = ctx.get_symbol_name(car(&value)).unwrap();
        assert_eq!("quote", qname);
    }
    
    
}
