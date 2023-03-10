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

fn skip_line(code: &[u8]) -> &[u8]{
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
        _ => false
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
        }else{
            break;
        }
    }
    return code2;
}

fn parse_rational<'a>(code0: &'a [u8], val: &mut f64) -> Option<&'a [u8]> {
    *val = 0.0;
    let mut code = code0;
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

fn parse_token<'a>(
    code0: &'a [u8],
    token: &str) -> Option<&'a [u8]> {
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

    while code.len() > 0
        && code[0] != b')'
        && code[0] != b'('
        && !is_whitespace(code[0])
    {
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

    let mut integer_value: i64 = 0;
    if let Some(code3) = parse_integer(code2, &mut integer_value) {
        *value = LispValue::Integer(integer_value);
        return Option::Some(code3);
    }
    let mut rational_value: f64 = 0.0;
    if let Some(code3) = parse_rational(code2, &mut rational_value) {
        *value = LispValue::Rational(rational_value);
        return Option::Some(code3);
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
            v = LispValue::Cons(Arc::new((sub.pop().unwrap(), v)))
        }
        *value = v;
        return Some(code2);
    }

    if code2[0] == b'"' {
        let mut str : Vec<u8> = Vec::new();
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
        if let Some(next) = parse_token(code2, "t") {
            *value = LispValue::T;
            return Some(next);
        }
        if let Some(next) = parse_token(code2, "&rest") {
            *value = LispValue::Rest;
            return Some(next);
        }
        if let Some(sym) = parse_symbol(ctx, code2, value) {
            if keyword {
                if let LispValue::Symbol(x) = value {
                    ctx.set_value(*x, value);
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
