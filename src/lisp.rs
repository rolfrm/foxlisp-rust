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

pub fn eq(a: &LispValue, b: &LispValue ) -> bool {
    return a == b
}
