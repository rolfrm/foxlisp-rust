

use std::borrow::Borrow;
use std::io::BufReader;
use std::io::Read;
use std::file;
use std::fmt;
use std::collections::HashMap;

enum LispValue{
    Cons{car: Box<LispValue>, cdr: Box<LispValue>},
    Nil,
    String(String),
    Rational(f64),
    Integer(i64),
    Symbol(i64)
}

impl LispValue {
    
    fn cons(car: LispValue, cdr: LispValue) -> LispValue {
        return LispValue::Cons{car: Box::new(car), cdr: Box::new(cdr)};

    }
}

impl From<i64> for LispValue {
    fn from(item: i64) -> Self {
        LispValue::Integer(item)
    }
}
impl From<String> for LispValue {
    fn from(item: String) -> Self {
        LispValue::String(item)
    }
}
impl From<&str> for LispValue {
    fn from(item: &str) -> Self {
        LispValue::String(item.to_string())
    }
}

impl TryInto<i64> for LispValue{
    type Error = ();
    fn try_into(self) -> Result<i64, Self::Error>{
        if let LispValue::Integer(v) = self {
            return Ok(v);
        }
        return Err(())
    }
}

impl fmt::Display for LispValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self {
            LispValue::Cons { car, cdr } =>{
                write!(f, "(").unwrap();
                write!(f, "{}", car).unwrap();
                if let LispValue::Nil = cdr.as_ref() {
                    return write!(f, ")");    
                }
                write!(f, " ").unwrap();
                write!(f, "{}", cdr).unwrap();
                return write!(f, ")");
            }
            LispValue::Nil => {
                return write!(f, "()");
            }
            LispValue::String(str) => {
                return write!(f, "{}", str);
            }
            LispValue::Symbol(id) => {
                return write!(f, "Symbol({})", id)
            }
            &LispValue::Rational(_) | &LispValue::Integer(_) => todo!()

        }
    
    }
}    

struct LispContext{
    symbols: HashMap<String, i64>,
    id_gen: i64
}

impl<'a> LispContext{
    fn new() -> LispContext{
        let x  = LispContext{symbols: HashMap::new(), id_gen: 1};
        return x
    }

    fn new2() -> HashMap<&'a str, i64> {
        HashMap::new()
    }

    fn NewSymbol(& mut self, name: String) -> LispValue{

        if let Option::Some(id) = self.symbols.get(&name) {
            return LispValue::Symbol(*id);
        }
        let id2  = self.id_gen;
        self.id_gen += 1;
        //let name2 = name.clone();
        self.symbols.insert(name.to_string(), id2);
        return LispValue::Symbol(id2)

    }

}

fn main() {
    println!("Hello, world!");
    let code = "(+ 1 2)";
    let nil = LispValue::Nil;
    println!("{}", nil);
    let X = "Hej";
    let str1 = LispValue::from("Hej");
    let str2 = LispValue::from("123");
    let x2 : i64 = LispValue::from(123).try_into().unwrap();
    let v1 = LispValue::cons(str1, LispValue::cons(str2, LispValue::Nil));
    println!("{}", v1);
    let f = BufReader::new(code.as_bytes());

    let mut ctx = LispContext::new();
    let a = ctx.NewSymbol("1234".to_string());
    let b = ctx.NewSymbol("123".to_string());
    let c = ctx.NewSymbol("123".to_string());

    println!("{} {} {}", a, b, c);

}
