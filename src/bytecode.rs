pub enum ByteCode {
    // Load NIL on the stack.
    LdNil = 1,
    // Loads a variable value on the stack.
    LdSym = 2,
    // Sets a variable value, popping the value from the stack
    SetSym = 3,
    // Calls a function on the stack
    Call = 4,
    // Loads a constant i64 value
    LdConstI = 5,
    // Define a new global variable.
    DefVar = 6,
    // Pops the top value from the stack and pushes whether it was nil or not.
    IsNil = 7,
    // Conditionally jumps a relative length. Depends on the current value on the top of the stack.
    CondJmp = 8,
    // Jumps a relative distance.
    Jmp = 9,
    // Loads 't on the stack.
    LdT = 10,
    // drops a value from the stack.
    Drop = 11,
    // returns from the scope (not implemented)
    Return = 12,
    // defines a function.
    Defun = 13,
    // Declares a local variable.
    Let = 14,
    // drops a number of variables.
    DropScope = 15,
    // duplicates the top value on the stack.
    Dup = 16,
    LdQuote = 17,
    Eval = 18,
    LdConstR = 19
}

pub trait ToByteCode {
    fn to_bytecode(&self) -> ByteCode;
}

impl ToByteCode for u8 {
    fn to_bytecode(&self) -> ByteCode {
        match self {
            1 => ByteCode::LdNil,
            2 => ByteCode::LdSym,
            3 => ByteCode::SetSym,
            4 => ByteCode::Call,
            5 => ByteCode::LdConstI,
            6 => ByteCode::DefVar,
            7 => ByteCode::IsNil,
            8 => ByteCode::CondJmp,
            9 => ByteCode::Jmp,
            10 => ByteCode::LdT,
            11 => ByteCode::Drop,
            12 => ByteCode::Return,
            13 => ByteCode::Defun,
            14 => ByteCode::Let,
            15 => ByteCode::DropScope,
            16 => ByteCode::Dup,
            17 => ByteCode::LdQuote,
            18 => ByteCode::Eval,
            19 => ByteCode::LdConstR,
            _ => panic!("Invalid upcode {}", self),
        }
    }
}
