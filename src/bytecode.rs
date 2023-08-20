
pub enum ByteCode {
    NoCode = 0,
    LdNil = 1,
    LdSym = 2,
    SetSym = 3,
    Call = 4,
    LdConstI = 5,
    DefVar = 6,
    IsNil = 7,
    CondJmp = 8,
    Jmp = 9,
    LdT = 10,
    Drop = 11,
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
            _ => panic!("Invalid upcode"),
        }
    }
    
}