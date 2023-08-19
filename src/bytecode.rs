
pub enum ByteCode {
    NoCode = 0,
    Nil = 1,
    LdSym = 2,
    SetSym = 3,
    Call = 4,
    LdConstI = 5,
    DefVar = 6,
    IsNil = 7,
    CondJmp = 8,
    Jmp = 9,
    T = 10,
    Drop = 11,
}