use crate::bytecode::ByteCode;

pub trait u128able {
    fn as_u128(&self) -> u128;
}

pub trait i128able {
    fn as_i128(&self) -> i128;
}

impl i128able for i128 {
    fn as_i128(&self) -> i128 {
        *self
    }
}

impl i128able for i64 {
    fn as_i128(&self) -> i128 {
        *self as i128
    }
}
impl i128able for i32 {
    fn as_i128(&self) -> i128 {
        *self as i128
    }
}
impl u128able for i32 {
    fn as_u128(&self) -> u128 {
        *self as u128
    }
}

impl u128able for usize {
    fn as_u128(&self) -> u128 {
     *self as u128
    }
}

impl u128able for u32 {
    fn as_u128(&self) -> u128 {
        return *self as u128;
    }
}

impl u128able for &i32 {
    fn as_u128(&self) -> u128 {
        return **self as u128;
    }
}

impl u128able for i128 {
    fn as_u128(&self) -> u128 {
        return *self as u128;
    }
}
impl u128able for u128 {
    fn as_u128(&self) -> u128 {
        return *self as u128;
    }
}

pub struct CodeWriter {
    pub bytes: Vec<u8>,
    pub labels: Vec<u32>
}

#[derive(Debug)]
pub struct CodeReader {
    pub bytes: Vec<u8>,
    pub offset: usize,
    pub labels: Vec<u32>
}

impl CodeReader {
    pub fn end(&self) -> bool {
        self.bytes.len() <= self.offset
    }
    pub fn new(bytes: Vec<u8>, labels: Vec<u32>) -> CodeReader {
        CodeReader {
            bytes: bytes,
            offset: 0,
            labels: labels
        }
    }
    
    pub fn from_code_writer(writer: CodeWriter) -> CodeReader {
        CodeReader{
            bytes: writer.bytes.clone(),
            offset : 0,
            labels: writer.labels.clone()
        }
    }
    
    pub fn jmp(&mut self, label: u32) {
        if label as usize > self.labels.len() {
            panic!("Invalid label {} > {}", self.offset, label);
        }
        self.offset = self.labels[label as usize] as usize;
    }
    pub fn read_u8(&mut self) -> u8 {
        let i = self.offset;
        self.offset += 1;
        return self.bytes[i];
    }

    pub fn read_i8(&mut self) -> i8 {
        let i = self.offset;
        self.offset += 1;
        return self.bytes[i] as i8;
    }

    pub fn read_uleb(&mut self) -> u64 {
        // read LEB128
        let mut value: u64 = 0;
        let mut offset: u32 = 0;
        loop {
            let chunk = self.read_u8();
            value |= ((chunk & 0b01111111) as u64) << offset;
            offset += 7;
            if (0b10000000 & chunk) == 0 {
                break;
            }
        }
        return value;
    }

    pub fn read_uleb_u128(&mut self) -> u128 {
        // read LEB128
        let mut value: u128 = 0;
        let mut offset: u32 = 0;
        loop {
            let chunk = self.read_u8();
            value |= ((chunk & 0b01111111) as u128) << offset;
            offset += 7;
            if (0b10000000 & chunk) == 0 {
                break;
            }
        }
        return value;
    }

    pub fn read_sleb(&mut self) -> i64 {
        let mut value = 0;
        let mut shift: u32 = 0;
        let mut chunk: u8;
        loop {
            chunk = self.read_u8();
            value |= (chunk as u64 & 0x7f) << shift;
            shift += 7;
            if chunk < 128 {
                break;
            }
        }
        if shift < 64 && (chunk & 0x40) > 0 {
            value |= (u64::MAX) << shift;
        }
        return value as i64;
    }
    pub fn read_sleb_i128(&mut self) -> i128 {
        let mut value = 0;
        let mut shift: u32 = 0;
        let mut chunk: u8;
        loop {
            chunk = self.read_u8();
            value |= (chunk as u128 & 0x7f) << shift;
            shift += 7;
            if chunk < 128 {
                break;
            }
        }
        if shift < 64 && (chunk & 0x40) > 0 {
            value |= (u128::MAX) << shift;
        }
        return value as i128;
    }

    pub fn read_f64(&mut self) -> f64 {
        let mut bytes: [u8; 8] = [0; 8];
        for i in 0..8 {
            bytes[i] = self.read_u8();
        }
        f64::from_be_bytes(bytes)
    }
}

impl CodeWriter {
    pub fn new() -> CodeWriter {
        CodeWriter { bytes: Vec::new(), labels: Vec::new()}
    }

    pub fn to_reader(self) -> CodeReader {
        CodeReader::new(self.bytes, self.labels)
    }

    pub fn offset(&self) -> usize {
        self.bytes.len()
    }

    pub fn emit(&mut self, byte_code: ByteCode) {
        self.bytes.push(byte_code as u8)
    }
    
    pub fn new_label(&mut self) -> usize {
        let idx = self.labels.len();
        self.labels.push(0);
        return idx;
    }
    
    pub fn assign_label(&mut self, label : usize) {
        self.labels[label] = self.offset() as u32;
    }

    pub fn emit_u8(&mut self, v: u8) {
        self.bytes.push(v)
    }

    pub fn emit_i8(&mut self, v: i8) {
        self.bytes.push(v as u8);
    }

    pub fn emit_f64(&mut self, v: &f64) {
        for b in v.to_be_bytes() {
            self.emit_u8(b);
        }
    }

    pub fn emit_sleb<T>(&mut self, in_value: T)
    where
        T: i128able,
    {
        let mut value = in_value.as_i128();
        loop {
            let bits = value as u8 & 0b01111111;
            let sign = value as u8 & 0b01000000;
            let next = value >> 7;
            if (next == 0 && sign == 0) || (sign > 0 && next == -1) {
                self.emit_u8(bits);
                break;
            } else {
                self.emit_u8(bits | 0b10000000);
                value = next;
            }
        }
    }

    pub fn emit_uleb<T>(&mut self, in_value: T)
    where
        T: u128able,
    {
        let mut value = in_value.as_u128();
        loop {
            let to_write = (value as u8) & 0b01111111;
            value >>= 7;
            if value > 0 {
                self.emit_u8(to_write | 0b10000000);
            } else {
                self.emit_u8(to_write);
                break;
            }
        }
    }
}

pub trait CodeReader2 {
    fn read_u8(&mut self) -> u8;
    fn end(&self) -> bool;
}

impl CodeReader2 for &[u8] {
    fn read_u8(&mut self) -> u8 {
        let r = self[0];
        *self = &self[1..];
        return r;
    }
    fn end(&self) -> bool {
        self.len() == 0
    }
}

pub fn read_sleb64<T>(s: &mut T) -> i64
where
    T: CodeReader2,
{
    let mut value = 0;
    let mut shift: u32 = 0;
    let mut chunk: u8;
    loop {
        chunk = s.read_u8();
        value |= (chunk as u64 & 0x7f) << shift;
        shift += 7;
        if chunk < 128 {
            break;
        }
    }
    if shift < 64 && (chunk & 0x40) > 0 {
        value |= (u64::MAX) << shift;
    }
    return value as i64;
}

pub fn read_sleb32<T>(s: &mut T) -> i32
where
    T: CodeReader2,
{
    let mut value = 0;
    let mut shift: u32 = 0;
    let mut chunk: u8;
    loop {
        chunk = s.read_u8();
        value |= (chunk as u32 & 0x7f) << shift;
        shift += 7;
        if chunk < 128 {
            break;
        }
    }
    if shift < 64 && (chunk & 0x40) > 0 {
        value |= (u32::MAX) << shift;
    }
    return value as i32;
}

pub fn read_uleb_u128<T>(s: &mut T) -> u128
where
    T: CodeReader2,
{
    // read LEB128
    let mut value: u128 = 0;
    let mut offset: u32 = 0;
    loop {
        let chunk = s.read_u8();
        value |= ((chunk & 0b01111111) as u128) << offset;
        offset += 7;
        if (0b10000000 & chunk) == 0 {
            break;
        }
    }
    return value;
}

pub fn read_uleb_u32<T>(s: &mut T) -> u32
where
    T: CodeReader2,
{
    // read LEB128
    let mut value: u32 = 0;
    let mut offset: u32 = 0;
    loop {
        let chunk = s.read_u8();
        value |= ((chunk & 0b01111111) as u32) << offset;
        offset += 7;
        if (0b10000000 & chunk) == 0 {
            break;
        }
    }
    return value;
}

pub fn read_f64<T>(s: &mut T) -> f64
where
    T: CodeReader2,
{
    let mut bytes: [u8; 8] = [0; 8];
    for i in 0..8 {
        bytes[i] = s.read_u8();
    }
    f64::from_be_bytes(bytes)
}

#[cfg(test)]
mod test {
    use crate::*;

    use super::CodeReader2;

    fn test_thing(slice: &[u8]) -> &[u8] {
        let mut y = slice;
        let a = y.read_u8();
        let b = y.read_u8();
        let c = y.read_u8();
        let d = y.read_u8();
        println!("{} {} {} {}", a, b, c, d);
        return y;
    }
    #[test]
    fn test_code_reader() {
        let x: [u8; 8] = [1, 2, 3, 4, 5, 6, 7, 8];
        let x2 = test_thing(&x);
        test_thing(x2);
    }
}
