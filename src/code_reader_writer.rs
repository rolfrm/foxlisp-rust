use std::rc::Rc;

use crate::{LispContext, bytecode::ByteCode};

pub trait u128able {
    fn as_u128(&self) -> u128;
}

pub trait i128able {
    fn as_i128(&self) -> i128;
}

impl i128able for i128{
    fn as_i128(&self) -> i128{
        *self
    }
}

impl i128able for i64{
    fn as_i128(&self) -> i128{
        *self as i128
    }
}
impl i128able for i32{
    fn as_i128(&self) -> i128{
        *self as i128
    }
}
impl u128able for i32 {
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
}

pub struct CodeReader {
    pub bytes: Vec<u8>,
    pub offset: usize,
}

impl CodeReader {
    
    pub fn end(&self) -> bool {
        self.bytes.len() <= self.offset
    }
    pub fn new(bytes: Vec<u8>) -> CodeReader {
        CodeReader {
            bytes: bytes,
            offset: 0,
        }
    }
    pub fn jmp(&mut self, amount: i64){
        if (self.offset as i64) + amount < 0 {
            panic!("Invalid jump {} > {}", self.offset, amount);
        }
        self.offset = (self.offset as i64 + amount) as usize;
    }
    pub fn read_u8(&mut self) -> u8 {
        let i = self.offset;
        self.offset += 1;
        return self.bytes[i];
    }
    pub fn read_uleb(&mut self) -> u64{
        // read LEB128
        let mut value : u64 = 0;
        let mut offset : u32 = 0;
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

      pub fn read_uleb_u128(&mut self) -> u128{
        // read LEB128
        let mut value : u128 = 0;
        let mut offset : u32 = 0;
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
}

impl CodeWriter {
    pub fn new() -> CodeWriter {
        CodeWriter {
            bytes: Vec::new(),
        }
    }

    pub fn to_reader(self) -> CodeReader{
        CodeReader::new(self.bytes)
    }

    pub fn offset(&self) -> usize {
        self.bytes.len()
    }

    pub fn emit(&mut self, byte_code: ByteCode) {
        self.bytes.push(byte_code as u8)
    }

    pub fn emit_u8(&mut self, v: u8) {
        self.bytes.push(v)
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
            if ((next == 0 && sign == 0) || (sign > 0 && next == -1)) {
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
