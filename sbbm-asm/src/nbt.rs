// Copyright 2015, Christopher Chambers
// Distributed under the GNU GPL v3. See COPYING for details.

use std::collections::HashMap;
use std::fmt;

pub type NbtCompound = HashMap<String, Nbt>;

#[derive(Clone, Debug, PartialEq)]
pub enum Nbt {
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    ByteArray(Vec<u8>),
    IntArray(Vec<i32>),
    String(String),
    List(Vec<Nbt>),
    Compound(NbtCompound),
}

impl fmt::Display for Nbt {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Nbt::Byte(b) => write!(formatter, "{}b", b),
            Nbt::Short(s) => write!(formatter, "{}s", s),
            Nbt::Int(i) => write!(formatter, "{}i", i),
            Nbt::Long(l) => write!(formatter, "{}L", l),
            Nbt::String(ref s) => write!(formatter, "\"{}\"", s),
            Nbt::Compound(ref c) => {
                try!(write!(formatter, "{{"));
                let mut first = true;
                for (k, v) in c {
                    if !first {
                        try!(write!(formatter, ", "));
                    }
                    first = false;
                    try!(write!(formatter, "{}: ", k));
                    try!(write!(formatter, "{}", v));
                }
                write!(formatter, "}}")
            }

            _ => unimplemented!(),
        }
    }
}
