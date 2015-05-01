use nbt::NbtCompound;
use std::cmp::{min, max};

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub id: String,
    pub data: u8,
    pub nbt: NbtCompound,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Vec3 {
    pub x: i32,
    pub y: i32,
    pub z: i32,
}

impl Vec3 {
    pub fn new(x: i32, y: i32, z: i32) -> Vec3 {
        Vec3 { x: x, y: y, z: z }
    }

    pub fn min(lhs: Vec3, rhs: Vec3) -> Vec3 {
        Vec3 {
            x: min(lhs.x, rhs.x),
            y: min(lhs.y, rhs.y),
            z: min(lhs.z, rhs.z),
        }
    }

    pub fn max(lhs: Vec3, rhs: Vec3) -> Vec3 {
        Vec3 {
            x: max(lhs.x, rhs.x),
            y: max(lhs.y, rhs.y),
            z: max(lhs.z, rhs.z),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Extent {
    Empty,
    // FIXME: A better name than MinMax
    MinMax(Vec3, Vec3),
}

impl Extent {
    pub fn add(&mut self, v: Vec3) {
        *self = match *self {
            Extent::Empty => Extent::MinMax(v, v),
            Extent::MinMax(min, max) => {
                Extent::MinMax(Vec3::min(min, v), Vec3::max(max, v))
            }
        };
    }
}
