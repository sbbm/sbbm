use nbt::NbtCompound;

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
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Extent {
    Empty,
    // FIXME: A better name than MinMax
    MinMax(Vec3, Vec3),
}
