use nbt::NbtCompound;
use std::cmp::{min, max};
use std::fmt;
use self::AbsRel::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub id: String,
    pub data: u8,
    pub nbt: NbtCompound,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AbsRel {
    Abs(i32),
    Rel(i32),
}

impl fmt::Display for AbsRel {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Abs(x) => write!(f, "{}", x),
            Rel(x) if x == 0 => write!(f, "~"),
            Rel(x) => write!(f, "~{}", x),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Interval<T : PartialOrd> {
    Min(T),
    Max(T),
    Bounded(T, T),
}

impl<T : PartialOrd> Interval<T> {
    // TODO: Return Result<Interval, {SomeErrorType}> instead of Option<Interval>
    pub fn new(min: Option<T>, max: Option<T>) -> Option<Interval<T>> {
        use self::Interval::*;

        match (min, max) {
            (Some(min), Some(max)) => Some(Bounded(min, max)),
            (Some(min), None) => Some(Min(min)),
            (None, Some(max)) => Some(Max(max)),
            (None, None) => None,
        }
    }

    pub fn min(&self) -> Option<&T> {
        use self::Interval::*;

        match *self {
            Min(ref min) | Bounded(ref min, _) => Some(min),
            Max(_) => None,
        }
    }

    pub fn max(&self) -> Option<&T> {
        use self::Interval::*;

        match *self {
            Max(ref max) | Bounded(_, ref max) => Some(max),
            Min(_) => None,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Pos3 {
    pub x: AbsRel,
    pub y: AbsRel,
    pub z: AbsRel,
}

pub const ABS_ZERO: Pos3 = Pos3 { x: Abs(0), y: Abs(0), z: Abs(0) };
pub const REL_ZERO: Pos3 = Pos3 { x: Rel(0), y: Rel(0), z: Rel(0) };

impl Pos3 {
    pub fn new(x: AbsRel, y: AbsRel, z: AbsRel) -> Pos3 {
        Pos3 { x: x, y: y, z: z }
    }

    pub fn rel(x: i32, y: i32, z: i32) -> Pos3 {
        Pos3::new(Rel(x), Rel(y), Rel(z))
    }

    pub fn rel_from_vec3(v: Vec3) -> Pos3 {
        Pos3::rel(v.x, v.y, v.z)
    }

    pub fn abs(x: i32, y: i32, z: i32) -> Pos3 {
        Pos3::new(Abs(x), Abs(y), Abs(z))
    }

    pub fn abs_from_vec3(v: Vec3) -> Pos3 {
        Pos3::abs(v.x, v.y, v.z)
    }
}

impl fmt::Display for Pos3 {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} {} {}", self.x, self.y, self.z)
    }
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

    pub fn as_abs(self) -> Pos3 {
        Pos3::abs_from_vec3(self)
    }

    pub fn as_rel(self) -> Pos3 {
        Pos3::rel_from_vec3(self)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Extent {
    Empty,
    // FIXME: A better name than MinMax
    MinMax(Vec3, Vec3),
}

impl Extent {
    pub fn normalize(&mut self) {
        if let Extent::MinMax(min, max) = *self {
            *self = Extent::MinMax(
                Vec3::min(min, max),
                Vec3::max(min, max));
        }
    }

    pub fn add(&mut self, v: Vec3) {
        use self::Extent::*;

        *self = match *self {
            Empty => MinMax(v, v),
            MinMax(min, max) => MinMax(Vec3::min(min, v), Vec3::max(max, v)),
        };
    }

    pub fn union(&mut self, extent: &Extent) {
        use self::Extent::*;

        match (*self, *extent) {
            (Empty, MinMax(_, _)) => { *self = *extent; }
            (MinMax(min_a, max_a), MinMax(min_b, max_b)) => {
                *self = MinMax(Vec3::min(min_a, min_b), Vec3::max(max_a, max_b));
            }
            _ => { }
        }
    }
}
