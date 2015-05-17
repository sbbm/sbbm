use types::{Extent, Vec3};

pub struct Computer {
    pub memory: Vec<MemoryRegion>,
}

pub enum MemoryStride {
    XY(u32, u32),
    ZY(u32, u32),
}

pub struct MemoryRegion {
    pub start: u32,
    pub size: u32,
    pub origin: Vec3,
    pub growth: Vec3,
    pub stride: MemoryStride,
}

impl MemoryRegion {
    pub fn extent(&self) -> Extent {
        // TODO: This function assumes that the Y component of MemoryStride is a
        // multiple of 8 (block-per-byte * bytes-per-word).  This invariant
        // should be enforced.

        if self.size == 0 {
            return Extent::Empty;
        }

        let blocks_per_byte = 2;
        let blocks = self.size * blocks_per_byte;

        let (x_size, y_size, z_size) = match self.stride {
            MemoryStride::XY(x_size, y_size) =>
                (x_size, y_size, blocks / y_size / x_size),
            MemoryStride::ZY(z_size, y_size)  =>
                (blocks / y_size / z_size, y_size, z_size),
        };

        // TODO: Deal with overflow.
        let end = Vec3::new(
            self.origin.x + (x_size as i32) * self.growth.x - self.growth.x,
            self.origin.y + (y_size as i32) * self.growth.y - self.growth.y,
            self.origin.z + (z_size as i32) * self.growth.z - self.growth.z);

        let mut extent = Extent::MinMax(self.origin, end);
        extent.normalize();
        extent
    }
}
