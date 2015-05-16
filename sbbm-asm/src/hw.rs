use types::Vec3;

pub struct Computer {
    pub memory: Vec<MemoryRegion>,
}

pub struct MemoryRegion {
    pub start: u32,
    pub size: u32,
    pub origin: Vec3,
    pub growth: Vec3,
}
