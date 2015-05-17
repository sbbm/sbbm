use commands::{
    Command, Selector, SelectorName, SelectorTeam, Target, ToTarget,
    self, objectives, players, teams};
use nbt::{Nbt, NbtCompound};
use types::{Extent, Pos3, Vec3};

use std::i32;
use std::io::{self, Write};

// REVIEW: Where should the write_cmd macro go?  sbbm-asm/main.rs would like to
// use it, too.
macro_rules! write_cmd {
    ($w:expr, $cmd:expr) => { write!($w, "{}\n", $cmd) }
}

pub struct Computer {
    pub name: String,
    pub origin: Vec3,
    pub memory: Vec<MemoryRegion>,
}

impl Computer {
    pub fn selector(&self) -> Selector {
        Selector {
            name: Some(SelectorName::Is(self.name.clone())),
            ..Selector::entity()
        }
    }

    pub fn write_init_script(&self, w: &mut Write) -> io::Result<()> {
        for init_destroy in INIT_DESTROY.iter() {
            try!(init_destroy.init(self, w))
        }
        Ok(())
    }

    pub fn write_destroy_script(&self, w: &mut Write) -> io::Result<()> {
        for init_destroy in INIT_DESTROY.iter() {
            try!(init_destroy.destroy(self, w))
        }
        Ok(())
    }
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

trait InitDestroy {
    fn init(&self, computer: &Computer, w: &mut Write) -> io::Result<()>;
    fn destroy(&self, computer: &Computer, w: &mut Write) -> io::Result<()>;
}

struct CompInitDestroy;
struct RegsInitDestroy;
struct BitwiseInitDestroy;
struct MemInitDestroy;

const INIT_DESTROY: [&'static InitDestroy; 4] = [
    &CompInitDestroy,
    &RegsInitDestroy,
    &BitwiseInitDestroy,
    &MemInitDestroy,
];

impl InitDestroy for CompInitDestroy {
    fn init(&self, computer: &Computer, w: &mut Write) -> io::Result<()> {
        let pos = computer.origin.as_abs();

        let mut data_tag = NbtCompound::new();
        data_tag.insert(
            "CustomName".to_string(),
            Nbt::String(computer.name.clone()));
        data_tag.insert("NoGravity".to_string(), Nbt::Byte(1));
        write_cmd!(w, &Command::Summon(
            "ArmorStand".to_string(), Some(pos),
            Some(Nbt::Compound(data_tag))))
    }

    fn destroy(&self, computer: &Computer, w: &mut Write) -> io::Result<()> {
        write_cmd!(w, &Command::Kill(computer.selector().to_target()))
    }
}

impl RegsInitDestroy {
    fn regs(&self) -> Vec<(String, i32)> {
        let mut regs = vec![];

        // General registers
        for i in (0..32) {
            regs.push((format!("r{}", i), 0));
        }

        // Predicate registers
        for i in (0..8) {
            regs.push((format!("p{}", i), 0));
        }

        // Temp registers (implementation details)
        for i in (0..4) {
            regs.push((format!("t{}", i), 0));
        }

        regs.push(("ZERO".to_string(), 0));
        regs.push(("TWO".to_string(), 2));
        regs.push(("MIN".to_string(), i32::MIN));

        regs
    }
}

impl InitDestroy for RegsInitDestroy {
    fn init(&self, computer: &Computer, w: &mut Write) -> io::Result<()> {
        let sel = computer.selector();
        for (obj, value) in self.regs() {
            try!(write_cmd!(w, &objectives::add(obj.clone(), "dummy".to_string(), None)));
            try!(write_cmd!(w, &players::set(sel.to_target(), obj.clone(), value, None)));
        }
        Ok(())
    }

    fn destroy(&self, _: &Computer, w: &mut Write) -> io::Result<()> {
        for (obj, _) in self.regs() {
            try!(write_cmd!(w, &objectives::remove(obj)));
        }
        Ok(())
    }
}

impl BitwiseInitDestroy {
    fn bit_comp(&self) -> String { "BitComponent".to_string() }
    fn bit_num(&self) -> String { "BitNumber".to_string() }
    fn team(&self) -> String { "Shifters".to_string() }

    fn objectives(&self) -> Vec<String> {
        vec![self.bit_comp(), self.bit_num()]
    }
}

impl InitDestroy for BitwiseInitDestroy {
    fn init(&self, computer: &Computer, w: &mut Write) -> io::Result<()> {
        for obj in self.objectives() {
            try!(write_cmd!(w, &objectives::add(obj, "dummy".to_string(), None)));
        }

        // Bitwise entities
        let mut entities = vec!();
        for i in (0..32) {
            let name = format!("bit_{}", i);
            let target = Target::Sel(Selector {
                name: Some(SelectorName::Is(name.clone())),
                ..Selector::entity()
            });
            entities.push(target.clone());

            let origin = computer.origin;
            let pos = Pos3::abs(origin.x, origin.y, origin.z + i);
            let mut data_tag = NbtCompound::new();
            data_tag.insert("CustomName".to_string(), Nbt::String(name));
            data_tag.insert("NoGravity".to_string(), Nbt::Byte(1));
            try!(write_cmd!(w, &Command::Summon(
                "ArmorStand".to_string(), Some(pos),
                Some(Nbt::Compound(data_tag)))));

            try!(write_cmd!(w, &players::set(target.clone(), self.bit_num(), i, None)));
            try!(write_cmd!(w, &players::set(target, self.bit_comp(), 1 << i, None)));
        }

        try!(write_cmd!(w, &teams::add(self.team(), None)));
        try!(write_cmd!(w, &teams::join(self.team(), entities)));

        Ok(())
    }

    fn destroy(&self, _: &Computer, w: &mut Write) -> io::Result<()> {
        for obj in self.objectives() {
            try!(write_cmd!(w, &objectives::remove(obj.to_string())));
        }

        let bit_team_target = Target::Sel(Selector {
            team: Some(SelectorTeam::On(self.team())),
            ..Selector::entity()
        });
        try!(write_cmd!(w, &Command::Kill(bit_team_target)));
        try!(write_cmd!(w, &teams::remove(self.team())));

        Ok(())
    }
}

impl InitDestroy for MemInitDestroy {
    fn init(&self, computer: &Computer, w: &mut Write) -> io::Result<()> {
        for region in computer.memory.iter() {
            let extent = region.extent();

            let clay = "minecraft:stained_hardened_clay".to_string();
            for cmd in commands::safe_fill(extent, clay, None, None, None) {
                try!(write_cmd!(w, &cmd));
            }
        }
        Ok(())
    }

    fn destroy(&self, computer: &Computer, w: &mut Write) -> io::Result<()> {
        for region in computer.memory.iter() {
            let extent = region.extent();

            let air = "minecraft:air".to_string();
            for cmd in commands::safe_fill(extent, air, None, None, None) {
                try!(write_cmd!(w, &cmd));
            }
        }
        Ok(())
    }
}
