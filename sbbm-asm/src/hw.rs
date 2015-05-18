use commands::{
    Command, IntoTarget, Selector, SelectorName, SelectorTeam, Target, ToTarget,
    self, objectives, players, teams};
use fab;
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

        let (x_size, y_size, z_size) = self.axis_sizes();

        // TODO: Deal with overflow.
        let end = Vec3::new(
            self.origin.x + (x_size as i32) * self.growth.x - self.growth.x,
            self.origin.y + (y_size as i32) * self.growth.y - self.growth.y,
            self.origin.z + (z_size as i32) * self.growth.z - self.growth.z);

        let mut extent = Extent::MinMax(self.origin, end);
        extent.normalize();
        extent
    }

    pub fn axis_sizes(&self) -> (u32, u32, u32) {
        let blocks_per_byte = 2;
        let blocks = self.size * blocks_per_byte;

        match self.stride {
            MemoryStride::XY(x_size, y_size) =>
                (x_size, y_size, blocks / y_size / x_size),
            MemoryStride::ZY(z_size, y_size)  =>
                (blocks / y_size / z_size, y_size, z_size),
        }
    }

    pub fn traversal(&self) -> Vec<(u32, Pos3)> {
        let blocks_per_byte = 2;
        let bytes_per_word = 4;
        let blocks_per_word = bytes_per_word * blocks_per_byte;

        let (x_size, y_size, z_size) = self.axis_sizes();

        let mut v = vec![];
        let mut i: i32;
        let mut component = 1;

        i = self.growth.x;
        while i.abs() < x_size as i32 {
            v.push((component, Pos3::rel(i, 0, 0)));
            i *= 2;
            component *= 2;
        }
        i = self.growth.y;
        while (i * blocks_per_word).abs() < y_size as i32 {
            v.push((component, Pos3::rel(0, i * blocks_per_word, 0)));
            i *= 2;
            component *= 2;
        }
        i = self.growth.z;
        while i.abs() < z_size as i32 {
            v.push((component, Pos3::rel(0, 0, i)));
            i *= 2;
            component *= 2;
        }
        v.reverse();
        v
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
        let name = &computer.name[..];
        let pos = computer.origin.as_abs();
        write_cmd!(w, make_entity(name, pos))
    }

    fn destroy(&self, computer: &Computer, w: &mut Write) -> io::Result<()> {
        write_cmd!(w, Command::Kill(computer.selector().to_target()))
    }
}

impl RegsInitDestroy {
    fn regs() -> Vec<(String, i32)> {
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

        regs.push(("IndAddr".to_string(), 0));
        regs.push(("lr".to_string(), 0));
        regs.push(("sp".to_string(), 0));

        regs
    }
}

impl InitDestroy for RegsInitDestroy {
    fn init(&self, computer: &Computer, w: &mut Write) -> io::Result<()> {
        let sel = computer.selector();
        for (obj, value) in Self::regs() {
            try!(write_cmd!(w, objectives::add(obj.clone(), "dummy".to_string(), None)));
            try!(write_cmd!(w, players::set(sel.to_target(), obj.clone(), value, None)));
        }
        Ok(())
    }

    fn destroy(&self, _: &Computer, w: &mut Write) -> io::Result<()> {
        for (obj, _) in Self::regs() {
            try!(write_cmd!(w, objectives::remove(obj)));
        }
        Ok(())
    }
}

impl BitwiseInitDestroy {
    fn bit_comp() -> String { "BitComponent".to_string() }
    fn bit_num() -> String { "BitNumber".to_string() }
    fn team() -> String { "Shifters".to_string() }

    fn objectives() -> Vec<String> {
        vec![Self::bit_comp(), Self::bit_num()]
    }
}

impl InitDestroy for BitwiseInitDestroy {
    fn init(&self, computer: &Computer, w: &mut Write) -> io::Result<()> {
        for obj in Self::objectives() {
            try!(write_cmd!(w, objectives::add(obj, "dummy".to_string(), None)));
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
            try!(write_cmd!(w, make_entity(&name[..], pos)));
            try!(write_cmd!(w, players::set(target.clone(), Self::bit_num(), i, None)));
            try!(write_cmd!(w, players::set(target, Self::bit_comp(), 1 << i, None)));
        }

        try!(write_cmd!(w, teams::add(Self::team(), None)));
        try!(write_cmd!(w, teams::join(Self::team(), entities)));

        Ok(())
    }

    fn destroy(&self, _: &Computer, w: &mut Write) -> io::Result<()> {
        for obj in Self::objectives() {
            try!(write_cmd!(w, objectives::remove(obj.to_string())));
        }

        let bit_team_target = Target::Sel(Selector {
            team: Some(SelectorTeam::On(Self::team())),
            ..Selector::entity()
        });
        try!(write_cmd!(w, Command::Kill(bit_team_target)));
        try!(write_cmd!(w, teams::remove(Self::team())));

        Ok(())
    }
}

impl MemInitDestroy {
    fn objectives() -> Vec<String> {
        vec!["MemOp".to_string(),
             "MemAddr".to_string(),
             "MemData".to_string(),
             "MemTag".to_string(),
             "MemMask".to_string()]
    }

    fn mask() -> String {
        "MemMask".to_string()
    }
}

impl InitDestroy for MemInitDestroy {
    fn init(&self, computer: &Computer, w: &mut Write) -> io::Result<()> {
        for obj in Self::objectives() {
            try!(write_cmd!(w, objectives::add(obj, "dummy".to_string(), None)));
        }

        for region in computer.memory.iter() {
            let extent = region.extent();

            let name = fab::mem_name(region);
            let pos = region.origin.as_abs();
            try!(write_cmd!(w, make_entity(&name[..], pos)));

            let mut entities = vec![];
            for comp in fab::mem_components(region) {
                try!(write_cmd!(w, make_entity(&comp.name[..], comp.home)));
                try!(write_cmd!(w, players::set(
                    comp.sel.to_target(), Self::mask(), comp.mask, None)));
                entities.push(comp.sel.into_target());
            }
            let team = fab::mem_team(region);
            try!(write_cmd!(w, teams::add(team.clone(), None)));
            try!(write_cmd!(w, teams::join(team, entities)));

            let clay = "minecraft:stained_hardened_clay".to_string();
            for cmd in commands::safe_fill(extent, clay, None, None, None) {
                try!(write_cmd!(w, cmd));
            }
        }
        Ok(())
    }

    fn destroy(&self, computer: &Computer, w: &mut Write) -> io::Result<()> {
        use commands::Command::Kill;

        for obj in Self::objectives() {
            try!(write_cmd!(w, objectives::remove(obj)));
        }

        for region in computer.memory.iter() {
            let extent = region.extent();

            let sel = fab::mem_selector(region);
            try!(write_cmd!(w, Kill(sel.into_target())));
            try!(write_cmd!(w, Kill(fab::mem_comps_selector(region).into_target())));
            try!(write_cmd!(w, teams::remove(fab::mem_team(region))));

            let air = "minecraft:air".to_string();
            for cmd in commands::safe_fill(extent, air, None, None, None) {
                try!(write_cmd!(w, cmd));
            }
        }
        Ok(())
    }
}

fn make_entity(name: &str, pos: Pos3) -> Command {
    let entity_name = "ArmorStand".to_string();

    let mut data_tag = NbtCompound::new();
    data_tag.insert("CustomName".to_string(), Nbt::String(name.to_string()));
    data_tag.insert("NoGravity".to_string(), Nbt::Byte(1));
    data_tag.insert("Invulnerable".to_string(), Nbt::Byte(1));

    Command::Summon(entity_name, Some(pos), Some(Nbt::Compound(data_tag)))
}
