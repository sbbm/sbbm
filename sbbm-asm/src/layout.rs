use assembler::{AssembledItem, PendingFn};
use assembler::AssembledItem::*;
use types::{Block, Extent, Vec3};

use std::collections::HashMap;
use std::collections::VecDeque;
use std::mem;

pub trait LayoutMotion {
    fn advance(&mut self);
    fn punctuate(&mut self);
    fn terminate(&mut self);
    fn pos(&self) -> Vec3;
    fn power_pos(&self) -> Vec3;
}

pub struct LinearMotion {
    start: Vec3,
    pos: Vec3,
}

impl LinearMotion {
    pub fn new(start: Vec3) -> LinearMotion {
        LinearMotion {
            start: start,
            pos: start,
        }
    }
}

impl LayoutMotion for LinearMotion {
    fn advance(&mut self) {
        self.pos.z += 1;
    }

    fn punctuate(&mut self) {
        // do nothing.
    }

    fn terminate(&mut self) {
        self.pos.x += 2;
        self.pos.z = self.start.z;
    }

    fn pos(&self) -> Vec3 {
        self.pos
    }

    fn power_pos(&self) -> Vec3 {
        let mut pos = self.pos;
        pos.y -= 1;
        pos
    }
}

pub struct PackedMotion {
    start: Vec3,
    pos: Vec3,
    dir: u8,
    level: u8,
}

impl PackedMotion {
    pub fn new(start: Vec3) -> PackedMotion {
        PackedMotion {
            start: start,
            pos: Vec3::new(start.x + 1, start.y, start.z),
            dir: 0,
            level: 0,
        }
    }
}

impl LayoutMotion for PackedMotion {
    fn advance(&mut self) {
        if self.dir == 3 {
            self.pos.z += 1;
        }
        self.dir += 1;
        self.dir %= 4;
    }

    fn punctuate(&mut self) {
        if self.dir != 0 {
            self.pos.z += 1;
            self.dir = 0;
        }
    }

    fn terminate(&mut self) {
        // TODO: A very tight packing is possible, but it's not important now.
        //       In order to really fill a 3d space, a bin-packing component
        //       will probably be needed.  It will take a little bit of thought
        //       to pack plus-shaped bins.
        if self.pos.z - self.start.z <= 14 {
            self.punctuate();
        } else {
            self.pos.x += 2;
            self.level = (self.level + 1) % 2;
            if self.level == 0 {
                self.pos.y -= 1;
            } else {
                self.pos.y += 1;
            }
            self.pos.z = self.start.z;
            self.dir = 0;
        }
    }

    fn pos(&self) -> Vec3 {
        let mut pos = self.pos;
        match self.dir {
            0 => { pos.x -= 1 }
            1 => { pos.x += 1 }
            2 => { pos.y -= 1 }
            3 => { pos.y += 1 }
            _ => { unreachable!() }
        }
        pos
    }

    fn power_pos(&self) -> Vec3 {
        self.pos
    }
}

pub struct Layout<Source>
    where Source : Iterator<Item=AssembledItem>
{
    input: Source,
    input_done: bool,
    motion: Box<LayoutMotion>,
    buffer: VecDeque<(Vec3, Block)>,
    complete_extents: HashMap<String, Extent>,
    active_extents: HashMap<String, Extent>,
    pending: Vec<(String, Vec3, PendingFn)>,
}

impl<Source> Layout<Source>
    where Source : Iterator<Item=AssembledItem>
{
    pub fn new(motion: Box<LayoutMotion>, input: Source) -> Layout<Source> {
        Layout {
            input: input,
            input_done: false,
            motion: motion,
            buffer: VecDeque::new(),
            complete_extents: HashMap::new(),
            active_extents: HashMap::new(),
            pending: Vec::new(),
        }
    }

    pub fn get_power_extent(&self, label: &str) -> Option<Extent> {
        self.complete_extents.get(label).map(|e| *e)
    }

    fn update_active_extents(&mut self) {
        let power_pos = self.motion.power_pos();
        for (_, extent) in self.active_extents.iter_mut() {
            extent.add(power_pos)
        }
    }

    fn emit(&mut self, block: Block) {
        let pos = self.motion.pos();
        self.emit_raw(pos, block);
        self.update_active_extents();
        self.motion.advance();
    }

    fn emit_raw(&mut self, pos: Vec3, block: Block) {
        self.buffer.push_back((pos, block));
    }

    fn add_label(&mut self, label: String) {
        self.motion.punctuate();
        let pos = self.motion.power_pos();
        let extent = Extent::MinMax(pos, pos);
        self.active_extents.insert(label, extent);
    }

    fn add_pending(&mut self, label: String, func: PendingFn) {
        match self.resolve_extent(&label) {
            Some(extent) => { self.emit(func(extent)); }
            None => {
                self.pending.push((label, self.motion.pos(), func));
                self.update_active_extents();
                self.motion.advance();
            }
        }
    }

    fn resolve_pending(&mut self) -> bool {
        let mut made_progress = false;
        for i in (0..self.pending.len()).rev() {
            match self.resolve_extent(&self.pending[i].0) {
                Some(extent) => {
                    let (_, pos, func) = self.pending.swap_remove(i);
                    self.emit_raw(pos, func(extent));
                    made_progress = true;
                }
                None => (),
            }
        }
        made_progress
    }

    fn resolve_extent(&self, label: &String) -> Option<Extent> {
        self.complete_extents.get(label).map(|e| *e)
    }

    fn new_line(&mut self) {
        // FIXME: Use drain when it is no longer unstable.
        let mut active_extents = HashMap::new();
        mem::swap(&mut self.active_extents, &mut active_extents);
        for (label, extent) in active_extents.into_iter() {
            self.complete_extents.insert(label, extent);
        }

        self.resolve_pending();
        self.motion.terminate();
    }

    fn layout_item(&mut self, item: AssembledItem) {
        match item {
            Label(label) => { self.add_label(label); }
            Complete(block) => { self.emit(block); }
            Pending(label, func) => { self.add_pending(label, func); }
            Terminal => { self.new_line(); }
        }
    }
}

// FIXME: Come up with a better idiom here.  It's nice to leave Layout alive
// after iteration so get_power_extent can be used, but having to do:
//     for .. in &mut layout { }
// is awkward.
impl<'a, Source> Iterator for &'a mut Layout<Source>
    where Source : Iterator<Item=AssembledItem>
{
    type Item = (Vec3, Block);

    fn next(&mut self) -> Option<(Vec3, Block)> {
        while self.buffer.is_empty() {
            if !self.input_done {
                match self.input.next() {
                    Some(item) => { self.layout_item(item); }
                    None => {
                        self.input_done = true;
                        self.new_line();
                    }
                }
            } else if !self.pending.is_empty() {
                if !self.resolve_pending() {
                    // If pending blocks are all that remain and no progress was
                    // made, we are in trouble.
                    // FIXME: Report errors in a nicer way.
                    let pending_labels: Vec<&str> =
                        self.pending.iter().map(|x| &x.0[..]).collect();
                    panic!("There are unresolved labels: {:?}", pending_labels);
                }
            } else {
                break;
            }
        }

        self.buffer.pop_front()
    }
}
