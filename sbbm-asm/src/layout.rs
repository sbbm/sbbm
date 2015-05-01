use assembler::{AssembledItem, PendingFn};
use assembler::AssembledItem::*;
use core::{Block, Extent, Vec3};
use std::collections::HashMap;
use std::collections::VecDeque;
use std::mem;

pub trait LayoutMotion {
    fn advance(&mut self);
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

pub struct Layout<Motion, Source>
    where Motion : LayoutMotion,
          Source : Iterator<Item=AssembledItem> {
    input: Source,
    input_done: bool,
    motion: Motion,
    buffer: VecDeque<(Vec3, Block)>,
    complete_extents: HashMap<String, Extent>,
    active_extents: HashMap<String, Extent>,
    pending: Vec<(String, Vec3, PendingFn)>,
}

impl<Motion, Source> Layout<Motion, Source>
    where Motion : LayoutMotion,
          Source : Iterator<Item=AssembledItem> {
    pub fn new(motion: Motion, input: Source) -> Layout<Motion, Source> {
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

    fn resolve_pending(&mut self) {
        for i in (0..self.pending.len()).rev() {
            match self.resolve_extent(&self.pending[i].0) {
                Some(extent) => {
                    let (_, pos, func) = self.pending.swap_remove(i);
                    self.emit_raw(pos, func(extent));
                }
                None => (),
            }
        }
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

impl<Motion, Source> Iterator for Layout<Motion, Source>
    where Motion : LayoutMotion,
          Source : Iterator<Item=AssembledItem> {
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
                // FIXME: Check that progress is being made.
                self.resolve_pending();
            } else {
                break;
            }
        }

        self.buffer.pop_front()
    }
}
