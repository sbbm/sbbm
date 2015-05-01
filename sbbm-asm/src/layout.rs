use assembler::{AssembledItem, PendingFn};
use assembler::AssembledItem::*;
use core::{Block, Extent, Vec3};
use std::collections::HashMap;
use std::collections::VecDeque;
use std::mem;

pub struct LinearLayout<Source : Iterator<Item=AssembledItem>> {
    input: Source,
    input_done: bool,
    start_pos: Vec3,
    pos: Vec3,
    buffer: VecDeque<(Vec3, Block)>,
    complete_extents: HashMap<String, Extent>,
    active_extents: HashMap<String, Vec3>,
    pending: Vec<(String, Vec3, PendingFn)>,
}

impl<Source : Iterator<Item=AssembledItem>> LinearLayout<Source> {
    pub fn new(start_pos: Vec3, input: Source) -> LinearLayout<Source> {
        LinearLayout {
            input: input,
            input_done: false,
            start_pos: start_pos,
            pos: start_pos,
            buffer: VecDeque::new(),
            complete_extents: HashMap::new(),
            active_extents: HashMap::new(),
            pending: Vec::new(),
        }
    }

    fn advance(&mut self) {
        self.pos.z += 1;
    }

    fn emit(&mut self, block: Block) {
        let pos = self.pos;
        self.emit_at(pos, block);
        self.advance();
    }

    fn emit_at(&mut self, pos: Vec3, block: Block) {
        self.buffer.push_back((pos, block));
    }

    fn add_label(&mut self, label: String) {
        let mut pos = self.pos;
        pos.y -= 1;
        self.active_extents.insert(label, pos);
    }

    fn add_pending(&mut self, label: String, func: PendingFn) {
        match self.resolve_extent(&label) {
            Some(extent) => { self.emit(func(extent)); }
            None => {
                self.pending.push((label, self.pos, func));
                self.advance();
            }
        }
    }

    fn resolve_pending(&mut self) {
        for i in (0..self.pending.len()).rev() {
            match self.resolve_extent(&self.pending[i].0) {
                Some(extent) => {
                    let (_, pos, func) = self.pending.swap_remove(i);
                    self.emit_at(pos, func(extent));
                }
                None => (),
            }
        }
    }

    fn resolve_extent(&self, label: &String) -> Option<Extent> {
        self.complete_extents.get(label).map(|e| *e)
    }

    fn new_line(&mut self) {
        let mut extent_end_pos = self.pos;
        extent_end_pos.y -= 1;
        extent_end_pos.z -= 1;
        // FIXME: Use drain when it is no longer unstable.
        let mut active_extents = HashMap::new();
        mem::swap(&mut self.active_extents, &mut active_extents);
        for (label, start) in active_extents.into_iter() {
            self.complete_extents.insert(label, Extent::MinMax(start, extent_end_pos));
        }

        self.resolve_pending();

        self.pos.x += 1;
        self.pos.z = self.start_pos.z;
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

impl<Source : Iterator<Item=AssembledItem>> Iterator for LinearLayout<Source> {
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
