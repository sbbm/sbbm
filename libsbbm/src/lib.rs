// Copyright 2015, Christopher Chambers
// Distributed under the GNU GPL v3. See COPYING for details.

#![feature(asm, core, lang_items, no_std)]
#![no_std]
#![crate_name = "sbbm"]
#![crate_type = "rlib"]

extern crate core;

#[macro_use]
pub mod macros;
pub mod raw;

mod commands;
mod predef;

pub use commands::*;
pub use predef::*;

use raw::{RawSelector, RawObjective};

pub type SuccessCount = i32;

pub trait Selector {
    fn get<O : RawObjective>(&self, objective: O) -> i32;
    fn set<O : RawObjective>(&self, objective: O, value: i32) -> SuccessCount;
    fn add<O : RawObjective>(&self, objective: O, count: i32) -> SuccessCount;
    fn remove<O : RawObjective>(&self, objective: O, count: i32) -> SuccessCount;
}

impl<T> Selector for T where T : RawSelector {
    fn get<O : RawObjective>(&self, objective: O) -> i32 {
        unsafe {
            self.emit_sel();
            objective.emit_obj();

            let mut value : i32;
            asm!("mov $0, %sel, %obj"
                 : "=r"(value)
                 : // no input
                 : // no clobbers
                 : "volatile");
            value
        }
    }

    fn set<O : RawObjective>(&self, objective: O, value: i32) -> SuccessCount {
        unsafe {
            self.emit_sel();
            objective.emit_obj();

            let mut res : SuccessCount;
            asm!("mov %sel, %obj, $1, $0"
                 : "=r"(res)
                 : "r"(value)
                 : // no clobbers
                 : "volatile");
            res
        }
    }

    fn add<O : RawObjective>(&self, objective: O, count: i32) -> SuccessCount {
        unsafe {
            self.emit_sel();
            objective.emit_obj();

            let mut res : SuccessCount;
            asm!("add %sel, %obj, $1, $0"
                 : "=r"(res)
                 : "r"(count)
                 : // no clobbers
                 : "volatile");
            res
        }
    }

    fn remove<O : RawObjective>(&self, objective: O, count: i32) -> SuccessCount {
        unsafe {
            self.emit_sel();
            objective.emit_obj();

            let mut res : SuccessCount;
            asm!("sub %sel, %obj, $1, $0"
                 : "=r"(res)
                 : "r"(count)
                 : // no clobbers
                 : "volatile");
            res
        }
    }
}

pub trait Objective {
    /// Adds this objective to the scoreboard.
    ///
    /// # Examples
    ///
    /// ```
    /// objective!(Stamina, "dummy");
    /// Stamina.add();
    /// ```
    fn add(&self);
    /// Removes this objective from the scoreboard.
    ///
    /// # Examples
    ///
    /// ```
    /// objective!(Stamina, "dummy");
    /// Stamina.remove();
    /// ```
    fn remove(&self);
}

impl<T> Objective for T where T : RawObjective {
    fn add(&self) {
        unsafe {
            self.emit_obj();
            self.emit_crit();
            self.emit_disp();
            asm!("raw scoreboard objectives add %obj %crit %disp"
                 :::: "volatile");
        }
    }

    fn remove(&self) {
        unsafe {
            self.emit_obj();
            asm!("raw scoreboard objectives remove %obj"
                 :::: "volatile");
        }
    }
}

// TODO: trait DisplaySlot

#[lang = "stack_exhausted"]
extern fn stack_exhausted() { }

#[lang = "eh_personality"]
extern fn eh_personality() { }

#[lang = "panic_fmt"]
fn panic_fmt() -> ! { loop { } }
