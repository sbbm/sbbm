// Copyright 2015, Christopher Chambers
// Distributed under the GNU GPL v3. See COPYING for details.

use raw::*;

// TODO: Think about how 'execute' should be exposed.  'particles', for example,
// is much more useful if it can be run globally or as a particular entity.  Is
// there a way to achieve this without massive duplication?  Maybe command types
// could be built up incrementally and have their own 'emit' function that
// finally places them.  Then it would be possible to nest execute commands. But
// getting the syntax to be ergonomic will be challenging.

// achievement
pub trait Achievements {
    #[inline(always)]
    fn give_achievement<A: Value>(&self, achieve: A);

    #[inline(always)]
    fn take_achievement<A: Value>(&self, achieve: A);
}

impl<S : RawSelector> Achievements for S {
    #[inline(always)]
    fn give_achievement<A: Value>(&self, achieve: A) {
        unsafe {
            self.emit_sel();
            emit_named!(achieve);
            asm!("raw achievement give %achieve %sel" :::: "volatile");
        }
    }

    #[inline(always)]
    fn take_achievement<A: Value>(&self, achieve: A) {
        unsafe {
            self.emit_sel();
            emit_named!(achieve);
            asm!("raw achievement take %achieve %sel" :::: "volatile");
        }
    }
}

// ban - Not allowed in command blocks.
// ban-ip - Not allowed in command blocks.
// banlist - Not allowed in command blocks.

// blockdata
pub fn blockdata<X, Y, Z, T>(x: X, y: Y, z: Z, data_tag:T)
    where X : Value, Y : Value, Z : Value, T : Value
{
    unsafe {
        emit_named!(x, y, z, data_tag);
        asm!("raw blockdata %x %y %z %data_tag" :::: "volatile");
    }
}

// clear
pub trait Items {
    #[inline(always)]
    fn detect_items<I, D, T>(&self, item: I, data: D, data_tag: T) -> i32
        where I : Value, D : Value, T : Value
    {
        self.clear_items(item, data, v!(0), data_tag)
    }

    #[inline(always)]
    fn clear_items<I, D, M, T>(
        &self, item: I, data: D, max_count: M, data_tag: T) -> i32
        where I : Value, D : Value, M : Value, T : Value;

    #[inline(always)]
    fn clear_all_items(&self) -> i32;
}

impl<S : RawSelector> Items for S {
    #[inline(always)]
    fn clear_items<I, D, M, T>(
        &self, item: I, data: D, max_count: M, data_tag: T) -> i32
        where I : Value, D : Value, M : Value, T : Value
    {
        let mut count: i32;
        unsafe {
            self.emit_sel();
            emit_named!(item, data, max_count, data_tag);
            asm!("raw~s clear %sel %item %data %data_tag"
                 : "=r"(count)
                 ::: "volatile");
        }
        count
    }

    #[inline(always)]
    fn clear_all_items(&self) -> i32 {
        let mut count: i32;
        unsafe {
            self.emit_sel();
            asm!("raw~s clear %sel"
                 : "=r"(count)
                 ::: "volatile");
        }
        count
    }
}

// clone
#[inline(always)]
pub fn clone<SX1, SY1, SZ1, SX2, SY2, SZ2, X, Y, Z, MM, CM, B>(
    src_x1: SX1, src_y1: SY1, src_z1: SZ1,
    src_x2: SX2, src_y2: SY2, src_z2: SZ2,
    x: X, y: Y, z: Z, mask_mode: MM, clone_mode: CM, block: B) -> i32
    where
        SX1 : Value, SY1 : Value, SZ1 : Value,
        SX2 : Value, SY2 : Value, SZ2 : Value,
        X : Value, Y : Value, Z : Value,
        MM : Value, CM : Value, B : Value
{
    let mut count:i32;
    unsafe {
        emit_named!(
            src_x1, src_y1, src_z1,
            src_x2, src_y2, src_z2,
            x, y, z,
            mask_mode, clone_mode, block);
        asm!(concat!(
            "raw~s $0, clone ",
            "%src_x1 %src_y1 %src_z1 ",
            "%src_x2 %src_y2 %src_z2",
            "%x %y %z ",
            "%mask_mode %clone_mode %block")
             : "=r"(count)
             ::: "volatile");
    }
    count
}


// defaultgamemode
#[inline(always)]
pub fn defaultgamemode<M : Value>(mode: M) {
    unsafe {
        emit_named!(mode);
        asm!("raw defaultgamemode %mode" :::: "volatile");
    }
}

// deop - Now allowed in command blocks.

// difficulty
#[inline(always)]
pub fn difficulty<D : Value>(difficulty: D) {
    unsafe {
        emit_named!(difficulty);
        asm!("raw difficulty %difficulty" :::: "volatile");
    }
}

/// A trait for objects that can have effects applied to them.
///
/// This trait is implemented by default for all standard targets (any object
/// that implements `RawSelector`).
pub trait Effects {
    /// Adds or removes a single effect from a target.
    #[inline(always)]
    fn effect<E, T, M, P>(&self, effect: E, time: T, mult: M, hide_particles: P)
        where E : Value, T : Value, M : Value, P : Value;

    #[inline(always)]
    fn clear_effects(&self);
}

impl<A : RawSelector> Effects for A {
    #[inline(always)]
    fn effect<E, T, M, P>(&self, effect: E, time: T, mult: M, hide_particles: P)
        where E : Value, T : Value, M : Value, P : Value
    {
        unsafe {
            self.emit_sel();
            emit_named!(effect, time, mult, hide_particles);
            asm!("raw effect %sel %effect %time %mult %hide_particles"
                :::: "volatile");
        }
    }

    #[inline(always)]
    fn clear_effects(&self) {
        unsafe {
            self.emit_sel();
            asm!("raw effect %sel clear" :::: "volatile");
        }
    }
}

// enchant
pub trait Enchantments {
    #[inline(always)]
    fn enchant<E : Value, L : Value>(&self, ench: E, level: L) -> i32;
}

impl<S : RawSelector> Enchantments for S {
    #[inline(always)]
    fn enchant<E : Value, L : Value>(&self, ench: E, level: L) -> i32 {
        let mut count: i32;
        unsafe {
            self.emit_sel();
            emit_named!(ench, level);
            asm!("raw~s enchant %sel %ench %level"
                 : "=r"(count)
                 ::: "volatile");
        }
        count
    }
}

// entitydata
pub trait EntityData {
    #[inline(always)]
    fn entitydata<T : Value>(&self, data_tag: T) -> i32;
}

impl<S : RawSelector> EntityData for S {
    #[inline(always)]
    fn entitydata<T : Value>(&self, data_tag: T) -> i32 {
        let mut count: i32;
        unsafe {
            self.emit_sel();
            emit_named!(data_tag);
            asm!("raw~s entitydata %sel %data_tag"
                 : "=r"(count)
                 ::: "volatile");
        }
        count
    }
}

// TODO: Think about how execute should be exposed.
// TODO: execute

#[inline(always)]
pub fn fill<SX1, SY1, SZ1, SX2, SY2, SZ2, X, Y, Z, B, D, A, T>(
    src_x1: SX1, src_y1: SY1, src_z1: SZ1,
    src_x2: SX2, src_y2: SY2, src_z2: SZ2,
    x: X, y: Y, z: Z,
    block: B, data: D, action: A, data_tag: T) -> i32
    where
        SX1 : Value, SY1 : Value, SZ1 : Value,
        SX2 : Value, SY2 : Value, SZ2 : Value,
        X : Value, Y : Value, Z : Value,
        B : Value, D : Value, A : Value, T : Value
{
    let mut count:i32;
    unsafe {
        emit_named!(
            src_x1, src_y1, src_z1,
            src_x2, src_y2, src_z2,
            x, y, z,
            block, data, action, data_tag);
        asm!(concat!(
            "raw~s $0, fill ",
            "%src_x1 %src_y1 %src_z1 ",
            "%src_x2 %src_y2 %src_z2",
            "%x %y %z ",
            "%block %data %action %data_tag")
             : "=r"(count)
             ::: "volatile");
    }
    count
}

#[inline(always)]
pub fn fill_replace<SX1, SY1, SZ1, SX2, SY2, SZ2, X, Y, Z, B, D, NB, ND>(
    src_x1: SX1, src_y1: SY1, src_z1: SZ1,
    src_x2: SX2, src_y2: SY2, src_z2: SZ2,
    x: X, y: Y, z: Z,
    block: B, data: D, new_block: NB, new_data: ND) -> i32
    where
        SX1 : Value, SY1 : Value, SZ1 : Value,
        SX2 : Value, SY2 : Value, SZ2 : Value,
        X : Value, Y : Value, Z : Value,
        B : Value, D : Value, NB : Value, ND : Value
{
    let mut count:i32;
    unsafe {
        emit_named!(
            src_x1, src_y1, src_z1,
            src_x2, src_y2, src_z2,
            x, y, z,
            block, data, new_block, new_data);
        asm!(concat!(
            "raw~s $0, fill ",
            "%src_x1 %src_y1 %src_z1 ",
            "%src_x2 %src_y2 %src_z2",
            "%x %y %z replace ",
            "%block %data %new_block %new_data")
             : "=r"(count)
             ::: "volatile");
    }
    count
}

// gamemode
pub trait GameMode {
    #[inline(always)]
    fn gamemode<M : Value>(&self, mode: M) -> i32;
}

impl<S : RawSelector> GameMode for S {
    #[inline(always)]
    fn gamemode<M : Value>(&self, mode: M) -> i32 {
        let mut count: i32;
        unsafe {
            self.emit_sel();
            emit_named!(mode);
            asm!("raw~s gamemode %mode %sel"
                : "=r"(count)
                ::: "volatile");
        }
        count
    }
}

// gamerule
pub mod gamerule {
    use raw::Value;
    use core::option::Option::{self, None, Some};

    #[inline(always)]
    pub fn get<R: Value>(rule: R) -> Option<i32> {
        let mut success: i32;
        let mut value: i32;
        unsafe {
            emit_named!(rule);
            asm!("raw~sq $0, $1, gamerule %rule"
                : "=r"(success), "=r"(value)
                ::: "volatile");
        }
        if success != 0 { Some(value) } else { None }
    }

    #[inline(always)]
    pub fn set<R : Value, V : Value>(rule: R, value: V) {
        unsafe {
            emit_named!(rule, value);
            asm!("raw gamerule %rule %value" :::: "volatile");
        }
    }

    #[inline(always)]
    pub fn default<R : Value, V : Value>(rule: R, value: V) -> Option<i32> {
        let existing = get(rule);
        if existing.is_none() {
            set(rule, value);
        }
        existing
    }
}


// give
pub trait Give {
    #[inline(always)]
    fn give<I, C, D, T>(&self, item: I, count: C, data: D, data_tag: T) -> i32
        where I : Value, C : Value, D : Value, T : Value;
}

impl<S : RawSelector> Give for S {
    #[inline(always)]
    fn give<I, C, D, T>(&self, item: I, count: C, data: D, data_tag: T) -> i32
        where I : Value, C : Value, D : Value, T : Value
    {
        let mut res_count: i32;
        unsafe {
            self.emit_sel();
            emit_named!(item, count, data, data_tag);
            asm!("raw~s give %sel %item %count %data %data_tag"
                : "=r"(res_count)
                ::: "volatile");
        }
        res_count
    }
}

// help - Not useful in command blocks (probably.  maybe through execute?)
// kick - Not allowed in command blocks

// kill
pub trait Kill {
    #[inline(always)]
    fn kill(&self) -> i32;
}

impl<S : RawSelector> Kill for S {
    #[inline(always)]
    fn kill(&self) -> i32 {
        let mut count: i32;
        unsafe {
            self.emit_sel();
            asm!("raw~s kill %sel"
                : "=r"(count)
                ::: "volatile");
        }
        count
    }
}

// list - Not useful in command blocks (unless success count is returned?)

// TODO: me - consider how to expose 'me' (via execute, probably)

// op - Not allowed in command blocks
// pardon - Not allowed in command blocks

// particle
#[inline(always)]
pub fn particle<N, X, Y, Z, RX, RY, RZ, S, C, M>(
    name: N, x: X, y: Y, z: Z, x_radius: RX, y_radius: RY, z_radius: RZ,
    speed: S, count: C, mode: M)
    where N : Value, X : Value, Y : Value, Z : Value,
          RX : Value, RY : Value, RZ : Value,
          S : Value, C : Value, M : Value
{
    unsafe {
        emit_named!(
            name, x, y, z, x_radius, y_radius, z_radius, speed, count, mode);
        asm!(concat!(
            "raw particle %name %x %y %z %x_radius %y_radius %z_radius ",
            "%speed %count %mode")
             :::: "volatile");
    }
}

// TODO: playsound
pub trait Sound {
    #[inline(always)]
    fn playsound<S, X, Y, Z, V, P, MV>(
        &self, sound: S, x: X, y: Y, z: Z, volume: V, pitch: P, min_vol: MV) -> i32
        where S : Value, X : Value, Y : Value, Z : Value,
              V : Value, P : Value, MV : Value;
}

impl<A : RawSelector> Sound for A {
    #[inline(always)]
    fn playsound<S, X, Y, Z, V, P, MV>(
        &self, sound: S, x: X, y: Y, z: Z, volume: V, pitch: P, min_vol: MV) -> i32
        where S : Value, X : Value, Y : Value, Z : Value,
              V : Value, P : Value, MV : Value
    {
        let mut count: i32;
        unsafe {
            self.emit_sel();
            emit_named!(sound, x, y, z, volume, pitch, min_vol);
            asm!("raw~s playsound %sel %x %y %z %volume %pick %min_vol"
                 : "=r"(count)
                 ::: "volatile");
        }
        count
    }
}

// publish - Not allowed in command blocks.

// TODO: replaceitem
// fn for blocks
// trait for selectors

// save-all - Not allowed in command blocks.
// save-off - Not allowed in command blocks.
// save-on - Not allowed in command blocks.

// say
#[inline(always)]
pub fn say<M : Value>(msg: M) {
    unsafe {
        emit_named!(msg);
        asm!("raw say %msg" :::: "volatile");
    }
}

// seed - Not useful in command blocks?  (Maybe query or success count returns
//        the actual seed?)

// setblock
#[inline(always)]
pub fn setblock<X, Y, Z, B, D, A, T>(
    x: X, y: Y, z: Z, block: B, data: D, action: A, data_tag: T) -> bool
    where
        X : Value, Y : Value, Z : Value,
        B : Value, D : Value, A : Value, T : Value
{
    let mut success: i32;
    unsafe {
        emit_named!(x, y, z, block, data, action, data_tag);
        asm!(concat!(
            "raw~s $0, setblock ",
            "%x %y %z ",
            "%block %data %action %data_tag")
             : "=r"(success)
             ::: "volatile");
    }
    success != 0
}

// setidletimeout - Not allowed in command blocks.

// setworldspawn
#[inline(always)]
pub fn setworldspawn<X, Y, Z>(x: X, y: Y, z: Z) -> bool
    where X : Value, Y : Value, Z : Value
{
    let mut success: i32;
    unsafe {
        emit_named!(x, y, z);
        asm!("raw~s $0, setworldspawn %x %y %z"
             : "=r"(success)
             ::: "volatile");
    }
    success != 0
}

// spawnpoint
pub trait Spawnpoint {
    #[inline(always)]
    fn set_spawnpoint<X, Y, Z>(&self, x: X, y: Y, z: Z) -> i32
        where X : Value, Y : Value, Z : Value;

    #[inline(always)]
    fn set_spawnpoint_here(&self) -> i32;
}

impl<A : RawSelector> Spawnpoint for A {
    #[inline(always)]
    fn set_spawnpoint<X, Y, Z>(&self, x : X, y: Y, z: Z) -> i32
        where X : Value, Y : Value, Z : Value
    {
        let mut count: i32;
        unsafe {
            self.emit_sel();
            emit_named!(x, y, z);
            asm!("raw~s $0, setspawnpoint %sel %x %y %z"
                : "=r"(count)
                ::: "volatile");
        }
        count
    }

    #[inline(always)]
    fn set_spawnpoint_here(&self) -> i32 {
        let mut count: i32;
        unsafe {
            self.emit_sel();
            asm!("raw~s $0, setspawnpoint %sel"
                : "=r"(count)
                ::: "volatile");
        }
        count
    }
}

// TODO: spreadplayers can accept a list of players, not just a selector.
// That should also be exposed.
// spreadplayers
trait SpreadPlayers {
    #[inline(always)]
    fn spreadplayers<X, Z, D, R, T>(
        &self, x: X, z: Z, distance: D, range: R, respect_teams: T) -> i32
        where X : Value, Z : Value, D : Value,
              R : Value, T : Value;
}

impl<A : RawSelector> SpreadPlayers for A {
    #[inline(always)]
    fn spreadplayers<X, Z, D, R, T>(
        &self, x: X, z: Z, distance: D, range: R, respect_teams: T) -> i32
        where X : Value, Z : Value, D : Value,
              R : Value, T : Value
    {
        let mut count: i32;
        unsafe {
            self.emit_sel();
            emit_named!(x, z, distance, range, respect_teams);
            asm!(concat!(
                "raw~s $0, spreadplayers ",
                "%x %z %distance %range %respect_teams %sel")
                : "=r"(count)
                ::: "volatile");
        }
        count
    }
}

// TODO: stats - Think about how (or if) to expose stats.  This would only be
// for some kind of secondary block placement and querying.  All the same stats
// are available for compiled blocks with asm! (raw~{specifiers})

// stop - Not allowed in command blocks.

// summon
#[inline(always)]
pub fn summon<E, X, Y, Z, T>(entity: E, x: X, y: Y, z: Z, data_tag: T) -> bool
    where E : Value, X : Value, Y : Value, Z : Value, T : Value
{
    let mut success: i32;
    unsafe {
        emit_named!(entity, x, y, z, data_tag);
        asm!("raw~s $0, summon %entity %x %y %z %data_tag"
             : "=r"(success)
             ::: "volatile");
    }
    success != 0
}

// tell
// tellraw
pub trait Tell {
    #[inline(always)]
    fn tell<M : Value>(&self, msg: M);

    #[inline(always)]
    fn tellraw<Json : Value>(&self, json: Json);
}

impl<A : RawSelector> Tell for A {
    #[inline(always)]
    fn tell<M : Value>(&self, msg: M) {
        unsafe {
            self.emit_sel();
            emit_named!(msg);
            asm!("raw tell %sel %msg" :::: "volatile");
        }
    }

    #[inline(always)]
    fn tellraw<Json : Value>(&self, json: Json) {
        unsafe {
            self.emit_sel();
            emit_named!(json);
            asm!("raw tellraw %sel %json" :::: "volatile");
        }
    }
}

// testfor
pub trait TestFor {
    #[inline(always)]
    fn testfor<T : Value>(&self, data_tag: T) -> i32;
}

impl<A : RawSelector> TestFor for A {
    #[inline(always)]
    fn testfor<T : Value>(&self, data_tag: T) -> i32 {
        let mut count: i32;
        unsafe {
            self.emit_sel();
            emit_named!(data_tag);
            asm!("raw testfor %sel %data_tag"
                 : "=r"(count)
                 ::: "volatile");
        }
        count
    }
}

// testforblock
// testforblocks
pub trait TestForBlocks {
    #[inline(always)]
    fn testforblock<X, Y, Z, B, V, T>(
        &self, x: X, y: Y, z: Z, block: B, value: V, data_tag: T) -> bool
        where
            X : Value, Y : Value, Z : Value,
            B : Value, V : Value, T : Value;

    #[inline(always)]
    fn testforblocks<SX1, SY1, SZ1, SX2, SY2, SZ2, X, Y, Z, M>(
        &self,
        src_x1: SX1, src_y1: SY1, src_z1: SZ1,
        src_x2: SX2, src_y2: SY2, src_z2: SZ2,
        x: X, y: Y, z: Z, mode: M) -> i32
        where
            SX1 : Value, SY1 : Value, SZ1 : Value,
            SX2 : Value, SY2 : Value, SZ2 : Value,
            X : Value, Y : Value, Z : Value, M : Value;
}

impl<A : RawSelector> TestForBlocks for A {
    #[inline(always)]
    fn testforblock<X, Y, Z, B, V, T>(
        &self, x: X, y: Y, z: Z, block: B, value: V, data_tag: T) -> bool
        where
            X : Value, Y : Value, Z : Value,
            B : Value, V : Value, T : Value
    {
        let mut success: i32;
        unsafe {
            self.emit_sel();
            emit_named!(x, y, z, block, value, data_tag);
            asm!(concat!(
                "raw~s $0, execute %sel ~ ~ ~ testforblock ",
                "%x %y %z %block %value %data_tag")
                 : "=r"(success)
                 ::: "volatile");
        }
        success != 0
    }

    #[inline(always)]
    fn testforblocks<SX1, SY1, SZ1, SX2, SY2, SZ2, X, Y, Z, M>(
        &self,
        src_x1: SX1, src_y1: SY1, src_z1: SZ1,
        src_x2: SX2, src_y2: SY2, src_z2: SZ2,
        x: X, y: Y, z: Z, mode: M) -> i32
        where
            SX1 : Value, SY1 : Value, SZ1 : Value,
            SX2 : Value, SY2 : Value, SZ2 : Value,
            X : Value, Y : Value, Z : Value, M : Value
    {
        let mut matched: i32;
        unsafe {
            self.emit_sel();
            emit_named!(
                src_x1, src_y1, src_z1,
                src_x2, src_y2, src_z2,
                x, y, z, mode);
            asm!(concat!(
                "raw~s $0, execute %sel ~ ~ ~ testforblocks ",
                "%src_x1 %src_y1 %src_z1 %src_x2 %src_y2 %src_z2 ",
                "%x %y %z %mode")
                : "=r"(matched)
                ::: "volatile");
        }
        matched
    }
}

// time
#[allow(non_camel_case_types)]
pub mod time {
    use raw::Value;

    // FIXME: Allow doc comments to apply to these predefs.
    /// The longest time generally allowed in commands that accept a
    /// time: 1 million ticks (~13.8 hours).
    make_value!(pub LONGEST, 1000000);

    #[inline(always)]
    pub fn query<K : Value>(kind: K) -> i32 {
        let mut result: i32;
        unsafe {
            emit_named!(kind);
            asm!("raw~q time query %kind"
                 : "=r"(result)
                 ::: "volatile")
        }
        result
    }

    #[inline(always)]
    pub fn add<V: Value>(value: V) {
        unsafe {
            emit_named!(value);
            asm!("raw time add %value" :::: "volatile");
        }
    }

    #[inline(always)]
    pub fn set<V : Value>(value: V) {
        unsafe {
            emit_named!(value);
            asm!("raw time set %value" :::: "volatile");
        }
    }
}

// title
pub trait Titles {
    #[inline(always)]
    fn title<Json : Value>(&self, json: Json);

    #[inline(always)]
    fn title_times<I, S, O>(&self, fade_in: I, stay: S, fade_out: O)
        where I : Value, S : Value, O : Value;
}

impl<A : RawSelector> Titles for A {
    #[inline(always)]
    fn title<Json : Value>(&self, json: Json) {
        unsafe {
            self.emit_sel();
            emit_named!(json);
            asm!("raw title %sel title %json" :::: "volatile");
        }
    }

    #[inline(always)]
    fn title_times<I, S, O>(&self, fade_in: I, stay: S, fade_out: O)
        where I : Value, S : Value, O : Value
    {
        unsafe {
            self.emit_sel();
            emit_named!(fade_in, stay, fade_out);
            asm!("raw title %sel times %fade_in %stay %fade_out" :::: "volatile");
        }
    }
}

// toggledownfall
#[inline(always)]
pub fn toggledownfall() {
    unsafe {
        asm!("raw toggledownfall" :::: "volatile");
    }
}

// tp
pub trait Teleport {
    // TODO: tp with rotation
    #[inline(always)]
    fn teleport<X, Y, Z>(&self, x: X, y: Y, z: Z) -> i32
        where X : Value, Y : Value, Z : Value;

    #[inline(always)]
    fn teleport_to<S : RawSelector>(&self, sel: S) -> i32;
}

impl<A : RawSelector> Teleport for A {
    #[inline(always)]
    fn teleport<X, Y, Z>(&self, x: X, y: Y, z: Z) -> i32
        where X : Value, Y : Value, Z : Value
    {
        let mut count: i32;
        unsafe {
            self.emit_sel();
            emit_named!(x, y, z);
            asm!("raw~s $0, tp %sel %x %y %z"
                 : "=r"(count)
                 ::: "volatile");
        }
        count
    }

    #[inline(always)]
    fn teleport_to<S : RawSelector>(&self, sel: S) -> i32
    {
        let mut count: i32;
        unsafe {
            sel.emit_sel();
            named!(dest, "%sel").emit_named();
            self.emit_sel();
            asm!("raw~s $0, tp %sel %dest"
                 : "=r"(count)
                 ::: "volatile");
        }
        count
    }
}

// TODO: trigger - Trigger is probably only really useful through
// 'execute', but I haven't figured out how that is going to work yet.

// weather
#[inline(always)]
pub fn weather<K : Value, T : Value>(kind: K, time: T) {
    unsafe {
        emit_named!(kind, time);
        asm!("weather %kind %time" :::: "volatile");
    }
}

// whitelist - Not allowed in command blocks.

// TODO: worldborder - There are a *lot* of variations of worldborder.

// xp
pub trait Experience {
    #[inline(always)]
    fn add_xp<X : Value>(&self, xp: X) -> i32;

    #[inline(always)]
    fn add_levels<L : Value>(&self, levels: L) -> i32;
}

impl<S : RawSelector> Experience for S {
    #[inline(always)]
    fn add_xp<X : Value>(&self, xp: X) -> i32 {
        let mut count: i32;
        unsafe {
            emit_named!(xp);
            asm!("raw xp %xp"
                 : "=r"(count)
                 ::: "volatile");
        }
        count
    }

    #[inline(always)]
    fn add_levels<L : Value>(&self, levels: L) -> i32 {
        let mut count: i32;
        unsafe {
            emit_named!(levels);
            // FIXME: %levelsL will currently expand to 1L, and though
            // this is the desired behavior, it is terrible.  Eventually,
            // the backend should move to a more reliable form of string
            // interpolation. %{levels}L, for example.
            asm!("raw xp %levelsL"
                 : "=r"(count)
                 ::: "volatile");
        }
        count
    }
}
