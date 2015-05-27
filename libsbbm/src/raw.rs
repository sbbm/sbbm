// Copyright 2015, Christopher Chambers
// Distributed under the GNU GPL v3. See COPYING for details.

use core::marker::Copy;
use core::clone::Clone;

pub trait Value : Copy+Clone {
    #[inline(always)]
    unsafe fn emit_value(&self);
}

pub trait NamedValue : Copy+Clone {
    #[inline(always)]
    unsafe fn emit_named(&self);
}

pub trait RawObjective : Copy+Clone {
    #[inline(always)]
    unsafe fn emit_obj(&self);
    #[inline(always)]
    unsafe fn emit_crit(&self);
    #[inline(always)]
    unsafe fn emit_disp(&self);
}

pub trait RawSelector : Copy+Clone {
    #[inline(always)]
    unsafe fn emit_sel(&self);
}
