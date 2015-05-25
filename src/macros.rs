/// Prepares the execution environment and then hands off execution to

/// the passed entry function.
///
/// This macro should be called exactly once with the name of your 'main'
/// function.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate sbbm;
///
/// bootstrap!(main);
///
/// fn main() {
///     say(v!("Hello, SBBM!"));
/// }
/// ```
#[macro_export]
macro_rules! bootstrap {
    ($main:ident) =>  {
        #[no_mangle]
        pub extern fn _start() -> ! {
            #[inline(never)]
            fn shim() {
                $main();
            }
            // TODO: Decide how to communicate the desired location of
            // the stack pointer to the sbbm-asm.
            unsafe { asm!("mov sp, #32784" :::: "volatile") };
            shim();
            unsafe { asm!("halt" :::: "volatile"); }
            loop { }
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! def_asm {
    ($def:ident, $value:expr) => {
        asm!(concat!("def ", stringify!($def), ", ", $value) :::: "volatile");
    }
}

/// Defines an inline `Value` that can be passed to Minecraft commands.
///
/// Command blocks require most values they use to be known at the time
/// of the command block's creation.  Because of this restriction,
/// ordinary variables are not allowed for most command parameters.  So,
/// the `v!` macro provides a convenient way to define a static value.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern create sbbm;
///
/// use sbbm::say;
///
/// bootstrap!(main);
/// fn main() {
///     say(v!("This string is a Value."));
/// }
/// ```
#[macro_export]
macro_rules! v {
    ($value:expr) => {{
        make_value!(V, $value);
        V
    }}
}

macro_rules! named {
    ($def:ident, $value:expr) => {{
        #[derive(Copy, Clone)]
        struct V;
        impl $crate::raw::NamedValue for V {
            #[inline(always)]
            unsafe fn emit_named(&self) {
                def_asm!($def, $value);
            }
        }
        V
    }}
}

macro_rules! emit_named {
    ($($var:ident),+) => {
        $(
            $var.emit_value();
            $crate::raw::NamedValue::emit_named(&named!($var, "%value"));
        )+
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! decl_def {
    (pub $name:ident) => {
        #[derive(Copy, Clone)]
        pub struct $name;
    };
    ($name:ident) => {
        #[derive(Copy, Clone)]
        struct $name;
    };
}


#[doc(hidden)]
#[macro_export]
macro_rules! impl_value {
    ($name:ident, $value:expr) => {
        impl $crate::raw::Value for $name {
            #[inline(always)]
            unsafe fn emit_value(&self) {
                def_asm!(value, $value);
            }
        }
    }
}

/// Defines a named `Value` for later use.
///
/// The `make_value!` macro is similar to `v!`, except that the `Value`
/// produced has a name that can be referred to later, whereas `Value`s
/// produced by `v!` must be used immediately.  `make_value!` is useful
/// for defining reusable constants.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate sbbm;
///
/// use sbbm::selectors::ALL_PLAYERS;
/// use sbbm::Titles;
///
/// make_value!(FADE_IN, 60);
/// make_value!(STAY, 100);
/// make_value!(FADE_OUT, 60);
///
/// make_value!(INTRO, "It was the best of times, it was the worst of times.");
///
/// bootstrap!(main);
/// fn main() {
///     ALL_PLAYERS.title_times(FADE_IN, STAY, FADE_OUT);
///     ALL_PLAYERS.title(INTRO);
/// }
/// ```
#[macro_export]
macro_rules! make_value {
    (pub $name:ident, $value:expr) => {
        decl_def!(pub $name);
        impl_value!($name, $value);
    };
    ($name:ident, $value:expr) => {
        decl_def!($name);
        impl_value!($name, $value);
    };
}

/// Defines a selector.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate sbbm;
///
/// selector!(RED_TEAM, "@a[team=Red]");
/// ```
#[macro_export]
macro_rules! selector {
    ($name:ident, $selector:expr) => {
        #[derive(Copy, Clone)]
        // REVIEW: Is it possible to pass attributes through?
        // allow(dead_code), in particular, would be nice to let
        // people choose for themselves.
        #[allow(non_camel_case_types)]
        struct $name;

        impl $crate::raw::RawSelector for $name {
            #[inline(always)]
            unsafe fn emit_sel(&self) {
                def_asm!(sel, $selector);
            }
        }
    }
}

/// Defines multiple selectors.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate sbbm;
///
/// selectors! {
///     RED_TEAM: "@a[team=Red]";
///     BLUE_TEAM: "@a[team=Blue]";
///     GREEN_TEAM: "@a[team=Green]";
/// }
/// ```
#[macro_export]
macro_rules! selectors {
    () => ();

    ($name:ident: $selector:expr; $($rest:tt)*) => {
        selector!($name, $selector);
        selectors!($($rest)*);
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! obj_decl {
    ($obj_ty:ident, $name:expr, $criteria:expr, $disp_str:expr) => {
        #[derive(Copy, Clone)]
        // REVIEW: Is it possible to pass attributes through?
        // allow(dead_code), in particular, would be nice to let
        // people choose for themselves.
        //#[allow(dead_code)]
        struct $obj_ty;

        impl $crate::raw::RawObjective for $obj_ty {
            #[inline(always)]
            unsafe fn emit_obj(&self) {
                def_asm!(obj, $name);
            }

            #[inline(always)]
            unsafe fn emit_crit(&self) {
                def_asm!(crit, $criteria);
            }

            #[inline(always)]
            unsafe fn emit_disp(&self) {
                def_asm!(disp, $disp_str);
            }
        }
    }
}

/// Defines an objective.
///
/// Minecraft requires objective names to be 16 characters or fewer.
/// This is not validated by the `objective!` macro, and will fail
/// silently if a longer name is used.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate sbbm;
///
/// objective!(Foo, "dummy");
/// objective!(Kills, "stat.mobKills", "Number of mobs killed.");
/// ```
#[macro_export]
macro_rules! objective {
    ($name:ident, $criteria:expr) => {
        obj_decl!($name, stringify!($name), $criteria, "");
    };
    ($name:ident, $criteria:expr, $display_name:expr) => {
        obj_decl!($name, stringify!($name), $criteria, $display_name);
    };
}

/// Defines multiple objectives.
///
/// Minecraft requires objective names to be 16 characters or fewer.
/// This is not validated by the `objectives!` macro, and will fail
/// silently if a longer name is used.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate sbbm;
///
/// objectives! {
///     Foo: "dummy";
///     Kills: "stat.mobKills", "Number of mobs killed.";
/// }
/// ```
#[macro_export]
macro_rules! objectives {
    () => ();

    ($name:ident: $criteria:expr, $display_name:expr; $($rest:tt)*) => {
        objective!($name, $criteria, $display_name);
        objectives!($($rest)*);
    };

    ($name:ident: $criteria:expr; $($rest:tt)*) => {
        objective!($name, $criteria);
        objectives!($($rest)*);
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! sbbm_iter_obj_str {
    () => {
        // iter_{module}_{line}_{column}
        concat!("iter_",
                module_path!(),
                "_",
                line!(),
                "_",
                column!());
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! sbbm_sel_cur_str {
    // @e[score_{iter}_min=1,score_{iter}=1]
    () => {
        concat!("@e[score_",
                sbbm_iter_obj_str!(),
                "_min=1,score_",
                sbbm_iter_obj_str!(),
                "=1]")
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! sbbm_sel_next_str {
    // "@r[score_{iter}_min=0,score_{iter}=0]"
    () => {
        concat!("@r[score_",
                sbbm_iter_obj_str!(),
                "_min=0,score_",
                sbbm_iter_obj_str!(),
                "=0]")
        }
}

/// Creates a random-order iterator over the set of entities in a selector.
///
/// Uses the `@r` selector to choose the next entity until all entities
/// have been processed.
///
/// When possible, avoid using `sel_iter!`, and operate directly on the
/// source selector.  If you just need to perform primitive operations
/// on a selector (e.g. add the same number to all), then it is much
/// more efficient to do it directly:  `ALL_PLAYERS.add(Score, 1)`.
/// `sel_iter!` is provided for cases where individual processing is
/// needed on each entity in a selector.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate sbbm;
///
/// use sbbm::commands::Tell;
/// use sbbm::selectors::ALL_PLAYERS;
///
/// pub fn main() {
///     for player in sel_iter!(ALL_PLAYERS) {
///         player.tell(v!("private message"));
///     }
/// }
/// ```
#[macro_export]
macro_rules! sel_iter {
    ($sel:ident) => {{
        obj_decl!(Iter, sbbm_iter_obj_str!(), "dummy", "");
        selector!(Cur, sbbm_sel_cur_str!());
        selector!(Next, sbbm_sel_next_str!());

        impl ::core::iter::Iterator for Iter {
            type Item = Cur;
            fn next(&mut self) -> ::core::option::Option<Cur> {
                Cur.add(*self, 1);
                if Next.set(*self, 1) > 0 {
                    ::core::option::Option::Some(Cur)
                } else {
                    self.remove();
                    ::core::option::Option::None
                }
            }
        }
        Iter.add();
        $sel.set(Iter, 0);
        Iter
    }}
}
