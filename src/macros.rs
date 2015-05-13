#[macro_export]
macro_rules! sbbm_def {
    ($name:ident, $value:expr) => {
        asm!(concat!("def ", stringify!($name), ", ", $value) :::: "volatile");
    }
}

#[macro_export]
macro_rules! d {
    ($name:ident, $value:expr) => {{
        #[derive(Copy, Clone)]
        struct D;
        impl ::sbbm::RawDef for D {
            #[inline(always)]
            unsafe fn emit(&self) {
                sbbm_def!($name, $value);
            }
        }
        D
    }}
}

#[macro_export]
macro_rules! sbbm_sel_decl {
    ($name:ident, $selector:expr) => {
        #[derive(Copy, Clone)]
        // REVIEW: Is it possible to pass attributes through?
        // allow(dead_code), in particular, would be nice to let
        // people choose for themselves.
        #[allow(non_camel_case_types)]
        struct $name;

        impl ::sbbm::RawSelector for $name {
            #[inline(always)]
            unsafe fn emit_sel(&self) {
                asm!(concat!("def sel, ", $selector) :::: "volatile");
            }
        }

        impl ::sbbm::RawDef for $name {
            #[inline(always)]
            unsafe fn emit(&self) {
                asm!(concat!("def sel, ", $selector) :::: "volatile");
            }
        }
    }
}

#[macro_export]
macro_rules! sbbm_obj_decl {
    ($obj_ty:ident, $name:expr, $criteria:expr, $disp_str:expr) => {
        #[derive(Copy, Clone)]
        // REVIEW: Is it possible to pass attributes through?
        // allow(dead_code), in particular, would be nice to let
        // people choose for themselves.
        //#[allow(dead_code)]
        struct $obj_ty;

        impl ::sbbm::RawObjective for $obj_ty {
            #[inline(always)]
            unsafe fn emit_obj(&self) {
                asm!(concat!("def obj, ", $name) :::: "volatile");
            }

            #[inline(always)]
            unsafe fn emit_crit(&self) {
                asm!(concat!("def crit, ", $criteria) :::: "volatile");
            }

            #[inline(always)]
            unsafe fn emit_disp(&self) {
                asm!(concat!("def disp, ", $disp_str) :::: "volatile");
            }
        }
    }
}

#[macro_export]
macro_rules! sbbm_objective {
    ($name:ident, $criteria:expr) => {
        sbbm_obj_decl!($name, stringify!($name), $criteria, "");
    };
    ($name:ident, $criteria:expr, $display_name:expr) => {
        sbbm_obj_decl!($name, stringify!($name), $criteria, $display_name);
    };
}

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

#[macro_export]
macro_rules! sel_iter {
    ($sel:ident) => {{
        sbbm_obj_decl!(Iter, sbbm_iter_obj_str!(), "dummy", "");
        sbbm_sel_decl!(Cur, sbbm_sel_cur_str!());
        sbbm_sel_decl!(Next, sbbm_sel_next_str!());

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
