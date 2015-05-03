#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    LabelStmt(String),
    Instr(Vec<Cond>, Op),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Cond {
    Min(Register, i32),
    Max(Register, i32),
    Range(Register, i32, i32),
}

impl Cond {
    // TODO: Return Result<Cond, {SomeErrorType}> instead of Option<Cond>
    pub fn new(reg: Register, min: Option<i32>, max: Option<i32>) -> Option<Cond> {
        match (min, max) {
            (Some(min), Some(max)) => Some(Cond::Range(reg, min, max)),
            (Some(min), None) => Some(Cond::Min(reg, min)),
            (None, Some(max)) => Some(Cond::Max(reg, max)),
            (None, None) => None,
        }
    }

    pub fn eq(reg: Register, value: i32) -> Cond {
        Cond::Range(reg, value, value)
    }

    pub fn lt(reg: Register, value: i32) -> Cond {
        Cond::Max(reg, value - 1)
    }

    pub fn le(reg: Register, value: i32) -> Cond {
        Cond::Max(reg, value)
    }

    pub fn gt(reg: Register, value: i32) -> Cond {
        Cond::Min(reg, value + 1)
    }

    pub fn ge(reg: Register, value: i32) -> Cond {
        Cond::Min(reg, value)
    }

    pub fn reg<'a>(&'a self) -> &'a Register {
        match *self {
            Cond::Min(ref r, _) |
            Cond::Max(ref r, _) |
            Cond::Range(ref r, _, _) => &r
        }
    }

    pub fn reg_mut<'a>(&'a mut self) -> &'a mut Register {
        match *self {
            Cond::Min(ref mut r, _) |
            Cond::Max(ref mut r, _) |
            Cond::Range(ref mut r, _, _) => r
        }
    }

    pub fn min(&self) -> Option<i32> {
        match *self {
            Cond::Min(_, min) | Cond::Range(_, min, _) => Some(min),
            Cond::Max(_, _) => None,
        }
    }

    pub fn max(&self) -> Option<i32> {
        match *self {
            Cond::Max(_, max) | Cond::Range(_, _, max) => Some(max),
            Cond::Min(_, _) => None,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Op {
    LdrRR(Register, Register),
    LdrhRR(Register, Register),
    LdrhsRR(Register, Register),
    LdrbRR(Register, Register),
    LdrbsRR(Register, Register),

    LdrRL(Register, String),
    LdrhRL(Register, String),
    LdrhsRL(Register, String),
    LdrbRL(Register, String),
    LdrbsRL(Register, String),

    StrRR(Register, Register),
    StrhRR(Register, Register),
    StrbRR(Register, Register),

    StrRL(Register, String),
    StrhRL(Register, String),
    StrbRL(Register, String),

    Push(Register),
    Pop(Register),

    AddRR(Register, Register),
    AddRI(Register, i32),
    AddRX(Register, String, String, Register),
    AddXI(String, String, i32, Register),
    AddXR(String, String, Register, Register),
    AddXX(String, String, String, String, Register),

    SubRR(Register, Register),
    SubRI(Register, i32),
    SubRX(Register, String, String, Register),
    SubXI(String, String, i32, Register),
    SubXR(String, String, Register, Register),
    SubXX(String, String, String, String, Register),

    And(Register, Register),
    Orr(Register, Register),
    Eor(Register, Register),

    AsrRR(Register, Register),
    AsrRI(Register, u8),
    LslRR(Register, Register),
    LslRI(Register, u8),
    LsrRR(Register, Register),
    LsrRI(Register, u8),

    MovRR(Register, Register),
    MovRI(Register, i32),
    MovRX(Register, String, String),
    MovXR(String, String, Register, Register),
    MovXI(String, String, i32, Register),
    MovXX(String, String, String, String, Register),

    MulRR(Register, Register),
    MulRX(Register, String, String, Register),
    MulXR(String, String, Register, Register),

    SdivRR(Register, Register),
    SdivRX(Register, String, String, Register),
    SdivXR(String, String, Register, Register),
    UdivRR(Register, Register),
    UdivRX(Register, String, String, Register),
    UdivXR(String, String, Register, Register),

    SremRR(Register, Register),
    SremRX(Register, String, String, Register),
    SremXR(String, String, Register, Register),
    UremRR(Register, Register),
    UremRX(Register, String, String, Register),
    UremXR(String, String, Register, Register),

    Srng(Register, Register, Option<i32>, Option<i32>),
    Urng(Register, Register, Option<u32>, Option<u32>),

    BrR(Register),
    BrL(String),
    BrLnkR(Register),
    BrLnkL(String),
    BrInd(Register),
    BRLnkInd(Register),

    Halt,

    RawCmd(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Register {
    Gen(i32),
    Pred(i32),
    Spec(String),
}
