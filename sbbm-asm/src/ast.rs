#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    LabelStmt(String),
    Instr(Vec<Cond>, Op),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Cond {
    pub reg: Register,
    pub min: Option<i32>,
    pub max: Option<i32>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Op {
    LdrRR(Register, Register),
    LdrRL(Register, String),

    AddRR(Register, Register),
    AddXI(String, String, i32, Register),
    AddXR(String, String, Register, Register),

    SubRR(Register, Register),
    SubXI(String, String, i32, Register),
    SubXR(String, String, Register, Register),

    MovRR(Register, Register),
    MovRI(Register, i32),
    MovRX(Register, String, String),
    MovXR(String, String, Register, Register),
    MovXI(String, String, i32, Register),

    Mul(Register, Register),

    Sdiv(Register, Register),
    Udiv(Register, Register),

    Srng(Register, Register, Option<i32>, Option<i32>),
    Urng(Register, Register, Option<u32>, Option<u32>),

    BrR(Register),
    BrL(String),
    BrLnkR(Register),
    BrLnkL(String),

    Halt,

    RawCmd(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Register {
    Gen(i32),
    Pred(i32),
    Spec(String),
}
