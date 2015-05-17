use commands::{Target, Objective};
use types::Interval;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    LabelStmt(String),
    Instr(Vec<Cond>, Op),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Cond {
    pub reg: Register,
    pub interval: Interval<i32>
}

impl Cond {
    pub fn new(reg: Register, interval: Interval<i32>) -> Cond {
        Cond { reg: reg, interval: interval }
    }

    pub fn eq(reg: Register, value: i32) -> Cond {
        Self::new(reg, Interval::Bounded(value, value))
    }

    pub fn lt(reg: Register, value: i32) -> Cond {
        Self::new(reg, Interval::Max(value - 1))
    }

    pub fn le(reg: Register, value: i32) -> Cond {
        Self::new(reg, Interval::Max(value))
    }

    pub fn gt(reg: Register, value: i32) -> Cond {
        Self::new(reg, Interval::Min(value + 1))
    }

    pub fn ge(reg: Register, value: i32) -> Cond {
        Self::new(reg, Interval::Min(value))
    }

    pub fn bounded(reg: Register, min: i32, max: i32) -> Cond {
        Self::new(reg, Interval::Bounded(min, max))
    }
}

#[derive(Clone, Debug, PartialEq)]
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
    AddRX(Register, Target, Objective, Register),
    AddXI(Target, Objective, i32, Register),
    AddXR(Target, Objective, Register, Register),
    AddXX(Target, Objective, Target, Objective, Register),

    SubRR(Register, Register),
    SubRI(Register, i32),
    SubRX(Register, Target, Objective, Register),
    SubXI(Target, Objective, i32, Register),
    SubXR(Target, Objective, Register, Register),
    SubXX(Target, Objective, Target, Objective, Register),

    AndRR(Register, Register),
    OrrRR(Register, Register),
    EorRR(Register, Register),

    AsrRR(Register, Register),
    AsrRI(Register, u8),
    LslRR(Register, Register),
    LslRI(Register, u8),
    LsrRR(Register, Register),
    LsrRI(Register, u8),

    MovRR(Register, Register),
    MovRI(Register, i32),
    MovRX(Register, Target, Objective),
    MovXR(Target, Objective, Register, Register),
    MovXI(Target, Objective, i32, Register),
    MovXX(Target, Objective, Target, Objective, Register),

    MulRR(Register, Register),
    MulRX(Register, Target, Objective, Register),
    MulXR(Target, Objective, Register, Register),

    SdivRR(Register, Register),
    SdivRX(Register, Target, Objective, Register),
    SdivXR(Target, Objective, Register, Register),
    UdivRR(Register, Register),
    UdivRX(Register, Target, Objective, Register),
    UdivXR(Target, Objective, Register, Register),

    SremRR(Register, Register),
    SremRX(Register, Target, Objective, Register),
    SremXR(Target, Objective, Register, Register),
    UremRR(Register, Register),
    UremRX(Register, Target, Objective, Register),
    UremXR(Target, Objective, Register, Register),

    Srng(Register, Register, Option<i32>, Option<i32>),
    Urng(Register, Register, Option<u32>, Option<u32>),

    BrR(Register),
    BrL(String),
    BrLnkR(Register),
    BrLnkL(String),
    BrInd(Register),
    BRLnkInd(Register),

    Halt,

    RawCmd(Vec<(CommandBlockOut, Register)>, String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CommandBlockOut {
    SuccessCount,
    AffectedBlocks,
    AffectedEntities,
    AffectedItems,
    QueryResult,
}

impl CommandBlockOut {
    pub fn selector(&self) -> &str {
        use self::CommandBlockOut::*;
        match *self {
            SuccessCount => "SuccessCountName",
            AffectedBlocks => "AffectedBlocksName",
            AffectedEntities => "AffectedEntitiesName",
            AffectedItems => "AffectedItemsName",
            QueryResult => "QueryResultName",
        }
    }

    pub fn objective(&self) -> &str {
        use self::CommandBlockOut::*;
        match *self {
            SuccessCount => "SuccessCountObjective",
            AffectedBlocks => "AffectedBlocksObjective",
            AffectedEntities => "AffectedEntitiesObjective",
            AffectedItems => "AffectedItemsObjective",
            QueryResult => "QueryResultObjective",
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Register {
    Gen(i32),
    Pred(i32),
    Spec(String),
}
