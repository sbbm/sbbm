use types::Pos3;
use nbt::Nbt;
use std::fmt;

pub type Selector = String;
pub type Objective = String;
pub type Team = String;
pub type DisplaySlot = String;
pub type BlockId = String;
pub type BlockData = i32;

pub enum Command {
    Execute(Selector, Pos3, Box<Command>),
    ExecuteDetect(Selector, Pos3, Pos3, BlockId, BlockData, Box<Command>),
    // FIXME: Option<Nbt> should be Option<NbtCompound>
    Fill(Pos3, Pos3, BlockId, Option<BlockData>, Option<FillAction>, Option<Nbt>),
    FillReplace(Pos3, Pos3, BlockId, BlockData, Option<BlockId>, Option<BlockData>),
    Kill(Selector),
    Say(String),
    // FIXME: Option<Nbt> should be Option<NbtCompound>
    SetBlock(Pos3, BlockId, Option<BlockData>, Option<SetBlockAction>, Option<Nbt>),
    Scoreboard(ScoreboardCmd),
    Summon(String, Option<Pos3>, Option<Nbt>),
    Raw(String),
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Command::*;

        match *self {
            Execute(ref sel, ref pos, ref cmd) =>
                write!(f, "execute {} {} {}", sel, pos, cmd),
            Fill(
                ref min, ref max, ref block_id, ref block_data, ref action,
                ref data_tag) =>
                {
                    try!(write!(
                        f, "fill {} {} {} {} {}",
                        min, max, block_id, block_data.unwrap_or(0),
                        action.unwrap_or(FillAction::Replace)));

                    if let Some(ref data_tag) = *data_tag {
                        try!(write!(f, " {}", data_tag));
                    }
                    Ok(())
                }
            Kill(ref sel) => write!(f, "kill {}", sel),
            Say(ref msg) => write!(f, "say {}", msg),
            SetBlock(
                ref pos, ref block_id, ref block_data, ref action, ref data_tag) =>
                {
                    try!(write!(
                        f, "setblock {} {} {} {}",
                        pos, block_id, block_data.unwrap_or(0),
                        action.unwrap_or(SetBlockAction::Replace)));

                    if let Some(ref data_tag) = *data_tag {
                        try!(write!(f, " {}", data_tag));
                    }
                    Ok(())
                }
            Scoreboard(ref cmd) => write!(f, "scoreboard {}", cmd),
            Summon(ref name, ref pos, ref data_tag) => {
                try!(write!(f, "summon {}", name));
                if let Some(ref pos) = *pos {
                    try!(write!(f, " {}", pos));
                }
                if let Some(ref data_tag) = *data_tag {
                    try!(write!(f, " {}", data_tag));
                }
                Ok(())
            }
            Raw(ref raw) => write!(f, "{}", raw),
            _ => unimplemented!()
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum SetBlockAction {
    Destroy,
    Keep,
    Replace,
}

impl fmt::Display for SetBlockAction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::SetBlockAction::*;

        f.write_str(match *self {
            Destroy => "destroy",
            Keep => "keep",
            Replace => "replace",
        })
    }
}

#[derive(Copy, Clone, Debug)]
pub enum FillAction {
    Destroy,
    Hollow,
    Keep,
    Outline,
    Replace,
}

impl fmt::Display for FillAction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::FillAction::*;
        f.write_str(match *self {
            Destroy => "destroy",
            Hollow => "hollow",
            Keep => "keep",
            Outline => "outline",
            Replace => "replace",
        })
    }
}

pub enum ScoreboardCmd {
    Objectives(ObjCmd),
    Players(PlayerCmd),
    Teams(TeamCmd),
}

impl fmt::Display for ScoreboardCmd {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::ScoreboardCmd::*;

        match *self {
            Objectives(ref cmd) => write!(f, "objectives {}", cmd),
            Players(ref cmd) => write!(f, "players {}", cmd),
            Teams(ref cmd) => write!(f, "teams {}", cmd),
        }
    }
}

pub enum ObjCmd {
    List,
    Add(Objective, String, Option<String>),
    Remove(Objective),
    SetDisplay(DisplaySlot, Option<Objective>),
}

impl fmt::Display for ObjCmd {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::ObjCmd::*;

        match *self {
            List => write!(f, "list"),
            Add(ref obj, ref criteria, ref disp) => {
                try!(write!(f, "add {} {}", obj, criteria));
                if let Some(ref disp) = *disp {
                    write!(f, " {}", disp)
                } else {
                    Ok(())
                }
            }
            Remove(ref obj) => write!(f, "remove {}", obj),
            SetDisplay(ref slot, ref obj) => {
                try!(write!(f, "setdisplay {}", slot));
                if let Some(ref obj) = *obj {
                    write!(f, " {}", obj)
                } else {
                    Ok(())
                }
            }
        }
    }
}

pub enum PlayerCmd {
    List(Option<Selector>),
    // FIXME: Option<Nbt> should be Option<NbtCompound>
    Set(Selector, Objective, i32, Option<Nbt>),
    // FIXME: Option<Nbt> should be Option<NbtCompound>
    Add(Selector, Objective, i32, Option<Nbt>),
    // FIXME: Option<Nbt> should be Option<NbtCompound>
    Remove(Selector, Objective, i32, Option<Nbt>),
    Reset(Selector, Option<Objective>),
    Enable(Selector, Objective),
    Test(Selector, Objective, Option<i32>, Option<i32>),
    Operation(Selector, Objective, PlayerOp, Selector, Objective),
}

impl fmt::Display for PlayerCmd {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::PlayerCmd::*;

        match *self {
            Set(ref sel, ref obj, ref value, ref data_tag) => {
                try!(write!(f, "set {} {} {}", sel, obj, value));
                if let Some(ref data_tag) = *data_tag {
                    try!(write!(f, " {}", data_tag));
                }
                Ok(())
            }
            Remove(ref sel, ref obj, ref count, ref data_tag) => {
                try!(write!(f, "remove {} {} {}", sel, obj, count));
                if let Some(ref data_tag) = *data_tag {
                    try!(write!(f, " {}", data_tag));
                }
                Ok(())
            }
            Operation(ref lsel, ref lobj, ref op, ref rsel, ref robj) => {
                write!(f, "operation {} {} {} {} {}", lsel, lobj, op, rsel, robj)
            }
            _ => unimplemented!(),
        }
    }
}

pub enum PlayerOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Asn,
    Min,
    Max,
    Swp,
}

impl fmt::Display for PlayerOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::PlayerOp::*;

        write!(f, "{}", match *self {
            Add => "+=",
            Sub => "-=",
            Mul => "*=",
            Div => "/=",
            Rem => "%=",
            Asn => "=",
            Min => "<",
            Max => ">",
            Swp => "><",
        })
    }
}

pub enum TeamCmd {
    List(Option<Team>),
    Add(Team, Option<String>),
    Remove(Team),
    Empty(Team),
    Join(Team, Vec<Selector>),
    JoinAll(Team),
    Leave(Option<Team>, Vec<Selector>),
    LeaveAll(Option<Team>),
    Color(Team, String),
    // TODO: friendlyfire, setFriendlyInvisibles, nametagVisibility
}

impl fmt::Display for TeamCmd {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::TeamCmd::*;

        match *self {
            Add(ref team, ref disp_name) => {
                try!(write!(f, "add {}", team));
                if let Some(ref disp_name) = *disp_name {
                    try!(write!(f, " {}", disp_name));
                }
                Ok(())
            }
            Remove(ref team) => write!(f, "remove {}", team),
            Join(ref team, ref selectors) => {
                try!(write!(f, "join {}", team));
                for sel in selectors.into_iter() {
                    try!(write!(f, " {}", sel));
                }
                Ok(())
            }
            _ => unimplemented!(),
        }
    }
}
