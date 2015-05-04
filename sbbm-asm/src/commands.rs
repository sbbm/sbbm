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

pub mod objectives {
    use super::{Command, DisplaySlot, ObjCmd, Objective};
    use super::ObjCmd::*;

    fn make_cmd(cmd: ObjCmd) -> Command {
        Command::Scoreboard(super::ScoreboardCmd::Objectives(cmd))
    }

    pub fn list() -> Command {
        make_cmd(List)
    }

    pub fn add(obj: Objective, criteria: String, display_name: Option<String>) -> Command {
        make_cmd(Add(obj, criteria, display_name))
    }

    pub fn remove(obj: Objective) -> Command {
        make_cmd(Remove(obj))
    }

    pub fn set_display(slot: DisplaySlot, obj: Option<Objective>) -> Command {
        make_cmd(SetDisplay(slot, obj))
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

pub mod players {
    use super::{Command, PlayerCmd, PlayerOp, Selector, Objective};
    use super::PlayerCmd::*;
    use nbt::Nbt;

    fn make_cmd(cmd: PlayerCmd) -> Command {
        Command::Scoreboard(super::ScoreboardCmd::Players(cmd))
    }

    pub fn list(sel: Option<Selector>) -> Command {
        make_cmd(List(sel))
    }

    pub fn set(sel: Selector, obj: Objective, value: i32, tag: Option<Nbt>) -> Command {
        make_cmd(Set(sel, obj, value, tag))
    }

    pub fn add(sel: Selector, obj: Objective, count: i32, tag: Option<Nbt>) -> Command {
        make_cmd(Add(sel, obj, count, tag))
    }

    pub fn remove(sel: Selector, obj: Objective, count: i32, tag: Option<Nbt>) -> Command {
        make_cmd(Remove(sel, obj, count, tag))
    }

    pub fn reset(sel: Selector, obj: Option<Objective>) -> Command {
        make_cmd(Reset(sel, obj))
    }

    pub fn enable(sel: Selector, trigger: Objective) -> Command {
        make_cmd(Enable(sel, trigger))
    }

    pub fn test(sel: Selector, obj: Objective, min: Option<i32>, max: Option<i32>) -> Command {
        make_cmd(Test(sel, obj, min, max))
    }

    pub fn op(
        sel_lhs: Selector, obj_lhs: Objective, op: PlayerOp,
        sel_rhs: Selector, obj_rhs: Objective) -> Command
    {
        make_cmd(Operation(sel_lhs, obj_lhs, op, sel_rhs, obj_rhs))
    }

    macro_rules! op_impl {
        ($name:ident, $op:ident) => {
            pub fn $name(
                sel_lhs: Selector, obj_lhs: Objective,
                sel_rhs: Selector, obj_rhs: Objective) -> Command
            {
                op(sel_lhs, obj_lhs, PlayerOp::$op, sel_rhs, obj_rhs)
            }
        }
    }

    op_impl!(add_op, Add);
    op_impl!(sub_op, Sub);
    op_impl!(mul_op, Mul);
    op_impl!(div_op, Div);
    op_impl!(rem_op, Rem);
    op_impl!(asn_op, Asn);
    op_impl!(min_op, Min);
    op_impl!(max_op, Max);
    op_impl!(swp_op, Swp);
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

pub mod teams {
    use super::{Command, TeamCmd, Selector, Team};
    use super::TeamCmd::*;

    fn make_cmd(cmd: TeamCmd) -> Command {
        Command::Scoreboard(super::ScoreboardCmd::Teams(cmd))
    }

    pub fn list(team: Option<Team>) -> Command {
        make_cmd(List(team))
    }

    pub fn add(team: Team, display_name: Option<String>) -> Command {
        make_cmd(Add(team, display_name))
    }

    pub fn remove(team: Team) -> Command {
        make_cmd(Remove(team))
    }

    pub fn empty(team: Team) -> Command {
        make_cmd(Empty(team))
    }

    pub fn join(team: Team, sels: Vec<Selector>) -> Command {
        make_cmd(Join(team, sels))
    }

    pub fn join_all(team: Team) -> Command {
        make_cmd(JoinAll(team))
    }

    pub fn leave(team: Option<Team>, sels: Vec<Selector>) -> Command {
        make_cmd(Leave(team, sels))
    }

    pub fn leave_all(team: Option<Team>) -> Command {
        make_cmd(LeaveAll(team))
    }

    pub fn color(team: Team, color: String) -> Command {
        make_cmd(Color(team, color))
    }
}
