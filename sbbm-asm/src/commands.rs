use types::{Pos3, Interval, Vec3};
use nbt::Nbt;
use std::collections::HashMap;
use std::fmt;

pub type Objective = String;
pub type Team = String;
pub type DisplaySlot = String;
pub type BlockId = String;
pub type BlockData = i32;

#[derive(Clone, Debug, PartialEq)]
pub enum Target {
    Sel(Selector),
    Name(String),
    Raw(String),
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Target::*;

        match *self {
            Sel(ref sel) => sel.fmt(f),
            Name(ref s) | Raw(ref s) => f.write_str(&s[..]),
        }
    }
}

// REVIEW: Consider a separate module for Selector.  It is substantial.
#[derive(Clone, Debug, PartialEq)]
pub struct Selector {
    pub kind: SelectorKind,
    pub pos: Option<Vec3>,
    pub radius: Option<Interval<i32>>,
    pub game_mode: Option<i32>,
    pub count: Option<i32>,
    pub level: Option<Interval<i32>>,
    pub scores: HashMap<String, Interval<i32>>,
    pub team: Option<SelectorTeam>,
    pub name: Option<SelectorName>,
    pub volume: Option<Vec3>,
    pub rot_x: Option<Interval<f32>>,
    pub rot_y: Option<Interval<f32>>,
    pub entity_type: Option<SelectorEntityType>,
}

impl Selector {

    pub fn player() -> Selector {
        Selector { kind: SelectorKind::Player, ..Default::default() }
    }

    pub fn random() -> Selector {
        Selector { kind: SelectorKind::Random, ..Default::default() }
    }

    pub fn all() -> Selector {
        Selector { kind: SelectorKind::All, ..Default::default() }
    }

    pub fn entity() -> Selector {
        Selector { kind: SelectorKind::Entity, ..Default::default() }
    }
}

impl Default for Selector {
    fn default() -> Selector {
        Selector {
            kind: SelectorKind::Player,
            pos: None,
            radius: None,
            game_mode: None,
            count: None,
            level: None,
            scores: HashMap::new(),
            team: None,
            name: None,
            volume: None,
            rot_x: None,
            rot_y: None,
            entity_type: None,
        }
    }
}

impl fmt::Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::SelectorKind::*;

        try!(write!(f, "@"));
        try!(write!(f, "{}", match self.kind {
            Player => 'p',
            Random => 'r',
            All =>    'a',
            Entity => 'e',
        }));

        type WriteFn = fn(&Selector, &mut fmt::Formatter) -> Result<(), fmt::Error>;
        let mut writers: Vec<WriteFn> = vec!();

        if self.pos.is_some()         { writers.push(Self::write_pos); }
        if self.radius.is_some()      { writers.push(Self::write_radius); }
        if self.game_mode.is_some()   { writers.push(Self::write_game_mode); }
        if self.count.is_some()       { writers.push(Self::write_count); }
        if self.level.is_some()       { writers.push(Self::write_level); }
        if self.team.is_some()        { writers.push(Self::write_team); }
        if self.name.is_some()        { writers.push(Self::write_name); }
        if self.volume.is_some()      { writers.push(Self::write_volume); }
        if self.rot_x.is_some()       { writers.push(Self::write_rot_x); }
        if self.rot_y.is_some()       { writers.push(Self::write_rot_y); }
        if self.entity_type.is_some() { writers.push(Self::write_entity_type); }

        let has_args = !writers.is_empty() || !self.scores.is_empty();
        if has_args {
            try!(write!(f, "["));
        }

        let mut first = true;
        for writer in writers {
            if !first {
                try!(write!(f, ","));
            }
            first = false;
            try!(writer(self, f));
        }

        for (name, interval) in self.scores.iter() {
            if !first {
                try!(write!(f, ","));
            }
            first = false;
            let min_name = format!("score_{}_min", name);
            let max_name = format!("score_{}", name);
            try!(Self::write_interval(interval, &min_name[..], &max_name[..], f));
        }

        if has_args {
            try!(write!(f, "]"));
        }

        Ok(())
    }
}

impl Selector {
    fn write_interval<T>(
        interval: &Interval<T>, min_name: &str, max_name: &str,
        f: &mut fmt::Formatter) -> Result<(), fmt::Error>
        where T : PartialOrd, T : fmt::Display
    {
        use types::Interval::*;

        match *interval {
            Min(ref min) => write!(f, "{}={}", min_name, min),
            Max(ref max) => write!(f, "{}={}", max_name, max),
            Bounded(ref min, ref max) =>
                write!(f, "{}={},{}={}", min_name, min, max_name, max),
        }
    }

    fn write_pos(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let pos = self.pos.as_ref().unwrap();
        write!(f, "x={},y={},z={}", pos.x, pos.y, pos.z)
    }

    fn write_radius(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        Self::write_interval(self.radius.as_ref().unwrap(), "rm", "r", f)
    }

    fn write_game_mode(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "m={}", self.game_mode.unwrap())
    }

    fn write_count(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "c={}", self.count.unwrap())
    }

    fn write_level(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        Self::write_interval(self.level.as_ref().unwrap(), "lm", "l", f)
    }

    fn write_team(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.team.as_ref().unwrap())
    }

    fn write_name(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name.as_ref().unwrap())
    }

    fn write_volume(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let volume = self.volume.as_ref().unwrap();
        write!(f, "dx={},dy={},dz={}", volume.x, volume.y, volume.z)
    }

    fn write_rot_x(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        Self::write_interval(self.rot_x.as_ref().unwrap(), "rxm", "rx", f)
    }

    fn write_rot_y(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        Self::write_interval(self.rot_y.as_ref().unwrap(), "rym", "ry", f)
    }

    fn write_entity_type(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.entity_type.as_ref().unwrap())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SelectorKind {
    Player,
    Random,
    All,
    Entity,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelectorTeam {
    On(Team),
    NotOn(Team),
    Unaffiliated,
}

impl fmt::Display for SelectorTeam {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::SelectorTeam::*;
        try!(write!(f, "team="));
        match *self {
            On(ref team) => { try!(write!(f, "{}", team)); }
            NotOn(ref team) => { try!(write!(f, "!{}", team)); }
            Unaffiliated => { },
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelectorName {
    Is(String),
    IsNot(String),
}

impl fmt::Display for SelectorName {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::SelectorName::*;
        try!(write!(f, "name="));
        match *self {
            Is(ref name) => try!(write!(f, "{}", name)),
            IsNot(ref name) => try!(write!(f, "!{}", name)),
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelectorEntityType {
    Is(String),
    IsNot(String),
}

impl fmt::Display for SelectorEntityType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::SelectorEntityType::*;

        try!(write!(f, "type="));
        match *self {
            Is(ref name) => try!(write!(f, "{}", name)),
            IsNot(ref name) => try!(write!(f, "!{}", name)),
        }
        Ok(())
    }
}

pub enum Command {
    Execute(Target, Pos3, Box<Command>),
    ExecuteDetect(Target, Pos3, Pos3, BlockId, BlockData, Box<Command>),
    // FIXME: Option<Nbt> should be Option<NbtCompound>
    Fill(Pos3, Pos3, BlockId, Option<BlockData>, Option<FillAction>, Option<Nbt>),
    FillReplace(Pos3, Pos3, BlockId, BlockData, Option<BlockId>, Option<BlockData>),
    Kill(Target),
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
            Execute(ref tgt, ref pos, ref cmd) =>
                write!(f, "execute {} {} {}", tgt, pos, cmd),
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
            Kill(ref tgt) => write!(f, "kill {}", tgt),
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
    List(Option<Target>),
    // FIXME: Option<Nbt> should be Option<NbtCompound>
    Set(Target, Objective, i32, Option<Nbt>),
    // FIXME: Option<Nbt> should be Option<NbtCompound>
    Add(Target, Objective, i32, Option<Nbt>),
    // FIXME: Option<Nbt> should be Option<NbtCompound>
    Remove(Target, Objective, i32, Option<Nbt>),
    Reset(Target, Option<Objective>),
    Enable(Target, Objective),
    Test(Target, Objective, Option<i32>, Option<i32>),
    Operation(Target, Objective, PlayerOp, Target, Objective),
}

impl fmt::Display for PlayerCmd {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::PlayerCmd::*;

        match *self {
            Set(ref tgt, ref obj, ref value, ref data_tag) => {
                try!(write!(f, "set {} {} {}", tgt, obj, value));
                if let Some(ref data_tag) = *data_tag {
                    try!(write!(f, " {}", data_tag));
                }
                Ok(())
            }
            Remove(ref tgt, ref obj, ref count, ref data_tag) => {
                try!(write!(f, "remove {} {} {}", tgt, obj, count));
                if let Some(ref data_tag) = *data_tag {
                    try!(write!(f, " {}", data_tag));
                }
                Ok(())
            }
            Operation(ref ltgt, ref lobj, ref op, ref rtgt, ref robj) => {
                write!(f, "operation {} {} {} {} {}", ltgt, lobj, op, rtgt, robj)
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
    use super::{Command, PlayerCmd, PlayerOp, Target, Objective};
    use super::PlayerCmd::*;
    use nbt::Nbt;

    fn make_cmd(cmd: PlayerCmd) -> Command {
        Command::Scoreboard(super::ScoreboardCmd::Players(cmd))
    }

    pub fn list(tgt: Option<Target>) -> Command {
        make_cmd(List(tgt))
    }

    pub fn set(tgt: Target, obj: Objective, value: i32, tag: Option<Nbt>) -> Command {
        make_cmd(Set(tgt, obj, value, tag))
    }

    pub fn add(tgt: Target, obj: Objective, count: i32, tag: Option<Nbt>) -> Command {
        make_cmd(Add(tgt, obj, count, tag))
    }

    pub fn remove(tgt: Target, obj: Objective, count: i32, tag: Option<Nbt>) -> Command {
        make_cmd(Remove(tgt, obj, count, tag))
    }

    pub fn reset(tgt: Target, obj: Option<Objective>) -> Command {
        make_cmd(Reset(tgt, obj))
    }

    pub fn enable(tgt: Target, trigger: Objective) -> Command {
        make_cmd(Enable(tgt, trigger))
    }

    pub fn test(tgt: Target, obj: Objective, min: Option<i32>, max: Option<i32>) -> Command {
        make_cmd(Test(tgt, obj, min, max))
    }

    pub fn op(
        tgt_lhs: Target, obj_lhs: Objective, op: PlayerOp,
        tgt_rhs: Target, obj_rhs: Objective) -> Command
    {
        make_cmd(Operation(tgt_lhs, obj_lhs, op, tgt_rhs, obj_rhs))
    }

    macro_rules! op_impl {
        ($name:ident, $op:ident) => {
            pub fn $name(
                tgt_lhs: Target, obj_lhs: Objective,
                tgt_rhs: Target, obj_rhs: Objective) -> Command
            {
                op(tgt_lhs, obj_lhs, PlayerOp::$op, tgt_rhs, obj_rhs)
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
    Join(Team, Vec<Target>),
    JoinAll(Team),
    Leave(Option<Team>, Vec<Target>),
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
            Join(ref team, ref targets) => {
                try!(write!(f, "join {}", team));
                for tgt in targets.into_iter() {
                    try!(write!(f, " {}", tgt));
                }
                Ok(())
            }
            _ => unimplemented!(),
        }
    }
}

pub mod teams {
    use super::{Command, TeamCmd, Target, Team};
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

    pub fn join(team: Team, targets: Vec<Target>) -> Command {
        make_cmd(Join(team, targets))
    }

    pub fn join_all(team: Team) -> Command {
        make_cmd(JoinAll(team))
    }

    pub fn leave(team: Option<Team>, targets: Vec<Target>) -> Command {
        make_cmd(Leave(team, targets))
    }

    pub fn leave_all(team: Option<Team>) -> Command {
        make_cmd(LeaveAll(team))
    }

    pub fn color(team: Team, color: String) -> Command {
        make_cmd(Color(team, color))
    }
}
