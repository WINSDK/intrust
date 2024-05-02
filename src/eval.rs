use std::fmt::{self, Debug};
use std::path::Path;

use miri::Machine;
use rustc_const_eval::CTRL_C_RECEIVED;
use rustc_hir::def_id::{DefId, LOCAL_CRATE};
use rustc_middle::mir::{self, Location};
use rustc_middle::ty::TyCtxt;
use rustc_session::config::EntryFnType;
use rustc_span::FileNameDisplayPreference;

use crate::repl::ReplCommand;

fn should_hide_stmt(stmt: &mir::Statement<'_>) -> bool {
    use rustc_middle::mir::StatementKind::*;
    match stmt.kind {
        StorageLive(_) | StorageDead(_) | Nop => true,
        _ => false,
    }
}

const NO_BREAKPOINT_ID: usize = 0;

#[derive(Clone, Copy)]
enum StepResult {
    Break,
    Continue,
    Exited(i64),
}

#[derive(Debug, PartialEq)]
enum BreakPointKind {
    Source { file_path: String, line: usize },
    Function { id: DefId },
}

impl BreakPointKind {
    fn from_str(tcx: &TyCtxt, loc: &str) -> Option<Self> {
        if let Some((file_path, line)) = loc
            .split_once(":")
            .and_then(|(file, line)| line.parse::<usize>().ok().map(|line| (file, line)))
            .filter(|(file, _)| Path::new(&file).exists())
        {
            let kind = BreakPointKind::Source {
                file_path: file_path.to_string(),
                line,
            };
            Some(kind)
        } else if let Some(id) = resolve_function_name_to_def_id(&tcx, &loc) {
            let kind = BreakPointKind::Function { id };
            Some(kind)
        } else {
            None
        }
    }
}

#[derive(Debug)]
struct BreakPoint {
    #[allow(dead_code)]
    id: usize,
    kind: BreakPointKind,
}

impl fmt::Display for BreakPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            BreakPointKind::Source { file_path, line } => {
                f.write_fmt(format_args!("{file_path}:{line}"))
            }
            BreakPointKind::Function { id } => id.fmt(f),
        }
    }
}

struct BreakPoints {
    cur_id: usize,
    inner: Vec<BreakPoint>,
}

impl BreakPoints {
    fn new() -> Self {
        Self {
            cur_id: NO_BREAKPOINT_ID + 1,
            inner: Vec::new(),
        }
    }

    /// Remove a [`BreakPoint`], returning whether it succeeded.
    fn remove(&mut self, kind: &BreakPointKind) -> bool {
        let mut success = false;
        self.inner.retain(|existing| {
            let eq = &existing.kind != kind;
            success |= !eq;
            eq
        });
        success
    }

    /// Add a [`BreakPoint`], returning if it it was already set.
    fn add(&mut self, kind: BreakPointKind) -> bool {
        let exists = self.remove(&kind);
        self.inner.push(BreakPoint {
            id: self.cur_id,
            kind,
        });
        self.cur_id += 1;
        exists
    }

    /// Find a [`BreakPoint`], looks only at it's [`BreakPointKind`].
    fn find(&mut self, kind: BreakPointKind) -> Option<&mut BreakPoint> {
        self.inner.iter_mut().find(|bp| bp.kind == kind)
    }
}

pub struct Context<'mir, 'tcx> {
    tcx: TyCtxt<'tcx>,
    ecx: miri::MiriInterpCx<'mir, 'tcx>,
    bps: BreakPoints,
}

impl<'mir, 'tcx> Context<'mir, 'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, entry_id: DefId, entry_type: EntryFnType) -> Self {
        let ecx = miri::create_ecx(
            tcx,
            entry_id,
            entry_type,
            &miri::MiriConfig {
                ignore_leaks: true,
                borrow_tracker: None,
                isolated_op: miri::IsolatedOp::Allow,
                data_race_detector: false,
                weak_memory_emulation: false,
                check_alignment: miri::AlignmentCheck::None,
                ..Default::default()
            },
        )
        .unwrap();

        Self {
            tcx,
            ecx,
            bps: BreakPoints::new(),
        }
    }

    /// Execute a [`ReplCommand`], returning whether it exited.
    pub fn run_cmd(&mut self, cmd: ReplCommand) -> bool {
        match cmd {
            ReplCommand::Nexti(()) => match self.step() {
                StepResult::Continue | StepResult::Break => self.print_code(),
                StepResult::Exited(code) => {
                    if code != 0 {
                        self.tcx
                            .dcx()
                            .warn(format!("Program exited with error code {code}."));
                    } else {
                        println!("Progeam exited with code {code}.");
                    }
                    return true;
                }
            },
            ReplCommand::Cont(()) => loop {
                match self.step() {
                    StepResult::Continue => continue,
                    StepResult::Break => {
                        self.print_code();
                        break;
                    }
                    StepResult::Exited(code) => {
                        if code != 0 {
                            self.tcx
                                .dcx()
                                .warn(format!("Program exited with error code {code}."));
                        } else {
                            println!("Program exited with code {code}.");
                        }
                        return true;
                    }
                }
            },
            ReplCommand::Backtrace(()) => {
                self.print_bt();
            }
            ReplCommand::Break(loc) => {
                if let Some(kind) = BreakPointKind::from_str(&self.tcx, &loc) {
                    if self.bps.add(kind) {
                        println!("Breakpoint '{loc}' already set.");
                    } else {
                        println!("Breakpoint set on {loc}.");
                    }
                } else {
                    println!("Function '{loc}' not found.");
                }
            }
            ReplCommand::BreakDelete(loc) => {
                if let Some(kind) = BreakPointKind::from_str(&self.tcx, &loc) {
                    if self.bps.remove(&kind) {
                        println!("Breakpoint {loc} removed.");
                    } else {
                        println!("Breakpoint '{loc}' was not set.");
                    }
                } else {
                    println!("Function '{loc}' not found.");
                }
            }
            _ => {}
        }

        false
    }

    fn step(&mut self) -> StepResult {
        if CTRL_C_RECEIVED.load(std::sync::atomic::Ordering::Relaxed) {
            return StepResult::Break;
        }

        match self.ecx.step() {
            Ok(true) => {}
            Ok(false) => return StepResult::Exited(0),
            Err(err) => {
                let (return_code, _) = miri::report_error(&self.ecx, err).unwrap();
                return StepResult::Exited(return_code);
            }
        }

        // Check breakpoints before each command execution
        if let Some(frame) = Machine::stack(&self.ecx).last() {
            let is_at_start = match frame.loc.left() {
                Some(loc) => loc == Location::START,
                None => true,
            };

            if is_at_start {
                let span = frame.current_source_info().unwrap().span;
                let src_loc = self.tcx.sess.source_map().lookup_char_pos(span.lo());
                let file_path = src_loc
                    .file
                    .name
                    .display(FileNameDisplayPreference::Remapped)
                    .to_string_lossy()
                    .into_owned();

                let kind = BreakPointKind::Source {
                    file_path,
                    line: src_loc.line,
                };
                if let Some(bp) = self.bps.find(kind) {
                    println!("{bp:?}");
                    return StepResult::Break;
                }

                let kind = BreakPointKind::Function {
                    id: frame.instance.def_id(),
                };
                if let Some(bp) = self.bps.find(kind) {
                    println!("   * +--- Hit {bp} ---+");
                    println!("     |");
                    return StepResult::Break;
                }
            }
        }

        StepResult::Continue
    }

    fn print_bt(&self) {
        for frame in Machine::stack(&self.ecx).iter() {
            println!("{:?}", frame.instance.def_id());
        }
    }

    const LINE_COUNT: usize = 3;

    fn print_code(&self) {
        if !self.print_src() {
            self.print_mir();
        }
    }

    fn print_mir(&self) {
        let frame = Machine::stack(&self.ecx).last().unwrap();
        let sp = frame.current_loc().left().unwrap_or(Location::START);

        let bb = &frame.body.basic_blocks[sp.block];
        let statements = &bb.statements[sp.statement_index..];
        for (idx, stat) in statements.iter().take(Self::LINE_COUNT).enumerate() {
            println!("{:4} | {:?}", idx + sp.statement_index + 1, stat);
        }
    }

    fn print_src(&self) -> bool {
        let mut found_lines = false;
        let frame = Machine::stack(&self.ecx).last().unwrap();
        if let Some(span) = frame.current_source_info().map(|si| si.span) {
            let loc = self.tcx.sess.source_map().lookup_char_pos(span.lo());
            let start_line = if loc.line >= 3 { loc.line - 1 } else { 0 };
            let end_line = loc.line + Self::LINE_COUNT;

            let source_file = self
                .tcx
                .sess
                .source_map()
                .get_source_file(&loc.file.name)
                .unwrap();

            for line_index in start_line..std::cmp::min(end_line, source_file.count_lines()) {
                if let Some(line) = source_file.get_line(line_index) {
                    println!("{:4} | {}", line_index + 1, line);
                    found_lines = true;
                }
            }
        }

        found_lines
    }
}

fn resolve_function_name_to_def_id(tcx: &TyCtxt, complete_path: &str) -> Option<DefId> {
    let (crate_name, path) = complete_path.split_once("::")?;

    if path.is_empty() || crate_name.is_empty() {
        return None;
    }

    if tcx.crate_name(LOCAL_CRATE).as_str() != crate_name {
        return None;
    }

    // Iterate through all the items in the type context
    tcx.hir()
        .items()
        .filter_map(|item_id| {
            let hir_id = item_id.hir_id();
            tcx.hir().fn_sig_by_hir_id(hir_id).and_then(|_| {
                // Convert the local `HirId` to a global `DefId`
                let def_id = hir_id.owner.to_def_id();

                // Get the identifier of the function
                let ident = tcx.hir().def_path(def_id.expect_local());

                // Check if the identifier matches the function name we're looking for
                if &ident.to_string_no_crate_verbose()[2..] == path {
                    Some(def_id)
                } else {
                    None
                }
            })
        })
        .next() // Return the first match, if any
}
