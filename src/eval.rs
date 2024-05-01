use std::sync::mpsc::Receiver;
use std::path::Path;
use std::fmt::{self, Debug};

use miri::Machine;
use rustc_const_eval::CTRL_C_RECEIVED;
use rustc_span::FileNameDisplayPreference;
use rustc_hir::def_id::{LOCAL_CRATE, DefId};
use rustc_middle::ty::TyCtxt;
use rustc_middle::mir::{self, Location};
use rustc_session::config::EntryFnType;

use crate::repl::ReplCommand;

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
            BreakPointKind::Function { id } => {
                id.fmt(f)
            }
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

    /// Add a [`BreakPoint`], returning if it it was already set.
    fn add(&mut self, kind: BreakPointKind) -> bool {
        let mut exists = false;
        self.inner.retain(|existing| {
            let eq = existing.kind != kind;
            exists |= !eq;
            eq
        });
        self.inner.push(BreakPoint { id: self.cur_id, kind });
        self.cur_id += 1;
        exists
    }

    /// Find a [`BreakPoint`], looks only at it's [`BreakPointKind`].
    fn find(&mut self, kind: BreakPointKind) -> Option<&mut BreakPoint> {
        self.inner.iter_mut().find(|bp| bp.kind == kind)
    }
}

struct Context<'mir, 'tcx> {
    tcx: TyCtxt<'tcx>,
    ecx: miri::MiriInterpCx<'mir, 'tcx>,
    bps: BreakPoints,
}

impl<'mir, 'tcx> Context<'mir, 'tcx> {
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
                Some(loc) => loc ==  Location::START,
                None => true,
            };

            if is_at_start {
                let span = frame.current_source_info().unwrap().span;
                let src_loc = self.tcx.sess.source_map().lookup_char_pos(span.lo());
                let file_path = src_loc.file.name
                    .display(FileNameDisplayPreference::Remapped)
                    .to_string_lossy()
                    .into_owned();

                let kind = BreakPointKind::Source { file_path, line: src_loc.line };
                if let Some(bp) = self.bps.find(kind) {
                    println!("{bp:?}");
                    return StepResult::Break;
                }

                let kind = BreakPointKind::Function { id: frame.instance.def_id() };
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
        for stat in bb.statements[sp.statement_index..].iter().take(Self::LINE_COUNT) {
            println!("{stat:?}");
        }
    }

    fn print_src(&self) -> bool {
        let mut found_lines = false;
        let frame = Machine::stack(&self.ecx).last().unwrap();
        if let Some(span) = frame.current_source_info().map(|si| si.span) {
            let loc = self.tcx.sess.source_map().lookup_char_pos(span.lo());
            let start_line = if loc.line >= 3 { loc.line - 1 } else { 0 };
            let end_line = loc.line + Self::LINE_COUNT;

            let source_file = self.tcx.sess.source_map().get_source_file(&loc.file.name).unwrap();

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

pub fn run<'tcx>(
    rx: Receiver<ReplCommand>,
    tcx: TyCtxt<'tcx>,
    entry_id: DefId,
    entry_type: EntryFnType,
) -> Option<i64> {
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

    let mut ctx = Context { tcx, ecx, bps: BreakPoints::new() };

    for cmd in rx {
        match cmd {
            ReplCommand::Nexti(()) => {
                match ctx.step() {
                    StepResult::Continue | StepResult::Break => ctx.print_code(),
                    StepResult::Exited(code) => {
                        if code != 0 {
                            tcx.dcx().warn(format!("Program exited with error code {code}."));
                        }

                        println!("Program exited with code {code}.");
                        return Some(code);
                    }
                }
            }
            ReplCommand::Continue(()) => {
                loop {
                    match ctx.step() {
                        StepResult::Continue => continue,
                        StepResult::Break => {
                            ctx.print_code();
                            break;
                        },
                        StepResult::Exited(code) => {
                            if code != 0 {
                                tcx.dcx().warn(format!("Program exited with error code {code}."));
                            }

                            println!("Program exited with code {code}");
                            return Some(code);
                        }
                    }
                }
            }
            ReplCommand::Backtrace(()) => {
                ctx.print_bt();
            }
            ReplCommand::Breakpoint(loc) => {
                if let Some((file_path, line)) = loc
                    .split_once(":")
                    .and_then(|(file, line)| line.parse::<usize>().ok().map(|line| (file, line)))
                    .filter(|(file, _)| Path::new(&file).exists())
                {
                    let kind = BreakPointKind::Source { file_path: file_path.to_string(), line };

                    if ctx.bps.add(kind) {
                        println!("Breakpoint already set.");
                    } else {
                        println!("Breakpoint set on function {file_path}:{line}.");
                    }
                } else if let Some(id) = resolve_function_name_to_def_id(&tcx, &loc) {
                    let kind = BreakPointKind::Function { id };
                    if ctx.bps.add(kind) {
                        println!("Breakpoint already set.");
                    } else {
                        println!("Breakpoint set on function {loc}.");
                    }
                } else {
                    println!("Function '{loc}' not found.");
                }
            }
            _ => {}
        }
    }

    None
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
