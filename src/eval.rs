use std::fmt::{self, Debug};
use std::path::Path;
use std::sync::atomic::Ordering;

use miri::{MiriEntryFnType, Machine};
use rustc_const_eval::interpret::Frame;
use rustc_const_eval::CTRL_C_RECEIVED;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_middle::mir::{self, Location};
use rustc_middle::ty::TyCtxt;
use rustc_span::{FileName, FileNameDisplayPreference};

use crate::mir::{def_path_res, write_mir_fn};
use crate::repl::ReplCommand;
use crate::{should_hide_stmt, Error};

const NO_BREAKPOINT_ID: usize = 0;

fn source_file_exists(tcx: TyCtxt, file_name: &str) -> bool {
    let file_name = Path::new(file_name);
    let source_map = tcx.sess.source_map();

    for file in source_map.files().iter() {
        if let FileName::Real(ref real_path) = file.name {
            if let Some(real_path) = real_path.local_path() {
                if let Some(name) = real_path.file_name() {
                    if file_name == name {
                        return true;
                    }
                }
            }
        }
    }

    false
}

fn resolve_function_name_to_def_id(tcx: TyCtxt, loc: &str) -> Result<DefId, Error> {
    let split_path: Vec<&str> = loc.split("::").collect();
    let paths = def_path_res(tcx, &split_path);

    // Take the first path we find, this might not be correct as
    // multiple things may share the same path.
    let path = match paths.get(0) {
        Some(path) => path,
        None => return Err(Error::UnknownPath(loc.to_string())),
    };

    match path {
        Res::Def(
            DefKind::Fn | DefKind::Closure | DefKind::GlobalAsm | DefKind::AssocFn,
            def_id,
        ) => Ok(*def_id),
        Res::Err => return Err(Error::UnknownPath(loc.to_string())),
        _ => Err(Error::NotACallable(path.clone())),
    }
}

#[derive(Debug, PartialEq)]
enum BreakPointKind {
    Source { file: String, line: usize },
    Function { def_id: DefId },
}

impl BreakPointKind {
    fn from_str(tcx: TyCtxt, loc: &str) -> Result<Self, Error> {
        if let Some((s_path, line)) = loc
            .split_once(":")
            .and_then(|(file, line)| line.parse::<usize>().ok().map(|line| (file, line)))
        {
            let path = Path::new(s_path);
            let file = match Path::new(path).file_name() {
                Some(file) => {
                    let file_name = file.to_string_lossy().into_owned();
                    if !source_file_exists(tcx, &file_name) {
                        return Err(Error::FileNotFound(s_path.to_string()));
                    }
                    file_name
                }
                None => return Err(Error::FileNotFound(s_path.to_string())),
            };

            return Ok(BreakPointKind::Source { file, line });
        }

        let def_id = resolve_function_name_to_def_id(tcx, loc)?;
        Ok(BreakPointKind::Function { def_id })
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
            BreakPointKind::Source {
                file: file_path,
                line,
            } => f.write_fmt(format_args!("{file_path}:{line}")),
            BreakPointKind::Function { def_id: id } => id.fmt(f),
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
    fn find(&self, kind: BreakPointKind) -> Option<&BreakPoint> {
        self.inner.iter().find(|bp| bp.kind == kind)
    }
}

#[derive(Clone, Copy)]
enum StepResult {
    Break,
    Continue,
    Exited(i32),
}

pub struct Context<'tcx> {
    tcx: TyCtxt<'tcx>,
    ecx: miri::MiriInterpCx<'tcx>,
    bps: BreakPoints,

    entry_id: DefId,
    entry_type: MiriEntryFnType,
}

const PRINT_LINE_COUNT: usize = 9;

fn print_body_mir(body: &mir::Body, sp: Location) {
    let mut stmts_shown = 0;

    // Traverse the basic blocks, skipping to sp.block.
    'printing: for bb in body.basic_blocks.reverse_postorder() {
        if *bb < sp.block {
            continue;
        }

        let stmt_idx = if *bb == sp.block {
            sp.statement_index
        } else {
            Location::START.statement_index
        };

        let bb = &body.basic_blocks[*bb];
        let stmts = &bb.statements[stmt_idx..];

        let mut blck_stmts_shown = 0;
        for (idx, stmt) in stmts.iter().enumerate() {
            if stmts_shown == PRINT_LINE_COUNT {
                break 'printing;
            }

            if should_hide_stmt(stmt) {
                continue;
            }

            println!("{:4} | {:?}", idx + sp.statement_index + 1, stmt);
            stmts_shown += 1;
            blck_stmts_shown += 1;
        }

        // If we have to traverse more than one block, print a separator.
        if stmts_shown < PRINT_LINE_COUNT && blck_stmts_shown > 0 {
            println!("-----+");
        }
    }
}

fn create_ecx<'tcx>(
    tcx: TyCtxt<'tcx>,
    entry_id: DefId,
    entry_type: MiriEntryFnType,
) -> miri::MiriInterpCx<'tcx> {
    miri::create_ecx(
        tcx,
        entry_id,
        entry_type,
        &miri::MiriConfig {
            ignore_leaks: true,
            borrow_tracker: None,
            isolated_op: miri::IsolatedOp::Allow,
            data_race_detector: false,
            weak_memory_emulation: false,
            provenance_mode: miri::ProvenanceMode::Permissive,
            check_alignment: miri::AlignmentCheck::None,
            backtrace_style: miri::BacktraceStyle::Short,
            args: vec![tcx.sess.io.input.filestem().to_string()],
            ..Default::default()
        },
        None
    )
    .unwrap()
}

impl<'tcx> Context<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, entry_id: DefId, entry_type: MiriEntryFnType) -> Self {
        Self {
            tcx,
            ecx: create_ecx(tcx, entry_id, entry_type),
            bps: BreakPoints::new(),
            entry_id,
            entry_type,
        }
    }

    // Define a function to handle the program exit logic
    fn handle_exit(&self, code: i32) {
        if code != 0 {
            self.tcx
                .dcx()
                .warn(format!("Program exited with error code {code}."));
        } else {
            println!("Program exited with code {code}.");
        }
    }

    /// Reset's miri context for re-runs.
    pub fn reset(&mut self) {
        self.ecx = create_ecx(self.tcx, self.entry_id, self.entry_type);
    }

    /// Execute a [`ReplCommand`], returning whether it exited.
    pub fn run_cmd(&mut self, cmd: ReplCommand) -> Result<bool, Error> {
        match cmd {
            ReplCommand::Nexti(()) => match self.step() {
                StepResult::Continue | StepResult::Break => self.print_code(),
                StepResult::Exited(code) => {
                    self.handle_exit(code);
                    return Ok(true);
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
                        self.handle_exit(code);
                        return Ok(true);
                    }
                }
            },
            ReplCommand::Backtrace(()) => self.print_bt(),
            ReplCommand::Break(loc) => {
                let kind = BreakPointKind::from_str(self.tcx, &loc)?;
                if self.bps.add(kind) {
                    println!("Breakpoint '{loc}' already set.");
                } else {
                    println!("Breakpoint set on {loc}.");
                }
            }
            ReplCommand::BreakDelete(loc) => {
                let kind = BreakPointKind::from_str(self.tcx, &loc)?;
                if self.bps.remove(&kind) {
                    println!("Breakpoint {loc} removed.");
                } else {
                    println!("Breakpoint '{loc}' was not set.");
                }
            }
            ReplCommand::List(()) => self.print_code(),
            ReplCommand::Disassemble(loc) => {
                let def_id = resolve_function_name_to_def_id(self.tcx, &loc)?;

                if self.tcx.is_mir_available(def_id) {
                    let body = self.tcx.optimized_mir(def_id);
                    println!("   * +--- {def_id:?} ---+");

                    let mut buf = Vec::new();
                    let _ = write_mir_fn(self.tcx, &body, &mut buf);
                    let out = String::from_utf8(buf).unwrap();

                    println!("     |");
                    for (idx, line) in out.lines().enumerate() {
                        println!("{:4} | {}", idx + 1, line);
                    }
                } else {
                    println!("MIR for {def_id:?} is not available.");
                }
            }
            ReplCommand::Locals(()) => {
                if let Some(frame) = self.frame() {
                    let body = &frame.body();
                    for debug_info in body.var_debug_info.iter() {
                        // This feels like a hack, it'd be better to query the frame's MIR,
                        // backtracking what the local is referring to.
                        if debug_info.name.as_str() == "tmp" {
                            continue;
                        }

                        let ty = match debug_info.value {
                            mir::VarDebugInfoContents::Place(place) => {
                                place.ty(&body.local_decls, self.tcx).ty
                            }
                            mir::VarDebugInfoContents::Const(op) => op.ty(),
                        };

                        println!("({}) {} = {:?}", ty, debug_info.name, debug_info.value);
                    }
                } else {
                    println!("No known frame to display.");
                }
            }
            _ => {}
        }

        Ok(false)
    }

    fn step(&mut self) -> StepResult {
        if CTRL_C_RECEIVED.load(Ordering::Relaxed) {
            CTRL_C_RECEIVED.store(false, Ordering::Relaxed);
            println!();
            return StepResult::Break;
        }

        match self.ecx.step().report_err() {
            Ok(true) => {}
            Ok(false) => return StepResult::Exited(0),
            Err(err) => {
                return match miri::report_error(&self.ecx, err) {
                    Some((return_code, _)) => StepResult::Exited(return_code),
                    None => StepResult::Exited(1),
                }
            }
        }

        // Check breakpoints before each command execution.
        if let Some(frame) = self.frame() {
            let mut is_at_start = true;

            if let Some(loc) = frame.current_loc().left() {
                let bb = &frame.body().basic_blocks[loc.block];

                // If the current statement should be hidden, keep stepping.
                if loc.statement_index != bb.statements.len()
                    && should_hide_stmt(&bb.statements[loc.statement_index])
                {
                    return self.step();
                }

                is_at_start = loc == Location::START;
            }

            if is_at_start {
                let span = frame.current_span();
                let src_loc = self.tcx.sess.source_map().lookup_char_pos(span.lo());
                let file_name = src_loc
                    .file
                    .name
                    .display(FileNameDisplayPreference::Short)
                    .to_string_lossy()
                    .into_owned();

                let kind = BreakPointKind::Source {
                    file: file_name,
                    line: src_loc.line,
                };
                if let Some(bp) = self.bps.find(kind) {
                    println!("   * +--- Hit {bp} ---+");
                    println!("     |");
                    return StepResult::Break;
                }

                let kind = BreakPointKind::Function {
                    def_id: frame.instance().def_id(),
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

    fn frame(&self) -> Option<&Frame<'tcx, miri::Provenance, miri::FrameExtra<'tcx>>> {
        Machine::stack(&self.ecx).last()
    }

    fn print_bt(&self) {
        for frame in Machine::stack(&self.ecx).iter() {
            println!("{:?}", frame.instance().def_id());
        }
    }

    fn print_code(&self) {
        if !self.print_src() {
            if !self.print_mir() {
                println!("No known frame to display.");
            }
        }
    }

    fn print_mir(&self) -> bool {
        if let Some(frame) = self.frame() {
            let sp = frame.current_loc().left().unwrap_or(Location::START);
            print_body_mir(&frame.body(), sp);
            return true;
        }

        false
    }

    fn print_src(&self) -> bool {
        let mut found_lines = false;
        let frame = match self.frame() {
            Some(frame) => frame,
            None => return found_lines,
        };

        let mut spans = if let Some(location) = frame.current_loc().left() {
            let stmt = location.statement_index;
            let block = location.block;
            if stmt == frame.body()[block].statements.len() {
                vec![frame.body()[block].terminator().source_info.span]
            } else {
                vec![frame.body()[block].statements[stmt].source_info.span]
            }
        } else {
            vec![frame.body().span]
        };

        // Get the original macro caller.
        spans.extend(
            spans
                .last()
                .unwrap()
                .macro_backtrace()
                .next()
                .map(|b| b.call_site)
        );

        for span in spans {
            let loc = self.tcx.sess.source_map().lookup_char_pos(span.lo());
            let start_line = if loc.line >= 3 { loc.line - 2 } else { 0 };
            let end_line = loc.line + PRINT_LINE_COUNT;

            let source_file = self
                .tcx
                .sess
                .source_map()
                .get_source_file(&loc.file.name)
                .unwrap();

            for idx in start_line..std::cmp::min(end_line, source_file.count_lines()) {
                if let Some(line) = source_file.get_line(idx) {
                    println!("{:4} | {}", idx + 1, line);
                    found_lines = true;
                }
            }
        }

        found_lines
    }
}
