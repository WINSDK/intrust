#![feature(rustc_private)]

extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_const_eval;
extern crate rustc_span;
extern crate rustc_errors;

mod eval;
mod repl;
mod mir;
mod error;

use std::process::Command;
use rustc_driver::Compilation;
use rustc_session::config::CrateType;
use rustc_hir::def::Res;
use rustc_middle::mir::{Statement, StatementKind};
use rustc_middle::ty::TyCtxt;
use rustc_session::EarlyDiagCtxt;
use rustc_session::config::ErrorOutputType;
use clap::{ValueHint, ArgAction, Parser};

#[derive(Debug)]
pub enum Error {
    NotACallable(Res),
    UnknownPath(String),
    FileNotFound(String),
}

#[derive(Debug, Parser)]
struct Cli {
    /// Commands to be scheduled when program runs.
    #[clap(short, long, action = ArgAction::Append)]
    exec: Vec<String>,

    /// Rustc arguments.
    #[clap(trailing_var_arg = true, value_hint = ValueHint::CommandWithArguments)]
    rustc: Vec<String>,
}

pub const DEFAULT_ARGS: &[&str] = &[
    "-Zalways-encode-mir",
    // Deduplicating diagnostics means we miss events when tracking what happens during an
    // execution. Let's not do that.
    "-Zdeduplicate-diagnostics=no",
];

fn main() {
    let cli = Box::leak(Box::new(Cli::parse()));
    let mut args = Vec::new();

    // Executable path.
    args.push(std::env::args().next().unwrap());

    // Any arguments that have to be passed through.
    args.extend(std::mem::take(&mut cli.rustc));

    let sysroot_flag = String::from("--sysroot");
    if !args.contains(&sysroot_flag) {
        create_sysroot();
        let sysroot = find_sysroot();
        args.push(sysroot_flag);
        args.push(sysroot);
    }

    for arg in DEFAULT_ARGS {
        let arg: String = arg.to_string();
        if !args.contains(&arg) {
            args.push(arg);
        }
    }

    let early_dcx = EarlyDiagCtxt::new(ErrorOutputType::default());

    // Install the ctrlc handler that sets `rustc_const_eval::CTRL_C_RECEIVED`.
    rustc_driver::install_ctrlc_handler();

    // Add an ICE bug report hook.
    rustc_driver::install_ice_hook("https://careful.observer", |_| {});
    rustc_driver::init_rustc_env_logger(&early_dcx);

    run_compiler(args, &mut IntrustCompilerCalls { cli });
}

/// Execute a compiler with the given CLI arguments and callbacks.
fn run_compiler(
    args: Vec<String>,
    callbacks: &mut (dyn rustc_driver::Callbacks + Send),
) -> ! {
    // Invoke compiler, and handle return code.
    let exit_code = rustc_driver::catch_with_exit_code(move || {
        rustc_driver::run_compiler(&args, callbacks)
    });
    std::process::exit(exit_code)
}

fn find_sysroot() -> String {
    // From cargo-miri/src/utils.rs.
    let user_dirs = directories::ProjectDirs::from("org", "rust-lang", "miri").unwrap();
    let cache_dir = user_dirs.cache_dir().to_string_lossy().into_owned();
    cache_dir
}

fn create_sysroot() {
    let exit_status = Command::new("cargo")
        .args(["+nightly", "miri", "setup"])
        .spawn()
        .expect("You must have miri installed")
        .wait()
        .expect("Failed to run cargo-miri setup")
        .code()
        .unwrap_or(1); // Might be killed by signal, in that case we just assume failure .

    if exit_status != 0 {
        std::process::exit(exit_status);
    }
}

struct IntrustCompilerCalls {
    cli: &'static Cli,
}

// This gets called for every crate being compiled I think.
impl rustc_driver::Callbacks for IntrustCompilerCalls {
    fn after_analysis<'tcx>(
        &mut self,
        _: &rustc_interface::interface::Compiler,
        tcx: TyCtxt<'tcx>,
    ) -> Compilation {
        if tcx.sess.dcx().has_errors_or_delayed_bugs().is_some() {
            tcx.dcx().fatal("Intrust cannot be run on programs that fail compilation");
        }

        if !tcx.crate_types().contains(&CrateType::Executable) {
            tcx.dcx().fatal("Intrust only works on bin crates");
        }

        let (entry_def_id, entry_type) = if let Some(entry_def) = tcx.entry_fn(()) {
            entry_def
        } else {
            tcx.dcx().fatal("Can only run programs that have a main function");
        };

        repl::run(self.cli, tcx, entry_def_id, miri::MiriEntryFnType::Rustc(entry_type))
    }
}

// Comes from priroda:
// https://github.com/oli-obk/priroda/blob/0cc9d44c37266e93822b0c4d4db96226d1368a50/src/main.rs#L46
fn should_hide_stmt(stmt: &Statement) -> bool {
    matches!(
        stmt.kind,
        StatementKind::StorageLive(_) | StatementKind::StorageDead(_) | StatementKind::Nop
    )
}
