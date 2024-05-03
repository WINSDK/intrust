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

use std::path::Path;
use std::process::Command;

use rustc_driver::Compilation;
use rustc_session::config::{CrateType, ErrorOutputType};
use rustc_session::EarlyDiagCtxt;
use rustc_hir::def::Res;
use rustc_middle::mir::{Statement, StatementKind};


#[derive(Debug)]
pub enum Error {
    NotACallable(Res),
    UnknownPath(String),
    FileNotFound(String),
}

pub const DEFAULT_ARGS: &[&str] = &[
    "-Zalways-encode-mir",
    // Deduplicating diagnostics means we miss events when tracking what happens during an
    // execution. Let's not do that.
    "-Zdeduplicate-diagnostics=no",
];

fn main() {
    let early_dcx = EarlyDiagCtxt::new(ErrorOutputType::default());

    let mut args = rustc_driver::args::raw_args(&early_dcx)
        .unwrap_or_else(|_| std::process::exit(rustc_driver::EXIT_FAILURE));

    let sysroot_flag = String::from("--sysroot");
    if !args.contains(&sysroot_flag) {
        args.push(sysroot_flag);

        // From cargo-miri/src/utils.rs.
        let user_dirs = directories::ProjectDirs::from("org", "rust-lang", "miri").unwrap();
        let cache_dir = user_dirs.cache_dir().to_string_lossy().into_owned();

        if !Path::new(&cache_dir).is_dir() {
            let exit_status = Command::new("cargo")
                .args(&["+nightly", "miri", "setup"])
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

        args.push(cache_dir);
    }

    for arg in DEFAULT_ARGS {
        let arg: String = arg.to_string();
        if !args.contains(&arg) {
            args.push(arg);
        }
    }

    // Install the ctrlc handler that sets `rustc_const_eval::CTRL_C_RECEIVED`.
    rustc_driver::install_ctrlc_handler();

    // Add an ICE bug report hook.
    let using_internal_features =
        rustc_driver::install_ice_hook("https://careful.observer", |_| ());

    run_compiler(args, &mut IntrustCompilerCalls { }, using_internal_features);
}

/// Execute a compiler with the given CLI arguments and callbacks.
fn run_compiler(
    args: Vec<String>,
    callbacks: &mut (dyn rustc_driver::Callbacks + Send),
    using_internal_features: std::sync::Arc<std::sync::atomic::AtomicBool>,
) -> ! {
    // Invoke compiler, and handle return code.
    let exit_code = rustc_driver::catch_with_exit_code(move || {
        rustc_driver::RunCompiler::new(&args, callbacks)
            .set_using_internal_features(using_internal_features)
            .run()
    });
    std::process::exit(exit_code)
}

struct IntrustCompilerCalls {}

// This gets called for every crate being compiled I think.
impl rustc_driver::Callbacks for IntrustCompilerCalls {
    fn after_analysis<'tcx>(
        &mut self,
        _: &rustc_interface::interface::Compiler,
        queries: &'tcx rustc_interface::Queries<'tcx>,
    ) -> Compilation {
        queries.global_ctxt().unwrap().enter(|tcx| {
            if !tcx.crate_types().contains(&CrateType::Executable) {
                tcx.dcx().fatal("Intrust only works on bin crates");
            }

            let (entry_def_id, entry_type) = if let Some(entry_def) = tcx.entry_fn(()) {
                entry_def
            } else {
                tcx.dcx().fatal("Can only run programs that have a main function");
            };

            repl::run(tcx, entry_def_id, entry_type)
        })
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
