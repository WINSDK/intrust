use crate::Cli;
use crate::eval::Context;

use repl_tools::HighlightAndComplete;
use rustyline::sqlite_history::SQLiteHistory;
use rustyline::{CompletionType, Config};
use rustyline::error::ReadlineError;

use rustc_middle::ty::TyCtxt;
use rustc_hir::def_id::DefId;
use miri::MiriEntryFnType;

repl_tools::define_repl_cmds!(enum ReplCommand {
    err = ReplCommandError;

    /// Step one instruction
    Nexti|ni: (),
    /// Continue the program being debugged
    Cont|c: (),
    /// Set a breakpoint at an symbol or file:line
    Break|b: String,
    /// Remove a breakpoint at an symbol or file:line
    BreakDelete|bd: String,
    // FIXME move the `read:` part before the `--` in the help
    /// read: List registers and their content for the current stack frame
    Registers|regs: String,
    /// Print backtrace of stack frames
    Backtrace|bt: (),
    /// Disassemble some a several instructions starting at the instruction pointer
    Disassemble|dis: String,
    /// Print all local variables of current stack frame
    Locals: (),
    /// Print this help
    Help|h: (),
    /// Print the source code execution point.
    List|l: (),
    /// Exit
    Exit|quit|q: (),
});

type ReplHelper = repl_tools::MakeHelper<ReplCommand>;
type Editor = rustyline::Editor<ReplHelper, SQLiteHistory>;

fn editor() -> rustyline::Result<Editor> {
    let config = Config::builder()
        .auto_add_history(true)
        .completion_type(CompletionType::List)
        .build();
    let history = SQLiteHistory::open(config, "/tmp/intrust.sqlite3")?;

    let mut rl = Editor::with_history(config, history)?;
    rl.set_helper(Some(ReplHelper::new(true /* color */)));
    Ok(rl)
}

pub fn run(cli: &'static Cli, tcx: TyCtxt, entry_id: DefId, entry_type: MiriEntryFnType) -> ! {
    let mut rl = editor().expect("Failed to initialize line editor");
    let color = rl.helper().unwrap().color;
    let mut ctx = Context::new(tcx, entry_id, entry_type);

    for cmd in cli.exec.iter() {
        run_cmd(&mut ctx, color, cmd);
    }

    let mut last_cmd = None;
    loop {
        match rl.readline("(intrust) ") {
            Ok(cmd) => {
                let mut cmd = cmd.trim().to_string();

                // Repeat command on enter.
                if cmd.is_empty() {
                    if let Some(last) = last_cmd {
                        cmd = last;
                    } else {
                        continue;
                    }
                }

                run_cmd(&mut ctx, color, &cmd);
                last_cmd = Some(cmd);
            }
            Err(ReadlineError::Interrupted) => {},
            Err(ReadlineError::Eof) => std::process::exit(0),
            Err(err) => {
                if color {
                    println!("\x1b[91mError: {:?}\x1b[0m", err);
                } else {
                    println!("Error: {:?}", err);
                }
                std::process::exit(1);
            }
        }
    }
}

fn run_cmd(ctx: &mut Context, color: bool, cmd: &str) {
    match ReplCommand::from_str(&cmd) {
        Ok(ReplCommand::Help(())) => ReplCommand::print_help(color),
        Ok(ReplCommand::Exit(())) => std::process::exit(0),
        Ok(cmd) => match ctx.run_cmd(cmd) {
            // If command exited the program, reset the context.
            Ok(true) => ctx.reset(),
            Ok(false) => {}
            Err(err) => {
                if color {
                    println!("\x1b[91mError: {}\x1b[0m", err);
                } else {
                    println!("Error: {}", err);
                }
            }
        }
        Err(err) => {
            if color {
                println!("\x1b[91mError: {}\x1b[0m", err);
            } else {
                println!("Error: {}", err);
            }
        }
    }
}
