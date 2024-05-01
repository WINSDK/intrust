use crate::Error;
use std::sync::mpsc::Sender;

use repl_tools::HighlightAndComplete;
use rustyline::sqlite_history::SQLiteHistory;
use rustyline::{CompletionType, Config, Editor};

repl_tools::define_repl_cmds!(enum ReplCommand {
    err = ReplCommandError;

    /// Step one instruction
    Nexti|ni: (),
    /// Continue the program being debugged
    Continue|c: (),
    /// Set a breakpoint at symbol or address
    Breakpoint|b: String,
    // FIXME move the `read:` part before the `--` in the help
    /// read: List registers and their content for the current stack frame
    Registers|regs: String,
    /// Print backtrace of stack frames
    Backtrace|bt: (),
    /// Disassemble some a several instructions starting at the instruction pointer
    Disassemble|dis: (),
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

fn editor() -> rustyline::Result<Editor<ReplHelper, SQLiteHistory>> {
    let config = Config::builder()
        .auto_add_history(true)
        .completion_type(CompletionType::List)
        .build();
    let history = SQLiteHistory::open(config, "/tmp/intrust.sqlite3")?;

    let mut rl = Editor::with_history(config, history)?;
    rl.set_helper(Some(ReplHelper::new(true /* color */)));
    Ok(rl)
}

pub fn run(tx: Sender<ReplCommand>) {
    let mut rl = editor().expect("Failed to initialize line editor");
    let mut last_cmd = None;
    loop {
        match rl.readline("(intrust) ") {
            Ok(mut command) => {
                // Repeat command on enter.
                if command == "" {
                    if let Some(last) = last_cmd {
                        command = last;
                    } else {
                        continue;
                    }
                }
                if command == "q" || command == "quit" || command == "exit" {
                    return;
                }
                match run_command(&tx, rl.helper().unwrap().color, &command) {
                    Ok(()) => {}
                    Err(err) => {
                        if rl.helper().unwrap().color {
                            println!("\x1b[91mError: {}\x1b[0m", err);
                        } else {
                            println!("Error: {}", err);
                        }
                    }
                }
                last_cmd = Some(command);
            }
            Err(rustyline::error::ReadlineError::Eof)
            | Err(rustyline::error::ReadlineError::Interrupted) => {
                return;
            }
            Err(err) => {
                if rl.helper().unwrap().color {
                    println!("\x1b[91mError: {:?}\x1b[0m", err);
                } else {
                    println!("Error: {:?}", err);
                }
                std::process::exit(1);
            }
        }
    }
}

fn run_command(tx: &Sender<ReplCommand>, color: bool, command: &str) -> Result<(), Error> {
    match ReplCommand::from_str(command)? {
        ReplCommand::Help(()) => {
            ReplCommand::print_help(std::io::stdout(), color).unwrap();
        }
        ReplCommand::Exit(()) => unreachable!(),
        command => {
            let _ = tx.send(command);
        }
    }
    Ok(())
}
