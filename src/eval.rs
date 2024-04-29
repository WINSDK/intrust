use rustc_hir::def_id::DefId;
use rustc_hir::definitions::DefPathData;
use rustc_middle::ty::TyCtxt;
use rustc_session::config::EntryFnType;
use rustc_middle::mir;
use rustc_const_eval::CTRL_C_RECEIVED;
use miri::Machine;

enum StepResult {
    Continue,
    Exited(i64),
    UserExit,
}

struct Context<'mir, 'tcx> {
    tcx: TyCtxt<'tcx>,
    ecx: miri::MiriInterpCx<'mir, 'tcx>,
}

impl<'mir, 'tcx> Context<'mir, 'tcx> {
    fn step(&mut self) -> StepResult {
       if CTRL_C_RECEIVED.load(std::sync::atomic::Ordering::Relaxed) {
            return StepResult::UserExit;
        }

        if let Some(frame) = Machine::stack(&self.ecx).last() {
            let src = frame.current_source_info().and_then(|source_info| {
                match &self.ecx.body().source_scopes[source_info.scope].local_data {
                    mir::ClearCrossCrate::Set(data) => Some(data.lint_root),
                    mir::ClearCrossCrate::Clear => None,
                }
            });

            if let Some(src) = src {
                let def_id = src.owner.def_id;
                // dbg!(self.tcx.hir().item(def_id));
                dbg!(frame.instance.to_string());
            }
        }

        match self.ecx.step() {
            Ok(true) => {},
            Ok(false) => return StepResult::Exited(0),
            Err(err) => {
                let (return_code, _) = miri::report_error(&self.ecx, err).unwrap();
                return StepResult::Exited(return_code);
            },
        }


        StepResult::Continue
    }

    fn dbg(&self) {
        let stack: Vec<(String, String, String)> = Machine::stack(&self.ecx)
            .iter()
            .map(|frame| {
                let instance = &frame.instance;
                let span = frame.current_source_info().unwrap().span;
                let name = if self
                    .tcx
                    .def_key(instance.def_id())
                    .disambiguated_data
                    .data
                    == DefPathData::Closure
                {
                    "inside call to closure".to_string()
                } else {
                    instance.to_string()
                };
                (name, format!("{span:?}"), format!("{:?}", instance.def_id()))
            })
            .collect();

        std::thread::sleep_ms(100);
        println!("{:#?}", stack.last());
        print!("{}[2J", 27 as char);
    }
}

pub fn run<'tcx>(tcx: TyCtxt<'tcx>, entry_id: DefId, entry_type: EntryFnType) -> Option<i64> {
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
        }
    ).unwrap();

    let mut ctx = Context {
        tcx,
        ecx,
    };

    loop {
        ctx.dbg();

        match ctx.step() {
            StepResult::Continue => {}
            StepResult::Exited(code) => {
                if code != 0 {
                    tcx.dcx().warn(format!("Program exited with error code {code}"));
                }

                return Some(code);
            }
            StepResult::UserExit => {
                println!();
                tcx.dcx().warn("User send ctrl-c, exiting the program");
                return None;
            }
        }
    }
}
