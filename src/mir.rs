//! All this comes from cargo_utils, don't want to like to it though.

use std::collections::HashMap;
use std::io::{self, Write};

use rustc_hir::{ItemKind, TraitItemRef, ImplItemRef, OwnerId, Node, PrimTy, Mutability};
use rustc_hir::def_id::{LocalDefId, DefId, CrateNum, LOCAL_CRATE};
use rustc_hir::def::{DefKind, Res};
use rustc_middle::ty::{TyCtxt, FloatTy, UintTy, IntTy};
use rustc_middle::ty::fast_reject::SimplifiedType;
use rustc_middle::mir::*;
use rustc_span::symbol::Ident;
use rustc_span::symbol::Symbol;

/// Resolves a def path like `std::vec::Vec`.
///
/// Can return multiple resolutions when there are multiple versions of the same crate, e.g.
/// `memchr::memchr` could return the functions from both memchr 1.0 and memchr 2.0.
///
/// Also returns multiple results when there are multiple paths under the same name e.g. `std::vec`
/// would have both a [`DefKind::Mod`] and [`DefKind::Macro`].
///
/// This function is expensive and should be used sparingly.
pub fn def_path_res(tcx: TyCtxt, path: &[&str]) -> Vec<Res> {
    fn find_crates(tcx: TyCtxt<'_>, name: Symbol) -> impl Iterator<Item = DefId> + '_ {
        tcx.crates(())
            .iter()
            .copied()
            .filter(move |&num| tcx.crate_name(num) == name)
            .map(CrateNum::as_def_id)
    }

    let (base, mut path) = match *path {
        [primitive] => {
            return vec![PrimTy::from_name(Symbol::intern(primitive)).map_or(Res::Err, Res::PrimTy)];
        },
        [base, ref path @ ..] => (base, path),
        _ => return Vec::new(),
    };

    let base_sym = Symbol::intern(base);

    let local_crate = if tcx.crate_name(LOCAL_CRATE) == base_sym {
        Some(LOCAL_CRATE.as_def_id())
    } else {
        None
    };

    let starts = find_primitive_impls(tcx, base)
        .chain(find_crates(tcx, base_sym))
        .chain(local_crate)
        .map(|id| Res::Def(tcx.def_kind(id), id));

    let mut resolutions: Vec<Res> = starts.collect();

    while let [segment, rest @ ..] = path {
        path = rest;
        let segment = Symbol::intern(segment);

        resolutions = resolutions
            .into_iter()
            .filter_map(|res| res.opt_def_id())
            .flat_map(|def_id| {
                // When the current def_id is e.g. `struct S`, check the impl items in
                // `impl S { ... }`
                let inherent_impl_children = tcx
                    .inherent_impls(def_id)
                    .into_iter()
                    .flatten()
                    .flat_map(|&impl_def_id| item_children_by_name(tcx, impl_def_id, segment));

                let direct_children = item_children_by_name(tcx, def_id, segment);

                inherent_impl_children.chain(direct_children)
            })
            .collect();
    }

    resolutions
}

fn item_children_by_name(tcx: TyCtxt<'_>, def_id: DefId, name: Symbol) -> Vec<Res> {
    if let Some(local_id) = def_id.as_local() {
        local_item_children_by_name(tcx, local_id, name)
    } else {
        non_local_item_children_by_name(tcx, def_id, name)
    }
}

fn non_local_item_children_by_name(tcx: TyCtxt<'_>, def_id: DefId, name: Symbol) -> Vec<Res> {
    match tcx.def_kind(def_id) {
        DefKind::Mod | DefKind::Enum | DefKind::Trait => tcx
            .module_children(def_id)
            .iter()
            .filter(|item| item.ident.name == name)
            .map(|child| child.res.expect_non_local())
            .collect(),
        DefKind::Impl { .. } => tcx
            .associated_item_def_ids(def_id)
            .iter()
            .copied()
            .filter(|assoc_def_id| tcx.item_name(*assoc_def_id) == name)
            .map(|assoc_def_id| Res::Def(tcx.def_kind(assoc_def_id), assoc_def_id))
            .collect(),
        _ => Vec::new(),
    }
}

fn local_item_children_by_name(tcx: TyCtxt<'_>, local_id: LocalDefId, name: Symbol) -> Vec<Res> {
    let hir = tcx.hir();

    let root_mod;
    let item_kind = match tcx.hir_node_by_def_id(local_id) {
        Node::Crate(r#mod) => {
            root_mod = ItemKind::Mod(r#mod);
            &root_mod
        },
        Node::Item(item) => &item.kind,
        _ => return Vec::new(),
    };

    let res = |ident: Ident, owner_id: OwnerId| {
        if ident.name == name {
            let def_id = owner_id.to_def_id();
            Some(Res::Def(tcx.def_kind(def_id), def_id))
        } else {
            None
        }
    };

    match item_kind {
        ItemKind::Mod(r#mod) => r#mod
            .item_ids
            .iter()
            .filter_map(|&item_id| res(hir.item(item_id).ident, item_id.owner_id))
            .collect(),
        ItemKind::Impl(r#impl) => r#impl
            .items
            .iter()
            .filter_map(|&ImplItemRef { ident, id, .. }| res(ident, id.owner_id))
            .collect(),
        ItemKind::Trait(.., trait_item_refs) => trait_item_refs
            .iter()
            .filter_map(|&TraitItemRef { ident, id, .. }| res(ident, id.owner_id))
            .collect(),
        _ => Vec::new(),
    }
}

fn find_primitive_impls<'tcx>(tcx: TyCtxt<'tcx>, name: &str) -> impl Iterator<Item = DefId> + 'tcx {
    let ty = match name {
        "bool" => SimplifiedType::Bool,
        "char" => SimplifiedType::Char,
        "str" => SimplifiedType::Str,
        "array" => SimplifiedType::Array,
        "slice" => SimplifiedType::Slice,
        // FIXME: rustdoc documents these two using just `pointer`.
        //
        // Maybe this is something we should do here too.
        "const_ptr" => SimplifiedType::Ptr(Mutability::Not),
        "mut_ptr" => SimplifiedType::Ptr(Mutability::Mut),
        "isize" => SimplifiedType::Int(IntTy::Isize),
        "i8" => SimplifiedType::Int(IntTy::I8),
        "i16" => SimplifiedType::Int(IntTy::I16),
        "i32" => SimplifiedType::Int(IntTy::I32),
        "i64" => SimplifiedType::Int(IntTy::I64),
        "i128" => SimplifiedType::Int(IntTy::I128),
        "usize" => SimplifiedType::Uint(UintTy::Usize),
        "u8" => SimplifiedType::Uint(UintTy::U8),
        "u16" => SimplifiedType::Uint(UintTy::U16),
        "u32" => SimplifiedType::Uint(UintTy::U32),
        "u64" => SimplifiedType::Uint(UintTy::U64),
        "u128" => SimplifiedType::Uint(UintTy::U128),
        "f32" => SimplifiedType::Float(FloatTy::F32),
        "f64" => SimplifiedType::Float(FloatTy::F64),
        #[allow(trivial_casts)]
        _ => {
            return Result::<_, rustc_errors::ErrorGuaranteed>::Ok(&[] as &[_])
                .into_iter()
                .flatten()
                .copied();
        },
    };

    tcx.incoherent_impls(ty).into_iter().flatten().copied()
}

const INDENT: &str = "    ";
/// Alignment for lining up comments following MIR statements
pub(crate) const ALIGN: usize = 40;

fn comment(tcx: TyCtxt<'_>, SourceInfo { span, scope }: SourceInfo) -> String {
    let location = tcx.sess.source_map().span_to_embeddable_string(span);
    format!("scope {} at {}", scope.index(), location,)
}


/// Write out a human-readable textual representation for the given function.
pub fn write_mir_fn<'tcx>(
    tcx: TyCtxt<'tcx>,
    body: &Body<'tcx>,
    w: &mut dyn io::Write,
) -> io::Result<()> {
    write_mir_intro(tcx, body, w)?;
    for block in body.basic_blocks.indices() {
        write_basic_block(tcx, block, body, w)?;
        if block.index() + 1 != body.basic_blocks.len() {
            writeln!(w)?;
        }
    }

    writeln!(w, "}}")?;

    Ok(())
}

pub fn write_mir_intro<'tcx>(
    tcx: TyCtxt<'tcx>,
    body: &Body<'_>,
    w: &mut dyn Write,
) -> io::Result<()> {
    write_mir_sig(tcx, body, w)?;
    writeln!(w, "{{")?;

    // construct a scope tree and write it out
    let mut scope_tree: HashMap<SourceScope, Vec<SourceScope>> = Default::default();
    for (index, scope_data) in body.source_scopes.iter().enumerate() {
        if let Some(parent) = scope_data.parent_scope {
            scope_tree.entry(parent).or_default().push(SourceScope::from_usize(index));
        } else {
            // Only the argument scope has no parent, because it's the root.
            assert_eq!(index, OUTERMOST_SOURCE_SCOPE.index());
        }
    }

    write_scope_tree(tcx, body, &scope_tree, OUTERMOST_SOURCE_SCOPE, 1, w)?;

    // Add an empty line before the first block is printed.
    writeln!(w)?;

    // if let Some(branch_info) = &body.coverage_branch_info {
    //     write_coverage_branch_info(branch_info, w)?;
    // }
    // if let Some(function_coverage_info) = &body.function_coverage_info {
    //     write_function_coverage_info(function_coverage_info, w)?;
    // }
    Ok(())
}

/// Header. I.e. fn func(arg1; String) -> !.
fn write_mir_sig(tcx: TyCtxt<'_>, body: &Body, w: &mut dyn Write) -> io::Result<()> {
    use rustc_hir::def::DefKind;

    let def_id = body.source.def_id();
    let kind = tcx.def_kind(def_id);
    let is_function = match kind {
        DefKind::Fn | DefKind::AssocFn | DefKind::Ctor(..) => true,
        _ => tcx.is_closure_like(def_id),
    };
    match (kind, body.source.promoted) {
        (_, Some(_)) => write!(w, "const ")?, // promoteds are the closest to consts
        (DefKind::Const | DefKind::AssocConst, _) => write!(w, "const ")?,
        (DefKind::Static { mutability: Mutability::Not, nested: false }, _) => {
            write!(w, "static ")?;
        }
        (DefKind::Static { mutability: Mutability::Mut, nested: false }, _) => {
            write!(w, "static mut ")?;
        }
        (_, _) if is_function => write!(w, "fn ")?,
        (DefKind::AnonConst | DefKind::InlineConst, _) => {} // things like anon const, not an item
        _ => todo!("Unexpected def kind {kind:?}"),
    }

    write!(w, "{}", tcx.def_path_str(def_id))?;

    if let Some(p) = body.source.promoted {
        write!(w, "::{:?}", p)?;
    }

    if body.source.promoted.is_none() && is_function {
        write!(w, "(")?;

        // fn argument types.
        for (i, arg) in body.args_iter().enumerate() {
            if i != 0 {
                write!(w, ", ")?;
            }
            write!(w, "{:?}: {}", Place::from(arg), body.local_decls[arg].ty)?;
        }

        write!(w, ") -> {} ", body.return_ty())?;
    } else {
        assert_eq!(body.arg_count, 0);
        write!(w, ": {} = ", body.return_ty())?;
    }

    if let Some(yield_ty) = body.yield_ty() {
        write!(w, "yields {} ", yield_ty)?;
    }

    // Next thing that gets printed is the opening {
    Ok(())
}

/// Prints local variables in a scope tree.
fn write_scope_tree(
    tcx: TyCtxt,
    body: &Body,
    scope_tree: &HashMap<SourceScope, Vec<SourceScope>>,
    parent: SourceScope,
    depth: usize,
    w: &mut dyn Write,
) -> io::Result<()> {
    let indent = depth * INDENT.len();

    // Local variable debuginfo.
    for var_debug_info in &body.var_debug_info {
        if var_debug_info.source_info.scope != parent {
            // Not declared in this scope.
            continue;
        }

        let indented_debug_info = format!("{0:1$}debug {2:?};", INDENT, indent, var_debug_info);

        if tcx.sess.opts.unstable_opts.mir_include_spans {
            writeln!(w, "{0:1$} // in {2}", indented_debug_info, ALIGN, comment(tcx, var_debug_info.source_info))?;
        } else {
            writeln!(w, "{indented_debug_info}")?;
        }
    }

    // Local variable types.
    for (local, local_decl) in body.local_decls.iter_enumerated() {
        if (1..body.arg_count + 1).contains(&local.index()) {
            // Skip over argument locals, they're printed in the signature.
            continue;
        }

        if local_decl.source_info.scope != parent {
            // Not declared in this scope.
            continue;
        }

        let mut_str = local_decl.mutability.prefix_str();

        let mut indented_decl = format!(
            "{0:1$}let {2}{3:?}: {4}",
            INDENT, indent, mut_str, local, local_decl.ty
        );
        if let Some(user_ty) = &local_decl.user_ty {
            for user_ty in user_ty.projections() {
                writeln!(w, " as {user_ty:?}")?;
            }
        }
        indented_decl.push(';');

        let local_name = if local == RETURN_PLACE { " return place" } else { "" };

        if tcx.sess.opts.unstable_opts.mir_include_spans {
            writeln!(w, "{0:1$} //{2} in {3}", indented_decl, ALIGN, local_name, comment(tcx, local_decl.source_info))?;
        } else {
            writeln!(w, "{indented_decl}")?;
        }
    }

    let Some(children) = scope_tree.get(&parent) else {
        return Ok(());
    };

    for &child in children {
        let child_data = &body.source_scopes[child];
        assert_eq!(child_data.parent_scope, Some(parent));

        let (special, span) = if let Some((callee, callsite_span)) = child_data.inlined {
            (
                format!(
                    " (inlined {}{})",
                    if callee.def.requires_caller_location(tcx) { "#[track_caller] " } else { "" },
                    callee
                ),
                Some(callsite_span),
            )
        } else {
            (String::new(), None)
        };

        let indented_header = format!("{0:1$}scope {2}{3} {{", "", indent, child.index(), special);

        if tcx.sess.opts.unstable_opts.mir_include_spans {
            if let Some(span) = span {
                writeln!(w, "{0:1$} // at {2}", indented_header, ALIGN, tcx.sess.source_map().span_to_embeddable_string(span))?;
            } else {
                writeln!(w, "{indented_header}")?;
            }
        } else {
            writeln!(w, "{indented_header}")?;
        }

        write_scope_tree(tcx, body, scope_tree, child, depth + 1, w)?;
        writeln!(w, "{0:1$}}}", "", depth * INDENT.len())?;
    }
    Ok(())
}

/// Write out a human-readable textual representation for the given basic block.
pub fn write_basic_block<'tcx>(
    tcx: TyCtxt<'tcx>,
    block: BasicBlock,
    body: &Body<'tcx>,
    w: &mut dyn io::Write,
) -> io::Result<()> {
    let data = &body[block];

    // Basic block label at the top.
    let cleanup_text = if data.is_cleanup { " (cleanup)" } else { "" };
    writeln!(w, "{INDENT}{block:?}{cleanup_text}: {{")?;

    // List of statements in the middle.
    let mut current_location = Location { block, statement_index: 0 };
    for stmt in &data.statements {
        if crate::should_hide_stmt(stmt) {
            continue;
        }

        let indented_body = format!("{INDENT}{INDENT}{stmt:?};");
        if tcx.sess.opts.unstable_opts.mir_include_spans {
            writeln!(
                w,
                "{:A$} // {}{}",
                indented_body,
                if tcx.sess.verbose_internals() {
                    format!("{current_location:?}: ")
                } else {
                    String::new()
                },
                comment(tcx, stmt.source_info),
                A = ALIGN,
            )?;
        } else {
            writeln!(w, "{indented_body}")?;
        }

        current_location.statement_index += 1;
    }

    // Terminator at the bottom.
    let indented_terminator = format!("{0}{0}{1:?};", INDENT, data.terminator().kind);
    if tcx.sess.opts.unstable_opts.mir_include_spans {
        writeln!(
            w,
            "{:A$} // {}{}",
            indented_terminator,
            if tcx.sess.verbose_internals() {
                format!("{current_location:?}: ")
            } else {
                String::new()
            },
            comment(tcx, data.terminator().source_info),
            A = ALIGN,
        )?;
    } else {
        writeln!(w, "{indented_terminator}")?;
    }

    writeln!(w, "{INDENT}}}")
}
