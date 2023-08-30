open Ast
open Llvm
open Symbol_table

let (>.) a b = let _ = a in b
(* `e >. ()` is equivalent to `ignore e` *)
let (@!) node = match node with {Ast.node; _} -> node
let (>>) f g x = g(f(x))
(* let (<<) f g x = f(g(x)) *)

let ctx = global_context()

let rec typ_to_llvmtype typ = (
  match typ with
    | TypI -> Llvm.i32_type
    | TypB -> Llvm.i1_type
    | TypC -> Llvm.i8_type
    | TypA (t, size) -> (
      match size with
        | None -> Llvm.void_type
        | Some s -> let ty = Llvm.array_type (typ_to_llvmtype t) s in (fun _ -> ty)
    )
    | TypP t -> let ty = Llvm.pointer_type (typ_to_llvmtype t) in (fun _ -> ty)
    | TypV -> Llvm.void_type
) ctx

let add_global_var mcmodule sym_table (typ, id) =
  sym_table |> (add_entry id (
    Llvm.declare_global (typ_to_llvmtype typ) id mcmodule)
  ) >. ()

let add_function_to_module mcmodule sym_table fundecl =
  let {
    typ;
    fname;
    formals;
    _
  } = fundecl in
  sym_table |> (add_entry fname (
    Llvm.declare_function fname (
      Llvm.function_type (typ_to_llvmtype typ) (
        formals |>
        List.map ((fun (typ, _) -> typ) >> typ_to_llvmtype) |>
        Array.of_list
      )
    ) mcmodule)
  ) >. ()


let add_topdecl_to_module mcmodule sym_table topdecl =
  match (@!) topdecl with
    | Fundecl fd -> add_function_to_module mcmodule sym_table fd
    | Vardec (typ, id) -> add_global_var mcmodule sym_table (typ, id)


let build_function _ = ()

let to_llvm_module program =
  match program with
    | Prog topdecls ->(
      let mcmodule = create_module ctx "program" in
      let sym_table = begin_block (empty_table ()) in

      topdecls |> List.iter
      (add_topdecl_to_module mcmodule sym_table);

      topdecls |>
      List.filter_map (fun td -> match (@!) td with | Fundecl fd -> Some fd | _ -> None) |>
      List.iter build_function;

      mcmodule
    )