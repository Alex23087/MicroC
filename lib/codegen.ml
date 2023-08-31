open Ast
open Llvm
open Symbol_table

let (>.) a b = let _ = a in b
(* `e >. ()` is equivalent to `ignore e` *)
let (@!) node = match node with {Ast.node; _} -> node
let (>>) f g x = g(f(x))
(* let (<<) f g x = f(g(x)) *)

let ctx = global_context()
let counter = ref 0
let get_unique_name () = let i = !counter in incr counter >. i |> string_of_int

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
    Llvm.define_global id (typ |> typ_to_llvmtype |> const_null) mcmodule)
  ) >. ()

let add_function_to_module mcmodule sym_table fundecl =
  let {
    typ;
    fname;
    formals;
    _
  } = fundecl in
  sym_table |> (add_entry fname (
    Llvm.define_function fname (
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


let rec build_function mcmodule sym_table fd = let {fname; formals; body; _} = fd in
  let fundef = lookup fname sym_table in
  let builder = Llvm.builder_at_end ctx (Llvm.entry_block fundef) in

  sym_table |> begin_block >. ();
  formals |> List.iteri (
    fun i (_, id) -> add_entry id (Llvm.param fundef i) sym_table >. ());

  build_stmt mcmodule sym_table builder body;

and build_stmt mcmodule sym_table builder stmt =
  match (@!) stmt with
    | If _ -> failwith "Not Implemented"
    | While _ -> failwith "Not Implemented"
    | Expr e -> build_expr mcmodule sym_table builder e (TypP TypV)>. ()
    | Return _ -> failwith "Not Implemented"
    | Block block -> build_block mcmodule sym_table builder block

and build_block mcmodule sym_table builder block =
  let sym_table = sym_table |> begin_block in
  block |> List.iter (fun sd -> (
    match (@!) sd with
      | Dec (typ, id) -> build_local_decl sym_table builder (typ, id)
      | Stmt stmt -> build_stmt mcmodule sym_table builder stmt
  ));
  sym_table |> end_block >. ()

and build_expr mcmodule sym_table builder expr nulltype =
  match (@!) expr with
    | Access _ -> failwith "Not Implemented"
    | Assign _ -> failwith "Not Implemented"
    | Addr _ -> failwith "Not Implemented"
    | ILiteral i -> Llvm.const_int (TypI |> typ_to_llvmtype) i
    | CLiteral c -> Llvm.const_int (TypC |> typ_to_llvmtype) (c |> int_of_char)
    | BLiteral b -> Llvm.const_int (TypB |> typ_to_llvmtype) (b |> Bool.to_int)
    | Nullptr -> Llvm.const_null (nulltype |> typ_to_llvmtype)
    | UnaryOp (uop, expr) -> (
      let exprval = build_expr mcmodule sym_table builder expr nulltype in
      match uop with
        | Not -> Llvm.build_neg exprval (get_unique_name()) builder
        | Neg -> Llvm.build_neg exprval (get_unique_name()) builder
    )
    | BinaryOp _ -> failwith "Not Implemented"
    | Call _ -> failwith "Not Implemented"

and build_local_decl sym_table builder (typ, id) =
  let vartyp = typ_to_llvmtype typ in
  let llvar = Llvm.build_alloca vartyp id builder in
  sym_table |> add_entry id llvar >. ()

let to_llvm_module program =
  match program with
    | Prog topdecls ->(
      let mcmodule = create_module ctx "program" in
      let sym_table = begin_block (empty_table ()) in

      topdecls |> List.iter
      (add_topdecl_to_module mcmodule sym_table);

      topdecls |>
      List.filter_map (fun td -> match (@!) td with | Fundecl fd -> Some fd | _ -> None) |>
      List.iter (build_function mcmodule sym_table);

      mcmodule
    )