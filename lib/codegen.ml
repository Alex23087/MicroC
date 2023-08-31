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

let add_external_function_to_module mcmodule sym_table (fname, typ, formals)=
  sym_table |> (add_entry fname (
    Llvm.declare_function fname (
      Llvm.function_type (typ_to_llvmtype typ) (
        formals |>
        List.map (typ_to_llvmtype) |>
        Array.of_list
      )
    ) mcmodule)
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

let rec build_function sym_table fd = let {fname; formals; body; typ} = fd in
  let fundef = lookup fname sym_table in
  let builder = Llvm.builder_at_end ctx (Llvm.entry_block fundef) in

  sym_table |> begin_block >. ();
  formals |> List.iteri (
    fun i (_, id) -> add_entry id (
      (* Promote parameters to local variables *)
      let param_i = Llvm.param fundef i in
      let param_i_typ = Llvm.type_of param_i in
      let param_i_loc = Llvm.build_alloca param_i_typ id builder in
      Llvm.build_store param_i param_i_loc builder >.
      param_i_loc
    ) sym_table >. ());

  build_stmt sym_table builder body;

  (match builder |> insertion_block |> Llvm.block_terminator with
    | Some _ -> ()
    | None -> (
      if typ = TypV
        then Llvm.build_ret_void builder >. ()
        else (typ |> typ_to_llvmtype |> Llvm.const_null |> Llvm.build_ret) builder >. ()
    )
  );

  sym_table |> end_block >. ()

and build_stmt sym_table builder stmt =
  match (@!) stmt with
    | If _ -> failwith "Not Implemented"
    | While (guard, body) -> build_while sym_table builder (guard, body)
    | Expr e -> build_expr sym_table builder e >. ()
    | Return _ -> failwith "Not Implemented"
    | Block block -> build_block sym_table builder block

and build_block sym_table builder block =
  let sym_table = sym_table |> begin_block in
  block |> List.iter (fun sd -> (
    match (@!) sd with
      | Dec (typ, id) -> build_local_decl sym_table builder (typ, id)
      | Stmt stmt -> build_stmt sym_table builder stmt
  ));
  sym_table |> end_block >. ()

and build_expr sym_table builder ?(nulltype = typ_to_llvmtype (TypP TypV)) expr =
  match (@!) expr with
    | Access acc -> build_access sym_table builder ~load:true acc
    | Assign (acc, expr) -> build_assign sym_table builder (acc, expr)
    | Addr _ -> failwith "Not Implemented"
    | ILiteral i -> Llvm.const_int (TypI |> typ_to_llvmtype) i
    | CLiteral c -> Llvm.const_int (TypC |> typ_to_llvmtype) (c |> int_of_char)
    | BLiteral b -> Llvm.const_int (TypB |> typ_to_llvmtype) (b |> Bool.to_int)
    | Nullptr -> Llvm.const_null nulltype
    | UnaryOp (uop, expr) -> (
      let exprval = build_expr sym_table builder ~nulltype expr in
      match uop with
        | Not -> Llvm.build_neg exprval (get_unique_name()) builder
        | Neg -> Llvm.build_neg exprval (get_unique_name()) builder
    )
    | BinaryOp (binop, lhs, rhs) -> (
      let lhsval = build_expr sym_table builder ~nulltype lhs in
      let rhsval = build_expr sym_table builder ~nulltype rhs in
      (match binop with
      | Add -> Llvm.build_nsw_add
      | Sub -> Llvm.build_nsw_sub
      | Mult  -> failwith "Not Implemented"
      | Div -> failwith "Not Implemented"
      | Mod -> failwith "Not Implemented"
      | Equal -> failwith "Not Implemented"
      | Neq -> failwith "Not Implemented"
      | Less  -> Llvm.build_icmp Icmp.Slt
      | Leq -> failwith "Not Implemented"
      | Greater -> Llvm.build_icmp Icmp.Sgt
      | Geq -> failwith "Not Implemented"
      | And -> failwith "Not Implemented"
      | Or -> failwith "Not Implemented")
      lhsval rhsval (get_unique_name()) builder
    )
    | Call (funcname, params) -> (
      let func = sym_table |> lookup funcname in
      let param_array = (
        params |>
        List.mapi
          (fun i -> build_expr sym_table builder 
            ~nulltype:(i |> Llvm.param func |> Llvm.type_of))
        |> Array.of_list) in
      Llvm.build_call func param_array (
        (* Functions with void return type should have an empty string as identifier for the return value *)
        if func |> type_of |> element_type |> return_type |> classify_type = TypeKind.Void
          then ""
          else get_unique_name()
      ) builder
    )

and build_local_decl sym_table builder (typ, id) =
  let vartyp = typ_to_llvmtype typ in
  let llvar = Llvm.build_alloca vartyp id builder in
  sym_table |> add_entry id llvar >. ()

and build_assign sym_table builder (acc, expr) =
  let lvalue = build_access sym_table builder ~load:false acc in
  let rvalue = build_expr sym_table builder ~nulltype:(Llvm.type_of lvalue) expr in
  Llvm.build_store rvalue lvalue builder

and build_access sym_table builder ?(load = false) acc =
  match (@!) acc with
    | AccVar id -> (
      let var = sym_table |> lookup id in
      if load
        then Llvm.build_load var (get_unique_name()) builder
        else var
    )
    | AccDeref expr -> (
      let addr = build_expr sym_table builder expr in
      if load
        then Llvm.build_load addr (get_unique_name()) builder
        else addr
    )
    | AccIndex (arr, ind) -> (
      let arr_addr = build_access sym_table builder ~load:false arr in
      let ind_val = build_expr sym_table builder ~nulltype:(typ_to_llvmtype TypI) ind in
      let addr = Llvm.build_in_bounds_gep arr_addr [|ind_val|] (get_unique_name()) builder in
      if load
        then Llvm.build_load addr (get_unique_name()) builder
        else addr
    )

and build_while sym_table builder (guard, body) =
  let current_block = builder |> insertion_block in
  let current_function = current_block |> block_parent in

  (* Define names for the blocks *)
  let guard_block_name = get_unique_name() in
  let body_block_name = get_unique_name() in
  let merge_block_name = get_unique_name() in
  
  (* Create blocks *)
  let guard_block = Llvm.append_block ctx guard_block_name current_function in
  let body_block = Llvm.append_block ctx body_block_name current_function in
  let merge_block = Llvm.append_block ctx merge_block_name current_function in

  (* Generate guard block *)
  Llvm.build_br guard_block builder >. ();
  let guard_builder = Llvm.builder_at_end ctx guard_block in
  let g = build_expr sym_table guard_builder guard in
  Llvm.build_cond_br g body_block merge_block guard_builder >. ();

  (* Generate body block *)
  let body_builder = Llvm.builder_at_end ctx body_block in
  build_stmt sym_table body_builder body >. ();
  Llvm.build_br guard_block body_builder >. ();

  (* Move builder at end of while *)
  Llvm.position_at_end merge_block builder

let to_llvm_module program =
  match program with
    | Prog topdecls ->(
      let mcmodule = create_module ctx "program" in
      let sym_table = begin_block (empty_table ()) in

      (* Declare external library functions *)
      add_external_function_to_module mcmodule sym_table ("print", TypV, [TypI]);
      add_external_function_to_module mcmodule sym_table ("getint", TypI, []);

      topdecls |> List.iter
      (add_topdecl_to_module mcmodule sym_table);

      topdecls |>
      List.filter_map (fun td -> match (@!) td with | Fundecl fd -> Some fd | _ -> None) |>
      List.iter (build_function sym_table);

      mcmodule
    )