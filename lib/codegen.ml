open Ast
open Llvm
open Symbol_table

(* Utility and function-compositing functions *)
let (>.) a b = let _ = a in b
(* `e >. ()` is equivalent to `ignore e` *)
let (@!) node = match node with {Ast.node; _} -> node
let (>>) f g x = g(f(x))
(* let (<<) f g x = f(g(x)) *)

let ctx = global_context()

(* Global counter to create unique identifiers for LLVM IR *)
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
    | TypF -> Llvm.float_type
) ctx

(* Define a global variable *)
let add_global_var mcmodule sym_table (typ, id) =
  sym_table |> (add_entry id (
    Llvm.define_global id (typ |> typ_to_llvmtype |> const_null) mcmodule)
  ) >. ()

(* Declare a global variable *)
let add_external_variable_to_module mcmodule sym_table (typ, id) =
  sym_table |> (add_entry id (
    Llvm.declare_global (typ |> typ_to_llvmtype) id mcmodule)
  ) >. ()

(* Declare a global function *)
let add_external_function_to_module mcmodule sym_table {fname; typ; formals}=
  sym_table |> (add_entry fname (
    Llvm.declare_function fname (
      Llvm.function_type (typ_to_llvmtype typ) (
        formals |>
        List.map ((fun (typ, _) -> match typ with | TypA (t, _) -> TypP t | _ -> typ) >> typ_to_llvmtype) |>
        Array.of_list
      )
    ) mcmodule)
  ) >. ()

(* Declare a global variable or function *)
let add_extern_to_module mcmodule sym_table ext =
  match ext with
    | Vardec (typ, id) -> add_external_variable_to_module mcmodule sym_table (typ, id)
    | Fundec fd -> add_external_function_to_module mcmodule sym_table fd
    | _ -> failwith "This shouldn't happen"

(* Define a global function *)
let add_function_to_module mcmodule sym_table fundef =
  let {
    typ;
    fname;
    formals;
  } = fundef in
  sym_table |> (add_entry fname (
    Llvm.define_function fname (
      Llvm.function_type (typ_to_llvmtype typ) (
        formals |>
        List.map ((fun (typ, _) -> match typ with | TypA (t, _) -> TypP t | _ -> typ) >> typ_to_llvmtype) |>
        Array.of_list
      )
    ) mcmodule)
  ) >. ()

let load_file filename =
  let ic = open_in filename in 
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let process_source filename = 
  let source = load_file filename in 
  let lexbuf = Lexing.from_string ~with_positions:true source in 
  lexbuf |>
  Parsing.parse Scanner.next_token

(* Table used to avoid including the same interface multiple times *)
let include_table = Hashtbl.create 5

(* Declare all included symbols from lib *)
let rec include_lib mcmodule sym_table lib =
  let lib_ast = process_source lib in
  match lib_ast with
    | Ast.Prog topdecls -> (
      topdecls |> List.iter ((@!) >> (fun td -> match td with
        | Include l-> (
          if (include_table |> Hashtbl.find_opt) l |> Option.is_none
            then ((include_table |> Hashtbl.add) l true ;include_lib mcmodule sym_table l)
        )
        | _ -> add_extern_to_module mcmodule sym_table td)
      )
    )

let add_topdecl_to_module mcmodule sym_table topdecl =
  match (@!) topdecl with
    | Fundef (fd, _) -> add_function_to_module mcmodule sym_table fd
    | Vardec (typ, id) -> add_global_var mcmodule sym_table (typ, id)
    | Fundec _ -> failwith "This shouldn't happen" (* Bare Fundecs are not allowed outside of interface files*)
    | Include lib -> include_lib mcmodule sym_table lib
    | Extern ext -> add_extern_to_module mcmodule sym_table ext

(* Does the current block the builder is in have a terminator? *)
let has_terminator = insertion_block >> block_terminator >> Option.is_some

let rec build_function sym_table (fd, body) = let {fname; formals; typ} = fd in
  (* At this point the function is already defined, get the llvalue *)
  let fundef = lookup fname sym_table in
  let builder = Llvm.builder_at_end ctx (Llvm.entry_block fundef) in

  (* Build the parameter block *)
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

  (* Build body block *)
  build_stmt sym_table builder body;

  ( (* Build terminator if the block does not have one *)
    if builder |> has_terminator |> not then
      if typ = TypV
        then Llvm.build_ret_void builder >. ()
        else (typ |> typ_to_llvmtype |> Llvm.const_null |> Llvm.build_ret) builder >. ()
  );

  sym_table |> end_block >. ()

and build_stmt sym_table builder stmt =
  (* builder |> insertion_block |> block_parent |> global_parent |> string_of_llmodule |> Printf.printf "%s\n\n\n\n%!"; *)
  if builder |> has_terminator then () else
  match (@!) stmt with
    | If (guard, thenstmt, elsestmt) -> build_if sym_table builder (guard, thenstmt, elsestmt)
    | While (guard, body) -> build_while sym_table builder (guard, body)
    | Expr e -> build_expr sym_table builder e >. ()
    | Return expr -> build_return sym_table builder expr
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
    | Addr expr -> build_access sym_table builder ~load:false expr
    | ILiteral i -> Llvm.const_int (TypI |> typ_to_llvmtype) i
    | CLiteral c -> Llvm.const_int (TypC |> typ_to_llvmtype) (c |> int_of_char)
    | BLiteral b -> Llvm.const_int (TypB |> typ_to_llvmtype) (b |> Bool.to_int)
    | FLiteral f -> Llvm.const_float (TypF |> typ_to_llvmtype) (f)
    | SLiteral s -> Llvm.const_stringz ctx s
    | Nullptr -> Llvm.const_null nulltype
    | UnaryOp (uop, expr) -> (
      let exprval = build_expr sym_table builder ~nulltype expr in
      match uop with
        | Not -> Llvm.build_not exprval (get_unique_name()) builder
        | Neg -> Llvm.build_neg exprval (get_unique_name()) builder
    )
    | BinaryOp (binop, lhs, rhs) -> (
      let lhsval = build_expr sym_table builder ~nulltype lhs in
      let rhsval = build_expr sym_table builder ~nulltype rhs in

      (* If one of the two operands is a float and the other one an int, convert the int to a float *)
      let lhtype = lhsval |> type_of |> classify_type in
      let rhtype = rhsval |> type_of |> classify_type in
      let lhlf = lhtype = TypeKind.Float in
      let rhlf = rhtype = TypeKind.Float in
      let cond = lhlf || rhlf in
      let lhsval = (
        if cond && (not lhlf)
          then Llvm.build_sitofp lhsval (Llvm.float_type ctx) (get_unique_name()) builder
          else lhsval
      ) in
      let rhsval = (
        if cond && (not rhlf)
          then Llvm.build_sitofp rhsval (Llvm.float_type ctx) (get_unique_name()) builder
          else rhsval
      ) in
      
      (match binop with
        | Add     -> if cond then Llvm.build_fadd else Llvm.build_nsw_add
        | Sub     -> if cond then Llvm.build_fsub else Llvm.build_nsw_sub
        | Mult    -> if cond then Llvm.build_fmul else Llvm.build_nsw_mul
        | Div     -> if cond then Llvm.build_fdiv else Llvm.build_sdiv
        | Mod     -> if cond then Llvm.build_frem else Llvm.build_srem
        | Equal   -> (if cond then Llvm.build_fcmp Fcmp.Oeq else Llvm.build_icmp Icmp.Eq)
        | Neq     -> (if cond then Llvm.build_fcmp Fcmp.One else Llvm.build_icmp Icmp.Ne)
        | Less    -> (if cond then Llvm.build_fcmp Fcmp.Olt else Llvm.build_icmp Icmp.Slt)
        | Leq     -> (if cond then Llvm.build_fcmp Fcmp.Ole else Llvm.build_icmp Icmp.Sle)
        | Greater -> (if cond then Llvm.build_fcmp Fcmp.Oge else Llvm.build_icmp Icmp.Sgt)
        | Geq     -> (if cond then Llvm.build_fcmp Fcmp.Ogt else Llvm.build_icmp Icmp.Sge)
        | And     -> Llvm.build_and
        | Or      -> Llvm.build_or)
      lhsval rhsval (get_unique_name()) builder
    )
    | Call (funcname, params) -> (
      let func = sym_table |> lookup funcname in
      let param_array = ( (* Convert the parameter list to the appropriate array *)
        params |>
        List.mapi
          (fun i expr ->
            let v = build_expr sym_table builder ~nulltype:(Llvm.param func i |> Llvm.type_of) expr in
            let v = ( (* If a string literal is passed to a function, it has to be allocated on the stack first *)
              match (@!) expr with
                | SLiteral _ -> (
                  let allocated_string = Llvm.build_array_alloca (v |> type_of) (Llvm.const_int (Llvm.i32_type ctx) 1) (get_unique_name()) builder in
                  Llvm.build_store v allocated_string builder >.
                  allocated_string
                )
                | _ -> v
            ) in
            (* Arrays are bitcasted to pointers for function calls *)
            match v |> type_of |> element_type |> classify_type with
              | TypeKind.Array -> Llvm.build_bitcast v (v |> type_of |> element_type |> element_type |> Llvm.pointer_type)(get_unique_name()) builder
              | _ -> v
          )
        |> Array.of_list) in
      Llvm.build_call func param_array (
        (* Functions with void return type should have an empty string as identifier for the return value *)
        if type_of func |> element_type |> return_type |> classify_type = TypeKind.Void
          then ""
          else get_unique_name()
      ) builder
    )
    | Prepost (pp, op, acc) -> (
      (* Get location of value to be changed, get value, increment/decrement it
         then return either the old value or the new one depending on pre/post operation *)
      let valloc = build_access sym_table builder ~load:false acc in
      let oldval = Llvm.build_load valloc (get_unique_name()) builder in
      let oldvaltype = oldval |> type_of |> classify_type in
      let newval = (
        match op with
          | Incr -> (
            match oldvaltype with
              | TypeKind.Integer  -> Llvm.build_nsw_add oldval (Llvm.const_int (Llvm.i32_type ctx) 1)
              | TypeKind.Float    -> Llvm.build_fadd oldval (Llvm.const_float (Llvm.float_type ctx) 1.0)
              | _                 -> failwith "Cannot increment non-numerical value"
          )
          | Decr -> (
            match oldvaltype with
              | TypeKind.Integer  -> Llvm.build_nsw_sub oldval (Llvm.const_int (Llvm.i32_type ctx) 1)
              | TypeKind.Float    -> Llvm.build_fsub oldval (Llvm.const_float (Llvm.float_type ctx) 1.0)
              | _                 -> failwith "Cannot decrement non-numerical value"
          )
      )  (get_unique_name()) builder in
      Llvm.build_store newval valloc builder >.
      match pp with
        | Pre -> newval
        | Post -> oldval
    )

(* Declare local variable (No initialisation )*)
and build_local_decl sym_table builder (typ, id) =
  let vartyp = typ_to_llvmtype typ in
  let llvar = Llvm.build_alloca vartyp id builder in
  sym_table |> add_entry id llvar >. ()

(* Build assignment *)
and build_assign sym_table builder (acc, expr) =
  let lvalue = build_access sym_table builder ~load:false acc in
  let rvalue = build_expr sym_table builder ~nulltype:(Llvm.type_of lvalue) expr in
  Llvm.build_store rvalue lvalue builder >. rvalue

(* Build access. The parameter load determines if the result should be the address or the value *)
and build_access sym_table builder ?(load = false) acc =
  let v = match (@!) acc with
    | AccVar id -> let var = sym_table |> lookup id in var
    | AccDeref expr -> let addr = build_expr sym_table builder expr in addr
    | AccIndex (arr, ind) -> (
      let arr_addr = build_access sym_table builder ~load:false arr in (* Address of the array *)
      let ind_val = build_expr sym_table builder ~nulltype:(typ_to_llvmtype TypI) ind in
      let addr = ( (* Address of the value *)
        match arr_addr |> type_of |> element_type |> classify_type with
          | Llvm.TypeKind.Array -> ( (* Regular arrays *)
            Llvm.build_in_bounds_gep arr_addr [|Llvm.const_int (Llvm.i32_type ctx) 0; ind_val|] (get_unique_name()) builder
          )
          | Llvm.TypeKind.Pointer -> ( (* Pointers to array data. Used in function parameters *)
            let data_addr = Llvm.build_load arr_addr (get_unique_name()) builder in
            Llvm.build_in_bounds_gep data_addr [|ind_val|] (get_unique_name()) builder
          )
          | _ -> failwith "Indexing invalid value"
      ) in addr
    ) in
  if load && (v |> type_of |> element_type |> classify_type <> TypeKind.Array)
    then Llvm.build_load v (get_unique_name()) builder
    else v

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
  if not (body_builder |> has_terminator) then Llvm.build_br guard_block body_builder >. ();

  (* Move builder at end of while *)
  Llvm.position_at_end merge_block builder

and build_if sym_table builder (guard, thenstmt, elsestmt) =
  let current_block = builder |> insertion_block in
  let current_function = current_block |> block_parent in

  (* Define names for the blocks *)
  let guard_block_name = get_unique_name() in
  let then_block_name = get_unique_name() in
  let else_block_name = get_unique_name() in
  let merge_block_name = get_unique_name() in

  (* Create blocks *)
  let guard_block = Llvm.append_block ctx guard_block_name current_function in
  let then_block = Llvm.append_block ctx then_block_name current_function in
  let else_block = Llvm.append_block ctx else_block_name current_function in
  let merge_block = Llvm.append_block ctx merge_block_name current_function in

  (* Generate guard block *)
  Llvm.build_br guard_block builder >. ();
  let guard_builder = Llvm.builder_at_end ctx guard_block in
  let g = build_expr sym_table guard_builder guard in
  Llvm.build_cond_br g then_block else_block guard_builder >. ();

  (* Generate then block *)
  let then_builder = Llvm.builder_at_end ctx then_block in
  build_stmt sym_table then_builder thenstmt >. ();
  if not (then_builder |> has_terminator) then Llvm.build_br merge_block then_builder >. ();

  (* Generate else block *)
  let else_builder = Llvm.builder_at_end ctx else_block in
  build_stmt sym_table else_builder elsestmt >. ();
  if not (else_builder |> has_terminator) then Llvm.build_br merge_block else_builder >. ();

  (* Move builder at end of if *)
  Llvm.position_at_end merge_block builder

and build_return sym_table builder expr =
  match expr with
    | None -> Llvm.build_ret_void builder >. ()
    | Some ex -> (
      let value = build_expr sym_table builder
        ~nulltype:(builder |> insertion_block |> block_parent |> type_of |> element_type |> return_type)
      ex in
      Llvm.build_ret value builder >. ()
    )

let to_llvm_module program =
  match program with
    | Prog topdecls ->(
      let mcmodule = create_module ctx "program" in
      let sym_table = begin_block (empty_table ()) in

      (* Declare external library functions *)
      (* Could be done with #include, but it's left here to make tests work without needing to include interfaces *)
      add_external_function_to_module mcmodule sym_table {Ast.fname = "print"; Ast.typ = TypV; Ast.formals = [(TypI, "n")]};
      add_external_function_to_module mcmodule sym_table {Ast.fname ="getint"; Ast.typ = TypI; Ast.formals = []};

      (* Add all topdecls to global scope *)
      topdecls |> List.iter
      (add_topdecl_to_module mcmodule sym_table);

      (* Generate code for all the functions *)
      topdecls |>
      List.filter_map (fun td -> match (@!) td with | Fundef fd -> Some fd | _ -> None) |>
      List.iter (build_function sym_table);

      mcmodule
    )