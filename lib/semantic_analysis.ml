open Symbol_table
exception Semantic_error of Location.code_pos * string

let except sym_table exn =
  Symbol_table.print_entries sym_table;
  raise exn

let (>.) a b = let _ = a in b   (* `e >. ()` is equivalent to `ignore e` *)
let (@!) node = match node with {Ast.node; _} -> node
let (@@) node = match node with {Ast.loc; _} -> loc
let (>>) f g x = g(f(x))
let (>>>) f g x y = g(f(x)(y))
(* let (<<) f g x = f(g(x)) *)

let rec typ_to_microc_type (typ: Ast.typ) = match typ with
  | Ast.TypI  -> TInt
  | Ast.TypB  -> TBool
  | Ast.TypC  -> TChar
  | Ast.TypV  -> TVoid
  | Ast.TypP typp  -> TPointer (typ_to_microc_type typp)
  | Ast.TypA (typa, size) -> TArray ((typ_to_microc_type typa), size)

let topdecl_is_fundecl td = match td with Ast.Fundecl _ -> true | Ast.Vardec _ -> false
let unwrap_topdecl_to_fundecl td = match td with Ast.Fundecl fd -> fd | _ -> failwith "Trying to unwrap vardecl as fundecl"
  
let add_vardec_to_scope sym_table (identifier, typ) loc  =
  if Symbol_table.lookup_local_block sym_table identifier = None
    then Symbol_table.add_entry identifier (typ_to_microc_type typ) sym_table
    else except sym_table (Semantic_error (loc, "Variable declared twice"))

let add_topdecl_to_scope (td: Ast.topdecl) sym_table =
  match ((@!) td) with
    | Ast.Fundecl {Ast.typ; Ast.fname; Ast.formals; _} ->
      if Symbol_table.lookup_local_block sym_table fname = None then
        sym_table |> Symbol_table.add_entry fname (TFunc (
          formals |> List.map (fst >> typ_to_microc_type), 
          typ_to_microc_type typ)
        )
      else except sym_table (Semantic_error ((@@) td, "Function declared twice"))
    | Ast.Vardec (typ, identifier) -> add_vardec_to_scope sym_table (identifier, typ) ((@@) td)
      

let formal_to_record frm = let (typ, id) = frm in (id, typ_to_microc_type typ)

let rec type_compatible lht rht =
  match (lht, rht) with
    | (TArray(lt, ls), TArray(rt, rs)) -> (type_compatible lt rt) && (ls = rs || ls = None)
    | (TPointer lt, TPointer rt) -> type_compatible lt rt
    | _ -> lht = rht

let rec type_check_fundecl sym_table fd = 
  let {
    (* Ast.typ; *)
    (* Ast.fname; *)
    Ast.formals;
    Ast.body;
    _
  } = fd in 
  let parameter_block = Symbol_table.append_block sym_table (formals |> List.map formal_to_record |> Symbol_table.of_alist |> Stack.pop) in
  (* Format.print_string "Function "; Format.print_string fname; Format.print_string ":\n"; Format.print_break 0 0;
  Symbol_table.print_entries parameter_block; *)
  type_check_stmt sym_table body;
  Symbol_table.end_block parameter_block >. ()

and type_check_stmt sym_table stmt = let {Ast.node; _} = stmt in match node with
  | Ast.If _       -> ()
  | Ast.While _    -> ()
  | Ast.Expr expr  -> type_check_expr sym_table expr >. ()
  | Ast.Return _   -> ()
  | Ast.Block stmts    -> type_check_block sym_table stmts

and type_check_expr sym_table expr = (* Could check for constexprs and fix them at compile time *)
match (@!) expr with
  | Ast.Access acc -> type_check_access sym_table acc
  | Ast.Assign (acc, exp) -> (
    let atype = type_check_access sym_table acc in
    match atype with TArray _ -> except sym_table (Semantic_error ((@@) expr, "Cannot assign array")) | _ ->
    let etype = type_check_expr sym_table exp in
    if atype = etype
      then etype
      else except sym_table (Semantic_error ((@@) expr, "Assigning value of a wrong type"))
  )
  | Ast.Addr acc    -> TPointer (type_check_access sym_table acc)
  | Ast.ILiteral _  -> TInt
  | Ast.CLiteral _  -> TChar
  | Ast.BLiteral _  -> TBool
  | Ast.UnaryOp (uop, exp)         -> (
    let etype = type_check_expr sym_table exp in
    match (uop, etype) with
    | (Ast.Neg, TInt) -> TInt
    | (Ast.Not, TBool) -> TBool
    | _ -> except sym_table (Semantic_error ((@@) expr, Printf.sprintf "Invalid operand for operator \"%s\"" (Ast.show_uop uop)))
  )
  | Ast.BinaryOp (binop, lhs, rhs)  -> (
    let lhstype = type_check_expr sym_table lhs in
    let rhstype = type_check_expr sym_table rhs in
    match (binop, lhstype, rhstype) with
      | (Ast.Add, TInt, TInt)         -> TInt
      | (Ast.Sub, TInt, TInt)         -> TInt
      | (Ast.Mult, TInt, TInt)        -> TInt
      | (Ast.Div, TInt, TInt)         -> TInt
      | (Ast.Mod, TInt, TInt)         -> TInt
      | (Ast.Equal, l, r) when l = r  -> TBool
      | (Ast.Neq, l, r)   when l = r  -> TBool
      | (Ast.Less, TInt, TInt)        -> TBool
      | (Ast.Leq, TInt, TInt)         -> TBool
      | (Ast.Greater, TInt, TInt)     -> TBool
      | (Ast.Geq, TInt, TInt)         -> TBool
      | (Ast.And, TBool, TBool)       -> TBool
      | (Ast.Or, TBool, TBool)        -> TBool
      
      | _ -> except sym_table (Semantic_error ((@@) expr, Printf.sprintf "Invalid operands for operator \"%s\"" (Ast.show_binop binop)))
  )
  | Ast.Call (fname, actuals)       -> (
    let functype_opt = Symbol_table.lookup_opt fname sym_table in
    match functype_opt with
      | Some functype -> (
        match functype with
          | TFunc (formals, return) -> (try (
              if List.exists2 (type_compatible >>> not) formals (List.map (type_check_expr sym_table) actuals)
                then except sym_table (Semantic_error ((@@) expr, "Formal and actual parameters are not compatible"))
                else return
            )
            with Invalid_argument _ -> except sym_table (Semantic_error ((@@) expr, (Printf.sprintf "Formal and actual parameters are not compatible: expected %d parameters, got %d" (List.length formals) (List.length actuals))))) (* Raised by exists2 if the lists are of different lengths *)
          | _ -> except sym_table (Semantic_error ((@@) expr, "Calling non-function variable"))
      )
      | None -> except sym_table (Semantic_error ((@@) expr, Printf.sprintf "Calling undefined function \"%s\"" fname))
  )

and type_check_access sym_table acc =
let {Ast.node; Ast.loc} = acc in
match node with
  | Ast.AccVar ident -> (
    match Symbol_table.lookup_opt ident sym_table with
      | Some t -> t
      | None -> except sym_table (Semantic_error (loc, (Printf.sprintf "Trying to access undeclared variable \"%s\"" ident)))
  )
  | Ast.AccDeref expr -> TPointer (type_check_expr sym_table expr)
  | Ast.AccIndex (acc, expr) -> (
    match (type_check_access sym_table acc) with
      | TArray (t, _) -> (
        match type_check_expr sym_table expr with
          | TInt -> t
          | _ -> except sym_table (Semantic_error ((@@) expr, "Subscripting with a non-integer expression"))
      ) (* TODO: modify here to check array bounds at compile time *)
      | _ -> except sym_table (Semantic_error (loc, "Subscripting non-array variable")) (* TODO: must modify here to implement array-pointer duality *)
  )

and type_check_block sym_table block =
  match block with
    | x::xs -> (
      (match (@!)x with
        | Ast.Stmt stmt -> type_check_stmt sym_table stmt
        | Ast.Dec (typ, identifier) -> add_vardec_to_scope sym_table (identifier, typ) ((@@)x) >. ());
      type_check_block sym_table xs;
    )
    | _ -> ()

let type_check _p = match _p with
  | Ast.Prog topdecls ->
    let sym_table = begin_block (empty_table()) in
    List.iter (fun td -> add_topdecl_to_scope td sym_table >. ()) topdecls;
    (* Symbol_table.print_entries sym_table; *)
    topdecls
    |> List.map (@!)
    |> List.filter topdecl_is_fundecl
    |> List.iter (unwrap_topdecl_to_fundecl >> (sym_table |> type_check_fundecl))
    >. _p