open Symbol_table
exception Semantic_error of Location.code_pos * string

let except _ exn =
  (* Symbol_table.print_entries sym_table; *)
  raise exn

let (>.) a b = let _ = a in b   (* `e >. ()` is equivalent to `ignore e` *)
let (@!) node = match node with {Ast.node; _} -> node
let (@@) node = match node with {Ast.loc; _} -> loc
let (>>) f g x = g(f(x))
let (>>>) f g x y = g(f(x)(y))
(* let (<<) f g x = f(g(x)) *)

(* Arrays with size < 1 should not be allowed *)
let rec array_size_check sym_table typ loc =
  match typ with
    | TArray (t, size) -> (
      if Option.value size ~default:2 < 1 (* Default value is 2 as arrays with no size declared should be valid *)
        then except sym_table (Semantic_error (loc, "Declaring an array of size less than 1"))
        else array_size_check sym_table t loc
    )
    | _ -> ()

(* Multidimensional arrays should not be allowed *)
let array_dimension_check sym_table typ loc =
  let rec array_dimension_check_aux ty array_encountered =
    match ty with
      | TPointer t -> array_dimension_check_aux t false
      | TArray (t, _) -> (
        if array_encountered
          then except sym_table (Semantic_error (loc, "Multidimensional arrays are not allowed"))
          else array_dimension_check_aux t true
      )
      | _ -> ()
  in array_dimension_check_aux typ false

let function_return_type_check sym_table typ loc =
  match typ with
    | TFunc (_, ret) -> (
      match ret with
        | TArray _ -> except sym_table (Semantic_error (loc, "Functions cannot return arrays"))
        | TPointer _ -> except sym_table (Semantic_error (loc, "Functions cannot return pointers"))
        | _ -> ()
    )
    | _ -> ()

let add_vardec_to_scope sym_table (identifier, typ) loc  =
  let microctyp = typ_to_microc_type typ in
  array_size_check sym_table microctyp loc >. ();
  array_dimension_check sym_table microctyp loc >. ();
  if microctyp = TVoid then except sym_table (Semantic_error (loc, Printf.sprintf "Variables cannot be of type %s" (show_microc_type TVoid)));
  function_return_type_check sym_table microctyp loc >. ();
  if Symbol_table.lookup_local_block sym_table identifier = None
    then Symbol_table.add_entry identifier microctyp sym_table
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
      

(* let formal_to_record frm = let (typ, id) = frm in (id, typ_to_microc_type typ) *)

let rec type_compatible lht rht =
  match (lht, rht) with
    | (TArray(lt, ls), TArray(rt, rs)) -> (type_compatible lt rt) && (ls = rs || ls = None) (* An array declared with no defined size can be assigned (in function parameters) another one with a defined size *)
    | (TPointer lt, TPointer rt) -> type_compatible lt rt
    | _ -> lht = rht

let rec type_check_fundecl sym_table fd = 
  let {
    Ast.typ;
    Ast.fname;
    Ast.formals;
    Ast.body;
  } = fd in 
  let funret = (Some (typ_to_microc_type typ)) in

  let parameter_block = begin_block sym_table in

  formals
  |> List.iter (fun (typ, id) -> add_vardec_to_scope parameter_block (id, typ) ((@@) body) >. ()) >. ();


  type_check_stmt parameter_block ~funret ~isfun:true body;
  (* Check the presence of a return statement if the function is non-void and not main. Typing is checked by type_check_stmt *)
  if fname <> "main" && funret <> Some TVoid && not (check_return_presence body) then
    except parameter_block (Semantic_error ((@@)body, "No return in non-void function"));
  Symbol_table.end_block parameter_block >. ()

and check_return_presence body =
  match (@!) body with
   (* If contains return if both branches contain return *)
   | Ast.If (_, thenexpr, elseexpr) -> (
    check_return_presence thenexpr &&
    check_return_presence elseexpr
   )
   (* While contains return if its body contains return (no check for whether it is reached) *)
   | Ast.While (_, body) -> check_return_presence body
   | Ast.Return _ -> true
   (* Block contains return if any of its statements contains return *)
   | Ast.Block stmts -> 
    stmts |>
    List.filter_map (
      (@!) >>
      (fun s -> match s with Ast.Stmt s -> Some s | _ -> None)
    ) |>
    List.exists (check_return_presence)
   | _ -> false

and type_check_stmt sym_table ?(funret = None) ?(isfun = false) stmt = let {Ast.node; Ast.loc} = stmt in match node with
  | Ast.If (guard, thenstmt, elsestmt)       -> type_check_if sym_table ~funret (guard, thenstmt, elsestmt)
  | Ast.While (guard, body)    -> type_check_while sym_table ~funret (guard, body)
  | Ast.Expr expr  -> type_check_expr sym_table expr >. ()
  | Ast.Return expr   -> (
    match funret with
      | None -> except sym_table (Semantic_error (loc, "Returning from non-function"))
      | Some funret-> (
        let rettype = Option.fold ~none:TVoid ~some:(type_check_expr sym_table) expr in
        if funret <> rettype
          then except sym_table (Semantic_error (loc, Printf.sprintf "Returning value of type %s from a function that requires a value of type %s" (Symbol_table.show_microc_type rettype) (Symbol_table.show_microc_type funret)))
      )
  )
  | Ast.Block stmts    -> type_check_block sym_table ~funret ~isfun stmts

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
  | Ast.ILiteral i  -> (
    if i > Int32.to_int Int32.max_int then
      except sym_table (Semantic_error ((@@) expr, "Integer greater than INT_MAX")) else
    if i < Int32.to_int Int32.min_int then
      except sym_table (Semantic_error ((@@) expr, "Integer greater than INT_MIN")) else
    TInt 
  )
  | Ast.CLiteral _  -> TChar
  | Ast.BLiteral _  -> TBool
  | Ast.Nullptr -> TPointer TVoid (* The C standard defines a null pointer as (void* )0 *)
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
      | Some t -> (
        match t with
          | TFunc _ -> except sym_table (Semantic_error (loc, "Cannot denote functions as variables"))
          | _ -> t
      )
      | None -> except sym_table (Semantic_error (loc, (Printf.sprintf "Trying to access undeclared variable \"%s\"" ident)))
  )
  | Ast.AccDeref expr -> (
    match (type_check_expr sym_table expr) with
      | TPointer p -> p
      | _ -> except sym_table (Semantic_error ((@@) expr, "Trying to dereference a non-pointer value"))
  )
  | Ast.AccIndex (acc, expr) -> (
    match (type_check_access sym_table acc) with
      | TArray (t, _) -> (
        match type_check_expr sym_table expr with
          | TInt -> t
          | _ -> except sym_table (Semantic_error ((@@) expr, "Subscripting with a non-integer expression"))
      ) (* TODO: modify here to check array bounds at compile time *)
      | _ -> except sym_table (Semantic_error (loc, "Subscripting non-array variable")) (* TODO: must modify here to implement array-pointer duality *)
  )

and type_check_block sym_table ?(funret = None) ?(isfun = false) block =
  if not isfun then begin_block sym_table >. ();
  let rec type_check_block_aux block = (
  (match block with
    | x::xs -> (
      (match (@!)x with
        | Ast.Stmt stmt -> type_check_stmt sym_table ~funret stmt
        | Ast.Dec (typ, identifier) -> add_vardec_to_scope sym_table (identifier, typ) ((@@)x) >. ());
      type_check_block_aux xs
    )
    | _ -> ());
  ) in type_check_block_aux block;
  if not isfun then end_block sym_table >. ()

and type_check_if sym_table ?(funret = None) (guard, thenstmt, elsestmt) =
  let guardtype = type_check_expr sym_table guard in
  if guardtype <> TBool
    then except sym_table (Semantic_error ((@@) guard, Printf.sprintf "Expected guard to be of type %s, got type %s instead" (show_microc_type TBool) (show_microc_type guardtype)));
  type_check_stmt sym_table ~funret thenstmt;
  type_check_stmt sym_table ~funret elsestmt

and type_check_while sym_table ?(funret = None) (guard, body) =
  let guardtype = type_check_expr sym_table guard in
  if guardtype <> TBool
    then except sym_table (Semantic_error ((@@) guard, Printf.sprintf "Expected guard to be of type %s, got type %s instead" (show_microc_type TBool) (show_microc_type guardtype)));
  type_check_stmt sym_table ~funret body


let add_library_functions sym_table =
  add_entry "print" (TFunc([TInt], TVoid)) sym_table >. ();
  add_entry "getint" (TFunc([], TInt)) sym_table >. ()

let type_check _p = 
  match _p with
  | Ast.Prog topdecls ->
    let sym_table = begin_block (empty_table()) in
    add_library_functions sym_table >.
    List.iter (fun td -> add_topdecl_to_scope td sym_table >. ()) topdecls;
    
    let maintype = lookup_opt "main" sym_table in(
      match maintype with
        | None -> except sym_table (Semantic_error (Location.dummy_code_pos, "No main function defined"))
        | Some t -> (
          match t with
            | TFunc ([], TInt) -> ()
            | TFunc ([], TVoid) -> ()
            | _ -> except sym_table (Semantic_error (Location.dummy_code_pos, "Main function has to be either `void main()` or `int main()`"))            
        )
    );

    topdecls
    |> List.filter_map (
      fun td -> match (@!) td with Ast.Fundecl f -> Some f | _ -> None)
    |> List.iter (sym_table |> type_check_fundecl)
    (* >. print_entries sym_table *)
    >. _p