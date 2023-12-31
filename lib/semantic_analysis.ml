open Symbol_table
exception Semantic_error of Location.code_pos * string

let except _ exn = (* Debug function to print the symbol table on failure *)
  (* Symbol_table.print_entries sym_table; *)
  raise exn

(* Utility and function-compositing functions *)
let (>.) a b = let _ = a in b   (* `e >. ()` is equivalent to `ignore e` *)
let (@!) node = match node with {Ast.node; _} -> node (* Get node *)
let (@@) node = match node with {Ast.loc; _} -> loc (* Get location*)
let (>>) f g x = g(f(x))
let (>>>) f g x y = g(f(x)(y))

(* Arrays with size < 1 should not be allowed.
   Arrays with no size are only allowed in function signatures. *)
let rec array_size_check sym_table typ array_no_size_allowed loc =
  match typ with
    | TArray (t, size) -> (
      if Option.value size ~default:(if array_no_size_allowed then 2 else 0) < 1 (* Default value is 2 as arrays with no size declared should be valid *)
        then except sym_table (Semantic_error (loc, "Declaring an array of size less than 1"))
        else array_size_check sym_table t array_no_size_allowed loc
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

(* Functions can only return scalar values (and void). This is already enforced by the grammar. *)
let function_return_type_check sym_table typ loc =
  match typ with
    | TFunc (_, ret) -> (
      match ret with
        | TArray _ -> except sym_table (Semantic_error (loc, "Functions cannot return arrays"))
        | TPointer _ -> except sym_table (Semantic_error (loc, "Functions cannot return pointers"))
        | _ -> ()
    )
    | _ -> ()

(* Add a variable to a scope. This also carries out the checks on the validity of the type. *)
let add_vardec_to_scope sym_table (identifier, typ) ?(array_no_size_allowed = false) loc  =
  let microctyp = typ_to_microc_type typ in
  array_size_check sym_table microctyp array_no_size_allowed loc >. ();
  array_dimension_check sym_table microctyp loc >. ();
  if microctyp = TVoid then except sym_table (Semantic_error (loc, Printf.sprintf "Variables cannot be of type %s" (show_microc_type TVoid)));
  function_return_type_check sym_table microctyp loc >. ();
  if Symbol_table.lookup_local_block sym_table identifier = None
    then Symbol_table.add_entry identifier microctyp sym_table
    else except sym_table (Semantic_error (loc, "Variable declared twice"))

let rec add_topdecl_to_scope (td: Ast.topdecl) sym_table =
  match ((@!) td) with
    (* External symbols are added just like regular ones to the symbol table *)
    | Ast.Extern ext -> add_topdecl_to_scope ({Ast.node = ext; Ast.loc = (@@) td}) sym_table
    | Ast.Fundec fd
    | Ast.Fundef (fd, _) -> let {Ast.typ; Ast.fname; Ast.formals} = fd in
      if Symbol_table.lookup_local_block sym_table fname = None then
        sym_table |> Symbol_table.add_entry fname (TFunc (
          formals |> List.map (fst >> typ_to_microc_type),
          typ_to_microc_type typ)
        )
      else except sym_table (Semantic_error ((@@) td, "Function defined twice"))
    | Ast.Vardec (typ, identifier) -> add_vardec_to_scope sym_table (identifier, typ) ((@@) td)
    | Ast.Include _ -> failwith "This shouldn't happen" (* Includes are removed before topdecls are passed here *)
      
(* statement -> ~ret:return has been encountered -> contains return*)
let rec check_deadcode ?(ret = false) stmt =
  match (@!) stmt with
    | Ast.If (_, thenstmt, elsestmt) -> (
      (* Bind to name to force function evaluation *)
      let tret = check_deadcode thenstmt ~ret in
      let eret = check_deadcode elsestmt ~ret in
      tret && eret
    )
    | Ast.While (_, body) -> (
      (* Check deadcode, but return false, as we don't know if the body will be executed *)
      check_deadcode body ~ret >. false
    )
    | Ast.Expr _ -> false (* Expressions cannot return *)
    | Ast.Return _ -> true (* Return returns *)
    | Ast.Block stmts -> (
      match stmts with
        | x::xs -> (
          if ret then raise (Semantic_error ((@@) x, "Dead code"));
          match (@!) x with
            | Ast.Stmt x -> (
              let r = check_deadcode x ~ret in
              (check_deadcode ({Ast.node = Ast.Block xs; Ast.loc = Location.dummy_code_pos}) ~ret:r)
              || r (* A block returns if one of its statements returns. However, the or is
                short-circuiting, so `|| r` has to be placed after checking for more statements. *)
            )
            | Ast.Dec _ -> check_deadcode ({Ast.node = Ast.Block xs; Ast.loc = Location.dummy_code_pos}) ~ret
        )
        | [] -> false
    )

(* Check type compatibility for assignment or parameter passing.
   This is a more coarse relation than equality, as arrays with size
   can be assigned to arrays with no size. *)
let rec type_compatible lht rht =
  match (lht, rht) with
    | (TArray(lt, ls), TArray(rt, rs)) -> (type_compatible lt rt) && (ls = rs || ls = None)
    | (TPointer lt, TPointer rt) -> type_compatible lt rt
    | _ -> lht = rht

let rec type_check_fundef sym_table (fd, body) = 
  let {
    Ast.typ;
    Ast.fname;
    Ast.formals;
  } = fd in 
  let funret = (Some (typ_to_microc_type typ)) in

  let parameter_block = begin_block sym_table in

  (* Add all parameters to the local scope *)
  formals
  |> List.iter (fun (typ, id) -> add_vardec_to_scope parameter_block (id, typ) ~array_no_size_allowed:true ((@@) body) >. ()) >. ();

  type_check_stmt parameter_block ~funret ~isfun:true body;
  (* Check the presence of a return statement if the function is non-void and not main. Typing is checked by type_check_stmt *)
  let returns = check_deadcode body in
  if fname <> "main" && funret <> Some TVoid && not returns then
    except parameter_block (Semantic_error ((@@)body, "No return in non-void function"));
  Symbol_table.end_block parameter_block >. ()

and type_check_stmt sym_table ?(funret = None) ?(isfun = false) stmt = let {Ast.node; Ast.loc} = stmt in match node with
  | Ast.If (guard, thenstmt, elsestmt) -> type_check_if sym_table ~funret (guard, thenstmt, elsestmt)
  | Ast.While (guard, body)            -> type_check_while sym_table ~funret (guard, body)
  | Ast.Expr expr   -> check_double_assign sym_table expr ((@@) expr) >. type_check_expr sym_table expr >. ()
  | Ast.Return expr -> ( (* Check for correct return type *)
    match funret with
      | None -> except sym_table (Semantic_error (loc, "Returning from non-function"))
      | Some funret-> (
        let rettype = Option.fold ~none:TVoid ~some:(type_check_expr sym_table) expr in
        if funret <> rettype
          then except sym_table (Semantic_error (loc, Printf.sprintf "Returning value of type %s from a function that requires a value of type %s" (Symbol_table.show_microc_type rettype) (Symbol_table.show_microc_type funret)))
      )
  )
  | Ast.Block stmts    -> type_check_block sym_table ~funret ~isfun stmts

and type_check_expr sym_table expr =
match (@!) expr with
  | Ast.Access acc -> type_check_access sym_table acc
  | Ast.Assign (acc, exp) -> (
    let atype = type_check_access sym_table acc in
    (* Only char arrays can be assigned for now. *)
    match atype with
      | TArray (t, _) when t <> TChar -> except sym_table (Semantic_error ((@@) expr, "Cannot assign array"))
      | _ ->
    let etype = type_check_expr sym_table exp in
    if type_compatible atype etype
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
  | Ast.FLiteral _  -> TFloat (*(
    let f32 = Int32.float_of_bits (Int32.bits_of_float f) in
    if f <> f32 && Float.abs(f -. f32) > Float.epsilon
      then except sym_table (Semantic_error ((@@) expr, "Literal is not a valid 32 bit float"))
      else TFloat
  )*)
  | Ast.SLiteral s  -> TArray (TChar, Some((String.length s) + 1))
  | Ast.Nullptr -> TPointer TVoid (* The C standard defines a null pointer as (void* )0 *)
  | Ast.UnaryOp (uop, exp)         -> (
    let etype = type_check_expr sym_table exp in
    match (uop, etype) with
    | (Ast.Neg, TInt) -> TInt
    | (Ast.Neg, TFloat) -> TFloat
    | (Ast.Not, TBool) -> TBool
    | _ -> except sym_table (Semantic_error ((@@) expr, Printf.sprintf "Invalid operand for operator \"%s\"" (Ast.show_uop uop)))
  )
  | Ast.BinaryOp (binop, lhs, rhs)  -> (
    let lhstype = type_check_expr sym_table lhs in
    let rhstype = type_check_expr sym_table rhs in
    let compatible lt rt = match (lt, rt) with
      | (TInt, TInt)      -> true
      | (TFloat, TFloat)  -> true
      | (TInt, TFloat)    -> true
      | (TFloat, TInt)    -> true
      | _                 -> false
    in let getT lt rt = match (lt, rt) with
      | (TInt, TInt)      -> TInt
      | (TFloat, TFloat)  -> TFloat
      | (TInt, TFloat)    -> TFloat
      | (TFloat, TInt)    -> TFloat
      | _                 -> failwith "Impossible"
    in
    match (binop, lhstype, rhstype) with
      | (Ast.Add, _, _)         when compatible lhstype rhstype -> getT lhstype rhstype
      | (Ast.Sub, _, _)         when compatible lhstype rhstype -> getT lhstype rhstype
      | (Ast.Mult, _, _)        when compatible lhstype rhstype -> getT lhstype rhstype
      | (Ast.Div, _, _)         when compatible lhstype rhstype -> getT lhstype rhstype
      | (Ast.Mod, _, _)         when compatible lhstype rhstype -> getT lhstype rhstype
      | (Ast.Equal, l, r)       when l = r                      -> TBool
      | (Ast.Neq, l, r)         when l = r                      -> TBool
      | (Ast.Less, _, _)        when compatible lhstype rhstype -> TBool
      | (Ast.Leq, _, _)         when compatible lhstype rhstype -> TBool
      | (Ast.Greater, _, _)     when compatible lhstype rhstype -> TBool
      | (Ast.Geq, _, _)         when compatible lhstype rhstype -> TBool
      | (Ast.And, TBool, TBool)                                 -> TBool
      | (Ast.Or, TBool, TBool)                                  -> TBool
      
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
  | Ast.Prepost (_, _, acc) -> (
    let typ = type_check_access sym_table acc in
    if typ = TInt || typ = TFloat
      then typ
      else except sym_table (Semantic_error ((@@) expr, "Trying to increment or decrement non-numerical value"))
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
  (* Block needs to be created only if this is not the top block in a
     function definition, otherwise function parameters could be shadowed
     as in Java. *)
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

(* Assigning to the same variable twice in the same statement is illegal, as it could lead to UB *)
and check_double_assign sym_table expr loc =
  let assigned_vars = Hashtbl.create 5 in
  let add_or_fail (var: string) =
    if (assigned_vars |> Hashtbl.find_opt) var |> Option.is_some
      then except sym_table (Semantic_error (loc, Printf.sprintf "Variable %s assigned twice in the same statement" var))
      else (assigned_vars |> Hashtbl.add) var true in
  let rec cda_aux expr =
    match expr with
      | Ast.Access acc -> (
        match (@!) acc with
          | Ast.AccVar _ -> ()
          | Ast.AccDeref expr -> cda_aux ((@!)expr)
          | Ast.AccIndex (acc, ind) -> cda_aux (Ast.Access(acc)); cda_aux ((@!)ind)
      )
      | Ast.Assign (acc,expr) -> ((
        match (@!) acc with
          | Ast.AccVar v -> add_or_fail v
          | _ -> () 
      ); cda_aux ((@!)expr))
      | Ast.Addr acc -> cda_aux (Ast.Access(acc))
      | Ast.UnaryOp (_, expr) -> cda_aux ((@!)expr)
      | Ast.BinaryOp (_, lexpr, rexpr) -> cda_aux ((@!)lexpr); cda_aux ((@!)rexpr)
      | Ast.Call (_, exprs) -> exprs |> List.iter (fun expr -> cda_aux ((@!)expr))
      | Ast.Prepost (_, _, acc) ->  ((
        match (@!) acc with
          | Ast.AccVar v -> add_or_fail v
          | _ -> () 
      ))
      | Ast.ILiteral _
      | Ast.CLiteral _
      | Ast.BLiteral _
      | Ast.FLiteral _
      | Ast.SLiteral _
      | Ast.Nullptr -> ()
  in cda_aux ((@!) expr)

let add_library_functions sym_table =
  add_entry "print" (TFunc([TInt], TVoid)) sym_table >. ();
  add_entry "getint" (TFunc([], TInt)) sym_table >. ()

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

(* Table that contains all included interfaces, to avoid includin them multiple times
   (this eliminates the issue of circular dependencies). *)
let include_table = Hashtbl.create 5

(* Take an AST and enqueue all the topdecls to be checked, merging them ones from included interfaces. *)
let rec handle_program topdecl_queue ?(is_included = false) _p =
  let handle_program_aux = handle_program topdecl_queue in
  let add_topdecl td = match (@!) td with
    | Ast.Include lib -> ( (* Check if the file has already been included *)
      if Hashtbl.find_opt include_table lib |> Option.is_none
        then (Hashtbl.add include_table lib true; lib |> process_source |> handle_program_aux ~is_included:true)
    )
    | Ast.Fundec _ when not is_included -> raise (Semantic_error ((@@)td, "Function declarations cannot appear outside of interfaces"))
    | Ast.Fundef _ when is_included     -> raise (Semantic_error (Location.dummy_code_pos, "Function definitions cannot appear inside interfaces")) (* Dummy pos is used because it's an error in an included file, which is not handled by the error display code *)
    | Ast.Extern _ when is_included     -> raise (Semantic_error (Location.dummy_code_pos, "Cannot use \"extern\" keyword in included interfaces, all symbols are extern by default"))
    | Ast.Fundec _
    | Ast.Extern _
    | Ast.Vardec _
    | Ast.Fundef _ -> Queue.add td topdecl_queue
  in
  match _p with
  | Ast.Prog topdecls ->
    topdecls |> List.iter add_topdecl

(* Main type checking function *)
let type_check _p = 
  let sym_table = begin_block (empty_table()) in
  add_library_functions sym_table >. ();

  (* Add all topdecls to the queue, including the included ones *)
  let topdecl_queue = Queue.create() in
  handle_program topdecl_queue _p;

  (* Add them all to the global scope, effectively hoisting all global symbols *)
  topdecl_queue |> Queue.iter (fun td -> add_topdecl_to_scope td sym_table >. ());
  
  (* Check typing of the main function*)
  let maintype = lookup_opt "main" sym_table in(
    match maintype with
      (* Main function presence check has been removed to enable separate compilation *)
      (* | None -> except sym_table (Semantic_error (Location.dummy_code_pos, "No main function defined")) *)
      | Some t -> (
        match t with
          | TFunc ([], TInt) -> ()
          | TFunc ([], TVoid) -> ()
          | _ -> except sym_table (Semantic_error (Location.dummy_code_pos, "Main function has to be either `void main()` or `int main()`"))            
      )
      | _ -> ()
  );

  (* Check all the function bodies *)
  topdecl_queue
  |> Queue.to_seq
  |> Seq.filter_map (
    fun td -> match (@!) td with Ast.Fundef f -> Some f | _ -> None)
  |> Seq.iter (sym_table |> type_check_fundef)
  (* >. print_entries sym_table *)
  >. _p