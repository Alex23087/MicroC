exception Semantic_error of Location.code_pos * string

let (>>) a b = let _ = a in b
let (@!) node = match node with {Ast.node = n; _} -> n

type microc_type = 
  | TInt
  | TBool
  | TChar
  | TVoid
  | TPointer of microc_type
  | TArray of microc_type
  | TFunc of microc_type list * microc_type

let rec typ_to_microc_type (typ: Ast.typ) = match typ with
  | Ast.TypI  -> TInt
  | Ast.TypB  -> TBool
  | Ast.TypC  -> TChar
  | Ast.TypV  -> TVoid
  | Ast.TypP typp  -> TPointer (typ_to_microc_type typp)
  | Ast.TypA (typa, _) -> TArray (typ_to_microc_type typa)
  
let add_topdecl_to_scope (td: Ast.topdecl) sym_table = match ((@!) td) with
    | Ast.Fundecl {Ast.typ = typ; Ast.fname = fname; Ast.formals = formals; Ast.body = _} ->
      Symbol_table.add_entry fname (TFunc (
        List.map (fun entry -> entry |> fst |> typ_to_microc_type) formals, 
        typ_to_microc_type typ)) sym_table
    | Ast.Vardec (typ, identifier) ->
      Symbol_table.add_entry identifier (typ_to_microc_type typ) sym_table

let type_check _p = match _p with
  | Ast.Prog topdecls ->
    let sym_table = Symbol_table.begin_block Symbol_table.empty_table in
    List.iter (fun td -> add_topdecl_to_scope td sym_table >> ()) topdecls;