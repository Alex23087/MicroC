exception DuplicateEntry of Ast.identifier
exception SymbolNotFound of Ast.identifier
  
type 'a symbol_record = Ast.identifier * 'a
type 'a symbol_block = ((Ast.identifier, 'a) Hashtbl.t)
type 'a symbol_table = 'a symbol_block Stack.t

type microc_type = 
  | TInt
  | TBool
  | TChar
  | TVoid
  | TPointer of microc_type
  | TArray of microc_type * int option
  | TFunc of microc_type list * microc_type
  [@@deriving show]

val typ_to_microc_type: Ast.typ -> microc_type 

val empty_table : unit -> 'a symbol_table
val begin_block : 'a symbol_table -> 'a symbol_table 
val end_block : 'a symbol_table -> 'a symbol_table
val add_entry : Ast.identifier -> 'a -> 'a symbol_table -> 'a symbol_table 
val lookup : Ast.identifier -> 'a symbol_table -> 'a
val lookup_opt : Ast.identifier -> 'a symbol_table -> 'a option
val of_alist : 'a symbol_record list -> 'a symbol_table 
val print_entries: microc_type symbol_table -> unit
val append_block: 'a symbol_table -> 'a symbol_block -> 'a symbol_table
val lookup_local_block: 'a symbol_table -> Ast.identifier -> 'a option