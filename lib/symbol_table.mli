exception DuplicateEntry of Ast.identifier
exception SymbolNotFound of Ast.identifier

type 'a symbol_record
type 'a symbol_block
type 'a symbol_table

val empty_table : 'a symbol_table
val begin_block : 'a symbol_table -> 'a symbol_table 
val end_block : 'a symbol_table -> 'a symbol_table
val add_entry : Ast.identifier -> 'a -> 'a symbol_table -> 'a symbol_table 
val lookup : Ast.identifier -> 'a symbol_table -> 'a
val of_alist : (Ast.identifier * 'a) list -> 'a symbol_table 
