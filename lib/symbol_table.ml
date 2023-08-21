exception DuplicateEntry of Ast.identifier
exception SymbolNotFound of Ast.identifier

let (>>) a b = let _ = a in b

type 'a symbol_record = Ast.identifier * 'a
type 'a symbol_block = ((Ast.identifier, 'a) Hashtbl.t)
type 'a symbol_table = 'a symbol_block Stack.t

let empty_table: 'a symbol_table = Stack.create ()

let begin_block (current_table: 'a symbol_table) = Stack.push (Hashtbl.create 1) current_table; current_table

let end_block (current_table: 'a symbol_table) = let _ = Stack.pop current_table in current_table

let add_entry (id: Ast.identifier) (a: 'a) (current_table: 'a symbol_table) = 
  let current_block = Stack.top current_table in
    match Hashtbl.find_opt current_block id with
      | Some _  -> raise (DuplicateEntry id)
      | None    -> Hashtbl.add current_block id a;
    current_table

let lookup_opt (id: Ast.identifier) (current_table: 'a symbol_table) = 
  let lookup_local_block (seq: 'a symbol_block Seq.t) =
    Option.bind (Seq.uncons seq) (fun blk -> Hashtbl.find_opt (fst blk) id)
  in let rec lookup_rec (seq: 'a symbol_block Seq.t) =
    match lookup_local_block seq with
    | None      -> Option.bind (Seq.uncons seq) (fun (_, xs) -> lookup_rec xs)
    | Some typ  -> Some typ
  in lookup_rec (Stack.to_seq current_table)

let lookup i c = match lookup_opt i c with
  | Some a  -> a
  | None    -> raise (SymbolNotFound i)

let of_alist (lst: 'a symbol_record list) =
  let table = empty_table in
    let table = begin_block table in
    List.iter (fun (id, typ) -> add_entry id typ table >> ()) lst;
    table