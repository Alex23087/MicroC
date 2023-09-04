exception DuplicateEntry of Ast.identifier
exception SymbolNotFound of Ast.identifier

let (>.) a b = let _ = a in b
  
type 'a symbol_record = Ast.identifier * 'a
type 'a symbol_block = ((Ast.identifier, 'a) Hashtbl.t)
type 'a symbol_table = ('a symbol_block) Stack.t

type microc_type = 
  | TInt
  | TBool
  | TChar
  | TVoid
  | TPointer of microc_type
  | TArray of microc_type * int option
  | TFunc of microc_type list * microc_type
  | TFloat
  [@@deriving show]

let rec typ_to_microc_type (typ: Ast.typ) = match typ with
  | Ast.TypI  -> TInt
  | Ast.TypB  -> TBool
  | Ast.TypC  -> TChar
  | Ast.TypV  -> TVoid
  | Ast.TypP typp  -> TPointer (typ_to_microc_type typp)
  | Ast.TypA (typa, size) -> TArray ((typ_to_microc_type typa), size)
  | Ast.TypF  -> TFloat


let empty_table = Stack.create

let begin_block (current_table: 'a symbol_table) = Stack.push (Hashtbl.create 1) current_table; current_table

let end_block (current_table: 'a symbol_table) = let _ = Stack.pop current_table in current_table

let add_entry (id: Ast.identifier) (a: 'a) (current_table: 'a symbol_table) = 
  let current_block = Stack.top current_table in
    match Hashtbl.find_opt current_block id with
      | Some _  -> raise (DuplicateEntry id)
      | None    -> Hashtbl.add current_block id a;
    current_table


let lookup_local_block (table: 'a symbol_table) id = let seq = Stack.to_seq table in
  Option.bind (Seq.uncons seq) (fun blk -> Hashtbl.find_opt (fst blk) id)

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
  let table = empty_table () in
    let table = begin_block table in
    List.iter (fun (id, typ) -> add_entry id typ table >. ()) lst;
    table

let reverse_stack stack =
  let rev = Stack.create() in
  stack |> Stack.iter (fun e -> Stack.push e rev) >. rev

let print_entries (table: microc_type symbol_table) =
  let sqn = table |> reverse_stack |> Stack.to_seq
  in let print_block (blk: microc_type symbol_block) =
    Hashtbl.iter (fun id a -> Format.print_string id; Format.print_string ": "; Format.print_string (show_microc_type a); Format.print_string "\n"; Format.print_break 0 0) blk;
  in let rec print_entries_rec indent sqn = match Seq.uncons sqn with
    | None                -> ()
    | Some (x, xs)        -> Format.print_string "{\n"; Format.print_break 0 0; Format.open_box 4; print_block x; print_entries_rec (indent + 1) xs;Format.close_box (); Format.print_string "}\n"; Format.print_break 0 0;
  in print_entries_rec 0 sqn
  (*TODO: Printing and indentation are reversed, because the recursion is done from the top, which is the inner scope, fix*)

(* let print_entries (table: microc_type symbol_table) =
  let sqn = Stack.to_seq table
  in let print_block (blk: microc_type symbol_block) =
    Hashtbl.iter (fun id a -> Format.print_string id; Format.print_string (show_microc_type a); Format.print_break 0 0) blk;
  in let rec print_entries_rec sqn = match Seq.uncons sqn with
    | None                -> 0
    | Some (x, xs)        -> Format.open_box 0; let ind = (print_entries_rec xs) + 1 in print_block x; Format.close_box (); ind
  in print_entries_rec sqn >. ()

let print_entries2 (table: microc_type symbol_table) =
  let sqn = Stack.to_seq table
  in let print_block (blk: microc_type symbol_block) =
    Hashtbl.iter (fun id a -> Format.print_string id; Format.print_string (show_microc_type a); Format.print_break 0 0) blk;
  in  *)

let append_block (table: 'a symbol_table) (block: 'a symbol_block): 'a symbol_table = Stack.push block table; table
(* let append_table (table: 'a symbol_table) (block: 'a symbol_table): 'a symbol_table = (table |> Stack.to_seq |> Seq.append) (block |> Stack.to_seq) |> Stack.of_seq *)

(* let reverse_stack stack =
  let rec rev_stack_aux s r = match Stack.pop_opt s with
    | Some a -> Stack.push a r; rev_stack_aux s r
    | None -> ()
  in let reversed = Stack.create()
  in rev_stack_aux stack reversed;
  reversed *)