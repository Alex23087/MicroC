{
    open Parser

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string

    let keyword_table = Hashtbl.create 72
    let _ = List.iter (fun (key, tkn) -> Hashtbl.add keyword_table key tkn)
        [
            ("if", IF);
            ("return", RETURN);
            ("else", ELSE);
            ("for", FOR);
            ("while", WHILE);
            ("int", INT);
            ("char", CHAR);
            ("void", VOID);
            ("NULL", NULL);
            ("bool", BOOL);
        ]

    let unescape ch = match ch with
        | '\''  ->  '\''
        | 'b'   ->  '\b'
        | 'f'   ->  char_of_int 0x0C
        | 't'   ->  '\t'
        | '\\'  ->  '\\'
        | 'r'   ->  '\r'
        | 'n'   ->  '\n'
        | _     ->  raise Lexing_error (Lexing.lexeme_start_p lexbuf) "Invalid escape character"

}

(* Scanner specification *)
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let alphanumeric = alpha | digit
let hex = digit | ['a'-'f' 'A'-'F']
let escape = '\\' ['\'' 'b' 'f' 't' '\\' 'r' 'n']

let identifier = (alpha | '_') (alphanumeric | '_')*
let integer = digit+ | ("0x" hex+)
let boolean = "true" | "false"

let operator = ['&' '+' '-' '*' '/' '%' '=' '<' '>' '!'] | "=" | "!=" | "<=" | ">=" | "&&" | "||"

let other = ['(' ')' '{' '}' '[' ']' '&' ';']

rule next_token = parse
| "/*"      {block_comment lexbuf}
| "//"      {single_line_comment lexbuf}
| identifier as ident
    { try
        Hashtbl.find keyword_table ident
        with Not_found -> IDENTIFIER ident
    }
| integer as intg    {INTEGER (int_of_string intg)} (* TODO: Range checks? *)
| '\'' ((_ | escape) as chara) '\''
    {
        CHARACTER (
            match chara.[1] with
                | '\\'  -> unescape (chara.[2])
                | c     -> c
        )
    }
| _         {EOF}

and single_line_comment = parse
    | '\n'  {next_token lexbuf}               (* Go back to main scanner *)
    | _     {single_line_comment lexbuf}  (* Ignore comment *)

and block_comment = parse
    | "*/"  {next_token lexbuf}               (* Go back to main scanner *)
    | _     {block_comment lexbuf}