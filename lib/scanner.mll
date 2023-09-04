{
    open Parser

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string

    let keyword_table = Hashtbl.create 12
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
            ("extern", EXTERN);
            ("declare", EXTERN);
        ]

    let unescape ch lexbuf = match ch with
        | '\''  ->  '\''
        | 'b'   ->  '\b'
        | 'f'   ->  char_of_int 0x0C
        | 't'   ->  '\t'
        | '\\'  ->  '\\'
        | 'r'   ->  '\r'
        | 'n'   ->  '\n'
        | _     ->  raise (Lexing_error ((Location.to_lexeme_position lexbuf), "Invalid escape character"))

}

(* Scanner specification *)
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let alphanumeric = alpha | digit
let hex = digit | ['a'-'f' 'A'-'F']
let escape = '\\' ['\'' 'b' 'f' 't' '\\' 'r' 'n']

let filename = (alphanumeric | ['.' '/' '-' '_'])*
let identifier = (alpha | '_') (alphanumeric | '_')*
let integer = digit+ | ("0x" hex+)
let boolean = "true" | "false"

let operator = ['&' '+' '-' '*' '/' '%' '=' '<' '>' '!'] | "==" | "!=" | "<=" | ">=" | "&&" | "||"

let other = ['(' ')' '{' '}' '[' ']' '&' ';']

let newline = ['\n' '\r'] | "\r\n"
let whitespace = [' ' '\t']

rule next_token = parse
| "/*"      {block_comment lexbuf}
| "//"      {single_line_comment lexbuf}

| "true"        {BOOLEAN (true)}
| "false"       {BOOLEAN (false)}

| "#include" (" ")? '<' (filename as file) '>'    {INCLUDE(file)}

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
                | '\\'  -> unescape (chara.[2]) lexbuf
                | c     -> c
        )
    }

| '('           {LPAREN}
| ')'           {RPAREN}
| '{'           {LBRACE}
| '}'           {RBRACE}
| '['           {LBRACK}
| ']'           {RBRACK}
| '&'           {AMP}
| ','           {COMMA}
| ';'           {SEMICOLON}

| '+'           {PLUS}
| '-'           {MINUS}
| '*'           {STAR}
| '/'           {SLASH}
| '%'           {PERC}
| '='           {GETS}
| '<'           {LT}
| '>'           {GT}
| '!'           {BANG}
| "=="          {EQ}
| "!="          {NEQ}
| "<="          {LEQ}
| ">="          {GEQ}
| "&&"          {LAND}
| "||"          {LOR}
| "++"          {INC}
| "--"          {DEC}
| "+="          {PLEQ}
| "-="          {MINEQ}
| "*="          {TIMEQ}
| "/="          {DIVEQ}
| "%="          {MODEQ}

| whitespace    {next_token lexbuf}
| newline       {Lexing.new_line lexbuf; next_token lexbuf}

| _ as c        {
    raise (Lexing_error (Location.to_lexeme_position lexbuf, Printf.sprintf "Unrecognised character: \'%c\'" c))
}
| eof         {EOF}

and single_line_comment = parse
    | newline  {Lexing.new_line lexbuf; next_token lexbuf}               (* Go back to main scanner *)
    | _     {single_line_comment lexbuf}  (* Ignore comment *)

and block_comment = parse
    | "*/"      {next_token lexbuf}               (* Go back to main scanner *)
    | newline   {Lexing.new_line lexbuf; block_comment lexbuf}
    | _         {block_comment lexbuf}