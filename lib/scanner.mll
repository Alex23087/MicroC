{
    open Parser

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string
    
    (* Buffer used to parse string literals *)
    let string_buffer = Buffer.create 256

    let keyword_table = Hashtbl.create 12
    let _ = List.iter (fun (key, tkn) -> Hashtbl.add keyword_table key tkn)
        [
            ("if", IF);
            ("return", RETURN);
            ("else", ELSE);
            ("for", FOR);
            ("while", WHILE);
            ("int", INT);
            ("float", FLT);
            ("char", CHAR);
            ("void", VOID);
            ("NULL", NULL);
            ("bool", BOOL);
            ("extern", EXTERN);
            ("declare", EXTERN);
        ]

    (* Transform an escape sequence into the escaped character *)
    let unescape ch lexbuf = match ch with
        | '0'   ->  char_of_int 0x00
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

(* Useful patterns *)
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let alphanumeric = alpha | digit
let hex = digit | ['a'-'f' 'A'-'F']
(* Escaped sequence, including leading backslash *)
let escape = '\\' ['0' '\'' 'b' 'f' 't' '\\' 'r' 'n']
(* Filenames allowed for includes. Not a complete set
of characters allowed in files, but a sufficient one *)
let filename = (alphanumeric | ['.' '/' '-' '_'])*
let identifier = (alpha | '_') (alphanumeric | '_')*
let integer = digit+ | ("0x" hex+)
let float = ((digit* '.' digit+) ('f')?) | (integer 'f') 
let boolean = "true" | "false"

(* Unused *)
let operator = ['&' '+' '-' '*' '/' '%' '=' '<' '>' '!'] | "==" | "!=" | "<=" | ">=" | "&&" | "||"
(* Unused *)
let other = ['(' ')' '{' '}' '[' ']' '&' ';']

let newline = ['\n' '\r'] | "\r\n"
let whitespace = [' ' '\t']

rule next_token = parse
| '"'       {
    Buffer.clear string_buffer;
    string_literal lexbuf;
    STRING (Buffer.contents string_buffer)
}
| "/*"      {block_comment lexbuf}
| "//"      {single_line_comment lexbuf}

| "true"        {BOOLEAN (true)}
| "false"       {BOOLEAN (false)}

| "#" ("include" | "import") (whitespace)* '<' (whitespace)* (filename as file) (whitespace)* '>'    {INCLUDE(file)}

| identifier as ident
    { try
        Hashtbl.find keyword_table ident
        with Not_found -> IDENTIFIER ident
    }
| float as flot     {FLOAT (
    if flot |> String.ends_with ~suffix:"f"
        then let s = String.sub flot 0 ((String.length flot) - 1) in
            Float.of_string s
        else Float.of_string flot
)}
| integer as intg    {INTEGER (int_of_string intg)}
| "'" ((_ | escape) as chara) "'"
    {
        CHARACTER (
            match chara.[0] with
                | '\\'  -> unescape (chara.[1]) lexbuf
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

| eof         {EOF}
| _ as c        {
    raise (Lexing_error (Location.to_lexeme_position lexbuf, Printf.sprintf "Unrecognised character: \'%c\'" c))
}

and single_line_comment = parse
    | newline   {Lexing.new_line lexbuf; next_token lexbuf}               (* Go back to main scanner *)
    | eof       {EOF}
    | _         {single_line_comment lexbuf}  (* Ignore comment *)

and block_comment = parse
    | "*/"      {next_token lexbuf}               (* Go back to main scanner *)
    | newline   {Lexing.new_line lexbuf; block_comment lexbuf}
    | eof       {raise (Lexing_error (Location.to_lexeme_position lexbuf, "Unterminated block comment"))}
    | _         {block_comment lexbuf}

and string_literal = parse
    | '"'       {()}
    | eof       {raise (Lexing_error (Location.to_lexeme_position lexbuf, "Unterminated string"))}
    | ((_ | escape) as chara)
    {
        Buffer.add_char string_buffer
        (
            match chara.[0] with
                | '\\'  -> unescape (chara.[1]) lexbuf
                | c     -> c
        ); string_literal lexbuf
    }