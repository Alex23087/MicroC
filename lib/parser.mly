/*
* MicroC Parser specification
*/

%{
     (* Auxiliary definitions *)   

%}

/* Tokens declarations */
%token EOF
%token IF
%token RETURN
%token ELSE
%token FOR
%token WHILE
%token INT
%token CHAR
%token VOID
%token NULL
%token BOOL
%token <string> IDENTIFIER
%token <int> INTEGER
%token <float> FLOAT
%token <char> CHARACTER
%token GETS LOR LAND EQ NEQ GT LT GEQ LEQ PLUS MINUS TIMES DIV MOD NOT BAND LBRACK

/* Precedence and associativity specification */
// %right    GETS              /* lowest precedence */
// %left     LOR
// %left     LAND
// %left     EQ  NEQ 
// %nonassoc GT LT GEQ LEQ
// %left     PLUS MINUS
// %left     TIMES DIV MOD
// %nonassoc NOT BAND
// %nonassoc LBRACK             /* highest precedence  */

/* Starting symbol */

%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */

%%

/* Grammar specification */

program:
  |  EOF                      {Ast.Prog([])}
;
