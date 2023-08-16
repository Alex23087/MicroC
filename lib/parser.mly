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
%token <bool> BOOLEAN
%token GETS LOR LAND EQ NEQ GT LT GEQ LEQ PLUS MINUS STAR SLASH PERC BANG AMP LBRACK
%token LPAREN RPAREN RBRACK LBRACE RBRACE SEMICOLON COMMA

/* Precedence and associativity specification */
// %right    GETS              /* lowest precedence */
// %left     LOR
// %left     LAND
// %left     EQ  NEQ 
// %nonassoc GT LT GEQ LEQ
// %left     PLUS MINUS
// %left     STAR SLASH PERC
// %nonassoc BANG AMP
// %nonassoc LBRACK             /* highest precedence  */

/* Starting symbol */

%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */
%type <Ast.topdecl> topdecl
%type <Ast.typ * Ast.identifier> vardecl
%type <Ast.fun_decl> fundecl
%type <Ast.identifier> vardesc
%type <Ast.stmt> block
%type <Ast.stmtordec> stmtordec
%type <Ast.typ> typ
%type <Ast.expr> expr
%type <Ast.expr> lexpr
%type <Ast.expr> rexpr

%%

/* Grammar specification */

program:
  | EOF                            {Ast.Prog([])}
  | tds = list(topdecl) EOF        {Ast.Prog(tds)}

topdecl:
  | vd = vardecl SEMICOLON          {vd}
  | fd = fundecl                    {fd}

vardecl:
  | t = typ vd = vardesc            {(t, vd)}

vardesc:
  | id = IDENTIFIER                 {id}
  | LPAREN id = IDENTIFIER RPAREN   {id}
  //TODO: Add remaining vardescs

fundecl:
  | t = typ id = IDENTIFIER LPAREN params = separated_list(COMMA, vardecl) RPAREN b = block         {{
    typ = t;
    fname = id;
    formals = params;
    body = b
  }}

stmtordec:
  | t = typ id = IDENTIFIER       {Ast.Dec(t, id)}
  | s = stmt                      {Ast.Stmt(stmt)}

block:
  | RBRACE stmts = separated_list(SEMICOLON, stmtordec) LBRACE        {Ast.Block(stmts)}

typ:
  | INT                           {Ast.TypI}
  | CHAR                          {Ast.TypC}
  | VOID                          {Ast.TypV}
  | BOOL                          {Ast.TypB}

stmt:
  | RETURN ex = option(expr) SEMICOLON    {Ast.Return(ex)}
  | ex = option(expr) SEMICOLON           {Ast.Expr(expr)}
  | WHILE LPAREN guard = expr RPAREN body = stmt  {Ast.While(expr, stmt)}
  //TODO: Add for
  // | FOR LPAREN init = option(expr) SEMICOLON guard = option(expr) SEMICOLON incr = option(expr) RPAREN body = stmt        {Ast.} 
  | IF LPAREN guard = expr RPAREN thenS = stmt ELSE elseS = expr    {Ast.If(guard, thenS, elseS)}
  //TODO: Add if with no else
  // | IF LPAREN guard = expr RPAREN thenS = stmt                      {}

expr:
  | lexpr {$1}
  | rexpr {$1}

lexpr:
  | id = IDENTIFIER       {}
  | LPAREN ex = lexpr RPAREN
  //TODO: Add remaining lexprs

rexpr:
  | aexpr
  | id = IDENTIFIER LPAREN separated_list(COMMA, expr) RPAREN
  | lex = lexpr GETS ex = expr
  | BANG ex = expr
  | MINUS ex = expr
  | lhs = expr bo = binop rhs = expr

binop:
  | PLUS
  | MINUS
  | STAR
  | PERC
  | SLASH
  | LAND
  | LOR
  | LT
  | GT
  | LEQ
  | GEQ
  | EQ
  | NEQ

aexpr:
  | i = INTEGER
  | c = CHARACTER
  | b = BOOLEAN
  | NULL
  | LPAREN ex = rexpr RPAREN
  | AMP ex = lexpr
