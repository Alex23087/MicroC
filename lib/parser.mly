/*
* MicroC Parser specification
*/

%{
     (* Auxiliary definitions *)   
     let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)
     let annotate_node node loc = {Ast.loc = Location.to_code_position loc; node = node}

     type vardesc = 
      | VDPlain   of string
      | VDPointer of vardesc
      | VDArray   of vardesc * int option

    (* Translate expressions like `x += y` into `x = x + y` *)
    let build_op_eq op lex ex loc =
      annotate_node(Ast.Assign(
        lex, 
        annotate_node(Ast.BinaryOp(
          op,
          annotate_node(Ast.Access(lex)) dummy_pos,
          ex
        )) dummy_pos
      )) loc
%}

/* Tokens declarations */
%token EOF
%token IF
%token RETURN
%token THEN ELSE
%token FOR
%token WHILE
%token INT
%token CHAR
%token VOID
%token NULL
%token BOOL
%token FLT
%token <string> IDENTIFIER
%token <int> INTEGER
%token <float> FLOAT
%token <char> CHARACTER
%token <bool> BOOLEAN
%token <string> STRING
%token GETS LOR LAND EQ NEQ GT LT GEQ LEQ PLUS MINUS STAR SLASH PERC BANG AMP LBRACK
%token LPAREN RPAREN RBRACK LBRACE RBRACE SEMICOLON COMMA
%token INC DEC
%token PLEQ MINEQ TIMEQ DIVEQ MODEQ
%token <string> INCLUDE
%token EXTERN

/* Precedence and associativity specification */
%nonassoc THEN
%nonassoc ELSE
%right    GETS PLEQ MINEQ TIMEQ DIVEQ MODEQ   /* lowest precedence */
%left     LOR
%left     LAND
%left     EQ  NEQ 
%nonassoc GT LT GEQ LEQ
%left     PLUS MINUS
%left     STAR SLASH PERC
// %left     INC DEC
%nonassoc BANG AMP
%nonassoc LBRACK             /* highest precedence  */

/* Starting symbol */

%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */
%type <Ast.topdecl> topdecl
%type <Ast.typ * Ast.identifier> vardecl
%type <Ast.fun_def> fundef
%type <Ast.fun_dec> fundecl
%type <vardesc> vardesc
%type <Ast.stmt> block
%type <Ast.stmtordec> stmtordec
%type <Ast.stmt> stmt
%type <Ast.typ> typ
%type <Ast.expr> expr
%type <Ast.access> lexpr
%type <Ast.expr> rexpr
%type <Ast.expr> aexpr
%type <Ast.binop> binop

%%

/* Grammar specification */

program:
  | tds = list(topdecl) EOF       {Ast.Prog(tds)}

topdecl:
  | EXTERN vd = vardecl SEMICOLON {let (typ, desc) = vd in annotate_node(Ast.Extern(Ast.Vardec(typ, desc))) $loc}
  | EXTERN fd = fundecl SEMICOLON {annotate_node(Ast.Extern(Ast.Fundec fd)) $loc}
  | fd = fundecl SEMICOLON        {annotate_node(Ast.Fundec fd) $loc}
  | vd = vardecl SEMICOLON        {let (typ, desc) = vd in annotate_node(Ast.Vardec(typ, desc)) $loc}
  | fd = fundef                   {annotate_node(Ast.Fundef(fd)) $loc}
  | inc = INCLUDE                 {annotate_node(Ast.Include(inc)) $loc}

vardecl:
  | t = typ vd = vardesc          {
    let rec build_type_id_tuple_from_vardesc ty vard =
      match vard with
        | VDPlain id          -> (ty, id)
        | VDPointer vd        -> build_type_id_tuple_from_vardesc (Ast.TypP ty) vd
        | VDArray (vd, size)  -> build_type_id_tuple_from_vardesc (Ast.TypA (ty, size)) vd
    in build_type_id_tuple_from_vardesc t vd
  }

vardesc:
  | id = IDENTIFIER                           {VDPlain id}
  | LPAREN vd = vardesc RPAREN                {vd}
  | STAR vd = vardesc                         {VDPointer vd}
  | vd = vardesc LBRACK RBRACK                {VDArray (vd, None)}
  | vd = vardesc LBRACK size = INTEGER RBRACK {VDArray (vd, (Some size))}

fundecl:
  | t = typ id = IDENTIFIER LPAREN params = separated_list(COMMA, vardecl) RPAREN {{
    Ast.typ = t;
    Ast.fname = id;
    Ast.formals = params;
  }}

fundef:
  | fd = fundecl b = block         {(fd, b)}

stmtordec:
  | vd = vardecl SEMICOLON                  {annotate_node(Ast.Dec (fst vd, snd vd)) $loc}
  | s = stmt                                {annotate_node(Ast.Stmt(s)) $loc}

block:
  | LBRACE stmts = list(stmtordec) RBRACE        {annotate_node(Ast.Block(stmts)) $loc}

typ:
  | INT                           {Ast.TypI}
  | CHAR                          {Ast.TypC}
  | VOID                          {Ast.TypV}
  | BOOL                          {Ast.TypB}
  | FLT                           {Ast.TypF}

stmt:
  | block                              {$1}
  | RETURN ex = option(expr) SEMICOLON {annotate_node(Ast.Return(ex)) $loc}
  | ex = option(expr) SEMICOLON        {
    annotate_node(
      match ex with
        | Some e -> Ast.Expr(e)
        | None   -> Ast.Block([])
    ) $loc
  }
  | WHILE LPAREN guard = expr RPAREN body = stmt {annotate_node(Ast.While(guard, body)) $loc}
  | FOR LPAREN init = option(expr) SEMICOLON guard = option(expr) SEMICOLON incr = option(expr) RPAREN body = stmt {
    (* Transform a for statement into an equivalent while statement *)
    annotate_node(
      Ast.Block([ (* Block that will contain the statements making up the loop *)
        (match init with (* Create initialisation statement. Empty statement if it is not present *)
          | Some i  -> annotate_node (Ast.Stmt(
              annotate_node (Ast.Expr i) ($startpos(init), $endpos(init))
            )) ($startpos(init), $endpos(init))
          | None    -> annotate_node (Ast.Stmt(
              annotate_node (Ast.Block([])) dummy_pos
            )) dummy_pos);

        annotate_node (Ast.Stmt(
        (annotate_node (Ast.While(
          (match guard with (* Guard expression. Always true if empty *)
            | Some g  -> g
            | None    -> annotate_node (Ast.BLiteral(true)) dummy_pos),
          
          annotate_node (
            Ast.Block([ (* Block that contains the body of the loop and the increment expression *)
              annotate_node (Ast.Stmt(body)) ($startpos(body), $endpos(body));
              
              (match incr with  (* Increment statement. Empty if not present *)
                | Some i  -> annotate_node (Ast.Stmt(
                    annotate_node (Ast.Expr i) ($startpos(incr), $endpos(incr))
                  )) ($startpos(incr), $endpos(incr))
                | None    -> annotate_node (Ast.Stmt(
                    annotate_node (Ast.Block([])) dummy_pos
                  )) dummy_pos);
            ])) dummy_pos
        )) $loc))) dummy_pos
      ])
    ) $loc

  } 
  | IF LPAREN guard = expr RPAREN thenS = stmt ELSE elseS = stmt    {annotate_node(Ast.If(guard, thenS, elseS)) $loc}
  | IF LPAREN guard = expr RPAREN thenS = stmt        %prec THEN    {annotate_node(Ast.If(guard, thenS, annotate_node(Ast.Block([])) dummy_pos)) $loc}

expr:
  | lexpr {annotate_node(Ast.Access($1)) $loc}
  | rexpr {$1}

lexpr:
  | id = IDENTIFIER                         {annotate_node(Ast.AccVar(id)) $loc}
  | LPAREN ex = lexpr RPAREN                {ex}
  | STAR ex = lexpr                         {annotate_node(Ast.AccDeref(annotate_node (Ast.Access ex) $loc)) $loc}
  | STAR ex = aexpr                         {annotate_node(Ast.AccDeref(ex)) $loc}
  | ex = lexpr LBRACK ind = expr RBRACK     {annotate_node(Ast.AccIndex(ex, ind)) $loc}

rexpr:
  | aexpr                                                              {$1}
  | id = IDENTIFIER LPAREN params = separated_list(COMMA, expr) RPAREN {annotate_node(Ast.Call(id, params)) $loc}
  | lex = lexpr GETS ex = expr                                         {annotate_node(Ast.Assign(lex, ex)) $loc}
  | lex = lexpr PLEQ ex = expr                                         {build_op_eq Ast.Add lex ex $loc}
  | lex = lexpr MINEQ ex = expr                                        {build_op_eq Ast.Sub lex ex $loc}
  | lex = lexpr TIMEQ ex = expr                                        {build_op_eq Ast.Mult lex ex $loc}
  | lex = lexpr DIVEQ ex = expr                                        {build_op_eq Ast.Div lex ex $loc}
  | lex = lexpr MODEQ ex = expr                                        {build_op_eq Ast.Mod lex ex $loc}
  | BANG ex = expr                                                     {annotate_node(Ast.UnaryOp(Ast.Not, ex)) $loc}
  | MINUS ex = expr                                                    {annotate_node(Ast.UnaryOp(Ast.Neg, ex)) $loc}
  | lhs = expr bo = binop rhs = expr                                   {annotate_node(Ast.BinaryOp(bo, lhs, rhs)) $loc}
  | ex = lexpr INC                                                     {annotate_node(Ast.Prepost(Ast.Post, Ast.Incr, ex)) $loc}
  | INC ex = lexpr                                                     {annotate_node(Ast.Prepost(Ast.Pre, Ast.Incr, ex)) $loc}
  | ex = lexpr DEC                                                     {annotate_node(Ast.Prepost(Ast.Post, Ast.Decr, ex)) $loc}
  | DEC ex = lexpr                                                     {annotate_node(Ast.Prepost(Ast.Pre, Ast.Decr, ex)) $loc}

%inline binop:
  | PLUS    {Ast.Add}
  | MINUS   {Ast.Sub}
  | STAR    {Ast.Mult}
  | PERC    {Ast.Mod}
  | SLASH   {Ast.Div}
  | LAND    {Ast.And}
  | LOR     {Ast.Or}
  | LT      {Ast.Less}
  | GT      {Ast.Greater}
  | LEQ     {Ast.Leq}
  | GEQ     {Ast.Geq}
  | EQ      {Ast.Equal}
  | NEQ     {Ast.Neq}

aexpr:
  | i = INTEGER                   {annotate_node(Ast.ILiteral(i)) $loc}
  | c = CHARACTER                 {annotate_node(Ast.CLiteral(c)) $loc}
  | b = BOOLEAN                   {annotate_node(Ast.BLiteral(b)) $loc}
  | f = FLOAT                     {annotate_node(Ast.FLiteral(f)) $loc}
  | s = STRING                    {annotate_node(Ast.SLiteral(s)) $loc}
  | NULL                          {annotate_node(Ast.Nullptr) $loc}
  | LPAREN ex = rexpr RPAREN      {ex}
  | AMP ex = lexpr                {annotate_node (Ast.Addr(ex)) $loc}