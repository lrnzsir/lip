%{
open Ast
%}


%token LPAREN
%token RPAREN

%token LBRACES
%token RBRACES
%token SKIP
%token INT
%token BOOL
%token FUN
%token RETURN
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token ASSIGN
%token SEMICOLON

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token LEQ
%token EQ
%token ADD
%token SUB
%token MUL

%token <string> ID
%token <string> NUM
%token EOF


%nonassoc IF WHILE
%right OR
%right AND
%nonassoc NOT
%nonassoc EQ LEQ
%left ADD SUB
%left MUL
%nonassoc INT BOOL FUN RETURN
%left SEMICOLON
%nonassoc THEN ELSE DO ASSIGN
%nonassoc LPAREN RPAREN LBRACES RBRACES ID NUM

%start <prog> prog

%%

prog:
  | d = decl; c = cmd; EOF { Prog(d, c) }
  | c = cmd; EOF { Prog(EmptyDecl, c) }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | x = ID { Var(x) }
  | n = NUM { Const(int_of_string n) }
  | NOT; e1 = expr { Not(e1) }
  | e1 = expr; AND; e2 = expr { And(e1, e2) }
  | e1 = expr; OR; e2 = expr { Or(e1, e2) }
  | e1 = expr; ADD; e2 = expr { Add(e1, e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1, e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1, e2) }
  | e1 = expr; EQ; e2 = expr { Eq(e1, e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1, e2) }
  | f = ID; LPAREN; e = expr; RPAREN { Call(f, e) }
  | LPAREN; e = expr; RPAREN { e }
;

decl:
  | INT; x = ID; SEMICOLON { IntVar(x) }
  | INT; x = ID; SEMICOLON; d = decl { DSeq(IntVar(x), d) }
  | FUN; f = ID; LPAREN; x = ID; RPAREN; LBRACES; c = cmd; SEMICOLON; RETURN; e = expr; RBRACES; SEMICOLON { Fun(f, x, c, e) }
  | FUN; f = ID; LPAREN; x = ID; RPAREN; LBRACES; c = cmd; SEMICOLON; RETURN; e = expr; RBRACES; SEMICOLON; d = decl { DSeq(Fun(f, x, c, e), d) }
;

cmd:
  | SKIP { Skip }
  | x = ID; ASSIGN; e = expr { Assign(x, e) }
  | c1 = cmd; SEMICOLON; c2 = cmd { Seq(c1, c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd { If(e, c1, c2) }
  | WHILE; e = expr; DO; c = cmd { While(e, c) }
  | LPAREN; c = cmd; RPAREN { c }
;
