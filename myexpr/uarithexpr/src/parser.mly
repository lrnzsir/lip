%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token NOT
%token AND
%token OR
%token IF
%token THEN
%token ELSE
%token ZERO
%token SUCC
%token PRED
%token ISZERO
%token EOF

%nonassoc IF THEN ELSE
%right OR
%right AND
%nonassoc NOT
%nonassoc ISZERO
%nonassoc SUCC PRED
%nonassoc LPAREN RPAREN

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | NOT; e1 = expr { Not(e1) }
  | e1 = expr; AND; e2 = expr { And(e1, e2) }
  | e1 = expr; OR; e2 = expr { Or(e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If(e1, e2, e3) }
  | LPAREN; e = expr; RPAREN {e}
  | ZERO { Zero }
  | SUCC; e = expr { Succ(e) }
  | PRED; e = expr { Pred(e) }
  | ISZERO; e = expr { IsZero(e) }
;

