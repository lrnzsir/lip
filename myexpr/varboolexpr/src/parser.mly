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
%token LET
%token IN
%token SEMICOLON
%token EQUALS
%token <string> ID
%token EOF

%nonassoc IF THEN ELSE
%right OR
%right AND
%nonassoc NOT
%nonassoc LPAREN RPAREN

%start <boolProg> prog

%%

prog:
  | e = expr; EOF { ([], e) }
  | LET; d = decl; IN; e = expr; EOF { (d, e) }
;

decl:
  | x = ID; EQUALS; e = expr; SEMICOLON; d = decl { (x, e)::d }
  | x = ID; EQUALS; e = expr { (x, e)::[] }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | NOT; e1 = expr { Not(e1) }
  | e1 = expr; AND; e2 = expr { And(e1, e2) }
  | e1 = expr; OR; e2 = expr { Or(e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If(e1, e2, e3) }
  | LPAREN; e = expr; RPAREN {e}
  | x = ID { Var x }
;

