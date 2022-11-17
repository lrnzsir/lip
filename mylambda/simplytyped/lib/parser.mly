%{
open Ast
%}

%token <string> VAR
%token ABS
%token BOOL
%token NAT
%token ARR
%token DOT
%token COLON
%token TRUE
%token FALSE
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
%token LPAREN
%token RPAREN
%token EOF

%start <term> prog

%nonassoc DOT ABS
%nonassoc IF THEN ELSE
%right OR
%right AND
%nonassoc NOT ISZERO SUCC PRED
%nonassoc LPAREN VAR BOOL NAT TRUE FALSE ZERO
%left ARR
%left APP

%%

prog:
  | t = term; EOF { t }
;

domain:
  | BOOL { TBool }
  | NAT { TNat }
  | d1=domain; ARR; d2=domain { TFun(d1,d2) }
  | LPAREN; d=domain; RPAREN { d }
;

term:
  | x = VAR { Var x }
  | TRUE { True }
  | FALSE { False }
  | NOT; t=term { Not t }
  | t1=term; AND; t2=term { And(t1,t2) }
  | t1=term; OR; t2=term { Or(t1,t2) }
  | IF; t1=term; THEN; t2=term; ELSE; t3=term { If(t1,t2,t3) }
  | ZERO { Zero }
  | SUCC; t=term { Succ t }
  | PRED t=term { Pred t }
  | ISZERO; t=term { IsZero t }
  | ABS; x = VAR; COLON; d=domain; DOT; t = term { Abs(x,d,t) }
  | LPAREN; t=term; RPAREN { t }
  | t1=term; t2=term { App(t1,t2) } %prec APP
;
