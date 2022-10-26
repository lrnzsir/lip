open Ast

let rec string_of_boolexpr = function
  | True -> "True"
  | False -> "False"
  | Not(e0) -> "Not(" ^ (string_of_boolexpr e0) ^ ")"
  | And(e0,e1) -> "And(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ ")"
  | Or(e0,e1) -> "Or(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ ")"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | Var(x) -> "Var(" ^ x ^ ")"
;;

exception Error of string

let rec typecheck p = 
  
  match p with (d, e) -> 

let parse (s : string) : boolProg =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

let rec find_decl d x = match d with
| (y, e)::_ when x = y -> e
| (_, _)::d' -> find_decl d' x
| [] -> raise NoRuleApplies
;;

let rec trace1 (d, e) = match e with
  | Not(True) -> False
  | Not(False) -> True
  | Not(e0) -> let e0' = trace1 (d, e0) in Not(e0')
  | And(False,_) -> False
  | And(True,e1) -> e1
  | And(e0,e1) -> let e0' = trace1 (d, e0) in And(e0',e1)
  | Or(True,_) -> True
  | Or(False,e1) -> e1
  | Or(e0,e1) -> let e0' = trace1 (d, e0) in Or(e0',e1)
  | If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 (d, e0) in If(e0',e1,e2)
  | Var(x) -> let e0 = trace1 (d, (find_decl d x)) in e0
  | _ -> raise NoRuleApplies
;;

let rec trace (d, e) = try
    let e' = trace1 (d, e)
    in e::(trace (d, e'))
  with NoRuleApplies -> [e]
;;

exception AlwaysUndefinedFunction

let rec eval (d, e) = 
  let rec env_of_decl d x = match d with
  | (y, e)::_ when x = y -> eval (d, e)
  | (_, _)::d' -> env_of_decl d' x
  | [] -> raise AlwaysUndefinedFunction in
  match e with
  | True -> true
  | False -> false
  | Not(e0) -> not(eval (d, e0))
  | And(e0,e1) -> (eval (d, e0)) && (eval (d, e1))
  | Or(e0,e1) -> (eval (d, e0)) || (eval (d, e1))
  | If(e0,e1,e2) -> if eval (d, e0) then eval (d, e1) else eval (d, e2)
  | Var(x) -> env_of_decl d x
;;
