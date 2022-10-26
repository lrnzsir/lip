open Ast

let rec string_of_expr = function
  | True -> "True"
  | False -> "False"
  | Not(e0) -> "Not(" ^ (string_of_expr e0) ^ ")"
  | And(e0,e1) -> "And(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | Or(e0,e1) -> "Or(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ(e0) -> "Succ(" ^ (string_of_expr e0) ^ ")"
  | Pred(e0) -> "Pred(" ^ (string_of_expr e0) ^ ")"
  | IsZero(e0) -> "IsZero(" ^ (string_of_expr e0) ^ ")"
;;

let string_of_val x = string_of_int(x);;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies
  
let rec trace1 = function
  | True -> Succ(Zero)
  | False -> Zero
  | Not(Succ(Zero)) -> Zero
  | Not(Zero) -> Succ(Zero)
  | Not(e0) -> let e0' = trace1 e0 in Not(e0')
  | And(Zero,_) -> Zero
  | And(Succ(_),e1) -> e1
  | And(e0,e1) -> let e0' = trace1 e0 in And(e0',e1)
  | Or(Succ(_),_) -> Succ(Zero)
  | Or(Zero,e1) -> e1
  | Or(e0,e1) -> let e0' = trace1 e0 in Or(e0',e1)
  | If(Succ(_),e1,_) -> e1
  | If(Zero,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Pred(Succ(e0)) -> e0
  | Pred(e0) -> let e0' = trace1 e0 in Pred(e0')
  | IsZero(Zero) -> Succ(Zero)
  | IsZero(Succ(_)) -> Zero
  | IsZero(e0) -> let e0' = trace1 e0 in IsZero(e0')
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

let rec eval = function
  | True -> 1
  | False -> 0
  | Not(e0) -> if (eval e0) = 0 then 1 else 0
  | And(e0,e1) -> if not((eval e0) = 0) && not((eval e1) = 0) then 1 else 0
  | Or(e0,e1) -> if (eval e0) = 0 && (eval e1) = 0 then 0 else 1
  | If(e0,e1,e2) -> if not((eval e0) = 0) then eval e1 else eval e2
  | Zero -> 0
  | Succ(e0) -> (eval e0) + 1
  | Pred(e0) -> let n = eval e0 in if n = 0 then 0 else n - 1
  | IsZero(e0) -> let n = eval e0 in if n = 0 then 1 else 0
;;
