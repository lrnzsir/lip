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
  | Var(x) -> x
  | Let(x, e0, e1) -> "Let " ^ x ^ " = " ^ (string_of_expr e0) ^ " In " ^ (string_of_expr e1)
;;

let string_of_val = function
  | Bool(b) -> string_of_bool(b)
  | Nat(n) -> string_of_int(n)
;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies
  
let rec trace1 = 
  let rec replace_var x v = function
    | Var(y) when y = x -> v
    | Not(e0) -> Not(replace_var x v e0)
    | And(e0,e1) -> And(replace_var x v e0,replace_var x v e1)
    | Or(e0,e1) -> Or(replace_var x v e0,replace_var x v e1)
    | If(e0,e1,e2) -> If(replace_var x v e0,replace_var x v e1,replace_var x v e2)
    | Succ(e0) -> Succ(replace_var x v e0)
    | Pred(e0) -> Pred(replace_var x v e0)
    | IsZero(e0) -> IsZero(replace_var x v e0)
    | e0 -> e0
  in function
  | Not(True) -> False
  | Not(False) -> True
  | Not(e0) -> let e0' = trace1 e0 in Not(e0')
  | And(False,_) -> False
  | And(True,e1) -> e1
  | And(e0,e1) -> let e0' = trace1 e0 in And(e0',e1)
  | Or(True,_) -> True
  | Or(False,e1) -> e1
  | Or(e0,e1) -> let e0' = trace1 e0 in Or(e0',e1)
  | If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Succ(e0) -> let e0' = trace1 e0 in Succ(e0')
  | Pred(Succ(e0)) -> e0
  | Pred(e0) -> let e0' = trace1 e0 in Pred(e0')
  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero(e0) -> let e0' = trace1 e0 in IsZero(e0')
  | Let(x,True,e1) -> replace_var x True e1
  | Let(x,False,e1) -> replace_var x False e1
  | Let(x,Zero,e1) -> replace_var x Zero e1
  | Let(x,Succ(e0),e1) -> replace_var x (Succ(e0)) e1
  | Let(x,e0,e1) -> let e0' = trace1 e0 in Let(x,e0',e1)
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

exception NAInt
exception NABool
exception NANat

let bool_of_val = function
  | Bool(b) -> b
  | _ -> raise NABool
;;

let int_of_val = function
  | Nat(n) -> n
  | _ -> raise NAInt
;;

exception NoVarApplies

let extend_rho rho x v = fun s -> if s = x then v else rho s;;

let eval e = 
  let rec eval' rho = function
    | True -> Bool(true)
    | False -> Bool(false)
    | Not(e0) -> Bool(not(bool_of_val(eval' rho e0)))
    | And(e0,e1) -> Bool(bool_of_val(eval' rho e0) && bool_of_val(eval' rho e1))
    | Or(e0,e1) -> Bool(bool_of_val(eval' rho e0) || bool_of_val(eval' rho e1))
    | If(e0,e1,e2) -> if bool_of_val(eval' rho e0) then eval' rho e1 else eval' rho e2
    | Zero -> Nat(0)
    | Succ(e0) -> Nat(int_of_val(eval' rho e0) + 1)
    | Pred(e0) -> let n = int_of_val(eval' rho e0) in
        if n > 0 then Nat(n - 1) else raise NANat
    | IsZero(Zero) -> Bool(true)
    | IsZero(e0) -> let n = int_of_val(eval' rho e0) in
        if n >= 0 then Bool(n = 0) else raise NANat
    | Let(x,e0,e1) -> let v = eval' rho e0 in eval' (extend_rho rho x v) e1
    | Var(x) -> rho x
  in eval' (fun _ -> raise NoVarApplies) e
;;
