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

exception TypeError of string;;

let string_of_type = function
  | BoolT -> "Bool"
  | NatT -> "Nat"
;;

let rec typecheck = function
  | True -> BoolT
  | False -> BoolT
  | Not(e0) -> let t = typecheck e0 in if t = BoolT then BoolT 
      else raise (TypeError ((string_of_expr e0) ^ " has type " ^ (string_of_type t) ^ ", but type Bool was expected"))
  | And(e0,e1) -> let t0 = typecheck e0 in if not(t0 = BoolT) 
      then raise (TypeError ((string_of_expr e0) ^ " has type " ^ (string_of_type t0) ^ ", but type Bool was expected"))
      else let t1 = typecheck e1 in if t1 = BoolT then BoolT
      else raise (TypeError ((string_of_expr e1) ^ " has type " ^ (string_of_type t1) ^ ", but type Bool was expected"))
  | Or(e0,e1) -> let t0 = typecheck e0 in if not(t0 = BoolT) 
      then raise (TypeError ((string_of_expr e0) ^ " has type " ^ (string_of_type t0) ^ ", but type Bool was expected"))
      else let t1 = typecheck e1 in if t1 = BoolT then BoolT
      else raise (TypeError ((string_of_expr e1) ^ " has type " ^ (string_of_type t1) ^ ", but type Bool was expected"))
  | If(e0,e1,e2) -> let t0 = typecheck e0 in if not(t0 = BoolT) 
      then raise (TypeError ((string_of_expr e0) ^ " has type " ^ (string_of_type t0) ^ ", but type Bool was expected")) 
      else let t1 = typecheck e1 in let t2 = typecheck e2 in if t1 = t2 then t1
      else raise (TypeError ((string_of_expr e1) ^ " and " ^ (string_of_expr e2) ^ " have different type, but same type was expected"))
  | Zero -> NatT
  | Succ(e0) -> let t = typecheck e0 in if t = NatT then NatT 
      else raise (TypeError ((string_of_expr e0) ^ " has type " ^ (string_of_type t) ^ ", but type Nat was expected"))
  | Pred(e0) -> let t = typecheck e0 in if t = NatT then NatT 
      else raise (TypeError ((string_of_expr e0) ^ " has type " ^ (string_of_type t) ^ ", but type Nat was expected"))
  | IsZero(e0) -> let t = typecheck e0 in if t = NatT then BoolT 
      else raise (TypeError ((string_of_expr e0) ^ " has type " ^ (string_of_type t) ^ ", but type Nat was expected"))
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
  
let rec trace1 = function
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

let eval e =
  let rec eval' = function
    | True -> Bool(true)
    | False -> Bool(false)
    | Not(e0) -> Bool(not(bool_of_val(eval' e0)))
    | And(e0,e1) -> Bool(bool_of_val(eval' e0) && bool_of_val(eval' e1))
    | Or(e0,e1) -> Bool(bool_of_val(eval' e0) || bool_of_val(eval' e1))
    | If(e0,e1,e2) -> if bool_of_val(eval' e0) then eval' e1 else eval' e2
    | Zero -> Nat(0)
    | Succ(e0) -> Nat(int_of_val(eval' e0) + 1)
    | Pred(e0) -> let n = int_of_val(eval' e0) in
        if n > 0 then Nat(n - 1) else raise NANat
    | IsZero(Zero) -> Bool(true)
    | IsZero(e0) -> let n = int_of_val(eval' e0) in
        if n >= 0 then Bool(n = 0) else raise NANat
  in let _ = typecheck e in eval' e
;;
