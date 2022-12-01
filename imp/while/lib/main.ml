open Ast

type exprval = Bool of bool | Nat of int        (* value of an expression *)
type state = ide -> exprval                     (* state = map from identifiers to expression values *)
type conf = St of state | Cmd of cmd * state    (* configuration = state | (command,state) *)

let string_of_val = function
  | Bool(b) -> string_of_bool(b)
  | Nat(n) -> string_of_int(n)
;;

let rec string_of_expr = function
  | True -> "true"
  | False -> "false"
  | Var(x) -> x
  | Const(n) -> string_of_int n
  | Not(e) -> "not (" ^ (string_of_expr e) ^ ")"
  | And(e1,e2) -> "(" ^ (string_of_expr e1) ^ ") and (" ^ (string_of_expr e2) ^ ")"
  | Or(e1,e2) -> "(" ^ (string_of_expr e1) ^ ") or (" ^ (string_of_expr e2) ^ ")"
  | Add(e1,e2) -> "(" ^ (string_of_expr e1) ^ ") + (" ^ (string_of_expr e2) ^ ")"
  | Sub(e1,e2) -> "(" ^ (string_of_expr e1) ^ ") - (" ^ (string_of_expr e2) ^ ")"
  | Mul(e1,e2) -> "(" ^ (string_of_expr e1) ^ ") * (" ^ (string_of_expr e2) ^ ")"
  | Eq(e1,e2) -> (string_of_expr e1) ^ " = " ^ (string_of_expr e2)
  | Leq(e1,e2) -> (string_of_expr e1) ^ " <= " ^ (string_of_expr e2)
;;

let rec string_of_cmd = function
    | Skip -> "skip"
    | Assign(x,e) -> x ^ " := " ^ (string_of_expr e)
    | Seq(c1,c2) -> (string_of_cmd c1) ^ " ; " ^ (string_of_cmd c2)
    | If(e,c1,c2) -> "if " ^ (string_of_expr e) ^ " then (" ^ (string_of_cmd c1) ^ ") else (" ^ (string_of_cmd c2) ^ ")"
    | While(e,c) -> "while " ^ (string_of_expr e) ^ " do (" ^ (string_of_cmd c) ^ ")"
;;

let rec string_of_state s = function
  | [] -> ""
  | x::[] -> x ^ " = " ^ (string_of_val (s x))
  | x::ides -> x ^ " = " ^ (string_of_val (s x)) ^ ", " ^ (string_of_state s ides)
;;

let string_of_conf ides = function
  | St(s) -> "(" ^ (string_of_state s ides) ^ ")"
  | Cmd(c,s) -> "(" ^ (string_of_cmd c) ^ ", (" ^ (string_of_state s ides) ^ "))"
;;

let rec string_of_trace ides = function
  | [] -> ""
  | c::[] -> string_of_conf ides c
  | c::t -> (string_of_conf ides c) ^ " -> " ^ (string_of_trace ides t)
;;

let vars_of_cmd c = 
  let rec is_in x = function
    | [] -> false
    | a::l -> (a = x) || (is_in x l) in
  let rec to_set = function
    | [] -> []
    | x::l when is_in x l -> to_set l
    | x::l -> x::(to_set l) in
  let rec vars_of_cmd' = function
    | Skip -> []
    | Assign(x,_) -> [x]
    | Seq(c1,c2) -> (vars_of_cmd' c1) @ (vars_of_cmd' c2)
    | If(_,c1,c2) -> (vars_of_cmd' c1) @ (vars_of_cmd' c2)
    | While(_,c) -> vars_of_cmd' c in
  to_set (vars_of_cmd' c)
;;

let rec last = function
  | [] -> failwith "No element in the list"
  | x::[] -> x
  | _::l -> last l
;;

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;


exception NABool
exception NANat
exception DiffTypeBranches

let rec eval_expr s = function
  | True -> Bool(true)
  | False -> Bool(false)
  | Var(x) -> s x
  | Const(n) -> Nat(n)
  | Not(e) -> (match eval_expr s e with Bool(b) -> Bool(not b) | _ -> raise NABool)
  | And(e1,e2) -> (match (eval_expr s e1, eval_expr s e2) with (Bool(b1),Bool(b2)) -> Bool(b1 && b2) | _ -> raise NABool)
  | Or(e1,e2) -> (match (eval_expr s e1, eval_expr s e2) with (Bool(b1),Bool(b2)) -> Bool(b1 || b2) | _ -> raise NABool)
  | Add(e1,e2) -> (match (eval_expr s e1, eval_expr s e2) with (Nat(n1),Nat(n2)) -> Nat(n1 + n2) | _ -> raise NANat)
  | Sub(e1,e2) -> (match (eval_expr s e1, eval_expr s e2) with (Nat(n1),Nat(n2)) -> Nat(n1 - n2) | _ -> raise NANat)
  | Mul(e1,e2) -> (match (eval_expr s e1, eval_expr s e2) with (Nat(n1),Nat(n2)) -> Nat(n1 * n2) | _ -> raise NANat)
  | Eq(e1,e2) -> (match (eval_expr s e1, eval_expr s e2) with (Bool(b1),Bool(b2)) -> Bool(b1 = b2) | (Nat(n1),Nat(n2)) -> Bool(n1 = n2) | _ -> raise DiffTypeBranches)
  | Leq(e1,e2) -> (match (eval_expr s e1, eval_expr s e2) with (Bool(b1),Bool(b2)) -> Bool(b1 <= b2) | (Nat(n1),Nat(n2)) -> Bool(n1 <= n2) | _ -> raise DiffTypeBranches)
;;


exception UnboundVar of string
exception NoRuleApplies

let empty x = raise (UnboundVar(x));;
let bind a b f = fun x -> if x = a then b else f x;;

let rec trace1 = function
  | St(_) -> raise NoRuleApplies
  | Cmd(c,s) -> match c with
    | Skip -> St(s)
    | Assign(x,e) -> let e' = eval_expr s e in St(bind x e' s)
    | Seq(c1,c2) -> (match trace1 (Cmd(c1,s)) with St(s') -> Cmd(c2,s') | Cmd(c1',s') -> Cmd(Seq(c1',c2),s'))
    | If(e,c1,c2) -> (match eval_expr s e with Bool(b) -> if b then Cmd(c1,s) else Cmd(c2,s) | _ -> raise NABool)
    | While(e,c) -> (match eval_expr s e with Bool(b) -> if b then (match trace1 (Cmd(c,s)) with St(s') -> Cmd(While(e,c),s') | Cmd(c',s') -> Cmd(Seq(c',While(e,c)),s')) else St(s) | _ -> raise NABool)
;;

let trace n commands =
  let rec trace_rec n c = 
    if n <= 0 then [c] 
    else (try
      let c' = trace1 c
      in c::(trace_rec (n - 1) c')
    with NoRuleApplies -> [c])
  in trace_rec n (Cmd(commands,empty))
;;
