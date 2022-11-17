open Ast

(* string_of_type : ty -> string *)

let rec string_of_type = function
  | TBool -> "bool"
  | TNat -> "nat"
  | TFun(t1,t2) -> "(" ^ (string_of_type t1) ^ "->" ^ (string_of_type t2) ^ ")"
;;

(* string_of_term : term -> string *)

let rec string_of_term = function
  | Var(x) -> x
  | Abs(x,d,t) -> "fun " ^ x ^ ": " ^ (string_of_type d) ^ ". " ^ (string_of_term t)
  | App(Var x,Var y) -> x ^ " " ^ y
  | App(Var x,t2) -> x ^ " (" ^ string_of_term t2 ^ ")"
  | App(t1,Var x) -> "(" ^ string_of_term t1 ^ ") " ^ x
  | App(t1,t2) -> "(" ^ string_of_term t1 ^ ") (" ^ string_of_term t2 ^ ")"
  | True -> "true"
  | False -> "false"
  | Not(t) -> "not(" ^ (string_of_term t) ^ ")"
  | And(t1,t2) -> "(" ^ (string_of_term t1) ^ ") and (" ^ (string_of_term t2) ^ ")"
  | Or(t1,t2) -> "(" ^ (string_of_term t1) ^ ") or (" ^ (string_of_term t2) ^ ")"
  | If(t1,t2,t3) -> "if (" ^ (string_of_term t1) ^ ") then (" ^ (string_of_term t2) ^ ") else (" ^ (string_of_term t3) ^ ")"
  | Zero -> "0"
  | Succ(t) -> "succ(" ^ (string_of_term t) ^ ")"
  | Pred(t) -> "pred(" ^ (string_of_term t) ^ ")"
  | IsZero(t) -> "iszero(" ^ (string_of_term t) ^ ")"
;;


let parse (s : string) : term =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(**********************************************************************
 max_nat : term -> int

 max_nat t computes the least n such that: 
 for all i : xi in vars_of_term t => i < n
  **********************************************************************)

let rec vars_of_term = function
  | Var x -> [x]
  | Abs(x,_,t) -> x::(vars_of_term t)
  | App(t1,t2) -> (vars_of_term t1) @ (vars_of_term t2)
  | True -> []
  | False -> []
  | Not(e) -> vars_of_term e
  | And(e1,e2) -> vars_of_term e1 @ vars_of_term e2
  | Or(e1,e2) -> vars_of_term e1 @ vars_of_term e2
  | If(e0,e1,e2) -> vars_of_term e0 @ vars_of_term e1 @ vars_of_term e2
  | Zero -> []
  | Succ(e) | Pred(e) | IsZero(e) -> vars_of_term e

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
            
let is_digit = function '0' .. '9' -> true | _ -> false

let explode s = List.map (fun x -> String.make 1 x |> int_of_string) (List.filter is_digit (List.init (String.length s) (String.get s)))

let nat_of_var xl = List.fold_left (fun x y -> x + y) 0 (List.mapi (fun i x -> x * (pow 10 i)) (List.rev (explode xl)))

let rec max_of_list = function 
  | [] -> 0
  | x::xs -> max x (max_of_list xs)
                
let max_nat t =
  let xl = vars_of_term t in
  let nl = List.map nat_of_var xl in
  1 +  max_of_list nl


(**********************************************************************
 is_free : string -> term -> bool

 Usage: is_free x t = true iff the variable x occurs free in t
 **********************************************************************)

let rec is_free x = function
  | Abs(y, _, _) when y = x -> false
  | Abs(_, _, t) -> is_free x t
  | Var(y) -> x = y
  | App(t1, t2) -> (is_free x t1) || (is_free x t2)
  | Not(t) -> is_free x t
  | And(t1, t2) -> (is_free x t1) || (is_free x t2)
  | Or(t1, t2) -> (is_free x t1) || (is_free x t2)
  | If(t1, t2, t3) -> (is_free x t1) || (is_free x t2) || (is_free x t3)
  | Succ(t) -> is_free x t
  | Pred(t) -> is_free x t
  | IsZero(t) -> is_free x t
  | _ -> false
;;


(**********************************************************************
 rename : string -> string -> term -> term

 Usage: rename x x' t replaces all free occurrences of x in t as x'

 Pre: x' does not occur (free or bound) in t
 **********************************************************************)

let rec rename x x' = function
  | Var(y) when y = x -> Var(x')
  | Var(y) when y = x' -> failwith ("name " ^ x' ^ " must be fresh!")
  | Var(y) -> Var(y)
  | Abs(y, d, t) when y = x -> Abs(y, d, t)
  | Abs(y, _, _) when y = x' -> failwith ("name " ^ x' ^ " must be fresh!")
  | Abs(y, d, t) -> Abs(y, d, rename x x' t)
  | App(t1, t2) -> App(rename x x' t1, rename x x' t2)
  | Not(t) -> Not(rename x x' t)
  | And(t1, t2) -> And(rename x x' t1, rename x x' t2)
  | Or(t1, t2) -> Or(rename x x' t1, rename x x' t2)
  | If(t1, t2, t3) -> If(rename x x' t1, rename x x' t2, rename x x' t3)
  | Succ(t) -> Succ(rename x x' t)
  | Pred(t) -> Pred(rename x x' t)
  | IsZero(t) -> IsZero(rename x x' t)
  | t -> t
;;


(**********************************************************************
 equiv : term -> term -> bool

 Usage: equiv t1 t2 = true iff t1 and t2 are alpha-equivalent
 **********************************************************************)

let rec equiv_rec t1 t2 vars = match (t1,t2) with
  | (Var(x), Var(y)) -> x = y
  | (Abs(x, _, t1'), Abs(y, _, t2')) -> equiv_rec (rename x ("x" ^ (string_of_int vars)) t1') (rename y ("x" ^ (string_of_int vars)) t2') (vars + 1)
  | (App(t1, t1'), App(t2, t2')) -> (equiv_rec t1 t2 vars) && (equiv_rec t1' t2' vars)
  | (True, True) -> true
  | (False, False) -> true
  | (Not(t1), Not(t2)) -> equiv_rec t1 t2 vars
  | (And(t1, t1'), And(t2, t2')) -> (equiv_rec t1 t2 vars) && (equiv_rec t1' t2' vars)
  | (Or(t1, t1'), Or(t2, t2')) -> (equiv_rec t1 t2 vars) && (equiv_rec t1' t2' vars)
  | (If(t1, t1', t1''), If(t2, t2', t2'')) -> (equiv_rec t1 t2 vars) && (equiv_rec t1' t2' vars) && (equiv_rec t1'' t2'' vars)
  | (Zero, Zero) -> true
  | (Succ(t1), Succ(t2)) -> equiv_rec t1 t2 vars
  | (Pred(t1), Pred(t2)) -> equiv_rec t1 t2 vars
  | (IsZero(t1), IsZero(t2)) -> equiv_rec t1 t2 vars
  | _ -> false
;;
    
let equiv t1 t2 = equiv_rec t1 t2 (max (max_nat t1) (max_nat t2))
                   

(**********************************************************************
 subst : string -> term -> int -> term -> term * int

 Usage: subst x t1 vars t2 = ([x -> t1] t2,vars')  
        where vars is the index of the next variable to be used for renaming
        and vars' is the next available index after the substitution
 **********************************************************************)

let rec subst x t1 vars = function
  | Var(y) when y = x -> (t1, vars)
  | Var(y) -> (Var(y), vars)
  | Abs(y, d, t2') when y = x -> (Abs(y, d, t2'), vars)
  | Abs(y, d, t2') when not(is_free y t1) -> let (t2_', vars') = subst x t1 vars t2' in (Abs(y, d, t2_'), vars')
  | Abs(y, d, t2') -> let (t2_', vars') = subst x t1 (vars + 1) (rename y ("x" ^ (string_of_int vars)) t2') in (Abs("x" ^ (string_of_int vars), d, t2_'), vars')
  | App(t2', t2'') -> let ((t2_', vars'), (t2_'', vars'')) = (subst x t1 vars t2', subst x t1 vars t2'') in (App(t2_', t2_''), max vars' vars'')
  | Not(t2) -> let (t2_, vars') = subst x t1 vars t2 in (Not(t2_), vars')
  | And(t2, t2') -> let ((t2_, vars'), (t2_', vars'')) = (subst x t1 vars t2, subst x t1 vars t2') in (And(t2_, t2_'), max vars' vars'')
  | Or(t2, t2') -> let ((t2_, vars'), (t2_', vars'')) = (subst x t1 vars t2, subst x t1 vars t2') in (Or(t2_, t2_'), max vars' vars'')
  | If(t2, t2', t2'') -> let ((t2_, vars'), (t2_', vars''), (t2_'', vars''')) = (subst x t1 vars t2, subst x t1 vars t2', subst x t1 vars t2'') in (If(t2_, t2_', t2_''), max (max vars' vars'') vars''')
  | Succ(t2) -> let (t2_, vars') = subst x t1 vars t2 in (Succ(t2_), vars')
  | Pred(t2) -> let (t2_, vars') = subst x t1 vars t2 in (Pred(t2_), vars')
  | IsZero(t2) -> let (t2_, vars') = subst x t1 vars t2 in (IsZero(t2_), vars')
  | t2 -> (t2, vars)
;;


(**********************************************************************
 typecheck : tenv * term -> ty
 **********************************************************************)

exception TypeError of string
exception UnboundVar of string
    
type tenv = string -> ty

let bot = fun x -> raise (UnboundVar x)

let bind f x v = fun y -> if y=x then v else f y

let rec typecheck (gamma:tenv) = function
  | Var(x) -> gamma x
  | Abs(x, d, t) -> TFun(d, typecheck (bind gamma x d) t)
  | App(t1, t2) -> (match (typecheck gamma t1, typecheck gamma t2) with
    | (TFun(d1, d1'), d2) when d1 = d2 -> d1'
    | (TFun(d1, _), d2) -> raise (TypeError ((string_of_type d1) ^ " type expected as argument, " ^ (string_of_type d2) ^ " found"))
    | _ -> raise (TypeError "Left side of app is not a fun"))
  | True -> TBool
  | False -> TBool
  | Not(t) -> (match typecheck gamma t with TBool -> TBool | _ -> raise (TypeError "Bool expected"))
  | And(t1, t2) -> (match (typecheck gamma t1, typecheck gamma t2) with (TBool, TBool) -> TBool | _ -> raise (TypeError "Bool expected"))
  | Or(t1, t2) -> (match (typecheck gamma t1, typecheck gamma t2) with (TBool, TBool) -> TBool | _ -> raise (TypeError "Bool expected"))
  | If(t1, t2, t3) -> (match (typecheck gamma t1, typecheck gamma t2, typecheck gamma t3) with 
    | (TBool, d', d'') when d' = d'' -> d' 
    | (TBool, _, _) -> raise (TypeError "Same type expected in if branches")
    | _ -> raise (TypeError "Bool expected in if condition"))
  | Zero -> TNat
  | Succ(t) -> (match typecheck gamma t with TNat -> TNat | _ -> raise (TypeError "Nat expected"))
  | Pred(t) -> (match typecheck gamma t with TNat -> TNat | _ -> raise (TypeError "Nat expected"))
  | IsZero(t) -> (match typecheck gamma t with TNat -> TBool | _ -> raise (TypeError "Nat expected"))



(**********************************************************************
 is_nv : term -> bool

 Usage: is_nv t = true iff t is a numerical value
 **********************************************************************)

let rec is_nv = function
    Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false

(**********************************************************************
 is_val : term -> bool

 Usage: is_val t = true iff t is a value (i.e., a lambda-abstraction)
 **********************************************************************)

let is_val = function
    Abs _ -> true
  | True -> true
  | False -> true
  | n when is_nv n -> true
  | _ -> false


(**********************************************************************
 trace1 : int -> term -> term * int

 Usage: trace1 vars t performs 1 step of the small-step call-by-value semantics,
 returning the obtained term and the index of the first fresh variable

 Pre:  xk does not occur in t, for all k>=vars

 Post: if trace_rec n i t = (t',i') then xk does not occur in t', 
       for all k>=i'
 **********************************************************************)

exception NoRuleApplies

let rec trace1 vars = function
  | App(Abs(x, _, t1), v2) when is_val v2 -> subst x v2 vars t1
  | App(t1, t2) -> (try
      let (t1', vars') = trace1 vars t1 in (App(t1', t2), vars')
    with NoRuleApplies -> let (t2', vars') = trace1 vars t2 in (App(t1, t2'), vars'))
  | Abs(x, d, t) -> let (t', vars') = trace1 vars t in (Abs(x, d, t'), vars')
  | Not(True) -> (False, vars)
  | Not(False) -> (True, vars)
  | Not(t) -> let (t', vars') = trace1 vars t in (Not(t'), vars')
  | And(False, _) -> (False, vars)
  | And(True, t2) -> (t2, vars)
  | And(t1, t2) -> let (t1', vars') = trace1 vars t1 in (And(t1', t2), vars')
  | Or(True, _) -> (True, vars)
  | Or(False, t2) -> (t2, vars)
  | Or(t1, t2) -> let (t1', vars') = trace1 vars t1 in (Or(t1', t2), vars')
  | If(True, t2, _) -> (t2, vars)
  | If(False, _, t3) -> (t3, vars)
  | If(t1, t2, t3) -> let (t1', vars') = trace1 vars t1 in (If(t1', t2, t3), vars')
  | Succ(t) -> let (t', vars') = trace1 vars t in (Succ(t'), vars')
  | Pred(Succ(t)) -> (t, vars)
  | Pred(Zero) -> (Zero, vars)
  | Pred(t) -> let (t', vars') = trace1 vars t in (Pred(t'), vars')
  | IsZero(Zero) -> (True, vars)
  | IsZero(Succ(_)) -> (False, vars)
  | IsZero(t) -> let (t', vars') = trace1 vars t in (IsZero(t'), vars')
  | _ -> raise NoRuleApplies


(**********************************************************************
 trace_rec : int -> term -> term list

 Usage: trace_rec i t performs one or more steps of the small-step semantics,
 until a non-reducible term is found

 Pre:  xk does not occur in t, for all k>=i
 **********************************************************************)

let rec trace_rec vars t = try
    let (t',vars') = trace1 vars t
    in t::(trace_rec vars' t')
  with NoRuleApplies -> [t]


(**********************************************************************
 trace : term -> term list

 Usage: trace t performs one or more steps of the small-step semantics
 until a non-reducible term is found
 **********************************************************************)

let trace t =
  let _ = typecheck bot t in
  trace_rec (max_nat t) t
