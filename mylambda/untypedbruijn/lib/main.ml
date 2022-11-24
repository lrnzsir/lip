open Ast

let rec string_of_term = function
    Var(x) -> x
  | Abs(x,t) -> "fun " ^ x ^ ". " ^ string_of_term t
  | App(Var x,Var y) -> x ^ " " ^ y
  | App(Var x,t2) -> x ^ " (" ^ string_of_term t2 ^ ")"
  | App(t1,Var x) -> "(" ^ string_of_term t1 ^ ") " ^ x
  | App(t1,t2) -> "(" ^ string_of_term t1 ^ ") (" ^ string_of_term t2 ^ ")"
;;

let rec string_of_termbruijn = function
    BVar(x) -> string_of_int x
  | BAbs(t) -> "fun. " ^ string_of_termbruijn t
  | BApp(BVar(x),BVar(y)) -> string_of_int x ^ " " ^ string_of_int y
  | BApp(BVar(x),t2) -> string_of_int x ^ " (" ^ string_of_termbruijn t2 ^ ")"
  | BApp(t1,BVar(x)) -> "(" ^ string_of_termbruijn t1 ^ ") " ^ string_of_int x
  | BApp(t1,t2) -> "(" ^ string_of_termbruijn t1 ^ ") (" ^ string_of_termbruijn t2 ^ ")"
;;

let parse (s : string) : termbruijn =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(**********************************************************************
 max_nat : term -> int

 max_nat t computes the least n such that: 
 for all i : xi in vars_of_term t => i < n
  **********************************************************************)

let rec vars_of_term = function
    Var x -> [x]
  | Abs(x,t) -> x::(vars_of_term t)
  | App(t1,t2) -> (vars_of_term t1) @ (vars_of_term t2)

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
  | Abs(y, _) when y = x -> false
  | Abs(_, t) -> is_free x t
  | Var(y) -> x = y
  | App(t1, t2) -> (is_free x t1) || (is_free x t2)
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
  | Abs(y, t) when y = x -> Abs(y, t)
  | Abs(y, _) when y = x' -> failwith ("name " ^ x' ^ " must be fresh!")
  | Abs(y, t) -> Abs(y, rename x x' t)
  | App(t1, t2) -> App(rename x x' t1, rename x x' t2)
;;


(**********************************************************************
 equiv : term -> term -> bool

 Usage: equiv t1 t2 = true iff t1 and t2 are alpha-equivalent
 **********************************************************************)
               
let rec equiv t1 t2 = match (t1, t2) with
  | (BVar(x), BVar(y)) -> x = y
  | (BAbs(t1'), BAbs(t2')) -> equiv t1' t2'
  | (BApp(t1', t1''), BApp(t2', t2'')) -> (equiv t1' t2') && (equiv t1'' t2'')
  | _ -> false
;;
                   

(**********************************************************************
 subst : string -> term -> int -> term -> term * int

 Usage: subst x t1 vars t2 = ([x -> t1] t2,vars')  
        where vars is the index of the next variable to be used for renaming
        and vars' is the next available index after the substitution
 **********************************************************************)

let rec shift c d = function
  | BVar(x) when x < c -> BVar(x)
  | BVar(x) -> BVar(x + d)
  | BAbs(t) -> BAbs(shift (c + 1) d t)
  | BApp(t1, t2) -> BApp(shift c d t1, shift c d t2)
;;

let subst x t1 t2 =
  let rec subst' x t1 d t2 = match t2 with
    | BVar(y) when y = x -> shift 0 d t1
    | BVar(y) -> BVar(y)
    | BAbs(t2') -> BAbs(subst' (x + 1) t1 (d + 1) t2')
    | BApp(t2', t2'') -> BApp(subst' x t1 d t2', subst' x t1 d t2'')
  in subst' x t1 0 t2
;;


(**********************************************************************
 is_val : term -> bool

 Usage: is_val t = true iff t is a value (i.e., a lambda-abstraction)
 **********************************************************************)

let is_val = function
  | Abs(_, _) -> true
  | _ -> false
;;


exception NoRuleApplies

(**********************************************************************
trace1 : int -> term -> term * int

Usage: trace1 vars t performs 1 step of the small-step call-by-value semantics,
returning the obtained term and the index of the first fresh variable

Pre:  xk does not occur in t, for all k>=vars

Post: if trace1 i t = (t',i') then xk does not occur in t', for all k>=i'
**********************************************************************)

let rec trace1 = function
  | BAbs(t) -> BAbs(trace1 t)
  | BApp(BAbs(t1), t2) -> subst 0 t2 t1
  | BApp(t1, t2) -> (try BApp(trace1 t1, t2) with NoRuleApplies -> BApp(t1, trace1 t2))
  | _ -> raise NoRuleApplies
;;


(**********************************************************************
trace_rec : int -> int -> term -> term list

Usage: trace_rec n vars t performs n steps of the small-step semantics

Pre:  xk does not occur in t, for all k>=vars
**********************************************************************)

let rec trace_rec n t = if n <= 0 then [t] else try
  let t' = trace1 t
  in t::(trace_rec (n - 1) t')
with NoRuleApplies -> [t]


(**********************************************************************
trace : int -> term -> term list

Usage: trace n t performs n steps of the small-step semantics
**********************************************************************)

let trace n t = trace_rec n t;;