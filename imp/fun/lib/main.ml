open Ast
open Types


let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let apply st x = match (topenv st) x with IVar l -> (getmem st) l | _ -> raise (NotVariable x)


(******************************************************************************)
(*                      Small-step semantics of commands                      *)
(******************************************************************************)

let bot = fun x -> raise (UnboundVar x)
let empty = fun x -> raise (EmptyLoc x)

let bind f x v = fun y -> if y=x then v else f y

let assign st x v = match (topenv st) x with
  | IVar l -> (getenv st,bind (getmem st) l v,getloc st)
  | _ -> raise (TypeError "Assign")

let rec state_of_decl st d = match d with
    EmptyDecl -> st
  | IntVar(x) -> let el' = (bind (topenv st) x (IVar (getloc st)))::(popenv st) in (el',getmem st,1 + getloc st)
  | Fun(f,x,c,e) -> let el' = (bind (topenv st) f (IFun(x,c,e)))::(popenv st) in (el',getmem st,getloc st)
  | DSeq(d1,d2) -> let st' = state_of_decl st d1 in state_of_decl st' d2


let rec trace1_expr (st : state) = function
  | Var x -> (match (topenv st) x with 
    | IVar l -> (Const ((getmem st) l),st)
    | _ -> raise (NotVariable x))

  | Not True -> (False,st)
  | Not False -> (True,st)
  | Not e -> let (e',st') = trace1_expr st e in (Not e',st')

  | And(False,_) -> (False,st)
  | And(True,e2) -> trace1_expr st e2
  | And(e1,e2) -> let (e1',st') = trace1_expr st e1 in (And(e1',e2),st')

  | Or(True,_) -> (True,st)
  | Or(False,e2) -> trace1_expr st e2
  | Or(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Or(e1',e2),st')

  | Add(Const n1,Const n2) -> (Const (n1 + n2),st)
  | Add(Const n1,e2) -> let (e2',st') = trace1_expr st e2 in (Add(Const n1,e2'),st')
  | Add(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Add(e1',e2),st')

  | Sub(Const n1,Const n2) -> (Const (n1 - n2),st)
  | Sub(Const n1,e2) -> let (e2',st') = trace1_expr st e2 in (Sub(Const n1,e2'),st')
  | Sub(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Sub(e1',e2),st')

  | Mul(Const n1,Const n2) -> (Const (n1 * n2),st)
  | Mul(Const n1,e2) -> let (e2',st') = trace1_expr st e2 in (Mul(Const n1,e2'),st')
  | Mul(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Mul(e1',e2),st')

  | Eq(True,True) -> (True,st)
  | Eq(True,False) -> (False,st)
  | Eq(False,True) -> (False,st)
  | Eq(False,False) -> (True,st)
  | Eq(Const n1,Const n2) -> if n1 = n2 then (True,st) else (False,st)
  | Eq(e1,e2) -> (try 
        let (e1',st') = trace1_expr st e1 in (Eq(e1',e2),st') 
      with NoRuleApplies -> let (e2',st') = trace1_expr st e2 in (Eq(e1,e2'),st'))
  
  | Leq(Const n1,Const n2) -> if n1 <= n2 then (True,st) else (False,st)
  | Leq(e1,e2) -> (try
        let (e1',st') = trace1_expr st e1 in (Leq(e1',e2),st') 
      with NoRuleApplies -> let (e2',st') = trace1_expr st e2 in (Leq(e1,e2'),st'))
  
  | Call(f,Const n) -> (match (topenv st) f with
    | IFun(x,c,e) -> let el' = (bind (topenv st) x (IVar (getloc st)))::(getenv st) in 
        (CallExec(c,e),(el',bind (getmem st) (getloc st) n,1 + getloc st))
    | _ -> raise (NotCallable f))
  | Call(f,e) -> let (e',st') = trace1_expr st e in (Call(f,e'),st')
  
  | CallExec(c,e) -> (match trace1_cmd (Cmd(c,st)) with
    | St st' -> (CallRet e,st')
    | Cmd(c',st') -> (CallExec(c',e),st'))

  | CallRet (Const n) -> (Const n,(popenv st,getmem st,getloc st))
  | CallRet e -> (try 
      let (e',st') = trace1_expr st e in (CallRet e',st') 
    with NoRuleApplies -> raise (TypeError "CallRet"))
  
  | _ -> raise NoRuleApplies


and trace1_cmd = function
    St _ -> raise NoRuleApplies
  | Cmd(c,st) -> match c with
      Skip -> St st
    | Assign(x,Const n) -> St (assign st x n)
    | Assign(x,e) -> let (e',st') = trace1_expr st e in Cmd(Assign(x,e'),st')
    | Seq(c1,c2) -> (match trace1_cmd (Cmd(c1,st)) with
        St st' -> Cmd(c2,st')
      | Cmd(c1',st') -> Cmd(Seq(c1',c2),st'))
    | If(True,c1,_) -> Cmd(c1,st)
    | If(False,_,c2) -> Cmd(c2,st)
    | If(e,c1,c2) -> (try
        let (e',st') = trace1_expr st e in Cmd(If(e',c1,c2),st')
      with NoRuleApplies -> match c1 with
        | Seq(c,While(_,c')) when c = c' -> raise (TypeError "While")
        | _ -> raise (TypeError "If"))
    | While(e,c) -> Cmd(If(e,Seq(c,While(e,c)),Skip),st)


(**********************************************************************
 trace_rec : int-> conf -> conf list

 Usage: trace_rec n t performs n steps of the small-step semantics

 **********************************************************************)

let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1_cmd t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]


(**********************************************************************
 trace : int -> prog -> conf list

 Usage: trace n t performs n steps of the small-step semantics
 **********************************************************************)

let trace n t = match t with Prog(d,c) -> trace_rec n (Cmd(c,state_of_decl (bot::[],empty,0) d))

