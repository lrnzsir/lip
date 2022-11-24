type term =
    Var of string
  | Abs of string * term
  | App of term * term

type termbruijn =
    BVar of int
  | BAbs of termbruijn
  | BApp of termbruijn * termbruijn

exception UnboundVar of string

let termbruijn_of_term t = 
  let bind f x = fun y -> if y=x then 0 else 1 + (f y) in
  let gamma = fun x -> match x with "x" -> 4 | "y" -> 3 | "z" -> 2 | "a" -> 1 | "b" -> 0 | _ -> raise (UnboundVar x) in
  let rec termbruijn_of_term' gamma = function
    | Var(x) -> BVar(gamma x)
    | Abs(x, t') -> BAbs(termbruijn_of_term' (bind gamma x) t')
    | App(t', t'') -> BApp(termbruijn_of_term' gamma t', termbruijn_of_term' gamma t'')
  in termbruijn_of_term' gamma t

