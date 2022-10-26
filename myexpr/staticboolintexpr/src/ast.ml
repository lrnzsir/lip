type expr =
  | True
  | False
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | If of expr * expr * expr
  | Zero
  | Succ of expr
  | Pred of expr
  | IsZero of expr
;;

type value = Bool of bool | Nat of int;;

type exprtype = BoolT | NatT;;