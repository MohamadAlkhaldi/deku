type ident = string

type prim =
  | Neg
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | And
  | Or
  | Xor
  | Lsl
  | Lsr
  | Asr

type expr =
  (* calculus *)
  | Var   of ident
  | Lam   of ident * expr
  | App   of expr * expr
  (* prims *)
  | Const of int64
  | Prim  of prim
  (* memory *)
  | Pair  of expr * expr
  | Fst   of expr
  | Snd   of expr

type value =
  | Int64 of int64
  | Pair  of value * value

type script = {
  param : ident;
  code : expr;
}
