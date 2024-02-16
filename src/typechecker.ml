open Utils
open Ast

(* TODO: define your own types *)
type ty  = Int | Bool | Double | Char | String 
  | List of ty
  | Pair of {
      fst : ty;
      snd : ty;
    }
  | Unit

let rec string_of_type(t: ty) : string =
  match t with
  | Int -> "int"
  | Bool -> "bool"
  | Double -> "double"
  | Char -> "char"
  | String -> "string"
  | List r -> "[" ^ string_of_type r ^ "]"
  | Pair r -> "(" ^ string_of_type r.fst ^ "," ^ string_of_type r.snd ^ ")"
  | Unit -> "()"

(* TODO: define your own type checking *)
let rec typecheck_prog(p : unit prog) : ty prog option =
  match p with
  | IntLit r -> Some (IntLit {r with prop = Int})
  | BoolLit r -> Some (BoolLit {r with prop = Bool})
  | BinExpr ({op = Add; _} as r)
  | BinExpr ({op = Mult; _} as r) -> (
    match (typecheck_prog r.left, typecheck_prog r.right) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (Int, Int) -> Some (BinExpr {r with
            prop = Int;
            left = left';
            right = right';
          })
        | (t1, t2) -> prerr_string
            (string_of_positions r.pos 
              ^ ": expected int operands, but found "
              ^ string_of_type t1 ^ ", "
              ^ string_of_type t2 ^ "\n");
          None
    )
    | _ -> None
  )
  | BinExpr ({op = BoolOr; _} as r)
  | BinExpr ({op = BoolAnd; _} as r) -> (
    match (typecheck_prog r.left, typecheck_prog r.right) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (Bool, Bool) -> Some (BinExpr {r with
            prop = Bool;
            left = left';
            right = right';
          })
        | (t1, t2) -> prerr_string
            (string_of_positions r.pos 
              ^ ": expected bool operands, but found "
              ^ string_of_type t1 ^ ", "
              ^ string_of_type t2 ^ "\n");
          None
    )
    | _ -> None
  )
  | _ -> None

let typecheck(p : unit prog) : bool =
  match typecheck_prog p with
  | Some _ -> true
  | None -> false
