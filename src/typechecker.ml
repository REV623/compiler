open Utils
open Ast

(* TODO: define your own types *)
type ty  = Integer | Bool | Double | Char | String
  | TypeVar of {
    name : string;
  }
  | List of ty
  | Pair of {
      fst : ty;
      snd : ty;
    }
  | Func of {
    param : ty;
    func : ty;
  }
  | Data of {
    name : string;
    tv_list : ty list;
    cons_list : ty list;
  }
  | Constructor of {
    name : string;
    functype : ty;
    data : ty;
  }
  | Unit
  | EmptyList

type binding = 
  | Map of {
    name : string;
    typ : ty;
  }

let rec find_binding_type(env : binding list)(name : string) : ty =
  match env with
  | Map hd::td -> if String.equal hd.name name then hd.typ else find_binding_type td name
  | [] -> assert false

let rec string_of_type(t: ty) : string =
  match t with
  | Integer -> "Integer"
  | Bool -> "Bool"
  | Double -> "Double"
  | Char -> "Char"
  | String -> "String"
  | TypeVar r -> r.name
  | List r -> "[" ^ string_of_type r ^ "]"
  | Pair r -> "(" ^ string_of_type r.fst ^ "," ^ string_of_type r.snd ^ ")"
  | Func r -> string_of_type r.param ^ "->" ^ string_of_type r.func
  | Data r -> r.name ^ (let rec str_loop tv_list : string = match tv_list with | hd::tl -> string_of_type hd ^ " " ^ str_loop tl | [] -> "" in str_loop r.tv_list)
  | Constructor r -> r.name
  | Unit -> "()"
  | EmptyList -> "[]"

(* TODO: define your own type checking *)
let rec typecheck_prog(p : unit prog) : ty prog option =
  let env = [] in
  match p with
  | Prog r -> (
    match (typecheck_imp_list r.imp_list, typecheck_defn_list r.defn_list env) with
    | (imp_list, Some defn_list) -> Some (Prog {r with
        prop = Unit;
        imp_list = imp_list;
        defn_list = defn_list;
      })
    | _ -> None
  )
  | _ -> None
and typecheck_imp_list imp_list : ty prog list = 
  match imp_list with
  | hd::td -> typecheck_imp hd::typecheck_imp_list td
  | [] -> []
and typecheck_defn_list defn_list env : ty prog list option = 
  match defn_list with
  | hd::td -> (
    match typecheck_defn hd env with
    | Some defn -> (
      match typecheck_defn_list td env with
      | Some r -> Some (defn::r) 
      | None -> None
    )
    | None -> None
  )
  | [] -> Some []
and typecheck_imp imp : ty prog = 
  match imp with
  | Import r -> (Import {r with prop = Unit})
  | _ -> assert false
and typecheck_defn(p : unit prog)(env : binding list) : ty prog option =
  match p with
  | DataDef _r -> None
  | TypeDef _r -> None
  | FuncDef r -> (
    match typecheck_expr r.expr env with
    | Some expr' -> Some (FuncDef {r with 
        prop = Unit;
        arg_list = expr'::[];
        return_type = expr';
        expr = expr';
      })
    | _ -> None
  )
  | _ -> None
and typecheck_expr(p : unit prog)(env : binding list) : ty prog option =
  match p with
  | IntLit r -> Some (IntLit {r with prop = Integer})
  | FloatLit r -> Some (FloatLit {r with prop = Double})
  | BoolLit r -> Some (BoolLit {r with prop = Bool})
  | CharLit r -> Some (CharLit {r with prop = Char})
  | StringLit r -> Some (StringLit {r with prop = String})
  | Ident r -> Some (Ident {r with prop = find_binding_type env r.name})
  | Unit _r -> Some (Unit {prop = Unit})
  | EmptyList _r -> Some (EmptyList {prop = List EmptyList}) (* prop = List EmptyList ??? *)
  | UnaExpr ({op = Sub; _} as r) -> (
    match typecheck_expr r.expr env with
    | Some expr' -> (
        match prop_of_prog expr' with
        | Integer -> Some (UnaExpr {r with
            prop = Integer;
            expr = expr';
          })
        | Double -> Some (UnaExpr {r with
            prop = Double;
            expr = expr';
          })
        | t -> prerr_string
            (string_of_positions r.pos 
              ^ ": expected Integer or Double operands, but found "
              ^ string_of_type t ^ "\n");
            None
      )
    | _ -> None
  )
  | BinExpr ({op = Add; _} as r)
  | BinExpr ({op = Sub; _} as r)
  | BinExpr ({op = Mult; _} as r) -> (
    match (typecheck_expr r.left env, typecheck_expr r.right env) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (Integer, Integer) -> Some (BinExpr {r with
            prop = Integer;
            left = left';
            right = right';
          })
        | (Double, Double)
        | (Integer, Double)
        | (Double, Integer) -> Some (BinExpr {r with
            prop = Double;
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
  | BinExpr ({op = Div; _} as r) -> (
    match (typecheck_expr r.left env, typecheck_expr r.right env) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (Integer, Integer)
        | (Double, Double)
        | (Integer, Double)
        | (Double, Integer) -> Some (BinExpr {r with
            prop = Double;
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
  | BinExpr ({op = IntDiv; _} as r) -> (
    match (typecheck_expr r.left env, typecheck_expr r.right env) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (Integer, Integer)
        | (Double, Integer) -> Some (BinExpr {r with
            prop = Integer;
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
  | BinExpr ({op = Mod; _} as r) -> (
    match (typecheck_expr r.left env, typecheck_expr r.right env) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (Integer, Integer) -> Some (BinExpr {r with
            prop = Integer;
            left = left';
            right = right';
          })
        | (Double, Integer) -> Some (BinExpr {r with
            prop = Double;
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
  | BinExpr ({op = Less; _} as r)
  | BinExpr ({op = Leq; _} as r)
  | BinExpr ({op = Geq; _} as r)
  | BinExpr ({op = Greater; _} as r) -> (
    match (typecheck_expr r.left env, typecheck_expr r.right env) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (Integer, Integer)
        | (Double, Double)
        | (Integer, Double)
        | (Double, Integer) -> Some (BinExpr {r with
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
  | BinExpr ({op = Equal; _} as r)
  | BinExpr ({op = Neq; _} as r) -> (
    match (typecheck_expr r.left env, typecheck_expr r.right env) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (Integer, Double)
        | (Double, Integer) -> Some (BinExpr {r with
            prop = Bool;
            left = left';
            right = right';
          })
        | (t1, t2) -> if t1 == t2 then (Some (BinExpr {r with
              prop = Bool;
              left = left';
              right = right';
            })
          ) else (prerr_string
              (string_of_positions r.pos 
                ^ ": expected bool operands, but found "
                ^ string_of_type t1 ^ ", "
                ^ string_of_type t2 ^ "\n");
              None
          )
      )
    | _ -> None
  )
  | BinExpr ({op = BoolOr; _} as r) 
  | BinExpr ({op = BoolAnd; _} as r) -> (
    match (typecheck_expr r.left env, typecheck_expr r.right env) with
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
  | BinExpr ({op = Con; _} as r) -> (
    match (typecheck_expr r.left env, typecheck_expr r.right env) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (t1, List t2) -> if (t1 == t2 || t2 == EmptyList) then (Some (BinExpr {r with
              prop = List t1;
              left = left';
              right = right';
            })
          ) else (prerr_string
              (string_of_positions r.pos 
                ^ ": expected bool operands, but found "
                ^ string_of_type t1 ^ ", "
                ^ string_of_type t2 ^ "\n");
              None
          )
        | (t1, t2) -> prerr_string
            (string_of_positions r.pos 
              ^ ": expected 't :: List 't operands, but found "
              ^ string_of_type t1 ^ ", "
              ^ string_of_type t2 ^ "\n");
            None
      )
    | _ -> None
  )
  | BinExpr ({op = Concat; _} as r) -> (
    match (typecheck_expr r.left env, typecheck_expr r.right env) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (List t1, List t2) -> if (t1 == t2 || t1 == EmptyList || t2 == EmptyList) then (Some (BinExpr {r with
              prop = List t1;
              left = left';
              right = right';
            })
          ) else (prerr_string
              (string_of_positions r.pos 
                ^ ": expected bool operands, but found "
                ^ string_of_type t1 ^ ", "
                ^ string_of_type t2 ^ "\n");
              None
          )
        | (t1, t2) -> prerr_string
            (string_of_positions r.pos 
              ^ ": expected List 't ++ List 't operands, but found "
              ^ string_of_type t1 ^ ", "
              ^ string_of_type t2 ^ "\n");
            None
      )
    | _ -> None
  )
  | IfExpr r -> (
    match typecheck_expr r.guard env with
    | Some guard' -> (
        match prop_of_prog guard' with
        | Bool -> (
            match (typecheck_expr r.conseq env, typecheck_expr r.alter env) with
            | (Some conseq', Some alter') -> (
                match (prop_of_prog conseq', prop_of_prog alter') with
                | (t1, t2) -> if t1 == t2 then (Some (IfExpr {r with
                    prop = t1;
                    guard = guard';
                    conseq = conseq';
                    alter = alter';
                  }) 
                ) else (prerr_string
                    (string_of_positions r.pos 
                      ^ ": expected then 't else 't, but found "
                      ^ string_of_type t1 ^ ", "
                      ^ string_of_type t2 ^ "\n");
                    None
                )
              )
            | _ -> None
          )
        | t -> prerr_string
            (string_of_positions r.pos 
              ^ ": expected Bool guard in if-expr, but found "
              ^ string_of_type t ^ "\n");
            None
      )
    | _ -> None
  )
  | _ -> None

let typecheck(p : unit prog) : bool =
  match typecheck_prog p with
  | Some _ -> true
  | None -> false
