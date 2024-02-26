open Utils
open Ast

(* TODO: define your own types *)
type ty = Integer | Bool | Double | Char | String
  | Var of {
      name : string;
    }
  | List of {
      list_type : ty;
    }
  | Pair of {
      fst : ty;
      snd : ty;
    }
  | Func of {
      ltype : ty;
      rtype : ty;
    }
  | Data of {
      name : string;
      ty_param_list : ty list;
    }
  | DataDecl of {
      name : string;
      tv_list : string list;
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

module SymTable = Map.Make(String)

module Env = struct
  let rec lookup envs name = match envs with
    | [] -> None
    | e::es -> match SymTable.find_opt name e with
      | None -> lookup es name
      | res -> res
  let add env name t = SymTable.add name t env
  let push envs = SymTable.empty::envs
end

let rec find_binding_type(env : binding list)(name : string) : ty =
  match env with
  | Map hd::tl -> if String.equal hd.name name then hd.typ else find_binding_type tl name
  | [] -> assert false

let rec string_of_type(t: ty) : string =
  match t with
  | Integer -> "Integer"
  | Bool -> "Bool"
  | Double -> "Double"
  | Char -> "Char"
  | String -> "String"
  | Var r -> r.name
  | List r -> "[" ^ string_of_type r.list_type ^ "]"
  | Pair r -> "(" ^ string_of_type r.fst ^ "," ^ string_of_type r.snd ^ ")"
  | Func r -> string_of_type r.ltype ^ "->" ^ string_of_type r.rtype
  | Data r -> r.name ^ (let rec str_loop list : string = match list with | hd::tl -> string_of_type hd ^ " " ^ str_loop tl | [] -> "" in str_loop r.ty_param_list)
  | DataDecl r -> r.name ^ (let rec str_loop tv_list : string = match tv_list with | hd::tl -> hd ^ " " ^ str_loop tl | [] -> "" in str_loop r.tv_list)
  | Constructor r -> r.name
  | Unit -> "()"
  | EmptyList -> "[]"

(* TODO: define your own type checking *)
let rec typecheck_prog(p : unit prog) : ty prog option =
  match p with
  | Prog r -> (
    match visit_data_defn_list r.defn_list with
    | None -> None
    | Some env -> (
      match visit_func_defn_list r.defn_list env with
      | None -> None 
      | Some env' -> if (Option.is_some (SymTable.find_opt "main" env')) then (
        match (typecheck_imp_list r.imp_list, typecheck_defn_list r.defn_list (env'::[])) with
        | (imp_list, Some defn_list) -> Some (Prog {r with
            prop = Unit;
            imp_list = imp_list;
            defn_list = defn_list;
          })
        | _ -> None
      ) else (
        prerr_string ("main function is not declared.\n");
        None
      )
    )
  )
  | _ -> None
and typecheck_imp_list imp_list : ty prog list = 
  match imp_list with
  | hd::tl -> typecheck_imp hd::typecheck_imp_list tl
  | [] -> []
and typecheck_imp imp : ty prog = 
  match imp with
  | Import r -> (Import {r with prop = Unit})
  | _ -> assert false
and visit_data_defn_list defn_list : ty SymTable.t option =
  match defn_list with
  | hd::tl -> (
    match hd with 
    | DataDef r -> (
      match visit_data_defn_list tl with
      | None -> None
      | Some env -> (
        match r.data_name with
        | "Integer" 
        | "Bool" 
        | "Double" 
        | "Char" 
        | "String" -> prerr_string
          (string_of_positions r.pos 
            ^ ": Data name is reserved name. -> "
            ^ r.data_name ^ "\n");
          None
        | _ -> (
          match (SymTable.find_opt ("data:" ^ r.data_name) env, (List.length (List.sort_uniq String.compare r.tv_list)) == List.length r.tv_list) with
          | (None, true) -> Some (Env.add env ("data:" ^ r.data_name) (DataDecl {
              name = r.data_name;
              tv_list = r.tv_list;
          }))
          | (None, false) -> prerr_string
            (string_of_positions r.pos
              ^ ": type variable name is duplicated in -> "
              ^ r.data_name ^ "\n");
            None
          | _ -> prerr_string
            (string_of_positions r.pos 
              ^ ": Data name is duplicated. -> "
              ^ r.data_name ^ "\n");
            None
        )
      )
    )
    | _ -> visit_data_defn_list tl
  )
  | [] -> Some SymTable.empty
and visit_func_defn_list defn_list env : ty SymTable.t option =
  match defn_list with
  | hd::tl -> (
    match hd with 
    | FuncDef r -> (
      match visit_func_defn_list tl env with
      | None -> None
      | Some env' -> (
        match SymTable.find_opt r.func_name env' with
        | None -> (
          match visit_func_defn hd env' with
          | Some t -> Some (Env.add env' r.func_name t)
          | _ -> None
        )
        | _ -> prerr_string
          (string_of_positions r.pos 
            ^ ": Function name is duplicated. -> "
            ^ r.func_name ^ "\n");
          None
      )
    )
    | _ -> visit_func_defn_list tl env
  )
  | [] -> Some env
and visit_func_defn defn env : ty option =
  match defn with
  | FuncDef r -> let rec find_func_type arg_list : ty option =
    match arg_list with 
    | Arg hd::tl -> (
      match (find_type hd.types (env::[]), find_func_type tl) with
      | (Some lt, Some rt) -> Some (Func {
        ltype = lt;
        rtype = rt;
      })
      | _ -> None
    )
    | [] -> find_type r.return_type (env::[])
    | _ -> assert false
    in find_func_type r.arg_list
  | _ -> assert false
and find_type typ env : ty option = 
  match typ with
  | TypeVar r -> Some (Var { name = r.name; })
  | ListType r -> (
    match find_type r.type_of_list env with
    | Some lt -> Some (List { list_type = lt; })
    | _ -> None
  )
  | PairType r -> (
    match (find_type r.fst_type env, find_type r.snd_type env) with
    | (Some fst', Some snd') -> Some (Pair {
      fst = fst';
      snd = snd';
    })
    | _ -> None
  )
  | DataType r -> (
    match r.name with
    | "Integer" | "Bool" | "Double" | "Char" | "String" -> (
      if List.length r.ty_list == 0 then 
        match r.name with
        | "Integer" -> Some Integer
        | "Bool" -> Some Bool
        | "Double" -> Some Double
        | "Char" -> Some Char
        | "String" -> Some String
        | _ -> assert false
      else (
        prerr_string
        (string_of_positions r.pos 
          ^ ": This Data type don't have any parameter. -> "
          ^ r.name ^ "\n");
        None
      )
    )
    | name -> match Env.lookup env ("data:" ^ name) with
      | None -> prerr_string
        (string_of_positions r.pos 
          ^ ": This Data type is not declared. -> "
          ^ name ^ "\n");
        None
      | Some DataDecl d -> (
        if List.length d.tv_list == List.length r.ty_list then
          let rec find_ty_param_list l : ty list option =
            match l with
            | hd::tl -> (
              match find_ty_param_list tl with
              | None -> None
              | Some list -> (
                match find_type hd env with
                | None -> None
                | Some t -> Some (t::list)
              )
            )
            | [] -> Some []
          in match find_ty_param_list r.ty_list with
            | None -> None
            | Some ty_list -> Some (Data {
              name = d.name;
              ty_param_list = ty_list
            })
        else (
          prerr_string
          (string_of_positions r.pos 
            ^ ": This Data type don't match with parameter type. -> "
            ^ name ^ "\n");
          None
        )
      )
      | _ -> assert false
  )
  | FuncType r -> (
    match (find_type r.typeLeft env, find_type r.typeRight env) with
    | (Some tl, Some tr) -> Some (Func {
      ltype = tl;
      rtype = tr;
    })
    | _ -> None
  )
  | _ -> assert false
and typecheck_types(typ : unit prog) env (data : ty option) : ty prog option = 
  match typ with
  | TypeVar r -> (
    match data with
    | Some DataDecl d -> if List.exists (String.equal r.name) d.tv_list
      then Some (TypeVar {r with
        prop = Var { name = r.name; };
      })
      else (prerr_string
          (string_of_positions r.pos 
            ^ ": This type variable is not declared in " ^ d.name ^ " data. -> "
            ^ r.name ^ "\n");
          None
      )
    | None -> Some (TypeVar {r with
      prop = Var { name = r.name; };
    })
    | _ -> assert false
  )
  | ListType r -> (
    match typecheck_types r.type_of_list env data with
    | Some lt -> Some (ListType {r with
      prop = List { list_type = prop_of_prog lt; };
      type_of_list = lt;
    })
    | _ -> None
  )
  | PairType r -> (
    match (typecheck_types r.fst_type env data, typecheck_types r.snd_type env data) with
    | (Some fst', Some snd') -> Some (PairType {r with
      prop = Pair {
        fst = prop_of_prog fst';
        snd = prop_of_prog snd';
      };
      fst_type = fst';
      snd_type = snd';
    })
    | _ -> None
  )
  | DataType r -> (
    match r.name with
    | "Integer" | "Bool" | "Double" | "Char" | "String" -> (
      if List.length r.ty_list == 0 then 
        match r.name with
        | "Integer" -> Some (DataType{r with prop = Integer; ty_list = []})
        | "Bool" -> Some (DataType{r with prop = Bool; ty_list = []})
        | "Double" -> Some (DataType{r with prop = Double; ty_list = []})
        | "Char" -> Some (DataType{r with prop = Char; ty_list = []})
        | "String" -> Some (DataType{r with prop = String; ty_list = []})
        | _ -> assert false
      else (
        prerr_string
        (string_of_positions r.pos 
          ^ ": This Data type don't have any parameter. -> "
          ^ r.name ^ "\n");
        None
      )
    )
    | name -> match (Env.lookup env ("data:" ^ name)) with
      | None -> prerr_string
        (string_of_positions r.pos 
          ^ ": This Data type is not declared. -> "
          ^ name ^ "\n");
        None
      | Some DataDecl d -> (
        if List.length d.tv_list == List.length r.ty_list then
          let rec find_ty_param_list l : ty prog list option =
            match l with
            | hd::tl -> (
              match find_ty_param_list tl with
              | None -> None
              | Some list -> (
                match typecheck_types hd env data with
                | None -> None
                | Some t -> Some (t::list)
              )
            )
            | [] -> Some []
          in match find_ty_param_list r.ty_list with
            | None -> None
            | Some ty_list -> Some (DataType {r with
              prop = Data {
                name = r.name;
                ty_param_list = let rec find_prop_list l =
                  match l with
                  | hd::tl -> (prop_of_prog hd)::(find_prop_list tl)
                  | [] -> []
                in find_prop_list ty_list;
              };
              ty_list = ty_list;
            })
        else (
          prerr_string
          (string_of_positions r.pos 
            ^ ": This Data type don't match with parameter type. -> "
            ^ name ^ "\n");
          None
        )
      )
      | _ -> assert false
  )
  | FuncType r -> (
    match (typecheck_types r.typeLeft env data, typecheck_types r.typeRight env data) with
    | (Some tl, Some tr) -> Some (FuncType {r with
      prop = Func {
        ltype = prop_of_prog tl;
        rtype = prop_of_prog tr;
      };
      typeLeft = tl;
      typeRight = tr;
    })
    | _ -> None
  )
  | _ -> assert false
and typecheck_defn_list defn_list env : ty prog list option = 
  match typecheck_data_defn_list defn_list env with
  | None -> None
  | Some data_list -> (
    match typecheck_func_defn_list defn_list env with
    | None -> None
    | Some func_list -> Some (List.append data_list func_list)
  )
and typecheck_data_defn_list defn_list env : ty prog list option =
  match defn_list with
  | hd::tl -> (
    match hd with
    | DataDef _r -> (
      match typecheck_defn hd env with
      | None -> None
      | Some data -> (
        match typecheck_data_defn_list tl env with
        | None -> None
        | Some data_list -> Some (data::data_list)
      )
    )
    | _ -> typecheck_data_defn_list tl env
  )
  | [] -> Some []
and typecheck_func_defn_list defn_list env : ty prog list option =
  match defn_list with
  | hd::tl -> (
    match hd with
    | FuncDef _r -> (
      match typecheck_defn hd env with
      | None -> None
      | Some func -> (
        match typecheck_func_defn_list tl env with
        | None -> None
        | Some func_list -> Some (func::func_list)
      )
    )
    | _ -> typecheck_func_defn_list tl env
  )
  | [] -> Some []
and typecheck_defn(p : unit prog) env : ty prog option =
  match p with
  | DataDef r -> (
    let rec typecheck_cons_decl_list cons_list env = 
      match cons_list with
      | hd::tl -> (
        match typecheck_cons_decl hd env (Option.get (Env.lookup env ("data:" ^ r.data_name))) with
        | None -> None
        | Some cons -> (
          match cons with
          | Cons c -> (
            match typecheck_cons_decl_list tl ((Env.add (List.hd env) ("cons" ^ c.cons_name) (prop_of_prog cons))::[]) with
            | None -> None
            | Some list -> Some (cons::list)
          )
          | _ -> assert false
        )
      )
      | [] -> Some []
    in
    match typecheck_cons_decl_list r.cons_list env with
    | None -> None
    | Some list -> Some (DataDef {r with
        prop = Option.get (Env.lookup env ("data:" ^ r.data_name));
        cons_list = list;
      })
  )
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
and typecheck_cons_decl cons env (data : ty) : ty prog option =
  match cons with
  | Cons r -> (
    if Option.is_none (Env.lookup env ("cons" ^ r.cons_name)) 
    then ( let rec find_ty_cons_list (cp_list : unit prog list) env : ty prog list option = 
        match cp_list with
        | hd::tl -> (
          match typecheck_types hd env (Some data) with
          | None -> None
          | Some ty -> (
            match find_ty_cons_list tl env with
            | None -> None
            | Some list -> Some (ty::list)
          )
        )
        | [] -> Some []
      in
      if List.length r.cons_param_list == 0
        then Some (Cons {r with
          prop = Func {
            ltype = Unit;
            rtype = data;
          };
          cons_param_list = [];
        })
        else match find_ty_cons_list r.cons_param_list env with
        | None -> None
        | Some list -> Some (Cons {r with
          prop = (
            let rec find_func_prop (cp_list : ty prog list) : ty = 
              match cp_list with
              | hd::tl -> Func {
                ltype = prop_of_prog hd;
                rtype = find_func_prop tl;
              }
              | [] -> data
          in find_func_prop list);
          cons_param_list = list;
        })
    )
    else (prerr_string
      (string_of_positions r.pos 
        ^ ": This Constructor name is duplicated. -> "
        ^ r.cons_name ^ "\n");
      None
    )
  )
  | _ -> assert false
and typecheck_expr(p : unit prog) env : ty prog option =
  match p with
  | IntLit r -> Some (IntLit {r with prop = Integer})
  | FloatLit r -> Some (FloatLit {r with prop = Double})
  | BoolLit r -> Some (BoolLit {r with prop = Bool})
  | CharLit r -> Some (CharLit {r with prop = Char})
  | StringLit r -> Some (StringLit {r with prop = String})
  | Ident r -> (
    match Env.lookup env r.name with 
    | Some t -> Some (Ident {r with prop = t})
    | None -> None
  )
  | Unit _r -> Some (Unit {prop = Unit})
  | EmptyList _r -> Some (EmptyList {prop = List { list_type = EmptyList }}) (* prop = List EmptyList ??? *)
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
        | (t1, List t2) -> if (t1 == t2.list_type || t2.list_type == EmptyList) then (Some (BinExpr {r with
              prop = List { list_type = t1; };
              left = left';
              right = right';
            })
          ) else (prerr_string
              (string_of_positions r.pos 
                ^ ": expected bool operands, but found "
                ^ string_of_type t1 ^ ", "
                ^ string_of_type t2.list_type ^ "\n");
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
        | (List t1, List t2) -> if (t1.list_type == t2.list_type || t1.list_type == EmptyList || t2.list_type == EmptyList) then (Some (BinExpr {r with
              prop = List t1;
              left = left';
              right = right';
            })
          ) else (prerr_string
              (string_of_positions r.pos 
                ^ ": expected bool operands, but found "
                ^ string_of_type t1.list_type ^ ", "
                ^ string_of_type t2.list_type ^ "\n");
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
