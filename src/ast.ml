type pos = Lexing.position * Lexing.position
(*
(* TODO: define your own AST *)
type 't prog =
  | IntLit of {
      prop : 't;
      pos : pos;
      value : int;
    }
  | BoolLit of {
      prop : 't;
      pos : pos;
      value : bool;
    }
  | BinExpr of {
      prop : 't;
      pos : pos;
      op : op;
      left : 't prog;
      right : 't prog;
    }
and op =
  | Add
  | Mult
  | LOr
  | LAnd
*)
(*
(* TODO: define your own AST *)
type 't prog =
  | Prog of {
      pos : pos;
      imp_list : imp_file list;
      defn_list : ('t defn) list;
    }
and imp_file = 
  | Import of {
      pos : pos;
      file_name : string;
    }
and 't defn =
  | DataDef of {
      pos : pos;
      data_name : string;
      tv_list : string list;
      cons_list : ('t constructor) list;
    }
  | TypeDef of {
      pos : pos;
      type_name : string;
      tv_list : string list;
      datatype : 't types;
    }
  | FuncDef of {
      pos : pos;
      func_name : string;
      arg_list : ('t arg) list;
      return_type : 't types;
      expr : 't expr;
    }
and 't constructor = 
  | Cons of {
      pos : pos;
      cons_name : string;
      cons_param_list : ('t types) list;
    }
and 't arg = 
  | Arg of {
      pos : pos;
      arg_name : string;
      types : 't types;
    }
and 't types =
  | TypeCons of pos * string
  | TypeVar of pos * string
  | DataType of pos * string * (string list)
  | ListType of pos * 't types
  | PairType of pos * 't types * 't types
  | FuncType of pos * 't types * 't types
and 't expr = 
  | IntLit of {
      prop : 't;
      pos : pos;
      value : int;
    }
  | FloatLit of {
      prop : 't;
      pos : pos;
      value : float;
    }
  | BoolLit of {
      prop : 't;
      pos : pos;
      value : bool;
    }
  | CharLit of {
      prop : 't;
      pos : pos;
      value : char;
    }
  | StringLit of {
      prop : 't;
      pos : pos;
      value : string;
    }
  | Ident of {
      prop : 't;
      pos : pos;
      name : string;
    }
  | Unit
  | EmptyList
  | Pairs of {
      prop : 't;
      pos : pos;
      fst : 't expr;
      snd : 't expr;
    }
  | FuncApp of {
      prop : 't;
      pos : pos;
      func : 't expr;
      arg : 't expr;
    }
  | UnaExpr of {
      prop : 't;
      pos : pos;
      op : op;
      expr : 't expr;
    }
  | BinExpr of {
      prop : 't;
      pos : pos;
      op : op;
      left : 't expr;
      right : 't expr;
    }
  | LetExpr of {
      prop : 't;
      pos : pos;
      binding_list : ('t letbinding) list;
      e_body : 't expr;
    }
  | MatchExpr of {
      prop : 't;
      pos : pos;
      value : 't expr;
      cases : ('t case) list;
    }
  | IfExpr of {
      prop : 't;
      pos : pos;
      guard : 't expr;
      conseq : 't expr;
      alter : 't expr;
    }
  | LambdaExpr of {
      prop : 't;
      pos : pos;
      arg_list : ('t arg) list;
      return_type : 't types;
      expr : 't expr;
    }
and 't case =
  | Case of {
      pos : pos;
      pattern : 't pattern;
      expr : 't expr;
    }
and 't pattern =
  | PairPattern of {
      prop : 't;
      pos : pos;
      fst : 't pattern;
      snd : 't pattern;
    }
  | ConsListPattern of {
      prop : 't;
      pos : pos;
      obj : 't pattern;
      list : 't pattern;
    }
  | ConstructorPattern of {
      prop : 't;
      pos : pos;
      cons_name : string;
      p_list : ('t pattern) list;
    }
  | Pattern of {
      prop : 't;
      pos : pos;
      expr : 't expr;
    }
  | Underscore
and 't letbinding =
  | Assign of {
      pos : pos;
      ident : 't expr;
      expr : 't expr;
    }
  | FuncDefForLet of {
      pos : pos;
      func_name : string;
      arg_list : ('t arg) list;
      return_type : 't types;
      expr : 't expr;
    }
and op =
  | Add
  | Sub
  | Mult
  | Div
  | IntDiv
  | Mod
  | Con
  | Concat
  | Greater
  | Geq
  | Less
  | Leq
  | Equal
  | Neq
  | BoolAnd
  | BoolOr
  | Dot
*)

(* TODO: define your own AST *)
type 't prog =
  | Prog of {
      prop : 't;
      pos : pos;
      imp_list : ('t prog) list;
      defn_list : ('t prog) list;
    }
  | Import of {
      prop : 't;
      pos : pos;
      file_name : string;
    }
  | DataDef of {
      prop : 't;
      pos : pos;
      data_name : string;
      tv_list : string list;
      cons_list : ('t prog) list;
    }
  | TypeDef of {
      prop : 't;
      pos : pos;
      type_name : string;
      tv_list : string list;
      datatype : 't prog;
    }
  | FuncDef of {
      prop : 't;
      pos : pos;
      func_name : string;
      arg_list : ('t prog) list;
      return_type : 't prog;
      expr : 't prog;
    }
  | Cons of {
      prop : 't;
      pos : pos;
      cons_name : string;
      cons_param_list : ('t prog) list;
    }
  | Arg of {
      prop : 't;
      pos : pos;
      arg_name : string;
      types : 't prog;
    }
  | TypeVar of {
      prop : 't;
      pos : pos;
      name : string;
    }
  | ListType of {
      prop : 't;
      pos : pos;
      type_of_list : 't prog;
    }
  | PairType of {
      prop : 't;
      pos : pos;
      fst_type : 't prog;
      snd_type : 't prog;
    }
  | DataType of {
      prop : 't;
      pos : pos;
      name : string;
      ty_list : ('t prog list)
    }
  | FuncType of {
      prop : 't;
      pos : pos;
      typeLeft : 't prog;
      typeRight : 't prog;
    }
  | IntLit of {
      prop : 't;
      pos : pos;
      value : int;
    }
  | FloatLit of {
      prop : 't;
      pos : pos;
      value : float;
    }
  | BoolLit of {
      prop : 't;
      pos : pos;
      value : bool;
    }
  | CharLit of {
      prop : 't;
      pos : pos;
      value : char;
    }
  | StringLit of {
      prop : 't;
      pos : pos;
      value : string;
    }
  | Ident of {
      prop : 't;
      pos : pos;
      name : string;
    }
  | IdentCons of {
      prop : 't;
      pos : pos;
      name : string;
    }
  | Unit of {prop : 't;}
  | EmptyList of {prop : 't;}
  | Pairs of {
      prop : 't;
      pos : pos;
      fst : 't prog;
      snd : 't prog;
    }
  | FuncApp of {
      prop : 't;
      pos : pos;
      func : 't prog;
      arg : 't prog;
    }
  | UnaExpr of {
      prop : 't;
      pos : pos;
      op : op;
      expr : 't prog;
    }
  | BinExpr of {
      prop : 't;
      pos : pos;
      op : op;
      left : 't prog;
      right : 't prog;
    }
  | LetExpr of {
      prop : 't;
      pos : pos;
      binding_list : ('t prog) list;
      e_body : 't prog;
    }
  | MatchExpr of {
      prop : 't;
      pos : pos;
      value : 't prog;
      cases : ('t prog) list;
    }
  | IfExpr of {
      prop : 't;
      pos : pos;
      guard : 't prog;
      conseq : 't prog;
      alter : 't prog;
    }
  | LambdaExpr of {
      prop : 't;
      pos : pos;
      arg_list : ('t prog) list;
      return_type : 't prog;
      expr : 't prog;
    }
  | Case of {
      prop : 't;
      pos : pos;
      pattern : 't prog;
      expr : 't prog;
    }
  | PairPattern of {
      prop : 't;
      pos : pos;
      fst : 't prog;
      snd : 't prog;
    }
  | ConsListPattern of {
      prop : 't;
      pos : pos;
      obj : 't prog;
      list : 't prog;
    }
  | ConstructorPattern of {
      prop : 't;
      pos : pos;
      cons_name : string;
      p_list : ('t prog) list;
    }
  | Pattern of {
      prop : 't;
      pos : pos;
      expr : 't prog;
    }
  | Underscore of {prop : 't;}
  | Assign of {
      prop : 't;
      pos : pos;
      ident : string;
      expr : 't prog;
    }
  | FuncDefForLet of {
      prop : 't;
      pos : pos;
      func_name : string;
      arg_list : ('t prog) list;
      return_type : 't prog;
      expr : 't prog;
    }
and op =
  | Add
  | Sub
  | Mult
  | Div
  | IntDiv
  | Mod
  | Con
  | Concat
  | Greater
  | Geq
  | Less
  | Leq
  | Equal
  | Neq
  | BoolAnd
  | BoolOr
  | Dot


let prop_of_prog(prog : 't prog) : 't = match prog with
  | IntLit r -> r.prop
  | BoolLit r -> r.prop
  | FloatLit r -> r.prop
  | CharLit r -> r.prop
  | StringLit r -> r.prop
  | Ident r -> r.prop
  | IdentCons r -> r.prop
  | Pairs r -> r.prop
  | FuncApp r -> r.prop
  | UnaExpr r -> r.prop
  | BinExpr r -> r.prop
  | LetExpr r -> r.prop
  | MatchExpr r -> r.prop
  | IfExpr r -> r.prop
  | LambdaExpr r -> r.prop
  | PairPattern r -> r.prop
  | ConsListPattern r -> r.prop
  | ConstructorPattern r -> r.prop
  | Pattern r -> r.prop
  | Unit r -> r.prop
  | EmptyList r -> r.prop
  | Underscore r -> r.prop
  | Assign r -> r.prop
  | FuncDefForLet r -> r.prop
  | Prog r -> r.prop
  | Import r -> r.prop
  | DataDef r -> r.prop
  | TypeDef r -> r.prop
  | FuncDef r -> r.prop
  | Cons r -> r.prop
  | Arg r -> r.prop
  | TypeVar r -> r.prop
  | DataType r -> r.prop
  | ListType r -> r.prop
  | PairType r -> r.prop
  | FuncType  r -> r.prop
  | Case r -> r.prop

(*
(* TODO: define pretty-printing *)
let rec string_of_prog prog : string = match prog with
  | IntLit r -> "(" ^ string_of_int r.value ^ ")"
  | BoolLit r -> "(" ^ string_of_bool r.value ^ ")"
  | BinExpr r -> "("
        ^ String.concat " " [string_of_op r.op; string_of_prog r.left; string_of_prog r.right]
        ^ ")"
and string_of_op op : string = match op with
  | Add -> "+"
  | Mult -> "*"
  | LOr -> "||"
  | LAnd -> "&&"
*)
(*
let prop_of_prog(prog : 't expr) : 't = match prog with
  | IntLit r -> r.prop
  | BoolLit r -> r.prop
  | BinExpr r -> r.prop
  | _ -> Unit
*)

(* TODO: define pretty-printing *)
let rec string_of_prog prog : string = match prog with
  | Prog r -> "(" ^ String.concat "" [("(" ^ string_of_imp r.imp_list ^ ")"); ("(" ^ string_of_defn r.defn_list ^ ")");] ^ ")"
  | _ -> ""
and string_of_imp imp : string = match imp with
  | Import r::tl -> "(import " ^ r.file_name ^ ") " ^ string_of_imp tl
  | [] -> ""
  | _ -> ""
and string_of_defn defn : string = match defn with
  | DataDef r::tl -> "(data " ^ (if r.tv_list == [] 
    then r.data_name 
    else "(" ^ r.data_name ^ " " ^ string_of_typedecl r.tv_list ^ ")") 
    ^ " (" ^ string_of_constructor_list r.cons_list ^ "))\n" ^ string_of_defn tl
  | TypeDef r::tl -> "(type " ^ (if r.tv_list == [] 
    then r.type_name 
    else "(" ^ r.type_name ^ " " ^ string_of_typedecl r.tv_list ^ ")") 
    ^ " " ^ string_of_typeExpr r.datatype ^ ")\n" ^ string_of_defn tl
  | FuncDef r::tl -> "(def " ^ r.func_name ^ "(" ^ string_of_param r.arg_list ^ ") "
    ^ string_of_typeExpr r.return_type ^ " " ^ string_of_expr r.expr ^ ")\n" ^ string_of_defn tl
  | [] -> ""
  | _ -> ""
and string_of_typedecl typedecl2 : string = match typedecl2 with
  | hd::tl -> hd ^ " " ^ string_of_typedecl tl
  | [] -> ""
and string_of_constructor_list constructor_list : string = match constructor_list with
  | hd::tl ->  string_of_constructor hd ^ " " ^ string_of_constructor_list tl
  | [] -> ""
and string_of_constructor constructor : string = match constructor with
  | Cons r -> (
    match r.cons_param_list with 
      | [] -> r.cons_name
      | _ -> "(" ^ r.cons_name ^ " " ^ string_of_cons_param_list r.cons_param_list ^ ")")
  | _ -> ""
and string_of_cons_param_list cons_param_list : string = match cons_param_list with
  | hd::tl -> string_of_typeExpr hd ^ " " ^ string_of_cons_param_list tl
  | [] -> ""
and string_of_param param : string = match param with
  | Arg r::tl -> "(" ^ r.arg_name ^ " " ^ string_of_typeExpr r.types ^ ") " ^ string_of_param tl
  | [] -> ""
  | _ -> ""
and string_of_typeExpr types : string = match types with
  | TypeVar r -> r.name
  | DataType r -> (
    match r.ty_list with
    | _::_ -> "(" ^ r.name ^ " " ^ (let rec str_loop ty_list : string = match ty_list with | hd::tl -> string_of_typeExpr hd ^ " " ^ str_loop tl | [] -> "" in str_loop r.ty_list) ^ ")"
    | [] -> r.name
  )
  | ListType r -> "([] " ^ string_of_typeExpr r.type_of_list ^ ")"
  | PairType r -> "(, " ^ string_of_typeExpr r.fst_type ^ " " ^ string_of_typeExpr r.snd_type ^ ")"
  | FuncType r -> "(-> " ^ string_of_typeExpr r.typeLeft ^ " " ^ string_of_typeExpr r.typeRight ^ ")"
  | _ -> ""
and string_of_expr expr : string = match expr with
  | IntLit r -> string_of_int r.value
  | FloatLit r -> string_of_float r.value
  | BoolLit r -> string_of_bool r.value
  | CharLit r -> "'" ^ Char.escaped r.value ^ "'"
  | StringLit r -> "\"" ^ String.escaped r.value ^ "\""
  | Ident r -> r.name
  | Unit _ -> "()"
  | EmptyList _ -> "[]"
  | Pairs r -> "(, " ^ string_of_expr r.fst ^ " " ^ string_of_expr r.snd ^ ")"
  | FuncApp r -> "(" ^ string_of_expr r.func ^ " " ^ string_of_expr r.arg ^ ")"
  | UnaExpr r -> "(" ^ String.concat " " [string_of_op r.op; string_of_expr r.expr]^ ")"
  | BinExpr r -> "(" ^ String.concat " " [string_of_op r.op; string_of_expr r.left; string_of_expr r.right] ^ ")"
  | LetExpr r -> "(let (" ^ string_of_letbinding_list r.binding_list ^ ") " ^ string_of_expr r.e_body ^ ")"
  | MatchExpr r -> "(match " ^ string_of_expr r.value ^ " (" ^ string_of_cases r.cases ^ "))"
  | IfExpr r -> "(if" ^ " " ^ string_of_expr r.guard ^ " " ^ string_of_expr r.conseq ^ " " ^ string_of_expr r.alter ^ ")"
  | LambdaExpr r -> "(\\ (" ^ string_of_param r.arg_list ^ ") " ^ string_of_typeExpr r.return_type ^ " " ^ string_of_expr r.expr ^ ")\n"
  | _ -> ""
and string_of_letbinding_list l_list : string = match l_list with
  | hd::tl -> string_of_letbinding hd ^ " " ^ string_of_letbinding_list tl
  | [] -> ""
and string_of_letbinding binding : string = match binding with
  | Assign r -> "(" ^ r.ident ^ " " ^ string_of_expr r.expr ^ ")"
  | FuncDefForLet r -> "(" ^ r.func_name ^ "(" ^ string_of_param r.arg_list ^ ") " ^ string_of_typeExpr r.return_type ^ " " ^ string_of_expr r.expr ^ ")"
  | _ -> ""
and string_of_cases cases : string = match cases with
  | hd::tl -> "(-> " ^ string_of_case hd ^ ")\n" ^ string_of_cases tl
  | [] -> ""
and string_of_case case : string = match case with
  | Case r -> string_of_pattern r.pattern ^ " " ^ string_of_expr r.expr
  | _ -> ""
and string_of_pattern ptrn : string = match ptrn with
  | PairPattern r -> "(, " ^ string_of_pattern r.fst ^ " " ^ string_of_pattern r.snd ^ ")"
  | ConsListPattern r -> "(:: " ^ string_of_pattern r.obj ^ " " ^ string_of_pattern r.list ^ ")"
  | ConstructorPattern r -> if r.p_list == [] then r.cons_name else "(" ^ r.cons_name ^ " " ^ string_of_plist r.p_list ^ ")"
  | Pattern r -> string_of_expr r.expr
  | Underscore _ -> "_"
  | _ -> ""
and string_of_plist plist : string = match plist with
  | hd::tl -> string_of_pattern hd ^ " " ^ string_of_plist tl
  | [] -> ""
and string_of_op op : string = match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | IntDiv -> "//"
  | Mod -> "%"
  | Con -> "::"
  | Concat -> "++"
  | Greater -> ">"
  | Geq -> ">="
  | Less -> "<"
  | Leq  -> "<="
  | Equal -> "=="
  | Neq -> "!="
  | BoolAnd -> "&&"
  | BoolOr -> "||"
  | Dot -> "."


(*
(* TODO: define pretty-printing *)
let rec string_of_prog prog : string = match prog with
  | Prog r -> "(" ^ String.concat "" [("(" ^ string_of_imp r.imp_list ^ ")"); ("(" ^ string_of_defn r.defn_list ^ ")");] ^ ")"
and string_of_imp imp : string = match imp with
  | Import r::tl -> "(import " ^ r.file_name ^ ") " ^ string_of_imp tl
  | [] -> ""
and string_of_defn defn : string = match defn with
  | DataDef r::tl -> "(data " ^ (if r.tv_list == [] 
    then r.data_name 
    else "(" ^ r.data_name ^ " " ^ string_of_typedecl r.tv_list ^ ")") 
    ^ " (" ^ string_of_constructor_list r.cons_list ^ "))\n" ^ string_of_defn tl
  | TypeDef r::tl -> "(type " ^ (if r.tv_list == [] 
    then r.type_name 
    else "(" ^ r.type_name ^ " " ^ string_of_typedecl r.tv_list ^ ")") 
    ^ " " ^ string_of_typeExpr r.datatype ^ ")\n" ^ string_of_defn tl
  | FuncDef r::tl -> "(def " ^ r.func_name ^ "(" ^ string_of_param r.arg_list ^ ") "
    ^ string_of_typeExpr r.return_type ^ " " ^ string_of_expr r.expr ^ ")\n" ^ string_of_defn tl
  | [] -> ""
and string_of_typedecl typedecl2 : string = match typedecl2 with
  | hd::tl -> hd ^ " " ^ string_of_typedecl tl
  | [] -> ""
and string_of_constructor_list constructor_list : string = match constructor_list with
  | hd::tl ->  string_of_constructor hd ^ " " ^ string_of_constructor_list tl
  | [] -> ""
and string_of_constructor constructor : string = match constructor with
  | Cons r ->
    match r.cons_param_list with 
      | [] -> r.cons_name
      | _ -> "(" ^ r.cons_name ^ " " ^ string_of_cons_param_list r.cons_param_list ^ ")"
and string_of_cons_param_list cons_param_list : string = match cons_param_list with
  | hd::tl -> string_of_cons_param hd ^ " " ^ string_of_cons_param_list tl
  | [] -> ""
and string_of_cons_param cons_param : string = match cons_param with
  | TypeVar (_, tv_name) -> tv_name
  | TypeCons (_, t_name) -> t_name
  | DataType (_, _dataname, _typevar_list) -> string_of_typeExpr cons_param
  | _ -> ""
and string_of_param param : string = match param with
  | Arg r::tl -> "(" ^ r.arg_name ^ " " ^ string_of_typeExpr r.types ^ ") " ^ string_of_param tl
  | [] -> ""
and string_of_typeExpr types : string = match types with
  | TypeCons (_, type_name) -> type_name
  | TypeVar (_, tv_name) -> tv_name
  | DataType (_, typeConsName, tv_list) -> "(" ^ typeConsName ^ " " ^ (let rec str_loop tv_list : string = match tv_list with | hd::tl -> hd ^ " " ^ str_loop tl | [] -> "" in str_loop tv_list) ^ ")"
  | ListType (_, type_of_list) -> "([] " ^ string_of_typeExpr type_of_list ^ ")"
  | PairType (_, type1, type2) -> "(, " ^ string_of_typeExpr type1 ^ " " ^ string_of_typeExpr type2 ^ ")"
  | FuncType (_, typeLeft, typeRight) -> "(-> " ^ string_of_typeExpr typeLeft ^ " " ^ string_of_typeExpr typeRight ^ ")"
and string_of_expr expr : string = match expr with
  | IntLit r -> string_of_int r.value
  | FloatLit r -> string_of_float r.value
  | BoolLit r -> string_of_bool r.value
  | CharLit r -> "'" ^ Char.escaped r.value ^ "'"
  | StringLit r -> "\"" ^ String.escaped r.value ^ "\""
  | Ident r -> r.name
  | Unit -> "()"
  | EmptyList -> "[]"
  | Pairs r -> "(, " ^ string_of_expr r.fst ^ " " ^ string_of_expr r.snd ^ ")"
  | FuncApp r -> "(" ^ string_of_expr r.func ^ " " ^ string_of_expr r.arg ^ ")"
  | UnaExpr r -> "(" ^ String.concat " " [string_of_op r.op; string_of_expr r.expr]^ ")"
  | BinExpr r -> "(" ^ String.concat " " [string_of_op r.op; string_of_expr r.left; string_of_expr r.right] ^ ")"
  | LetExpr r -> "(let (" ^ string_of_letbinding_list r.binding_list ^ ") " ^ string_of_expr r.e_body ^ ")"
  | MatchExpr r -> "(match " ^ string_of_expr r.value ^ " (" ^ string_of_cases r.cases ^ "))"
  | IfExpr r -> "(if" ^ " " ^ string_of_expr r.guard ^ " " ^ string_of_expr r.conseq ^ " " ^ string_of_expr r.alter ^ ")"
  | LambdaExpr r -> "(\\ (" ^ string_of_param r.arg_list ^ ") " ^ string_of_typeExpr r.return_type ^ " " ^ string_of_expr expr ^ ")\n"
and string_of_letbinding_list l_list : string = match l_list with
  | hd::tl -> string_of_letbinding hd ^ " " ^ string_of_letbinding_list tl
  | [] -> ""
and string_of_letbinding binding : string = match binding with
  | Assign r -> "(" ^ string_of_expr r.ident ^ " " ^ string_of_expr r.expr ^ ")"
  | FuncDefForLet r -> "(" ^ r.func_name ^ "(" ^ string_of_param r.arg_list ^ ") " ^ string_of_typeExpr r.return_type ^ " " ^ string_of_expr r.expr ^ ")"
and string_of_cases cases : string = match cases with
  | hd::tl -> "(-> " ^ string_of_case hd ^ ")\n" ^ string_of_cases tl
  | [] -> ""
and string_of_case case : string = match case with
  | Case r -> string_of_pattern r.pattern ^ " " ^ string_of_expr r.expr
and string_of_pattern ptrn : string = match ptrn with
  | PairPattern r -> "(, " ^ string_of_pattern r.fst ^ " " ^ string_of_pattern r.snd ^ ")"
  | ConsListPattern r -> "(:: " ^ string_of_pattern r.obj ^ " " ^ string_of_pattern r.list ^ ")"
  | ConstructorPattern r -> if r.p_list == [] then r.cons_name else "(" ^ r.cons_name ^ " " ^ string_of_plist r.p_list ^ ")"
  | Pattern r -> string_of_expr r.expr
  | Underscore -> "_"
and string_of_plist plist : string = match plist with
  | hd::tl -> string_of_pattern hd ^ " " ^ string_of_plist tl
  | [] -> ""
and string_of_op op : string = match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | IntDiv -> "//"
  | Mod -> "%"
  | Con -> "::"
  | Concat -> "++"
  | Greater -> ">"
  | Geq -> ">="
  | Less -> "<"
  | Leq  -> "<="
  | Equal -> "=="
  | Neq -> "!="
  | BoolAnd -> "&&"
  | BoolOr -> "||"
  | Dot -> "."
*)