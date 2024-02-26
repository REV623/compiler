(** header section **)
%{
open Ast
%}

(** declarations section **)

(* token declarations *)
%token IMPORT DEF DATA TYPE MATCH WITH IF THEN ELSE LET IN AND
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <string> STR_LIT IDENT TYPECONS
%token <bool> BOOL_LIT
%token PLUS "+" MINUS "-" TIMES "*" DIV "/" INTDIV "//" MOD "%" BOOL_AND "&&" BOOL_OR "||"
  GREATER ">" GEQ ">=" LESS "<" LEQ "<=" EQUAL "==" NEQ "!=" 
  LPAREN "(" RPAREN ")" LBRACK "[" RBRACK "]"
  COLON ":" DOT "." COMMA "," ASSIGN "=" CONS "::" BAR "|" UNDERSCORE "_"
  ARROW "->" BACKSLASH "\\" DOUARROW "=>" CONCAT "++" DOLLAR "$"
%token EOF

%type <'t prog> expr_all

(* precedence and associativity declarations *)
//%right DOLLAR
//%right BOOL_OR
//%right BOOL_AND
//%nonassoc EQUAL NEQ LESS LEQ GEQ GREATER
//%right CONS CONCAT
//%left PLUS MINUS
//%left TIMES DIV INTDIV MOD
//%right DOT

(* starting nonterminal *)
%start <unit prog> start
%%

(** rules section **)
start:
  | prog = prog; EOF { prog }

prog:
  | imp_list = imp_block; defn_list = defn_block { Prog {
      prop = ();
      pos = $loc;
      imp_list = imp_list;
      defn_list = defn_list;
    }}

imp_block:
  | imp = imp_stmt; imp_list = imp_block { imp::imp_list }
  | { [] }

imp_stmt:
  | IMPORT; filename = IDENT { Import {
      prop = ();
      pos = $loc;
      file_name = filename;
    }}

defn_block:
  | data = data_def; defn_list = defn_block { data::defn_list }
  | typs = type_def; defn_list = defn_block { typs::defn_list }
  | func = func_def; defn_list = defn_block { func::defn_list }
  | func = func_def { func::[] }

data_def:
  | DATA; dataname = TYPECONS; tv_list = type_vars_list; "="; cons_list = constructor_list; { DataDef {
      prop = ();
      pos = $loc;
      data_name = dataname;
      tv_list = tv_list;
      cons_list = cons_list;
    }}

type_vars_list:
  | tv = IDENT; tv_list = type_vars_list { tv::tv_list }
  | { [] }

constructor_list:
  | cons = constructor; "|"; cons_list = constructor_list { cons::cons_list }
  | cons = constructor; { cons::[] }

constructor:
  | consName = TYPECONS; cp_list = cons_param_type_list; { Cons {
      prop = ();
      pos = $loc;
      cons_name = consName;
      cons_param_list = cp_list;
    }}

cons_param_type_list:
  | cp = types; cp_list = cons_param_type_list; { cp::cp_list }
  | { [] }

type_def:
  | TYPE; typename = TYPECONS; tv_list = type_vars_list; "="; data = data_type; { TypeDef {
      prop = ();
      pos = $loc;
      type_name = typename;
      tv_list = tv_list;
      datatype = data;
    }}

func_def:
  | DEF; fname = IDENT; args_list = args_poly; ":"; return_type = types; "="; e = expr_all { FuncDef {
      prop = ();
      pos = $loc;
      func_name = fname;
      arg_list = args_list;
      return_type = return_type;
      expr = e;
    }}

args_poly:
  | args_list = args { args_list }
  | "("; ")" { [] }

args:
  | arg = arg; args_list = args { arg::args_list }
  | arg = arg { arg::[] }

arg:
  | "("; id = IDENT; ":"; t = type_of_arg; ")" { Arg {
      prop = ();
      pos = $loc;
      arg_name = id;
      types = t;
    }}

type_of_arg:
  | ty = types_all { ty }
  | ft = func_type { ft }

types_all:
  | ty = types { ty }
  | dt = data_type; { dt }

types:
  | tv = IDENT { TypeVar {
      prop = ();
      pos = $loc;
      name = tv; 
    }}
  | tc = TYPECONS { DataType {
      prop = ();
      pos = $loc;
      name = tc;
      ty_list = [];
    }}
  | lt = list_type { lt }
  | pt = pairs_type { pt }
  | "("; ty = data_type; ")"; { ty }

list_type:
  | "["; t = types; "]" { ListType {
      prop = ();
      pos = $loc;
      type_of_list = t;
    }}

pairs_type:
  | "("; t1 = types; ","; t2 = types; ")" { PairType {
      prop = ();
      pos = $loc;
      fst_type = t1;
      snd_type = t2;
    }}

data_type:
  | dataname = TYPECONS; ty_list = type_list_for_data; { DataType {
      prop = ();
      pos = $loc;
      name = dataname;
      ty_list = ty_list;
    }}

type_list_for_data:
  | ty = types; ty_list = type_list_for_data { ty::ty_list }
  | ty = types; { ty::[] }

func_type:
  | t = types; "->"; ft = func_type; { FuncType {
      prop = ();
      pos = $loc;
      typeLeft = t;
      typeRight = ft;
    }}
  | tl = types; "->"; tr = types; { FuncType {
      prop = ();
      pos = $loc;
      typeLeft = tl;
      typeRight = tr;
    }}

expr_all: // can generate all type of expression
  | e = expr; { e }
  | m = match_expr; { m }

expr: // can generate all type of expression except match expr
  | e = func_app_Rassoc_expr; { e }
  | e = let_expr; { e }
  | e = if_expr; { e }
  | e = lambda_expr; { e }

let_expr:
  | LET; l_list = letbinding_list; IN; e = expr; { LetExpr {
      prop = ();
      pos = $loc;
      binding_list = l_list;
      e_body = e;
    }}

letbinding_list:
  | l = letbinding; AND; l_list = letbinding_list; { l::l_list }
  | l = letbinding; { l::[] }

letbinding:
  | f = func_for_let; { f }
  | id = IDENT; "="; e = expr_all; { Assign {
      prop = ();
      pos = $loc;
      ident = Ident {
        prop = ();
        pos = $loc;
        name = id;
      };
      expr = e;
    }}

func_for_let:
  | fname = IDENT; args_list = args_poly; ":"; return_type = types; "="; e = expr_all { FuncDefForLet {
      prop = ();
      pos = $loc;
      func_name = fname;
      arg_list = args_list;
      return_type = return_type;
      expr = e;
    }}

match_expr:
  | MATCH; e = expr_all; WITH; cases = cases; { MatchExpr {
      prop = ();
      pos = $loc;
      value = e;
      cases = cases;
    }}

cases:
  | c = case; c_list = cases; { c::c_list }
  | c = case; { c::[] }
  | c = case_with_match_expr; { c::[] }

case:
  | "|"; ptrn = pattern; "->"; e = expr; { Case {
      prop = ();
      pos = $loc;
      pattern = ptrn;
      expr = e;
    }}

case_with_match_expr:
  | "|"; ptrn = pattern; "->"; e = expr_for_dangling_case { Case {
      prop = ();
      pos = $loc;
      pattern = ptrn;
      expr = e;
    }}

expr_for_dangling_case:
  | e = match_expr; { e }
  | IF; e = expr_all; THEN; e1 = expr_all; ELSE; e2 = match_expr; { IfExpr {
      prop = ();
      pos = $loc;
      guard = e;
      conseq = e1;
      alter = e2;
    }}
  | LET; l_list = letbinding_list; IN; e = match_expr; { LetExpr {
      prop = ();
      pos = $loc;
      binding_list = l_list;
      e_body = e;
    }}
  | "\\"; args_list = args_poly; ":"; return_type = types; "=>"; e = match_expr { LambdaExpr {
      prop = ();
      pos = $loc;
      arg_list = args_list;
      return_type = return_type;
      expr = e;
    }}

pattern:
  | p1 = pattern_constructor; "::" p2 = pattern; { ConsListPattern {
      prop = ();
      pos = $loc;
      obj = p1;
      list = p2;
    }}
  | p = pattern_constructor; { p }

pattern_constructor:
  | tc = TYPECONS; p_list = pattern_list; { ConstructorPattern {
      prop = ();
      pos = $loc;
      cons_name = tc;
      p_list = p_list;
    }}
  | p = pattern_literal; { p }

pattern_list:
  | p = pattern_literal; p_list = pattern_list; { p::p_list }
  | { [] }

pattern_literal:
  | id = IDENT { Pattern {
      prop = ();
      pos = $loc;
      expr = Ident {
        prop = ();
        pos = $loc;
        name = id;
      };
    }}
  | n = INT_LIT { Pattern {
      prop = ();
      pos = $loc;
      expr = IntLit {
        prop = ();
        pos = $loc;
        value = n;
      };
    }}
  | d = FLOAT_LIT { Pattern {
      prop = ();
      pos = $loc;
      expr = FloatLit {
        prop = ();
        pos = $loc;
        value = d;
      };
    }}
  | b = BOOL_LIT { Pattern {
      prop = ();
      pos = $loc;
      expr = BoolLit {
        prop = ();
        pos = $loc;
        value = b;
      };
    }}
  | c = CHAR_LIT { Pattern {
      prop = ();
      pos = $loc;
      expr = CharLit {
        prop = ();
        pos = $loc;
        value = c;
      };
    }}
  | s = STR_LIT { Pattern {
      prop = ();
      pos = $loc;
      expr = StringLit {
        prop = ();
        pos = $loc;
        value = s;
      };
    }}
  | "("; ")"; { Pattern {
      prop = ();
      pos = $loc;
      expr = Unit {
        prop = ();
      };
    }}
  | "["; "]"; { Pattern {
      prop = ();
      pos = $loc;
      expr = EmptyList {
        prop = ();
      };
    }}
  | "_"; { Underscore {
      prop = ();
    }}
  | "("; p1 = pattern; ","; p2 = pattern; ")"; { PairPattern {
      prop = ();
      pos = $loc;
      fst = p1;
      snd = p2;
    }}
  | "("; p = pattern; ")"; { p }
  
if_expr:
  | IF; e = expr_all; THEN; e1 = expr_all; ELSE; e2 = expr; { IfExpr {
      prop = ();
      pos = $loc;
      guard = e;
      conseq = e1;
      alter = e2;
    }}

lambda_expr:
  | "\\"; args_list = args_poly; ":"; return_type = types; "=>"; e = expr { LambdaExpr {
      prop = ();
      pos = $loc;
      arg_list = args_list;
      return_type = return_type;
      expr = e;
    }}

func_app_Rassoc_expr: // right-assoc | functype $ functype $ arg = (functype $ (functype $ arg)) ; e1 : functype, e2 : type
  | e1 = bin_boolOr_expr; "$"; e2 = func_app_Rassoc_expr; { FuncApp {
      prop = ();
      pos = $loc;
      func = e1;
      arg = e2;
    }}
  | e = bin_boolOr_expr; { e }

bin_boolOr_expr: // right-assoc
  | e1 = bin_boolAnd_expr; "||"; e2 = bin_boolOr_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = BoolOr;
      left = e1;
      right = e2;
    }}
  | e = bin_boolAnd_expr; { e }

bin_boolAnd_expr: // right-assoc
  | e1 = compare_expr; "&&"; e2 = bin_boolAnd_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = BoolAnd;
      left = e1;
      right = e2;
    }}
  | e = compare_expr; { e }

compare_expr: // non-assoc
  | e1 = list_expr; ">"; e2 = list_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Greater;
      left = e1;
      right = e2;
    }}
  | e1 = list_expr; ">="; e2 = list_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Geq;
      left = e1;
      right = e2;
    }}
  | e1 = list_expr; "<"; e2 = list_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Less;
      left = e1;
      right = e2;
    }}
  | e1 = list_expr; "<="; e2 = list_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Leq;
      left = e1;
      right = e2;
    }}
  | e1 = list_expr; "=="; e2 = list_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Equal;
      left = e1;
      right = e2;
    }}
  | e1 = list_expr; "!="; e2 = list_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Neq;
      left = e1;
      right = e2;
    }}
  | e = list_expr; { e }

list_expr: // right-assoc
  | e1 = bin_arith_expr_s; "::"; e2 = list_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Con;
      left = e1;
      right = e2;
    }}
  | e1 = bin_arith_expr_s; "++"; e2 = list_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Concat;
      left = e1;
      right = e2;
    }}
  | e = bin_arith_expr_s; { e }

bin_arith_expr_s: // left-assoc
  | e1 = bin_arith_expr_s; "+"; e2 = bin_arith_expr_t { BinExpr {
      prop = ();
      pos = $loc;
      op = Add;
      left = e1;
      right = e2;
    }}
  | e1 = bin_arith_expr_s; "-"; e2 = bin_arith_expr_t { BinExpr {
      prop = ();
      pos = $loc;
      op = Sub;
      left = e1;
      right = e2;
    }}
  | e = bin_arith_expr_t; { e }

bin_arith_expr_t: // left-assoc
  | e1 = bin_arith_expr_t; "*"; e2 = una_arith_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Mult;
      left = e1;
      right = e2;
    }}
  | e1 = bin_arith_expr_t; "/"; e2 = una_arith_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Div;
      left = e1;
      right = e2;
    }}
  | e1 = bin_arith_expr_t; "//"; e2 = una_arith_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = IntDiv;
      left = e1;
      right = e2;
    }}
  | e1 = bin_arith_expr_t; "%"; e2 = una_arith_expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Mod;
      left = e1;
      right = e2;
    }}
  | e = una_arith_expr; { e }

una_arith_expr: // non-assoc
  | "-"; e = func_comp_expr { UnaExpr {
      prop = ();
      pos = $loc;
      op = Sub;
      expr = e;
    }}
  | e = func_comp_expr; { e }

func_comp_expr: // right-assoc
  | e1 = func_app_expr; "."; e2 = func_comp_expr; { BinExpr {
      prop = ();
      pos = $loc;
      op = Dot;
      left = e1;
      right = e2;
    }}
  | e = func_app_expr; { e }

func_app_expr: // left-assoc
  | e1 = func_app_expr; e2 = literal_expr; { FuncApp {
      prop = ();
      pos = $loc;
      func = e1;
      arg = e2;
    }}
  | e = literal_expr; { e }

literal_expr:
  | id = IDENT { Ident {
      prop = ();
      pos = $loc;
      name = id;
    }}
  | n = INT_LIT { IntLit {
      prop = ();
      pos = $loc;
      value = n;
    }}
  | d = FLOAT_LIT { FloatLit {
      prop = ();
      pos = $loc;
      value = d;
    }}
  | b = BOOL_LIT { BoolLit {
      prop = ();
      pos = $loc;
      value = b;
    }}
  | c = CHAR_LIT { CharLit {
      prop = ();
      pos = $loc;
      value = c;
    }}
  | s = STR_LIT { StringLit {
      prop = ();
      pos = $loc;
      value = s;
    }}
  | "("; ")"; { Unit {
      prop = ();
    }}
  | "["; "]"; { EmptyList {
      prop = ();
    }}
  | "("; p1 = expr_all; ","; p2 = expr_all; ")"; { Pairs {
      prop = ();
      pos = $loc;
      fst = p1;
      snd = p2;
    }}
  | "("; e = expr_all; ")" { e }
