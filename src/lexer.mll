(*
(** header: optional imports & setup **)
{
open Parser

exception SyntaxError of string * Lexing.position
let error msg lexbuf =
  raise (SyntaxError(msg, Lexing.lexeme_start_p lexbuf))

let string_buf = Buffer.create 256
}

(** regex patterns **)
let line_terminator = '\r' | '\n' | "\r\n"
let whitespace = (line_terminator | [' ' '\t'])+
let digit = ['0'-'9']

(** lexing rules **)
rule read = parse
  | whitespace { read lexbuf }
  | "+" { PLUS }
  | "*" { TIMES }
  | "||" { OROR }
  | "&&" { ANDAND }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | digit+ as x { INT_LIT (int_of_string x) }
  | "true" { TRUE }
  | "false" { FALSE }
  | eof { EOF }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }
*)

(** header: optional imports & setup **)
{
open Parser

exception SyntaxError of string * Lexing.position
let error msg lexbuf =
  raise (SyntaxError(msg, Lexing.lexeme_start_p lexbuf))

let string_buf = Buffer.create 256
}

(** regex patterns **)
let line_terminator = '\r' | '\n' | "\r\n"
let whitespace = (line_terminator | [' ' '\t'])+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier = ['a'-'z'](digit | letter | '_')*
let typeCons = ['A'-'Z'](digit | letter | '_')*

(** lexing rules **)
rule read = parse
  | whitespace { read lexbuf }
  (* Keyword *)
  | "import" { IMPORT }
  | "def" { DEF }
  | "data" { DATA }
  | "type" { TYPE }
  | "match" { MATCH }
  | "with" { WITH }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "and" { AND }
  (* Symbol *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "//" { INTDIV }
  | "%" { MOD }
  | ">" { GREATER }
  | ">=" { GEQ }
  | "<" { LESS }
  | "<=" { LEQ }
  | "==" { EQUAL }
  | "!=" { NEQ }
  | "&&" { BOOL_AND }
  | "||" { BOOL_OR }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | ":" { COLON }
  | "." { DOT }
  | "," { COMMA }
  | "=" { ASSIGN }
  | "::" { CONS }
  | "|" { BAR }
  | "_" { UNDERSCORE }
  | "->" { ARROW }
  | "\\" { BACKSLASH }
  | "=>" { DOUARROW }
  | "++" { CONCAT }
  | "$" { DOLLAR }
  (* Literals *)
  | "True" { BOOL_LIT (true) }
  | "False" { BOOL_LIT (false) }
  | digit+ as x { INT_LIT (int_of_string x) }
  | digit+ '.' digit+ as x { FLOAT_LIT (float_of_string x) }
  | "\'" { read_char lexbuf }
  | "\""  { Buffer.clear string_buf;
            read_string lexbuf;
            STR_LIT (Buffer.contents string_buf) }
  (* var & typecons name *)
  | identifier as ident { IDENT (ident) }
  | typeCons as ident { TYPECONS (ident) }
  | eof { EOF }
  (* Comment *)
  | "#" { read_comment lexbuf }
  | "(*" { read_multiline_comment lexbuf }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }

and read_char = parse
  | ([^ '\'' '\"' '\\'] as ch) "\'" { CHAR_LIT (ch) }
  | "\\\\" "\'" { CHAR_LIT ('\\') }
  | "\\\'" "\'" { CHAR_LIT ('\'') }
  | "\\\"" "\'" { CHAR_LIT ('\"') }
  | "\\n" "\'" { CHAR_LIT ('\n') }
  | "\\r" "\'" { CHAR_LIT ('\r') }
  | "\\t" "\'" { CHAR_LIT ('\t') }
  | "\\b" "\'" { CHAR_LIT ('\b') }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }

and read_string = parse
  | [^ '\"' '\'' '\\'] as ch  { Buffer.add_char string_buf ch;
                                read_string lexbuf }
  | "\\\\"  { Buffer.add_char string_buf '\\';
              read_string lexbuf }
  | "\\\'"  { Buffer.add_char string_buf '\'';
              read_string lexbuf }
  | "\\\""  { Buffer.add_char string_buf '\"';
              read_string lexbuf }
  | "\\n" { Buffer.add_char string_buf '\n';
            read_string lexbuf }
  | "\\r" { Buffer.add_char string_buf '\r';
            read_string lexbuf }
  | "\\t" { Buffer.add_char string_buf '\t';
            read_string lexbuf }
  | "\\b" { Buffer.add_char string_buf '\b';
            read_string lexbuf }
  | "\""  { Buffer.add_string string_buf ""; }

and read_comment = parse
  | line_terminator { read lexbuf }
  | _ { read_comment lexbuf }

and read_multiline_comment = parse
  | "*)" { read lexbuf }
  | _ { read_multiline_comment lexbuf }
  | eof { error (Lexing.lexeme lexbuf) lexbuf }
