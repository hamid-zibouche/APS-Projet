type token =
  | NUM of (int)
  | IDENT of (string)
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | ECHO
  | PVIR
  | POIN2
  | VIRG
  | ETOI
  | VERS
  | CONST
  | FUN
  | REC
  | IF
  | AND
  | OR
  | BOOL
  | INT

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.cmd list
