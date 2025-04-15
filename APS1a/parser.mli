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
  | VAR
  | PROC
  | SET
  | IFB
  | WHILE
  | CALL
  | IF
  | AND
  | OR
  | BOOL
  | INT
  | VARP
  | ADR

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.cmd list
