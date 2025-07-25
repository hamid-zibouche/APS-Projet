{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
rule token = parse
    [' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
  | '['              { LBRA }
  | ']'              { RBRA }
  | '('              { LPAR }
  | ')'              { RPAR }
  | ';'              { PVIR }
  | ':'              { POIN2 }
  | ','              { VIRG }
  | '*'              { ETOI }
  | "->"             { VERS }
  | "CONST"          { CONST }
  | "FUN"            { FUN }
  | "REC"            { REC }
  | "VAR"            { VAR }
  | "PROC"           { PROC }
  | "ECHO"           { ECHO }
  | "SET"            { SET }
  | "IF"             { IFB }
  | "WHILE"          { WHILE }
  | "CALL"           { CALL }
  | "if"             { IF }
  | "and"            { AND }
  | "or"             { OR }
  | "bool"           { BOOL }
  | "int"            { INT }
  | "var"            { VARP }
  | "adr"            { ADR }
  | ('-')?['0'-'9']+ as lxm { NUM(int_of_string lxm) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }
