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
  | "alloc"          { ALLOC }
  | "len"            { LEN }
  | "nth"            { NTH }
  | "vset"           { VSET }
  | "vec"            { VEC }
  | "RETURN"        { RETURN }
  | "(*"             { comment lexbuf } (* Démarrer le traitement des commentaires *)
  | ('-')?['0'-'9']+ as lxm { NUM(int_of_string lxm) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }

and comment = parse
    "*)"            { token lexbuf } (* Fin du commentaire, revenir à l'analyse normale *)
  | _               { comment lexbuf }
  | eof             { raise Eof } (* Gérer la fin de fichier dans un commentaire *)