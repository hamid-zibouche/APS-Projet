
type typee =
    ASTBool 
    | ASTInt 
    | ASTTypes of typee list * typee

type arg =
    ASTArg of string * typee

type expr =
    ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr 
  | ASTOr of expr * expr
  | ASTApp of expr * expr list
  | ASTFerm of arg list * expr

type stat =
    ASTEcho of expr
    |ASTSet of expr * expr
    |ASTIFB of expr * cmd list * cmd list
    |ASTWhile of expr  * cmd list
    |ASTCall of expr  * expr list
and
   
cmd =
    ASTStat of stat
  | ASTConst of  expr * typee * expr
  | ASTFun of expr * typee * arg list * expr 
  | ASTFunRec of expr * typee * arg list * expr
  | ASTVar of expr * typee
  | ASTProc of expr * arg list * cmd list
  | ASTProcRec of expr * arg list * cmd list





	
