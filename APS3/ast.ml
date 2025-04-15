(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type typee =
    ASTBool 
    | ASTInt 
    | ASTTypes of typee list * typee
    | ASTVec of typee

type arg =
    ASTArg of string * typee

type argp =
    ASTArgp of string * typee
    | ASTVargp of string * typee

type expr =
    ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTApp of expr * expr list
  | ASTFerm of arg list * expr
  | ASTAlloc of expr
  | ASTLen of expr
  | ASTNthExpr of expr * expr
  | ASTVset of expr * expr * expr

type lval =
    ASTIdLval of string
    | ASTNthLval of lval * expr

type exprp =
    | ASTExpr of expr
    | ASTAdr of string

type stat =
    ASTEcho of expr
    |ASTSet of expr * expr
    |ASTIFB of expr * cmd list * cmd list
    |ASTWhile of expr  * cmd list
    |ASTCall of expr  * exprp list
    |ASTSetTab of lval * expr

and
   
cmd =
    ASTStat of stat
  | ASTConst of  expr * typee * expr
  | ASTFun of expr * typee * arg list * expr 
  | ASTFunRec of expr * typee * arg list * expr
  | ASTVar of expr * typee
  | ASTProc of expr * argp list * cmd list
  | ASTProcRec of expr * argp list * cmd list
  | ASTFunCMD of expr * typee * argp list * cmd list
  | ASTFunRecCMD of expr * typee * argp list * cmd list
  | ASTRet of ret

and 

ret = 
    ASTReturn of expr