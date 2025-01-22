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
      
type cmd =
    ASTStat of stat
  | ASTConst of  string * typee * expr
  | ASTFun of string * typee * arg list * expr 
  | ASTFunRec of string * typee * arg list * expr




	
