(* Types de base et types composés *)
type typee =
    ASTBool                   (* booléen *)
    | ASTInt                  (* entier *)
    | ASTTypes of typee list * typee  (* type fonction (args → retour) *)
    | ASTVec of typee         (* tableau *)

(* Arguments de fonctions/procédures *)
type arg = ASTArg of string * typee  
type argp =  
    ASTArgp of string * typee
    | ASTVargp of string * typee 

(* Expressions du langage *)
type expr =
    ASTNum of int             (* entier *)
  | ASTId of string           (* variable *)
  | ASTIf of expr * expr * expr  (* if-then-else *)
  | ASTAnd of expr * expr     (* ET logique *)
  | ASTOr of expr * expr      (* OU logique *)
  | ASTApp of expr * expr list  (* appel de fonction *)
  | ASTFerm of arg list * expr  (* fonction anonyme *)
  | ASTAlloc of expr          (* allocation tableau *)
  | ASTLen of expr            (* taille tableau *)
  | ASTNthExpr of expr * expr (* accès tableau *)
  | ASTVset of expr * expr * expr (* modification tableau *)

(* Emplacements modifiables (l-values) *)
type lval =
    ASTIdLval of string       (* variable *)
    | ASTNthLval of lval * expr (* élément de tableau *)

(* Paramètres pouvant être passés *)
type exprp =
    | ASTExpr of expr         (* expression standard *)
    | ASTAdr of string       (* passage par adresse *)

(* Instructions *)
type stat =
    ASTEcho of expr           (* affichage *)
    | ASTSet of expr * expr   (* affectation *)
    | ASTIFB of expr * cmd list * cmd list  (* if-then-else *)
    | ASTWhile of expr * cmd list (* boucle while *)
    | ASTCall of expr * exprp list (* appel de procédure *)
    | ASTSetTab of lval * expr (* affectation dans un tableau *)

(* Commandes (déclarations ou instructions) *)
and cmd =
    ASTStat of stat           (* instruction simple *)
  | ASTConst of expr * typee * expr (* constante *)
  | ASTFun of expr * typee * arg list * expr (* fonction *)
  | ASTFunRec of expr * typee * arg list * expr (* fonction récursive *)
  | ASTVar of expr * typee    (* variable *)
  | ASTProc of expr * argp list * cmd list (* procédure *)
  | ASTProcRec of expr * argp list * cmd list (* procédure récursive *)
  | ASTFunCMD of expr * typee * arg list * cmd list (* fonction avec blocs *)
  | ASTFunRecCMD of expr * typee * arg list * cmd list (* fonction réc. avec blocs *)
  | ASTRet of ret             (* retour *)

(* Retour de fonction *)
and ret = ASTReturn of expr