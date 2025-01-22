%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR 
%token LBRA RBRA
%token ECHO
%token PVIR POIN2 VIRG ETOI VERS
%token CONST FUN REC 
%token IF AND OR 
%token BOOL INT

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.cmd list> prog
%type <Ast.typee> type
%type <Ast.typee list> types
%type <Ast.arg> arg
%type <Ast.arg list> args
%type <Ast.def> def


%start prog

%%
prog: LBRA cmds RBRA    { $2 }
;

cmds:
  stat                  { [ASTStat $1] }
  | def PVIR cmds       { (ASTStat $1)::$3 }
;

def:
  CONST IDENT type expr                   { ASTConst($2,$3,$4) }        
  | FUN IDENT type LBRA args RBRA expr    { ASTFun($2,$3,$5,$7) }
  | FUN REC IDENT type LBRA args RBRA expr { ASTFunRec($3,$4,$6,$8) }
;

type:
  BOOL    { [ASTBool] }
  | INT   { [ASTInt] }
  | LPAR types VERS type RPAR  { ASTTypes($2,$4) }
;

types:
  type     { [$1] }
  | type ETOI types  { $1::$3 } 
;

args:
  arg     { [$1] }
  | arg VIRG args  { $1::$2 }
;

arg:
  IDENT POIN2 type  { ASTArg($1, $3) }
;

stat:
  ECHO expr             { ASTEcho($2) }
;

expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| LPAR IF expr expr expr RPAR  { ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR      { ASTAnd($3, $4) }
| LPAR OR expr expr RPAR       { ASTOr($3, $4) }
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
| LBRA args RBRA expr   { ASTFerm($2, $4) }
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

