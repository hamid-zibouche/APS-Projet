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
%token CONST FUN REC VAR PROC SET IFB WHILE CALL
%token IF AND OR 
%token BOOL INT VARP ADR ALLOC LEN NTH VSET VEC RETURN

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.cmd list> prog
%type <Ast.typee> type
%type <Ast.typee list> types
%type <Ast.arg> arg
%type <Ast.arg list> args
%type <Ast.argp> argp
%type <Ast.argp list> argsp
%type <Ast.cmd> def


%start prog

%%
prog: block    { $1 }
;

block: LBRA cmds RBRA    { $2 }
;

cmds:
  stat                  { [ASTStat $1] }
  | def PVIR cmds       { $1::$3 }
  | stat PVIR cmds      { ASTStat($1)::$3 }
  | ret   { [ASTRet $1] }
;

ret :
  RETURN expr { ASTReturn ($2) }

def:
  CONST IDENT type expr                        { ASTConst(ASTId($2),$3,$4) }        
  | FUN IDENT type LBRA args RBRA expr         { ASTFun(ASTId($2),$3,$5,$7) }
  | FUN REC IDENT type LBRA args RBRA expr     { ASTFunRec(ASTId($3),$4,$6,$8) }
  | VAR IDENT type                             { ASTVar(ASTId($2),$3) }
  | PROC IDENT LBRA argsp RBRA block       { ASTProc(ASTId($2),$4,$6) }
  | PROC REC IDENT LBRA argsp RBRA block   { ASTProcRec(ASTId($3),$5,$7) }
  | FUN IDENT type LBRA args RBRA LBRA cmds RBRA        { ASTFunCMD(ASTId($2),$3,$5,$8) }
  | FUN REC IDENT type LBRA args RBRA LBRA cmds RBRA     { ASTFunRecCMD(ASTId($3),$4,$6,$9) }
;

type:
  BOOL    { ASTBool }
  | INT   { ASTInt }
  | LPAR types VERS type RPAR  { ASTTypes($2,$4) }
  | LPAR VEC type RPAR { ASTVec $3 }
;

types:
  type     { [$1] }
  | type ETOI types  { $1::$3 } 
;

args:
  arg     { [$1] }
  | arg VIRG args  { $1::$3 }
;

arg:
  IDENT POIN2 type  { ASTArg($1, $3) }
;

argsp:
  argp     { [$1] }
  | argp VIRG argsp  { $1::$3 }
;

argp:
   IDENT POIN2 type  { ASTArgp($1, $3) }
  | VARP IDENT POIN2 type  { ASTVargp($2, $4) }
;

stat:
  ECHO expr                   { ASTEcho($2) }
  |SET IDENT expr             { ASTSet(ASTId($2),$3) }
  |IFB expr block block       { ASTIFB($2,$3,$4) }
  |WHILE expr block           { ASTWhile($2,$3) }
  |CALL IDENT exprsp           { ASTCall(ASTId($2),$3) }
  |SET lval expr              { ASTSetTab ($2, $3)}
;

lval:
  IDENT                      {ASTIdLval $1}
  | LPAR NTH lval expr RPAR   {ASTNthLval ($3, $4)}

exprsp :
  exprp       { [$1] }
| exprp exprsp { $1::$2 }
;

exprp :
  expr                        { ASTExpr $1 }
  | LPAR ADR IDENT RPAR               { ASTAdr ($3) }

expr:
  NUM                          { ASTNum($1) }
| IDENT                        { ASTId($1) }
| LPAR IF expr expr expr RPAR  { ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR      { ASTAnd($3, $4) }
| LPAR OR expr expr RPAR       { ASTOr($3, $4) }
| LPAR expr exprs RPAR         { ASTApp($2, $3) }
| LBRA args RBRA expr          { ASTFerm($2, $4) }
| LPAR ALLOC expr RPAR         { ASTAlloc $3 }
| LPAR LEN expr RPAR           { ASTLen $3 }
| LPAR NTH expr expr RPAR      { ASTNthExpr ($3,$4) }
| LPAR VSET expr expr expr RPAR { ASTVset ($3, $4, $5) }
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

