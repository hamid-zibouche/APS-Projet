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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
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

# 39 "parser.ml"
let yytransl_const = [|
  259 (* LPAR *);
  260 (* RPAR *);
  261 (* LBRA *);
  262 (* RBRA *);
  263 (* ECHO *);
  264 (* PVIR *);
  265 (* POIN2 *);
  266 (* VIRG *);
  267 (* ETOI *);
  268 (* VERS *);
  269 (* CONST *);
  270 (* FUN *);
  271 (* REC *);
  272 (* IF *);
  273 (* AND *);
  274 (* OR *);
  275 (* BOOL *);
  276 (* INT *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\003\000\003\000\009\000\009\000\009\000\005\000\005\000\
\005\000\006\000\006\000\008\000\008\000\007\000\010\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000\
\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\004\000\007\000\008\000\001\000\001\000\
\005\000\001\000\003\000\001\000\003\000\003\000\002\000\001\000\
\001\000\006\000\005\000\005\000\004\000\004\000\001\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\025\000\000\000\000\000\000\000\000\000\
\000\000\002\000\016\000\017\000\000\000\000\000\015\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\008\000\000\000\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\
\000\000\000\000\024\000\021\000\014\000\013\000\022\000\000\000\
\000\000\000\000\000\000\000\000\019\000\020\000\011\000\000\000\
\000\000\000\000\018\000\009\000\005\000\000\000\006\000"

let yydgoto = "\002\000\
\038\000\039\000\008\000\004\000\043\000\044\000\026\000\027\000\
\009\000\010\000"

let yysindex = "\006\000\
\007\255\000\000\026\255\000\000\008\255\012\255\000\255\017\255\
\016\255\000\000\000\000\000\000\033\255\023\255\000\000\002\255\
\002\255\027\255\000\000\026\255\008\255\008\255\008\255\008\255\
\019\255\031\255\036\255\002\255\000\000\000\000\008\255\040\255\
\002\255\000\000\008\255\008\255\008\255\008\255\042\255\002\255\
\023\255\008\255\037\255\035\255\000\000\023\255\047\255\008\255\
\049\255\050\255\000\000\000\000\000\000\000\000\000\000\002\255\
\002\255\051\255\023\255\052\255\000\000\000\000\000\000\054\255\
\008\255\053\255\000\000\000\000\000\000\008\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\055\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\058\255\000\000\000\000\
\000\000\000\000\043\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\251\255\025\000\044\000\000\000\243\255\010\000\000\000\216\255\
\000\000\000\000"

let yytablesize = 66
let yytable = "\015\000\
\054\000\017\000\031\000\032\000\028\000\058\000\001\000\024\000\
\011\000\012\000\013\000\003\000\014\000\016\000\018\000\035\000\
\036\000\037\000\066\000\047\000\029\000\030\000\019\000\020\000\
\025\000\045\000\053\000\040\000\033\000\048\000\049\000\050\000\
\005\000\011\000\012\000\013\000\055\000\014\000\006\000\007\000\
\041\000\042\000\060\000\064\000\046\000\052\000\057\000\056\000\
\021\000\022\000\023\000\059\000\061\000\062\000\010\000\067\000\
\065\000\068\000\070\000\069\000\012\000\023\000\051\000\034\000\
\071\000\063\000"

let yycheck = "\005\000\
\041\000\002\001\016\000\017\000\003\001\046\000\001\000\013\000\
\001\001\002\001\003\001\005\001\005\001\002\001\015\001\021\000\
\022\000\023\000\059\000\033\000\019\001\020\001\006\001\008\001\
\002\001\031\000\040\000\009\001\002\001\035\000\036\000\037\000\
\007\001\001\001\002\001\003\001\042\000\005\001\013\001\014\001\
\010\001\006\001\048\000\057\000\005\001\004\001\012\001\011\001\
\016\001\017\001\018\001\005\001\004\001\004\001\012\001\004\001\
\006\001\004\001\006\001\065\000\006\001\004\001\038\000\020\000\
\070\000\056\000"

let yynames_const = "\
  LPAR\000\
  RPAR\000\
  LBRA\000\
  RBRA\000\
  ECHO\000\
  PVIR\000\
  POIN2\000\
  VIRG\000\
  ETOI\000\
  VERS\000\
  CONST\000\
  FUN\000\
  REC\000\
  IF\000\
  AND\000\
  OR\000\
  BOOL\000\
  INT\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 40 "parser.mly"
                        ( _2 )
# 175 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 44 "parser.mly"
                        ( [ASTStat _1] )
# 182 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 45 "parser.mly"
                        ( _1::_3 )
# 190 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typee) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                                          ( ASTConst(_2,_3,_4) )
# 199 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typee) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                                          ( ASTFun(_2,_3,_5,_7) )
# 209 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typee) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                                           ( ASTFunRec(_3,_4,_6,_8) )
# 219 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
          ( ASTBool )
# 225 "parser.ml"
               : Ast.typee))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
          ( ASTInt )
# 231 "parser.ml"
               : Ast.typee))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.typee list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typee) in
    Obj.repr(
# 57 "parser.mly"
                               ( ASTTypes(_2,_4) )
# 239 "parser.ml"
               : Ast.typee))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee) in
    Obj.repr(
# 61 "parser.mly"
           ( [_1] )
# 246 "parser.ml"
               : Ast.typee list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typee) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee list) in
    Obj.repr(
# 62 "parser.mly"
                     ( _1::_3 )
# 254 "parser.ml"
               : Ast.typee list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 66 "parser.mly"
          ( [_1] )
# 261 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 67 "parser.mly"
                   ( _1::_3 )
# 269 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee) in
    Obj.repr(
# 71 "parser.mly"
                    ( ASTArg(_1, _3) )
# 277 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                        ( ASTEcho(_2) )
# 284 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 79 "parser.mly"
                        ( ASTNum(_1) )
# 291 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
                        ( ASTId(_1) )
# 298 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 81 "parser.mly"
                               ( ASTIf(_3, _4, _5) )
# 307 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 82 "parser.mly"
                               ( ASTAnd(_3, _4) )
# 315 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 83 "parser.mly"
                               ( ASTOr(_3, _4) )
# 323 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 84 "parser.mly"
                        ( ASTApp(_2, _3) )
# 331 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 85 "parser.mly"
                        ( ASTFerm(_2, _4) )
# 339 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 89 "parser.mly"
             ( [_1] )
# 346 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 90 "parser.mly"
             ( _1::_2 )
# 354 "parser.ml"
               : Ast.expr list))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.cmd list)
