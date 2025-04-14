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
  | ALLOC
  | LEN
  | NTH
  | VSET
  | VEC

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

# 52 "parser.ml"
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
  272 (* VAR *);
  273 (* PROC *);
  274 (* SET *);
  275 (* IFB *);
  276 (* WHILE *);
  277 (* CALL *);
  278 (* IF *);
  279 (* AND *);
  280 (* OR *);
  281 (* BOOL *);
  282 (* INT *);
  283 (* VARP *);
  284 (* ADR *);
  285 (* ALLOC *);
  286 (* LEN *);
  287 (* NTH *);
  288 (* VSET *);
  289 (* VEC *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\012\000\003\000\003\000\003\000\011\000\011\000\011\000\
\011\000\011\000\011\000\005\000\005\000\005\000\005\000\006\000\
\006\000\008\000\008\000\007\000\010\000\010\000\009\000\009\000\
\013\000\013\000\013\000\013\000\013\000\013\000\015\000\015\000\
\014\000\014\000\016\000\016\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\002\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\001\000\001\000\005\000\004\000\001\000\
\003\000\001\000\003\000\003\000\001\000\003\000\003\000\004\000\
\002\000\003\000\004\000\003\000\003\000\003\000\001\000\005\000\
\001\000\002\000\001\000\004\000\001\000\001\000\006\000\005\000\
\005\000\004\000\004\000\004\000\004\000\005\000\006\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\050\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\037\000\038\000\000\000\000\000\025\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\013\000\000\000\000\000\000\000\009\000\000\000\
\000\000\026\000\000\000\030\000\000\000\028\000\000\000\035\000\
\029\000\000\000\004\000\005\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\031\000\000\000\027\000\000\000\034\000\
\000\000\000\000\000\000\044\000\045\000\000\000\000\000\049\000\
\042\000\020\000\019\000\043\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\040\000\041\000\046\000\000\000\015\000\017\000\000\000\
\000\000\000\000\023\000\000\000\022\000\010\000\000\000\032\000\
\036\000\039\000\047\000\014\000\007\000\000\000\024\000\011\000\
\008\000"

let yydgoto = "\002\000\
\045\000\077\000\015\000\004\000\082\000\083\000\047\000\048\000\
\089\000\090\000\016\000\005\000\017\000\065\000\031\000\066\000"

let yysindex = "\004\000\
\008\255\000\000\064\255\000\000\000\000\054\255\007\255\001\255\
\015\255\009\255\024\255\054\255\054\255\018\255\035\255\050\255\
\053\255\000\000\000\000\123\255\041\255\000\000\028\255\028\255\
\060\255\028\255\061\255\065\255\054\255\044\255\054\255\008\255\
\008\255\087\255\000\000\064\255\064\255\054\255\054\255\054\255\
\054\255\054\255\054\255\054\255\054\255\063\255\069\255\088\255\
\019\255\000\000\000\000\054\255\090\255\028\255\000\000\002\255\
\091\255\000\000\045\255\000\000\008\255\000\000\109\255\000\000\
\000\000\087\255\000\000\000\000\054\255\054\255\054\255\089\255\
\094\255\054\255\054\255\054\255\095\255\028\255\041\255\054\255\
\028\255\092\255\093\255\000\000\041\255\096\255\098\255\100\255\
\103\255\102\255\002\255\000\000\054\255\000\000\113\255\000\000\
\054\255\112\255\114\255\000\000\000\000\115\255\054\255\000\000\
\000\000\000\000\000\000\000\000\116\255\028\255\028\255\111\255\
\041\255\028\255\120\255\002\255\008\255\124\255\117\255\118\255\
\130\255\000\000\000\000\000\000\131\255\000\000\000\000\138\255\
\054\255\137\255\000\000\028\255\000\000\000\000\008\255\000\000\
\000\000\000\000\000\000\000\000\000\000\054\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\142\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\143\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\140\255\000\000\000\000\000\000\000\000\
\000\000\139\255\000\000\000\000\000\000\000\000\000\000\000\000\
\144\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\250\255\024\000\014\000\000\000\251\255\046\000\000\000\185\255\
\000\000\180\255\000\000\225\255\000\000\091\000\099\000\000\000"

let yytablesize = 158
let yytable = "\022\000\
\061\000\062\000\024\000\087\000\001\000\032\000\033\000\107\000\
\023\000\033\000\027\000\033\000\003\000\112\000\118\000\025\000\
\026\000\052\000\053\000\034\000\055\000\049\000\058\000\028\000\
\060\000\029\000\030\000\064\000\088\000\094\000\049\000\069\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\133\000\
\035\000\130\000\046\000\050\000\051\000\084\000\092\000\030\000\
\086\000\067\000\068\000\081\000\050\000\051\000\018\000\019\000\
\020\000\036\000\021\000\064\000\037\000\054\000\097\000\098\000\
\099\000\056\000\057\000\102\000\103\000\076\000\006\000\078\000\
\106\000\108\000\059\000\109\000\007\000\008\000\079\000\009\000\
\010\000\011\000\012\000\013\000\014\000\134\000\119\000\018\000\
\019\000\063\000\121\000\021\000\100\000\080\000\085\000\091\000\
\125\000\101\000\105\000\104\000\113\000\115\000\110\000\144\000\
\111\000\128\000\114\000\117\000\131\000\018\000\019\000\020\000\
\116\000\021\000\120\000\122\000\129\000\123\000\124\000\126\000\
\136\000\137\000\141\000\018\000\019\000\020\000\143\000\021\000\
\132\000\135\000\038\000\039\000\040\000\138\000\139\000\145\000\
\095\000\041\000\042\000\043\000\044\000\140\000\142\000\048\000\
\038\000\039\000\040\000\003\000\018\000\021\000\016\000\041\000\
\042\000\043\000\044\000\127\000\096\000\093\000"

let yycheck = "\006\000\
\032\000\033\000\002\001\002\001\001\000\012\000\013\000\079\000\
\002\001\006\001\002\001\008\001\005\001\085\000\091\000\015\001\
\002\001\023\000\024\000\002\001\026\000\003\001\029\000\015\001\
\031\000\002\001\003\001\034\000\027\001\061\000\003\001\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\116\000\
\006\001\113\000\002\001\025\001\026\001\052\000\002\001\003\001\
\054\000\036\000\037\000\033\001\025\001\026\001\001\001\002\001\
\003\001\008\001\005\001\066\000\008\001\002\001\069\000\070\000\
\071\000\005\001\002\001\074\000\075\000\076\000\007\001\009\001\
\078\000\080\000\031\001\081\000\013\001\014\001\010\001\016\001\
\017\001\018\001\019\001\020\001\021\001\117\000\093\000\001\001\
\002\001\003\001\097\000\005\001\004\001\006\001\005\001\005\001\
\103\000\004\001\004\001\076\000\005\001\002\001\011\001\135\000\
\012\001\111\000\009\001\006\001\114\000\001\001\002\001\003\001\
\010\001\005\001\002\001\004\001\006\001\004\001\004\001\004\001\
\004\001\004\001\129\000\001\001\002\001\003\001\132\000\005\001\
\009\001\006\001\022\001\023\001\024\001\004\001\004\001\142\000\
\028\001\029\001\030\001\031\001\032\001\004\001\006\001\004\001\
\022\001\023\001\024\001\006\001\006\001\006\001\012\001\029\001\
\030\001\031\001\032\001\110\000\066\000\059\000"

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
  VAR\000\
  PROC\000\
  SET\000\
  IFB\000\
  WHILE\000\
  CALL\000\
  IF\000\
  AND\000\
  OR\000\
  BOOL\000\
  INT\000\
  VARP\000\
  ADR\000\
  ALLOC\000\
  LEN\000\
  NTH\000\
  VSET\000\
  VEC\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 42 "parser.mly"
               ( _1 )
# 272 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 45 "parser.mly"
                         ( _2 )
# 279 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 49 "parser.mly"
                        ( [ASTStat _1] )
# 286 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 50 "parser.mly"
                        ( _1::_3 )
# 294 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 51 "parser.mly"
                        ( ASTStat(_1)::_3 )
# 302 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typee) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                                               ( ASTConst(ASTId(_2),_3,_4) )
# 311 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typee) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                                               ( ASTFun(ASTId(_2),_3,_5,_7) )
# 321 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typee) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                                               ( ASTFunRec(ASTId(_3),_4,_6,_8) )
# 331 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee) in
    Obj.repr(
# 58 "parser.mly"
                                               ( ASTVar(ASTId(_2),_3) )
# 339 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.argp list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 59 "parser.mly"
                                           ( ASTProc(ASTId(_2),_4,_6) )
# 348 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.argp list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 60 "parser.mly"
                                           ( ASTProcRec(ASTId(_3),_5,_7) )
# 357 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
          ( ASTBool )
# 363 "parser.ml"
               : Ast.typee))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
          ( ASTInt )
# 369 "parser.ml"
               : Ast.typee))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.typee list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typee) in
    Obj.repr(
# 66 "parser.mly"
                               ( ASTTypes(_2,_4) )
# 377 "parser.ml"
               : Ast.typee))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typee) in
    Obj.repr(
# 67 "parser.mly"
                       ( ASTVec _3 )
# 384 "parser.ml"
               : Ast.typee))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee) in
    Obj.repr(
# 71 "parser.mly"
           ( [_1] )
# 391 "parser.ml"
               : Ast.typee list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typee) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee list) in
    Obj.repr(
# 72 "parser.mly"
                     ( _1::_3 )
# 399 "parser.ml"
               : Ast.typee list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 76 "parser.mly"
          ( [_1] )
# 406 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 77 "parser.mly"
                   ( _1::_3 )
# 414 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee) in
    Obj.repr(
# 81 "parser.mly"
                    ( ASTArg(_1, _3) )
# 422 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.argp) in
    Obj.repr(
# 85 "parser.mly"
           ( [_1] )
# 429 "parser.ml"
               : Ast.argp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.argp list) in
    Obj.repr(
# 86 "parser.mly"
                     ( _1::_3 )
# 437 "parser.ml"
               : Ast.argp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee) in
    Obj.repr(
# 90 "parser.mly"
                     ( ASTArgp(_1, _3) )
# 445 "parser.ml"
               : Ast.argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee) in
    Obj.repr(
# 91 "parser.mly"
                           ( ASTVargp(_2, _4) )
# 453 "parser.ml"
               : Ast.argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 95 "parser.mly"
                              ( ASTEcho(_2) )
# 460 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 96 "parser.mly"
                              ( ASTSet(ASTId(_2),_3) )
# 468 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 97 "parser.mly"
                              ( ASTIFB(_2,_3,_4) )
# 477 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 98 "parser.mly"
                              ( ASTWhile(_2,_3) )
# 485 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 99 "parser.mly"
                               ( ASTCall(ASTId(_2),_3) )
# 493 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 100 "parser.mly"
                              ( ASTSetTab (_2, _3))
# 501 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "parser.mly"
                             (ASTIdLval _1)
# 508 "parser.ml"
               : 'lval))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'lval) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 105 "parser.mly"
                              (ASTNthLval (_3, _4))
# 516 "parser.ml"
               : 'lval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprp) in
    Obj.repr(
# 108 "parser.mly"
              ( [_1] )
# 523 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 109 "parser.mly"
               ( _1::_2 )
# 531 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 113 "parser.mly"
                              ( ASTExpr _1 )
# 538 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 114 "parser.mly"
                                      ( ASTAdr (_3) )
# 545 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 117 "parser.mly"
                               ( ASTNum(_1) )
# 552 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 118 "parser.mly"
                               ( ASTId(_1) )
# 559 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 119 "parser.mly"
                               ( ASTIf(_3, _4, _5) )
# 568 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 120 "parser.mly"
                               ( ASTAnd(_3, _4) )
# 576 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 121 "parser.mly"
                               ( ASTOr(_3, _4) )
# 584 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 122 "parser.mly"
                               ( ASTApp(_2, _3) )
# 592 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 123 "parser.mly"
                               ( ASTFerm(_2, _4) )
# 600 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 124 "parser.mly"
                               ( ASTAlloc _3 )
# 607 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 125 "parser.mly"
                               ( ASTLen _3 )
# 614 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 126 "parser.mly"
                               ( ASTNthExpr (_3,_4) )
# 622 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 127 "parser.mly"
                                ( ASTVset (_3, _4, _5) )
# 631 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 131 "parser.mly"
             ( [_1] )
# 638 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 132 "parser.mly"
             ( _1::_2 )
# 646 "parser.ml"
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
