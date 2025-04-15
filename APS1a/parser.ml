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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

open Ast

# 38 "parser.ml"
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
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\012\000\003\000\003\000\003\000\011\000\011\000\011\000\
\011\000\011\000\011\000\005\000\005\000\005\000\006\000\006\000\
\008\000\008\000\007\000\010\000\010\000\009\000\009\000\013\000\
\013\000\013\000\013\000\013\000\014\000\014\000\015\000\015\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\002\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\001\000\001\000\005\000\001\000\003\000\
\001\000\003\000\003\000\001\000\003\000\003\000\004\000\002\000\
\003\000\004\000\003\000\003\000\001\000\002\000\001\000\004\000\
\001\000\001\000\006\000\005\000\005\000\004\000\004\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\042\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\034\000\000\000\000\000\024\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\013\000\000\000\000\000\000\000\
\009\000\000\000\000\000\025\000\000\000\027\000\000\000\031\000\
\028\000\000\000\004\000\005\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\026\000\000\000\
\030\000\000\000\000\000\000\000\041\000\038\000\019\000\018\000\
\039\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\036\000\037\000\016\000\000\000\
\000\000\000\000\022\000\000\000\021\000\010\000\000\000\032\000\
\035\000\014\000\007\000\000\000\023\000\011\000\008\000"

let yydgoto = "\002\000\
\039\000\065\000\015\000\004\000\069\000\070\000\041\000\042\000\
\076\000\077\000\016\000\005\000\017\000\057\000\058\000"

let yysindex = "\012\000\
\015\255\000\000\077\255\000\000\000\000\078\255\022\255\006\255\
\023\255\049\255\026\255\078\255\078\255\037\255\043\255\046\255\
\051\255\000\000\000\000\045\255\061\255\000\000\035\255\035\255\
\063\255\035\255\048\255\064\255\078\255\015\255\015\255\099\255\
\000\000\077\255\077\255\078\255\078\255\078\255\078\255\062\255\
\065\255\066\255\035\255\000\000\000\000\078\255\069\255\035\255\
\000\000\002\255\072\255\000\000\015\255\000\000\013\255\000\000\
\000\000\099\255\000\000\000\000\078\255\078\255\078\255\078\255\
\081\255\035\255\061\255\078\255\071\255\074\255\000\000\061\255\
\083\255\080\255\101\255\095\255\100\255\002\255\000\000\105\255\
\000\000\078\255\104\255\107\255\000\000\000\000\000\000\000\000\
\000\000\035\255\035\255\103\255\061\255\035\255\106\255\002\255\
\015\255\108\255\109\255\112\255\000\000\000\000\000\000\113\255\
\078\255\114\255\000\000\035\255\000\000\000\000\015\255\000\000\
\000\000\000\000\000\000\078\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\115\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\116\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\255\000\000\000\000\000\000\000\000\000\000\119\255\
\000\000\000\000\000\000\000\000\117\255\000\000\000\000\000\000\
\000\000\000\000\000\000\118\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\048\000\231\255\000\000\235\255\028\000\000\000\206\255\
\000\000\179\255\000\000\237\255\000\000\061\000\000\000"

let yytablesize = 129
let yytable = "\022\000\
\098\000\046\000\047\000\074\000\049\000\030\000\031\000\024\000\
\059\000\060\000\053\000\054\000\001\000\018\000\019\000\020\000\
\088\000\021\000\109\000\003\000\025\000\092\000\052\000\023\000\
\026\000\056\000\073\000\029\000\075\000\061\000\062\000\063\000\
\064\000\079\000\036\000\037\000\038\000\043\000\032\000\071\000\
\080\000\029\000\106\000\029\000\087\000\018\000\019\000\020\000\
\033\000\021\000\027\000\056\000\050\000\034\000\082\000\083\000\
\084\000\064\000\035\000\044\000\045\000\089\000\040\000\028\000\
\048\000\051\000\036\000\037\000\038\000\104\000\066\000\068\000\
\107\000\072\000\067\000\100\000\078\000\110\000\018\000\019\000\
\020\000\090\000\021\000\006\000\086\000\091\000\117\000\093\000\
\094\000\007\000\008\000\118\000\009\000\010\000\011\000\012\000\
\013\000\014\000\115\000\018\000\019\000\055\000\095\000\021\000\
\096\000\097\000\099\000\101\000\105\000\119\000\102\000\085\000\
\112\000\111\000\108\000\113\000\114\000\103\000\081\000\116\000\
\003\000\017\000\040\000\020\000\000\000\000\000\000\000\000\000\
\015\000"

let yycheck = "\006\000\
\078\000\023\000\024\000\002\001\026\000\012\000\013\000\002\001\
\034\000\035\000\030\000\031\000\001\000\001\001\002\001\003\001\
\067\000\005\001\096\000\005\001\015\001\072\000\029\000\002\001\
\002\001\032\000\048\000\002\001\027\001\036\000\037\000\038\000\
\039\000\053\000\022\001\023\001\024\001\003\001\002\001\046\000\
\028\001\006\001\093\000\008\001\066\000\001\001\002\001\003\001\
\006\001\005\001\002\001\058\000\005\001\008\001\061\000\062\000\
\063\000\064\000\008\001\025\001\026\001\068\000\002\001\015\001\
\002\001\002\001\022\001\023\001\024\001\091\000\009\001\006\001\
\094\000\005\001\010\001\082\000\005\001\097\000\001\001\002\001\
\003\001\011\001\005\001\007\001\004\001\012\001\108\000\005\001\
\009\001\013\001\014\001\111\000\016\001\017\001\018\001\019\001\
\020\001\021\001\105\000\001\001\002\001\003\001\002\001\005\001\
\010\001\006\001\002\001\004\001\006\001\116\000\004\001\064\000\
\004\001\006\001\009\001\004\001\004\001\090\000\058\000\006\001\
\006\001\006\001\004\001\006\001\255\255\255\255\255\255\255\255\
\012\001"

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
# 33 "parser.mly"
               ( _1 )
# 228 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 36 "parser.mly"
                         ( _2 )
# 235 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 40 "parser.mly"
                        ( [ASTStat _1] )
# 242 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 41 "parser.mly"
                        ( _1::_3 )
# 250 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 42 "parser.mly"
                        ( ASTStat(_1)::_3 )
# 258 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typee) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 46 "parser.mly"
                                               ( ASTConst(ASTId(_2),_3,_4) )
# 267 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typee) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 47 "parser.mly"
                                               ( ASTFun(ASTId(_2),_3,_5,_7) )
# 277 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typee) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
                                               ( ASTFunRec(ASTId(_3),_4,_6,_8) )
# 287 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee) in
    Obj.repr(
# 49 "parser.mly"
                                               ( ASTVar(ASTId(_2),_3) )
# 295 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.argp list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 50 "parser.mly"
                                           ( ASTProc(ASTId(_2),_4,_6) )
# 304 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.argp list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 51 "parser.mly"
                                           ( ASTProcRec(ASTId(_3),_5,_7) )
# 313 "parser.ml"
               : Ast.cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
          ( ASTBool )
# 319 "parser.ml"
               : Ast.typee))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
          ( ASTInt )
# 325 "parser.ml"
               : Ast.typee))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.typee list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typee) in
    Obj.repr(
# 57 "parser.mly"
                               ( ASTTypes(_2,_4) )
# 333 "parser.ml"
               : Ast.typee))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee) in
    Obj.repr(
# 61 "parser.mly"
           ( [_1] )
# 340 "parser.ml"
               : Ast.typee list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typee) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee list) in
    Obj.repr(
# 62 "parser.mly"
                     ( _1::_3 )
# 348 "parser.ml"
               : Ast.typee list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 66 "parser.mly"
          ( [_1] )
# 355 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 67 "parser.mly"
                   ( _1::_3 )
# 363 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee) in
    Obj.repr(
# 71 "parser.mly"
                    ( ASTArg(_1, _3) )
# 371 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.argp) in
    Obj.repr(
# 75 "parser.mly"
           ( [_1] )
# 378 "parser.ml"
               : Ast.argp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.argp list) in
    Obj.repr(
# 76 "parser.mly"
                     ( _1::_3 )
# 386 "parser.ml"
               : Ast.argp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee) in
    Obj.repr(
# 80 "parser.mly"
                     ( ASTArgp(_1, _3) )
# 394 "parser.ml"
               : Ast.argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.typee) in
    Obj.repr(
# 81 "parser.mly"
                           ( ASTVargp(_2, _4) )
# 402 "parser.ml"
               : Ast.argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 85 "parser.mly"
                              ( ASTEcho(_2) )
# 409 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 86 "parser.mly"
                              ( ASTSet(ASTId(_2),_3) )
# 417 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 87 "parser.mly"
                              ( ASTIFB(_2,_3,_4) )
# 426 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 88 "parser.mly"
                              ( ASTWhile(_2,_3) )
# 434 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 89 "parser.mly"
                               ( ASTCall(ASTId(_2),_3) )
# 442 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprp) in
    Obj.repr(
# 93 "parser.mly"
              ( [_1] )
# 449 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 94 "parser.mly"
               ( _1::_2 )
# 457 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 98 "parser.mly"
                              ( ASTExpr _1 )
# 464 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 99 "parser.mly"
                                      ( ASTAdr (_3) )
# 471 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 102 "parser.mly"
                               ( ASTNum(_1) )
# 478 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 103 "parser.mly"
                               ( ASTId(_1) )
# 485 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 104 "parser.mly"
                               ( ASTIf(_3, _4, _5) )
# 494 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 105 "parser.mly"
                               ( ASTAnd(_3, _4) )
# 502 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 106 "parser.mly"
                               ( ASTOr(_3, _4) )
# 510 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 107 "parser.mly"
                               ( ASTApp(_2, _3) )
# 518 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 108 "parser.mly"
                               ( ASTFerm(_2, _4) )
# 526 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 112 "parser.mly"
             ( [_1] )
# 533 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 113 "parser.mly"
             ( _1::_2 )
# 541 "parser.ml"
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
