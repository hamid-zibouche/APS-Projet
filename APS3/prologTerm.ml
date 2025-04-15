open Ast

(* Conversion des types vers leur représentation textuelle *)
let rec print_typee c =
  match c with
    ASTBool  -> "bool_t"
    |ASTInt  -> "int_t"
    | ASTTypes (args, ret) ->  (* Type fonction avec arguments et type de retour *)
      let args_str = "[" ^ String.concat ", " (List.map print_typee args) ^ "]" in
      "fun_t(" ^ args_str ^ ", " ^ print_typee ret ^ ")"
    | ASTVec t -> "vec_t(" ^ print_typee t ^ ")"  (* Type tableau *)

(* Affichage d'un argument de fonction *)
let print_arg (ASTArg (name, typee)) =
  "(id(\"" ^ name ^ "\"), " ^ print_typee typee ^ ")"

(* Affichage d'une liste d'arguments *)
let print_args args ="[" ^
  (String.concat ", " (List.map print_arg args)) ^ "]"

(* Affichage d'un paramètre de procédure (normal ou variable) *)
let print_argp a = 
  match a with 
   (ASTArgp (name, typee)) -> "(id(\"" ^ name ^ "\"), " ^ print_typee typee ^ ")"
  | (ASTVargp (name, typee)) -> "(varp(\"" ^ name ^ "\"), " ^ print_typee typee ^ ")"
  
let print_argsp args ="[" ^
  (String.concat ", " (List.map print_argp args)) ^ "]"

(* Affichage des expressions du langage *)
let rec print_expr e =
  match e with
      ASTNum n -> Printf.printf"num(%d)" n
    | ASTId x -> Printf.printf"id(\"%s\")" x
    | ASTIf (e1,e2,e3) -> Printf.printf "if("; print_expr e1; Printf.printf"," ; print_expr e2; Printf.printf","; print_expr e3; Printf.printf")"
    | ASTAnd (e1,e2) -> Printf.printf"and("; print_expr e1; Printf.printf","; print_expr e2; Printf.printf")"
    | ASTOr (e1,e2) -> Printf.printf"or("; print_expr e1; Printf.printf","; print_expr e2; Printf.printf")"
    | ASTApp(e, es) -> Printf.printf"app("; print_expr e; Printf.printf",["; print_exprs es; Printf.printf"])"
    | ASTFerm(al,e) -> Printf.printf("ferm(");
            Printf.printf("%s") (print_args al);
            Printf.printf(",");
            print_expr e ; 
            Printf.printf(")")
    | ASTAlloc(e) -> Printf.printf "alloc("; print_expr e ; Printf.printf ")"  (* Allocation tableau *)
    | ASTLen (e) -> Printf.printf "len("; print_expr e ; Printf.printf ")"    (* Longueur tableau *)
    | ASTNthExpr (e1, e2) -> Printf.printf "nth("; print_expr e1; Printf.printf ","; print_expr e2 ; Printf.printf ")"  (* Accès tableau *)
    | ASTVset (e1, e2, e3) -> Printf.printf "vset("; print_expr e1; Printf.printf ","; print_expr e2; Printf.printf ","; print_expr e3 ; Printf.printf ")"  (* Modification tableau *)

and print_exprs es =
  match es with
      [] -> ()
    | [e] -> print_expr e
    | e::es -> (
  print_expr e;
  print_char ',';
  print_exprs es
      )

(* Affichage des l-values (emplacements modifiables) *)
let rec print_lval v = 
  match v with
  | ASTIdLval x -> Printf.printf"id(\"%s\")" x
  | ASTNthLval (value,s) -> Printf.printf("nth("); print_lval(value); Printf.printf(","); print_expr(s); Printf.printf(")") 

(* Affichage des paramètres d'appel *)
let rec print_exprp e =
  match e with
  | ASTExpr exp -> print_expr exp
  | ASTAdr s -> Printf.printf "adr(\"%s\")" s  (* Passage par adresse *)
      
and print_exprsp es =
    match es with
        [] -> ()
      | [e] -> print_exprp e
      | e::es -> (
    print_exprp e;
    print_char ',';
    print_exprsp es
        )

let print_ret (ASTReturn e) = (Printf.printf("ret("); print_expr(e); Printf.printf(")"))

(* Affichage des instructions *)
let rec print_stat s =
  match s with
    ASTEcho e -> (Printf.printf("echo("); print_expr(e); Printf.printf(")"))
    |ASTSet (ident,v) -> (Printf.printf("set("); print_expr(ident); Printf.printf(","); print_expr(v); Printf.printf(")"))
    |ASTIFB (e,b1,b2) -> Printf.printf "ifb("; print_expr e; Printf.printf"," ; print_cmds b1; Printf.printf","; print_cmds b2; Printf.printf ")"
    |ASTWhile (e,b) -> Printf.printf "while("; print_expr e; Printf.printf"," ; print_cmds b ; Printf.printf ")"
    |ASTCall (ident,args) -> Printf.printf "call("; print_expr ident; Printf.printf",[" ; print_exprsp args ; Printf.printf "])"
    |ASTSetTab (value,e) -> Printf.printf "setTab("; print_lval (value); Printf.printf ","; print_expr(e); Printf.printf")"  (* Affectation tableau *)

and

(* Affichage des déclarations et commandes *)
 print_cmd c =
  match c with
      ASTStat s -> print_stat s
    | ASTConst (ident,t,e) -> Printf.printf("const(");
                          print_expr(ident);
                          Printf.printf(",%s") (print_typee t); 
                          Printf.printf(",");
                          print_expr e ; 
                          Printf.printf(")")
    | ASTFun (ident,t,al,e) -> Printf.printf("fun(");
                          print_expr(ident);
                          Printf.printf(",%s") (print_typee t); 
                          Printf.printf(",%s") (print_args al);
                          Printf.printf(",");
                          print_expr e ; 
                          Printf.printf(")")
    | ASTFunRec (ident,t,al,e) -> Printf.printf("funRec(");
                          print_expr(ident);
                          Printf.printf(",%s") (print_typee t); 
                          Printf.printf(",%s") (print_args al);
                          Printf.printf(",");
                          print_expr e ; 
                          Printf.printf(")")
    | ASTVar (ident,t) -> Printf.printf("var(");
                          print_expr(ident);
                          Printf.printf(",%s") (print_typee t); 
                          Printf.printf(")")
    | ASTProc (ident,args,cmds) -> Printf.printf("proc(");
                          print_expr(ident);
                          Printf.printf(",%s") (print_argsp args);
                          Printf.printf(",");
                          print_cmds cmds ; 
                          Printf.printf(")")
    | ASTProcRec (ident,args,cmds) -> Printf.printf("procRec(");
                          print_expr(ident);
                          Printf.printf(",%s") (print_argsp args);
                          Printf.printf(",");
                          print_cmds cmds ; 
                          Printf.printf(")");
    | ASTFunCMD (ident,t,args,cmds) ->  Printf.printf("funCmd(");
                          print_expr(ident);
                          Printf.printf(",%s") (print_typee t); 
                          Printf.printf(",%s") (print_args args);
                          Printf.printf(",");
                          print_cmds cmds ; 
                          Printf.printf(")");
    | ASTFunRecCMD (ident,t,args,cmds) ->  Printf.printf("funRecCmd(");
                          print_expr(ident);
                          Printf.printf(",%s") (print_typee t); 
                          Printf.printf(",%s") (print_args args);
                          Printf.printf(",");
                          print_cmds cmds ; 
                          Printf.printf(")");
    | ASTRet r -> print_ret r
    
and

(* Affichage d'un bloc de commandes *)
 print_cmds cs =
  Printf.printf "block([";
  (match cs with
  | [] -> ()
  | _ -> 
      let rec aux = function
        | [c] -> print_cmd c  
        | c :: cs -> 
            print_cmd c;
            Printf.printf ",";
            aux cs
        | _ -> failwith "not yet implemented"
      in aux cs);
  Printf.printf "])"
	
(* Point d'entrée principal - affichage du programme complet *)
let print_prog p =
  Printf.printf("prog(");
  print_cmds p;
  Printf.printf(")")
;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_prog p;
      print_string ".\n"
  with Lexer.Eof ->
    exit 0