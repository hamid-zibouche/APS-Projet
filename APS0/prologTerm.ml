(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast

let rec print_typee c =
  match c with
    ASTBool  -> "bool_t"
    |ASTInt  -> "int_t"
    | ASTTypes (args, ret) ->
      let args_str = "[" ^ String.concat ", " (List.map print_typee args) ^ "]" in
      "(" ^ args_str ^ ", " ^ print_typee ret ^ ")"
 
let print_arg (ASTArg (name, typee)) =
  "(id(" ^ name ^ "), " ^ print_typee typee ^ ")"

let print_args args ="[" ^
  (String.concat ", " (List.map print_arg args)) ^ "]"


let rec print_expr e =
  match e with
      ASTNum n -> Printf.printf"num(%d)" n
    | ASTId x -> Printf.printf"id(%s)" x
    | ASTIf (e1,e2,e3) -> Printf.printf "if("; print_expr e1; Printf.printf"," ; print_expr e2; Printf.printf","; print_expr e3; Printf.printf")"
    | ASTAnd (e1,e2) -> Printf.printf"and("; print_expr e1; Printf.printf","; print_expr e2; Printf.printf")"
    | ASTOr (e1,e2) -> Printf.printf"or("; print_expr e1; Printf.printf","; print_expr e2; Printf.printf")"
    | ASTApp(e, es) -> Printf.printf"app("; print_expr e; Printf.printf",["; print_exprs es; Printf.printf"])"
    | ASTFerm(al,e) -> Printf.printf("ferm(");
            Printf.printf("%s") (print_args al);
            Printf.printf(",");
            print_expr e ; 
            Printf.printf(")")
  
and print_exprs es =
  match es with
      [] -> ()
    | [e] -> print_expr e
    | e::es -> (
  print_expr e;
  print_char ',';
  print_exprs es
      )

let print_stat s =
  match s with
      ASTEcho e -> (
  Printf.printf("echo(");
  print_expr(e);
  Printf.printf(")")
      )

let print_cmd c =
  match c with
      ASTStat s -> print_stat s
    | ASTConst (s,t,e) -> Printf.printf("const(");
                          print_expr(s);
                          Printf.printf(",%s") (print_typee t); 
                          Printf.printf(",");
                          print_expr e ; 
                          Printf.printf(")")
    | ASTFun (s,t,al,e) -> Printf.printf("fun(");
                          print_expr(s);
                          Printf.printf(",%s") (print_typee t); 
                          Printf.printf(",%s") (print_args al);
                          Printf.printf(",");
                          print_expr e ; 
                          Printf.printf(")")
    | ASTFunRec (s,t,al,e) -> Printf.printf("funRec(");
                          print_expr(s);
                          Printf.printf(",%s") (print_typee t); 
                          Printf.printf(",%s") (print_args al);
                          Printf.printf(",");
                          print_expr e ; 
                          Printf.printf(")")
	

let rec print_cmds cs =
  match cs with
      c::[] -> print_cmd c
      |c1::c2 -> print_cmd c1;Printf.printf(",") ;print_cmds c2
    | _ -> failwith "not yet implemented"
	
let print_prog p =
  Printf.printf("prog([");
  print_cmds p;
  Printf.printf("])")
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
      
