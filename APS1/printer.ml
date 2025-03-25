open Ast

(* Fonctions d'impression pour les types *)
let rec string_of_typee = function
  | ASTBool -> "bool"
  | ASTInt -> "int"
  | ASTTypes (args, ret) ->
      let args_str = String.concat " * " (List.map string_of_typee args) in
      "(" ^ args_str ^ " -> " ^ string_of_typee ret ^ ")"

(* Impression des arguments *)
let string_of_arg (ASTArg (name, typee)) =
  name ^ ": " ^ string_of_typee typee

let string_of_args args =
  String.concat ", " (List.map string_of_arg args)

(* Fonctions d'impression pour les expressions *)
let rec string_of_expr = function
  | ASTNum n -> string_of_int n
  | ASTId id -> id
  | ASTIf (e1, e2, e3) ->
      "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | ASTAnd (e1, e2) ->
      string_of_expr e1 ^ " and " ^ string_of_expr e2
  | ASTOr (e1, e2) ->
      string_of_expr e1 ^ " or " ^ string_of_expr e2
  | ASTApp (e, args) ->
      string_of_expr e ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | ASTFerm (args, body) ->
      "[" ^ string_of_args args ^ "] -> " ^ string_of_expr body

(* Fonctions d'impression pour les instructions, définitions et commandes *)
let rec string_of_stat = function
  | ASTEcho expr ->
      "echo " ^ string_of_expr expr
  | ASTSet (id, expr) ->
      "set " ^ string_of_expr id ^ " = " ^ string_of_expr expr
  | ASTIFB (cond, block1, block2) ->
      "ifb " ^ string_of_expr cond ^ " then " ^ string_of_cmds block1 ^ " else " ^ string_of_cmds block2
  | ASTWhile (cond, block) ->
      "while " ^ string_of_expr cond ^ " do " ^ string_of_cmds block
  | ASTCall (id, exprs) ->
      "call " ^ string_of_expr id ^ "(" ^ String.concat ", " (List.map string_of_expr exprs) ^ ")"

and string_of_cmd = function
  | ASTStat stat ->
      string_of_stat stat
  | ASTConst (name, typee, expr) ->
      "const " ^ string_of_expr name ^ ": " ^ string_of_typee typee ^ " = " ^ string_of_expr expr
  | ASTFun (name, typee, args, body) ->
      "fun " ^ string_of_expr name ^ ": " ^ string_of_typee typee ^
      " (" ^ string_of_args args ^ ") = " ^ string_of_expr body
  | ASTFunRec (name, typee, args, body) ->
      "fun rec " ^ string_of_expr name ^ ": " ^ string_of_typee typee ^
      " (" ^ string_of_args args ^ ") = " ^ string_of_expr body
  | ASTVar (name, typee) ->
      "var " ^ string_of_expr name ^ ": " ^ string_of_typee typee
  | ASTProc (name, args, block) ->
      "proc " ^ string_of_expr name ^ ": " ^
      " (" ^ string_of_args args ^ ") " ^ string_of_cmds block
  | ASTProcRec (name, args, block) ->
      "proc rec " ^ string_of_expr name ^ ": " ^ 
      " (" ^ string_of_args args ^ ") " ^ string_of_cmds block

and string_of_cmds cmds =
  String.concat "\n" (List.map string_of_cmd cmds)

(* Fonction d'affichage du programme *)
let print_prog prog =
  print_endline (string_of_cmds prog)

(* Programme principal *)
let () =
  let fname = Sys.argv.(1) in
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
    print_prog p;
    print_string ".\n"
  with Lexer.Eof ->
    exit 0
