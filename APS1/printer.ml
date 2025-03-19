open Ast

let rec string_of_typee = function
  | ASTBool -> "bool"
  | ASTInt -> "int"
  | ASTTypes (args, ret) ->
      let args_str = String.concat " * " (List.map string_of_typee args) in
      "(" ^ args_str ^ " -> " ^ string_of_typee ret ^ ")"

let string_of_arg (ASTArg (name, typee)) =
  name ^ ": " ^ string_of_typee typee

let string_of_args args =
  String.concat ", " (List.map string_of_arg args)

let rec string_of_expr = function
  | ASTNum n -> string_of_int n
  | ASTId id -> id
  | ASTIf (e1, e2, e3) ->
      "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | ASTAnd (e1, e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | ASTOr (e1, e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2
  | ASTApp (e, args) ->
      string_of_expr e ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | ASTFerm (args, body) ->
      "[" ^ string_of_args args ^ "] -> " ^ string_of_expr body

let string_of_stat = function
  | ASTEcho expr -> "echo " ^ string_of_expr expr

let rec string_of_cmd = function
  | ASTStat stat -> string_of_stat stat
  | ASTConst (name, typee, expr) ->
      "const " ^ string_of_expr name ^ ": " ^ string_of_typee typee ^ " = " ^ string_of_expr expr
  | ASTFun (name, typee, args, body) ->
      "fun " ^ string_of_expr name  ^ ": " ^ string_of_typee typee ^ " (" ^ string_of_args args ^ ") = " ^ string_of_expr body
  | ASTFunRec (name, typee, args, body) ->
      "fun rec " ^ string_of_expr name  ^ ": " ^ string_of_typee typee ^ " (" ^ string_of_args args ^ ") = " ^ string_of_expr body

let string_of_cmds cmds =
  String.concat "\n" (List.map string_of_cmd cmds)

let print_prog prog =
  print_endline (string_of_cmds prog)
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
