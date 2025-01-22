open Lexer
open Parser
open Printer

let () =
  try
    (* Lire depuis l'entrée standard ou un fichier d'entrée *)
    let lexbuf = Lexing.from_channel stdin in
    (* Parse le programme *)
    let prog = Parser.prog Lexer.token lexbuf in
    (* Affiche le programme analysé en utilisant le printer *)
    print_prog prog
  with
  | Lexer.Eof -> exit 0
  | Parsing.Parse_error -> print_endline "Erreur de syntaxe"
