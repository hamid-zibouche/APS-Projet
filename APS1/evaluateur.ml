open Ast

type ztype = InZ of int  

type fType = InF of expr * (string list) * env

type fRType = InFR of expr * string * (string list) * env

type aType = InA of int

type pType = InP of (cmd list) * (string list) * env

type pRType = InPR of (cmd list) * string * (string list) * env

type valType = 
  | InZ of int
  | InF of expr * (string list) * env  
  | InFR of expr * string * (string list) * env
  | InA of int
  | InP of (cmd list) * (string list) * env
  | InPR of (cmd list) * string * (string list) * env

type memoire = (aType * valType) list

type env = (string * valType) list 

type flux_S = ztype list  

(* compteur d'adresses de la mémoire*)
let compteur = ref 0

let incrementer () =
  compteur := !compteur + 1;
  !compteur


let rec sorteArgs args = 
  match args with
  | []->[]
  | ASTArg(id,_)::q -> id :: sorteArgs q 

let rec getNlist i list =
  match list with
  [] -> failwith "la liste n'est pas assez grande"
  |(t::q) -> if(i = 1) then t else getNlist (i - 1) q

let valIntToInt x =
  match x with 
  |InZ(e) -> e 
  | _ -> failwith "valIntToInt : Valeur inattendue (pas un entier)"

let string_of_val = function
| InZ n -> string_of_int n
| InF (_, args, _) -> "<function> with args: " ^ String.concat ", " args
| InFR (_, name, args, _) -> "<recursive function " ^ name ^ "> with args: " ^ String.concat ", " args

(* evaluation de chaque expression *)
let rec evalExpr exp env memoire =
  match exp with
  |ASTId ("true") -> InZ(1)
  |ASTId("false") -> InZ(0)
  |ASTNum(n) -> InZ(n)
  |ASTIf(e1,v1,v2) -> if(evalExpr e1 env = InZ(1) ) then  evalExpr v1 env else evalExpr v2 env
  |ASTAnd(e1,e2) -> if(evalExpr e1 env = InZ(1)) then evalExpr e2 env else InZ(0)
  |ASTOr (e1,e2) -> if(evalExpr e1 env = InZ(1)) then InZ(1) else evalExpr e2 env
  |ASTFerm (args , e1) -> InF( e1 , sorteArgs args ,env)
  |ASTApp (func , exprs) ->
    (match func with 
      |ASTId ("not") -> if(evalExpr (getNlist 1 exprs) env = InZ(0)) then InZ(1) else InZ(0)
      |ASTId ("eq") -> if ((evalExpr (getNlist 1 exprs) env) = (evalExpr (getNlist 2 exprs) env)) then InZ (1) else InZ(0)
      |ASTId ("lt") -> if(valIntToInt (evalExpr (getNlist 1 exprs) env ) < valIntToInt(evalExpr (getNlist 2 exprs ) env)) then InZ(1) else InZ(0)
      |ASTId ("add") -> InZ(valIntToInt (evalExpr (getNlist 1 exprs) env) + valIntToInt(evalExpr (getNlist 2 exprs ) env))
      |ASTId ("sub") ->InZ(valIntToInt (evalExpr (getNlist 1 exprs) env) - valIntToInt(evalExpr (getNlist 2 exprs ) env))
      |ASTId ("mul") ->InZ(valIntToInt (evalExpr (getNlist 1 exprs) env) * valIntToInt(evalExpr (getNlist 2 exprs )env))
      |ASTId ("div") -> InZ(valIntToInt (evalExpr (getNlist 1 exprs) env) / valIntToInt(evalExpr (getNlist 2 exprs ) env))
      |_ ->
        (match (evalExpr func env) with
          InF(eprime, argsfun, envF) ->  evalExpr eprime (ajouteArgsEnv argsfun exprs envF env)
          |InFR(eprime,id ,argsfun, envF) -> evalExpr eprime ( (id,InFR(eprime,id ,argsfun, envF))
                                            :: ajouteArgsEnv argsfun exprs envF env)
          |_ -> failwith "Fonction non définie"
        ) 
    )
  | ASTId e -> 
      (try 
          let valeur = List.assoc e env in 
          match valeur with
          | InA a -> 
              (try 
                  List.assoc (InA a) memoire
              with Not_found -> failwith ("Adresse " ^ string_of_int a ^ " non trouvée en mémoire"))
          | v -> v  
      with Not_found -> failwith ("Variable " ^ e ^ " non trouvée dans l'environnement"))
  
        and

  ajouteArgsEnv args exprs env envCours=
    match (args, exprs) with
    | ([],[]) -> env
    | ([],_) -> failwith "Trop d'arguments"
    | (_,[]) -> failwith "Pas assez d'arguments"
    | (a1::aq,e1::eq) -> let new_env = ajouteArgsEnv aq eq env envCours in
                        (a1, evalExpr e1 envCours) :: List.remove_assoc a1 new_env

(* evaluation de chaque commande *)
let rec evalInst inst env memoire sortie = 
  match inst with
  ASTEcho(n) -> (env, memoire, sortie @ [evalExpr n env])
  |ASTSet(ASTId(s), e) -> let adresse = List.assoc s env in  
    (env,  (adresse, evalExpr v env) :: List.remove_assoc s env  ,  sortie  )
  |

let rec evalCmd cmd env memoire sortie = 
  match cmd with
  ASTConst (ASTId(s),_,v) -> ((s, evalExpr v env) :: List.remove_assoc s env , memoire, sortie)
  |ASTFun (ASTId(s),_,args,e) -> ((s, InF( e , sorteArgs args ,env)) :: List.remove_assoc s env, memoire, sortie) 
  |ASTFunRec (ASTId(s),_,args,e) -> ((s, InFR( e ,s, sorteArgs args ,env)) :: List.remove_assoc s env , memoire,  sortie)
  |ASTStat(e) -> evalInst e env memoire sortie
  |ASTVar(ASTId(s),_) -> ( (s, InA (incrementer ())) :: List.remove_assoc s env , memoire, sortie)
  |ASTProc (ASTId(s),args,bk) -> ((s, InP( bk , sorteArgs args ,env)) :: List.remove_assoc s env, memoire, sortie)
  |ASTProcRec (ASTId(s),args,bk) -> ((s, InPR(bk ,s, sorteArgs args ,env)) :: List.remove_assoc s env , memoire,  sortie)
  |_ -> failwith "Erreur dans la syntaxe"

(* evaluation de la suite de commandes *)
let rec evalCmds cmds env sortie =
  match cmds with
  | [] -> sortie
  | cmd::q ->( match (evalCmd cmd env sortie) with
      | (e, s) -> s @ (evalCmds q e sortie))

(* evaluation du programme *)
let rec evalProg prog = evalCmds prog [] []

let rec afficheliste l = 
  match l with 
  |[]->""
  |(InZ n) :: q -> (string_of_int n) ^ "\n" ^ (afficheliste q)
  |_ -> failwith "Erreur dans la syntaxe";;

let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_string ( afficheliste (evalProg p) );
      print_string ".\n"
  with Lexer.Eof ->
    exit 0