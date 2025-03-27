open Ast

type ztype = InZ of int  

type fType = InF of expr * (string list) * env and

 fRType = InFR of expr * string * (string list) * env and

 pType = InP of (cmd list) * (string list) * env and

 pRType = InPR of (cmd list) * string * (string list) * env and

 valType = 
  | InZ of int
  | InF of expr * (string list) * env  
  | InFR of expr * string * (string list) * env
  | InA of int
  | InP of (cmd list) * (string list) * env
  | InPR of (cmd list) * string * (string list) * env and

 memoire = (valType * valType) list and

 env = (string * valType) list 

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
  |ASTIf(e1,v1,v2) -> if(evalExpr e1 env memoire = InZ(1) ) then  evalExpr v1 env memoire else evalExpr v2 env memoire
  |ASTAnd(e1,e2) -> if(evalExpr e1 env memoire = InZ(1)) then evalExpr e2 env memoire else InZ(0)
  |ASTOr (e1,e2) -> if(evalExpr e1 env memoire = InZ(1)) then InZ(1) else evalExpr e2 env memoire
  |ASTFerm (args , e1) -> InF( e1 , sorteArgs args ,env)
  |ASTApp (func , exprs) ->
    (match func with 
      |ASTId ("not") -> if(evalExpr (getNlist 1 exprs) env memoire = InZ(0)) then InZ(1) else InZ(0)
      |ASTId ("eq") -> if ((evalExpr (getNlist 1 exprs) env memoire) = (evalExpr (getNlist 2 exprs) env memoire)) then InZ (1) else InZ(0)
      |ASTId ("lt") -> if(valIntToInt (evalExpr (getNlist 1 exprs) env memoire) < valIntToInt(evalExpr (getNlist 2 exprs ) env memoire)) then InZ(1) else InZ(0)
      |ASTId ("add") -> InZ(valIntToInt (evalExpr (getNlist 1 exprs) env memoire) + valIntToInt(evalExpr (getNlist 2 exprs ) env memoire))
      |ASTId ("sub") ->InZ(valIntToInt (evalExpr (getNlist 1 exprs) env memoire) - valIntToInt(evalExpr (getNlist 2 exprs ) env memoire))
      |ASTId ("mul") ->InZ(valIntToInt (evalExpr (getNlist 1 exprs) env memoire) * valIntToInt(evalExpr (getNlist 2 exprs )env memoire))
      |ASTId ("div") -> InZ(valIntToInt (evalExpr (getNlist 1 exprs) env memoire) / valIntToInt(evalExpr (getNlist 2 exprs ) env memoire))
      |_ ->
        (match (evalExpr func env memoire) with
          InF(eprime, argsfun, envF) ->  let (new_env,new_mem) = ajouteArgsEnv argsfun exprs envF env memoire in
                                          evalExpr eprime new_env new_mem
          |InFR(eprime,id ,argsfun, envF) -> let (new_env,new_mem) = ajouteArgsEnv argsfun exprs envF env memoire in
                                          evalExpr eprime ( (id,InFR(eprime,id ,argsfun, envF)) ::new_env ) new_mem
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

  ajouteArgsEnv args exprs env envCours memoire=
    match (args, exprs) with
    | ([],[]) -> (env,memoire)
    | ([],_) -> failwith "Trop d'arguments"
    | (_,[]) -> failwith "Pas assez d'arguments"
    | (a1::aq,e1::eq) -> let (new_env,new_mem) = ajouteArgsEnv aq eq env envCours memoire in
                         let adresse = InA(incrementer()) in 
                        (((a1, adresse) :: List.remove_assoc a1 new_env), (adresse,evalExpr e1 envCours memoire)::List.remove_assoc adresse new_mem)

 
(*         and *)

 (*  ajouteArgsEnv args exprs env envCours memoire=
    match (args, exprs) with
    | ([],[]) -> env
    | ([],_) -> failwith "Trop d'arguments"
    | (_,[]) -> failwith "Pas assez d'arguments"
    | (a1::aq,e1::eq) ->
      match e1 with
      |ASTId e -> let new_env = ajouteArgsEnv aq eq env envCours memoire in
                        (a1, getAdresse e1 new_env  ) :: List.remove_assoc a1 new_env
                      
      | e1 -> let new_env = ajouteArgsEnv aq eq env envCours memoire in
      (a1, evalExpr e1 envCours memoire) :: List.remove_assoc a1 new_env
and
getAdresse e env = 
  match e with
  | ASTId s -> 
      (try 
         List.assoc s env  (* Retourne directement la valeur associée *)
       with Not_found -> 
         failwith ("Erreur : La variable '" ^ s ^ "' est introuvable dans l'environnement."))
  | _ -> failwith "Erreur : Expression invalide pour getAdresse"
 *)

    
(* evaluation de chaque commande *)
let rec evalInst inst env memoire sortie = 
  match inst with
  ASTEcho(n) -> (env, memoire, sortie @ [evalExpr n env memoire])
  |ASTSet(ASTId(s), e) -> let adresse = List.assoc s env in  
    (env,  (adresse, evalExpr e env memoire) :: List.remove_assoc adresse memoire  ,  sortie  )
  |ASTIFB(e,b1,b2) -> if(evalExpr e env memoire = InZ(1) ) then  evalCmds b1 env memoire sortie else evalCmds b2 env memoire sortie
  | ASTWhile(e, b) -> 
    let rec boucle_while env memoire sortie =
      if evalExpr e env memoire = InZ(1) then 
        let (new_env, new_memoire, new_sortie) = evalCmds b env memoire sortie in  
        boucle_while new_env new_memoire new_sortie 
      else 
        (env, memoire, sortie)
    in
    boucle_while env memoire sortie
  |ASTCall(proc,exprs) -> 
    (match (evalExpr proc env memoire) with
      InP(block, argsproc, envP) -> 
        let (new_env_Proc,new_mem_Proc) = ajouteArgsEnv argsproc exprs envP env memoire 
        in let (new_env,new_memoire,new_sortie) = evalCmds block new_env_Proc new_mem_Proc sortie
        in (new_env,new_memoire,new_sortie)
      |InPR(block,id ,argsproc, envP) -> 
        let (new_env_Proc,new_mem_Proc) = ajouteArgsEnv argsproc exprs envP env memoire 
        in let (new_env,new_memoire,new_sortie) = evalCmds block ( (id,InPR(block,id ,argsproc, envP))
                                    :: new_env_Proc ) new_mem_Proc sortie
      in (new_env,new_memoire,new_sortie)
      |_ -> failwith "procedure non définie"
    ) 
and  
 evalCmd cmd env memoire sortie = 
  match cmd with
  ASTConst (ASTId(s),_,v) -> ((s, evalExpr v env memoire) :: List.remove_assoc s env , memoire, sortie)
  |ASTFun (ASTId(s),_,args,e) -> ((s, InF( e , sorteArgs args ,env)) :: List.remove_assoc s env, memoire, sortie) 
  |ASTFunRec (ASTId(s),_,args,e) -> ((s, InFR( e ,s, sorteArgs args ,env)) :: List.remove_assoc s env , memoire,  sortie)
  |ASTStat(e) -> evalInst e env memoire sortie
  |ASTVar(ASTId(s),_) -> ( (s, InA (incrementer ())) :: List.remove_assoc s env , memoire, sortie)
  |ASTProc (ASTId(s),args,bk) -> ((s, InP( bk , sorteArgs args ,env)) :: List.remove_assoc s env, memoire, sortie)
  |ASTProcRec (ASTId(s),args,bk) -> ((s, InPR(bk ,s, sorteArgs args ,env)) :: List.remove_assoc s env , memoire,  sortie)
  |_ -> failwith "Erreur dans la syntaxe"

and
(* evaluation de la suite de commandes *)
evalCmds cmds env memoire sortie =
  match cmds with
  | [] -> (env, memoire, sortie)
  | cmd::q -> 
      let (new_env, new_memoire, new_sortie) = evalCmd cmd env memoire sortie in
      evalCmds q new_env new_memoire new_sortie  

(* evaluation du programme *)
let rec evalProg prog = evalCmds prog [] [] []

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
    let (_, _, sortie_finale) = evalProg p in
      print_string (afficheliste sortie_finale);
      print_string ".\n"
  with Lexer.Eof ->
    exit 0