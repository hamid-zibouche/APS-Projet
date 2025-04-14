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
  | InB of valType * int 
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

(*  *)
let rec sorteArgsP argsP = 
  match argsP with
  | []->[]
  | ASTArgp(id,_)::q -> id :: sorteArgsP q 
  |ASTVargp(id,_)::q -> id :: sorteArgsP q

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
| _ -> ""

let allocn n = 
  let prem = InA (incrementer ()) in 
  let rec aux n =
    if n > 0 then (incrementer (); aux (n - 1))
    else ()
  in 
  aux (n - 1); 
  prem

(* evaluation de chaque expression *)
let rec evalExpr exp env memoire =
  match exp with
  |ASTId ("true") -> (InZ(1),memoire)
  |ASTId("false") -> (InZ(0),memoire)
  |ASTNum(n) -> (InZ(n),memoire)
  |ASTIf(e1,v1,v2) -> let (v,mem2) = evalExpr e1 env memoire in
                       if v = InZ(1)  then  evalExpr v1 env mem2 
                       else evalExpr v2 env mem2         
  |ASTAnd(e1,e2) -> let (v1, mem1) = evalExpr e1 env memoire in
                    if v1 = InZ(1) then evalExpr e2 env mem1 
                    else (InZ(0), mem1)
  |ASTOr (e1,e2) -> let (v1, mem1) = evalExpr e1 env memoire in
                    if v1 = InZ(1) then (InZ(1), mem1) 
                    else evalExpr e2 env mem1
  |ASTFerm (args , e1) -> (InF( e1 , sorteArgs args ,env),memoire)
  |ASTApp (func , exprs) ->
    (match func with 
      |ASTId ("not") -> 
          let (v1, memoire2) = evalExpr (getNlist 1 exprs) env memoire in
          if v1 = InZ(0) then (InZ(1), memoire2) else (InZ(0), memoire2)
      |ASTId ("eq") -> 
          let (v1, mem1) = evalExpr (getNlist 1 exprs) env memoire in
          let (v2, mem2) = evalExpr (getNlist 2 exprs) env mem1 in
          if v1 = v2 then (InZ(1), mem2) else (InZ(0), mem2)
      |ASTId ("lt") -> 
          let (v1, mem1) = evalExpr (getNlist 1 exprs) env memoire in
          let (v2, mem2) = evalExpr (getNlist 2 exprs) env mem1 in
          if valIntToInt v1 < valIntToInt v2 then (InZ(1), mem2) else (InZ(0), mem2)
      |ASTId ("add") -> 
          let (v1, mem1) = evalExpr (getNlist 1 exprs) env memoire in
          let (v2, mem2) = evalExpr (getNlist 2 exprs) env mem1 in
          (InZ(valIntToInt v1 + valIntToInt v2), mem2)
      |ASTId ("sub") -> 
          let (v1, mem1) = evalExpr (getNlist 1 exprs) env memoire in
          let (v2, mem2) = evalExpr (getNlist 2 exprs) env mem1 in
          (InZ(valIntToInt v1 - valIntToInt v2), mem2)
      |ASTId ("mul") -> 
          let (v1, mem1) = evalExpr (getNlist 1 exprs) env memoire in
          let (v2, mem2) = evalExpr (getNlist 2 exprs) env mem1 in
          (InZ(valIntToInt v1 * valIntToInt v2), mem2)
      |ASTId ("div") -> 
          let (v1, mem1) = evalExpr (getNlist 1 exprs) env memoire in
          let (v2, mem2) = evalExpr (getNlist 2 exprs) env mem1 in
          (InZ(valIntToInt v1 / valIntToInt v2), mem2)
      |_ -> let (e,mem) = evalExpr func env memoire in
        (match (e) with
          InF(eprime, argsfun, envF) ->  let (new_env,new_mem) = ajouteArgsEnvFun argsfun exprs envF env mem in
                                          evalExpr eprime new_env new_mem
          |InFR(eprime,id ,argsfun, envF) -> let (new_env,new_mem) = ajouteArgsEnvFun argsfun exprs envF env mem in
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
                  (List.assoc (InA a) memoire,memoire)
              with Not_found -> failwith ("Adresse " ^ string_of_int a ^ " non trouvée en mémoire"))
          | v -> (v,memoire)  
      with Not_found -> failwith ("Variable " ^ e ^ " non trouvée dans l'environnement"))
  | ASTAlloc e -> let (InZ(taille),mem) = evalExpr e env memoire in 
                  (InB((allocn taille),taille), mem)

  | ASTVset (e1, e2, e3) -> 
      let (v1, mem1) = evalExpr e1 env memoire in
      let (v2, mem2) = evalExpr e2 env mem1 in
      let (v3, mem3) = evalExpr e3 env mem2 in
      (match (v1, v2) with
      | (InB(InA(a), n), InZ(i)) -> 
          if i < 0 || i >= n then 
            failwith "Erreur: indice hors limites du tableau"
          else 
            let mem = (InA(a + i), v3) :: List.remove_assoc (InA(a + i)) mem3 in
            (InB(InA(a), n), mem)
      | _ -> failwith "Erreur: types inattendus pour ASTVset")

  | ASTNthExpr (e1, e2) -> 
      let (v1, mem1) = evalExpr e1 env memoire in
      let (v2, mem2) = evalExpr e2 env mem1 in
      (match (v1, v2) with
      | (InB(InA(a), n), InZ(i)) -> 
          if i < 0 || i >= n then 
            failwith "Erreur: indice hors limites du tableau"
          else 
            (try 
              (List.assoc (InA(a + i)) mem2, mem2)
            with Not_found -> failwith ("Adresse " ^ string_of_int (a + i) ^ " non trouvée en mémoire"))
      | _ -> failwith "Erreur: types inattendus pour ASTNthExpr")

  | ASTLen (e) -> let (InB(InA(a),n),mem1) = evalExpr e env memoire in (InZ(n),mem1)
  
  and

  evalExpar exp env memoire = 
   match exp with 
   |ASTExpr(e) -> 
      let (v,mem) = evalExpr e env memoire in (v)
   |ASTAdr (ident) ->(try
     List.assoc ident env
    with Not_found -> failwith ("Variable adr " ^ ident ^ " non trouvée dans l'environnement"))

  and 

  ajouteArgsEnvFun args exprs env envCours memoire=
    match (args, exprs) with
    | ([],[]) -> (env,memoire)
    | ([],_) -> failwith "Trop d'arguments"
    | (_,[]) -> failwith "Pas assez d'arguments"
    | (a1::aq,e1::eq) -> let (new_env,new_mem) = ajouteArgsEnvFun aq eq env envCours memoire in
                         let (v,mem) = evalExpr e1 envCours memoire in
                        (((a1, v ) :: List.remove_assoc a1 new_env), new_mem)
  and 

  ajouteArgsEnvProc args exprs env envCours memoire=
    match (args, exprs) with
    | ([],[]) -> (env,memoire)
    | ([],_) -> failwith "Trop d'arguments"
    | (_,[]) -> failwith "Pas assez d'arguments"
    | (a1::aq,e1::eq) -> let (new_env,new_mem) = ajouteArgsEnvProc aq eq env envCours memoire in
                        (((a1, evalExpar e1 envCours memoire ) :: List.remove_assoc a1 new_env), new_mem)

(* evaluation de chaque lval *)
let rec evalLval lval env memoire = 
  match lval with
  |ASTIdLval(s) -> (try       
                let adresse = List.assoc s env in 
                (adresse,memoire) 
              with Not_found -> failwith ("Variable " ^ s ^ " non trouvée dans l'environnement"))

  | ASTNthLval(l, e) -> 
      let (v1, mem1) = evalLval l env memoire in
      let (v2, mem2) = evalExpr e env mem1 in
      (match (v1, v2) with
      | (InB(InA(a), n), InZ(i)) -> 
          if i < 0 || i >= n then 
            failwith "Erreur: indice hors limites du tableau"
          else 
            (InA(a + i), mem2)
      | (InA(a),InZ(i)) -> 
        let (InB(InA(a2),n)) = List.assoc (InA(a)) mem2 in

        if i < 0 || i >= n then 
        failwith "Erreur: indice hors limites du tableau"
        else 
        (InA(a2 + i), mem2)

      | _ -> failwith "Erreur: types inattendus pour ASTNthLval")
  |_ -> failwith "Erreur dans EvalLval"
                        
(* evaluation de chaque commande *)
let rec evalInst inst env memoire sortie = 
  match inst with
  ASTEcho(n) -> 
    let (v, _) = evalExpr n env memoire in 
    (match v with
    | InZ(n) -> (env, memoire, sortie @ [InZ(n)])
(*     | InA(a) -> 
        (try 
          let InZ(n) = List.assoc (InA(a)) memoire in 
          (env, memoire, sortie @ [InZ(n)])
        with Not_found -> failwith ("Adresse " ^ string_of_int a ^ " non trouvée en mémoire")) *)
    | _ -> failwith "Erreur dans ASTEcho";)
  |ASTSet(ASTId(s), e) -> 
      let adresse = List.assoc s env in  
      let (v,mem) = evalExpr e env memoire in
      (env,  (adresse, v ) :: List.remove_assoc adresse mem  ,  sortie  )
  |ASTIFB(e,b1,b2) -> 
      let (v, mem1) = evalExpr e env memoire in
      if v = InZ(1) then evalCmds b1 env mem1 sortie 
      else evalCmds b2 env mem1 sortie
  | ASTWhile(e, b) -> 
    let rec boucle_while env memoire sortie =
      let (v, mem1) = evalExpr e env memoire in
      if v = InZ(1) then 
        let (new_env, new_memoire, new_sortie) = evalCmds b env mem1 sortie in  
        boucle_while new_env new_memoire new_sortie 
      else 
        (env, mem1, sortie)
    in
    boucle_while env memoire sortie
  |ASTCall(proc,exprs) -> 
    let (v, mem1) = evalExpr proc env memoire in
    (match v with
      InP(block, argsproc, envP) -> 
        let (new_env_Proc,new_mem_Proc) = ajouteArgsEnvProc argsproc exprs envP env mem1 
        in let (new_env,new_memoire,new_sortie) = evalCmds block new_env_Proc new_mem_Proc sortie
        in (env,new_memoire,new_sortie)
      |InPR(block,id ,argsproc, envP) -> 
        let (new_env_Proc,new_mem_Proc) = ajouteArgsEnvProc argsproc exprs envP env memoire 
        in let (new_env,new_memoire,new_sortie) = evalCmds block ( (id,InPR(block,id ,argsproc, envP))
                                    :: new_env_Proc ) new_mem_Proc sortie
      in (env,new_memoire,new_sortie)
      |_ -> failwith "procedure non définie"
    ) 
  |ASTSetTab(lval,e) -> 
    let (v,mem1) = evalExpr e env memoire in 
    let (adresse,mem2) =  evalLval lval env mem1 in
    (env, ((adresse, v) :: List.remove_assoc adresse mem2) , sortie)        
    
  |_ -> failwith "Erreur dans la syntaxe"
  
and  
 evalCmd cmd env memoire sortie = 
  match cmd with
  ASTConst (ASTId(s),_,v) -> let (e,mem) = evalExpr v env memoire in ((s, e) :: List.remove_assoc s env , mem, sortie)
  |ASTFun (ASTId(s),_,args,e) -> ((s, InF( e , sorteArgs args ,env)) :: List.remove_assoc s env, memoire, sortie) 
  |ASTFunRec (ASTId(s),_,args,e) -> ((s, InFR( e ,s, sorteArgs args ,env)) :: List.remove_assoc s env , memoire,  sortie)
  |ASTStat(e) -> evalInst e env memoire sortie
  |ASTVar(ASTId(s),_) -> ( (s, InA (incrementer ())) :: List.remove_assoc s env , memoire, sortie)
  |ASTProc (ASTId(s),args,bk) -> ((s, InP( bk , sorteArgsP args ,env)) :: List.remove_assoc s env, memoire, sortie)
  |ASTProcRec (ASTId(s),args,bk) -> ((s, InPR(bk ,s, sorteArgsP args ,env)) :: List.remove_assoc s env , memoire,  sortie)
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
  |InA(a) :: q -> (string_of_int a) ^ "\n" ^ (afficheliste q)
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