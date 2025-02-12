open Ast

type ztype = InZ of int  

type fType = InF of expr * (string list) * env  and

fRType = InFR of expr * string * (string list) * env  and

valType = 
  | InZ of int
  | InF of expr * (string list) * env  
  | InFR of expr * string * (string list) * env  and

env = (string * valType) list  

type flux_S = ztype list  


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
    InZ(e) -> e 

(* evaluation de chaque expression *)
let rec evalExpr exp env =
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
      ASTId ("not") -> if(evalExpr (getNlist 1 exprs) env = InZ(0)) then InZ(1) else InZ(0)
      |ASTId ("eq") -> if (evalExpr (getNlist 1 exprs) = evalExpr (getNlist 2 exprs)) then InZ (1) else InZ(0)
      |ASTId ("lt") -> if(valIntToInt (evalExpr (getNlist 1 exprs) env ) < valIntToInt(evalExpr (getNlist 2 exprs ) env)) then InZ(1) else InZ(0)
      |ASTId ("add") -> InZ(valIntToInt (evalExpr (getNlist 1 exprs) env) + valIntToInt(evalExpr (getNlist 2 exprs ) env))
      |ASTId ("sub") ->InZ(valIntToInt (evalExpr (getNlist 1 exprs) env) - valIntToInt(evalExpr (getNlist 2 exprs ) env))
      |ASTId ("mul") ->InZ(valIntToInt (evalExpr (getNlist 1 exprs) env) * valIntToInt(evalExpr (getNlist 2 exprs )env))
      |ASTId ("div") -> InZ(valIntToInt (evalExpr (getNlist 1 exprs) env) / valIntToInt(evalExpr (getNlist 2 exprs ) env))
      |_ ->
        (match (evalExpr func env) with
          InF(eprime, argsfun, envF) ->  evalExpr eprime (ajouteArgsEnv argsfun exprs envF)
          |InFR(eprime,id ,argsfun, envF) -> evalExpr eprime ( (id,InFR(eprime,id ,argsfun, envF))
                                            :: ajouteArgsEnv argsfun exprs envF)
        )
    )
    
  |ASTId (e) -> 
    (try 
    let valeur = List.assoc e env in valeur
  with Not_found -> failwith("Variable non definie:" ^ e)) and

  ajouteArgsEnv args exprs env =
    match (args, exprs) with
    | ([],[]) -> env
    | ([],_) -> failwith "Trop d'arguments"
    | (_,[]) -> failwith "Pas assez d'arguments"
    | (a1::aq,e1::eq) -> (a1, evalExpr e1 env) :: List.remove_assoc a1 (ajouteArgsEnv aq eq env)

(* Environnement initial *)
let env = [
  ("true", InZ 1);
  ("false", InZ 0);
]

(* Fonction pour afficher une valeur *)
let string_of_val = function
  | InZ n -> string_of_int n
  | InF (_, args, _) -> "<function> with args: " ^ String.concat ", " args
  | InFR (_, name, args, _) -> "<recursive function " ^ name ^ "> with args: " ^ String.concat ", " args

(* Fonction de test *)
let run_test name expr env =
  print_endline ("Test: " ^ name);
  let result = evalExpr expr env in
  print_endline ("Result: " ^ string_of_val result);
  print_endline "------------------------"

(* Tests *)
let () =
  run_test "Évaluation d’un entier" (ASTNum 42) env;
  run_test "Opération logique AND" (ASTAnd (ASTId "true", ASTId "false")) env;
  run_test "Opération logique OR" (ASTOr (ASTId "false", ASTId "true")) env;
  run_test "Condition if-then-else (true)" (ASTIf (ASTId "true", ASTNum 100, ASTNum 500)) env;
  run_test "Condition if-then-else (false)" (ASTIf (ASTId "false", ASTNum 100, ASTNum 500)) env;
   run_test "add" (ASTIf (ASTId "false", ASTNum 100, ASTNum 500)) env;

(* Test avec variable *)
  let env_with_x = ("x", InZ 5) :: env in
  run_test "Récupération de variable" (ASTId "x") env_with_x; 

   (* Fonction lambda *)
  let lambda = ASTFerm ([ASTArg ("x", ASTInt)], ASTId "x") in
  run_test "Fonction lambda" lambda env; 

   (* Application de fonction *)
  let applied_lambda = ASTApp (lambda, [ASTNum 10]) in
  run_test "Application de fonction" applied_lambda env; 
  

  (* app *)
  let applied_lambda = ASTApp (ASTId("add"), [ASTNum (10), ASTNum(5)]) in
  run_test "Application de fonction" applied_lambda env; 