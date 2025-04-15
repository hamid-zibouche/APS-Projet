bt_prog(prog(Cs)) :- is_init_env(G), bt_cmds(G,Cs).

is_init_env(G) :- G = [ ("true", bool_t),
    ("false", bool_t),
    ("not", fun_t([bool_t], bool_t)),
    ("eq", fun_t([int_t,int_t], bool_t)),
    ("lt", fun_t([int_t,int_t], bool_t)),
    ("add", fun_t([int_t,int_t], int_t)),
    ("sub", fun_t([int_t,int_t], int_t)),
    ("mul", fun_t([int_t,int_t], int_t)),
    ("div", fun_t([int_t,int_t], int_t))].

%% commandes
bt_cmds(G,[X]) :- bt_stat(G,X).
bt_cmds(G,[T|L]) :- bt_def(G,T,G2), bt_cmds(G2,L).

%% instruction
bt_stat(G,echo(E)) :- bt_expr(G,E,int_t).

%% definitions
bt_def(G1, const(id(X), T, E), G2) :- 
    bt_expr(G1, E, T), 
    update(G1,X,T,G2).
bt_def(G1,fun(id(X),T,A,E),G2) :- ajoutRec(G1,A,G3), 
                                    bt_expr(G3,E,T),
                                    parcoursArgFun(A,TR),
                                    update(G1,X,fun_t(TR,T),G2).
bt_def(G1,funRec(id(X),T,A,E),G2) :- parcoursArgFun(A,TR),
                                    update(G1,X,fun_t(TR,T),G2),
                                    ajoutRec(G2,A,G3), 
                                    bt_expr(G3,E,T).

%% expressions
bt_expr(_,num(_),int_t).
bt_expr(G,id(X),T) :- member((X,T),G).
bt_expr(G,if(E1,E2,E3),T) :- bt_expr(G,E1,bool_t), bt_expr(G,E2,T), bt_expr(G,E3,T).
bt_expr(G,and(E1,E2),bool_t) :- bt_expr(G,E1,bool_t), bt_expr(G,E2,bool_t).
bt_expr(G,or(E1,E2),bool_t) :- bt_expr(G,E1,bool_t), bt_expr(G,E2,bool_t).
bt_expr(G,app(id(F),A),T) :- parcoursArg(G,A,L2), member((F,fun_t(L2,T)), G).
bt_expr(G,app(F,A),T) :- parcoursArg(G,A,L2), bt_expr(G,F,fun_t(L2,T)).
bt_expr(G,ferm(A,E),fun_t(TA,T)) :- parcoursArgFun(A,TA), ajoutRec(G,A,G1), bt_expr(G1,E,T).

%% ajout dans un environnement
ajoutRec(G, [], G).
ajoutRec(G, [(id(X),T)|L], G1) :- 
    ajoutRec([(X,T)|G], L, G1).

%% parcours arguments des appels
parcoursArg(_, [], []). 
parcoursArg(G, [A|AL], [T|TL]) :- 
    bt_expr(G, A, T),
    parcoursArg(G, AL, TL).

%% parcours arguments des fonctions
parcoursArgFun([], []).
parcoursArgFun([(id(_),T)|AL], [T|L]) :- 
    parcoursArgFun(AL, L).

%% mise à jour environnement
update(G1, X, T, G2) :-
    (   select((X,_), G1, G1SansX) 
    ->  G2 = [(X,T)|G1SansX]  
    ;   G2 = [(X,T)|G1]       
    ).

%% lecture du programme
:-  read(X),
    bt_prog(X).