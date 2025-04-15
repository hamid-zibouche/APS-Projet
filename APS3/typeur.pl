bt_prog(prog(Bk)) :- is_init_env(G), bt_block(G,Bk,_).

is_init_env(G) :- G = [ ("true", bool_t),
    ("false", bool_t),
    ("not", fun_t([bool_t], bool_t)),
    ("eq", fun_t([int_t,int_t], bool_t)),
    ("lt", fun_t([int_t,int_t], bool_t)),
    ("add", fun_t([int_t,int_t], int_t)),
    ("sub", fun_t([int_t,int_t], int_t)),
    ("mul", fun_t([int_t,int_t], int_t)),
    ("div", fun_t([int_t,int_t], int_t))].

%% bloc 
bt_block(G,block(Cs),T) :- bt_cmds(G,Cs,T).

%% commandes
bt_cmds(G,[ret(E)],T) :- bt_expr(G,E,T).
bt_cmds(G,[S],T) :- bt_stat(G,S,T).
bt_cmds(G,[T|L],TY) :- bt_stat(G,T,void_t), bt_cmds(G,L,TY).
bt_cmds(G,[T|L],TY) :- bt_stat(G,T,(TY,void_t)), bt_cmds(G,L,TY), TY\=void_t.
bt_cmds(G,[T|L],_) :- bt_def(G,T,G2), bt_cmds(G2,L,_).

%% instructions
bt_stat(G,echo(E),void_t) :- bt_expr(G,E,int_t).
bt_stat(G,set(id(X),E),void_t):- member((X,ref(T)),G), bt_expr(G,E,T).
bt_stat(G,ifb(E,BK1,BK2),(T,void_t)) :- bt_expr(G,E,bool_t), bt_block(G,BK1,T), bt_block(G,BK2,void_t), T \=void_t.
bt_stat(G,ifb(E,BK1,BK2),(T,void_t)) :- bt_expr(G,E,bool_t), bt_block(G,BK1,void_t), bt_block(G,BK2,T), T \=void_t.
bt_stat(G,ifb(E,BK1,BK2),T) :- bt_expr(G,E,bool_t), bt_block(G,BK1,T), bt_block(G,BK2,T).
bt_stat(G,while(E,BK), void_t) :- bt_expr(G,E,bool_t), bt_block(G,BK,void_t).
bt_stat(G,while(E,BK), (T,void_t)) :- bt_expr(G,E,bool_t), bt_block(G,BK,(T,void_t)).
bt_stat(G,while(E,BK), (T,void_t)) :- bt_expr(G,E,bool_t), bt_block(G,BK,T).
bt_stat(G,call(F,A),void_t) :- parcoursArgPar(G,A,L2), bt_expr(G,F,fun_t(L2,void_t)).
bt_stat(G,setTab(X,E),void_t) :- bt_expr(G,X,T), bt_expr(G,E,T).  

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
bt_def(G1, var(id(X),int_t), G2) :- update(G1,X,ref(int_t),G2).
bt_def(G1, var(id(X),bool_t), G2) :- update(G1,X,ref(bool_t),G2).
bt_def(G1, proc(id(X),A,B), G2) :- changeArg(A,AN),
                                   parcoursArgFun(AN, TR),
                                   ajoutRec(G1,AN,G3),
                                   bt_block(G3,B,void_t),
                                   update(G1,X,fun_t(TR,void_t),G2).
bt_def(G1, procRec(id(X),A,B), G2) :- changeArg(A, AN),
                                    parcoursArgFun(AN,TR),
                                   update(G1,X,fun_t(TR,void_t),G2),
                                   ajoutRec(G2,AN,G3),
                                   bt_block(G3,B,void_t).
bt_def(G1,funCmd(id(X),T,A,B),G2) :- ajoutRec(G1,A,G3), 
                                    bt_block(G3,B,T),
                                    parcoursArgFun(A,TR),
                                    update(G1,X,fun_t(TR,T),G2).
bt_def(G1,funRecCmd(id(X),T,A,B),G2) :- parcoursArgFun(A,TR),
                                    update(G1,X,fun_t(TR,T),G2),
                                    ajoutRec(G2,A,G3), 
                                    bt_block(G3,B,T).

%% paramètres appel
bt_expar(G,adr(X),ref(T)) :- member((X,ref(T)),G).
bt_expar(G,X,T) :- bt_expr(G,X,T).

%% expressions
bt_expr(_,num(_),int_t).
bt_expr(G,id(X),T) :- member((X,ref(T)),G).
bt_expr(G,id(X),T) :- member((X,T),G), T\=ref(_).
bt_expr(G,if(E1,E2,E3),T) :- bt_expr(G,E1,bool_t), bt_expr(G,E2,T), bt_expr(G,E3,T).
bt_expr(G,and(E1,E2),bool_t) :- bt_expr(G,E1,bool_t), bt_expr(G,E2,bool_t).
bt_expr(G,or(E1,E2),bool_t) :- bt_expr(G,E1,bool_t), bt_expr(G,E2,bool_t).
bt_expr(G,app(id(F),A),T) :- parcoursArg(G,A,L2), member((F,fun_t(L2,T)), G).
bt_expr(G,app(F,A),T) :- parcoursArg(G,A,L2), bt_expr(G,F,fun_t(L2,T)).
bt_expr(G,ferm(A,E),fun_t(TA,T)) :- parcoursArgFun(A,TA), ajoutRec(G,A,G1), bt_expr(G1,E,T).
bt_expr(G,alloc(E),vec_t(_)) :- bt_expr(G, E, int_t).
bt_expr(G,len(E),int_t)  :- bt_expr(G, E, vec_t(_)).
bt_expr(G,nth(E1,E2),T) :- bt_expr(G,E1,vec_t(T)), bt_expr(G,E2,int_t).
bt_expr(G,vset(E1,E2,E3),vec_t(T)) :- bt_expr(G,E1,vec_t(T)), bt_expr(G,E2,int_t), bt_expr(G,E3,T). 

ajoutRec(G, [], G).
ajoutRec(G, [(id(X),T)|L], G1) :- 
    ajoutRec([(X,T)|G], L, G1).

parcoursArg(_, [], []). 
parcoursArg(G, [A|AL], [T|TL]) :- 
    bt_expr(G, A, T),
    parcoursArg(G, AL, TL).

parcoursArgPar(_, [], []). 
parcoursArgPar(G, [A|AL], [T|TL]) :- 
    bt_expar(G, A, T),
    parcoursArgPar(G, AL, TL).

parcoursArgFun([], []).
parcoursArgFun([(id(_),T)|AL], [T|L]) :- 
    parcoursArgFun(AL, L).

changeArg([],[]).
changeArg([(varp(X),T)|AL], [(id(X),ref(T))|L]) :- changeArg(AL,L).
changeArg([(id(X),T)|AL], [(id(X),T)|L]) :- changeArg(AL,L).

update(G1, X, T, G2) :-
    (   select((X,_), G1, G1SansX) 
    ->  G2 = [(X,T)|G1SansX]  
    ;   G2 = [(X,T)|G1]       
    ).

:-  read(X),
    bt_prog(X).