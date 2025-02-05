bt_prog(prog(Cs)) :- is_init_env(G), bt_cmds(G,Cs).

is_init_env(G) :- G = [ (true, bool_t),
    (false, bool_t),
    (not, fun_t(bool_t, bool_t)),
    (eq, fun_t([int_t,int_t], bool_t)),
    (lt, fun_t([int_t,int_t], bool_t)),
    (add, fun_t([int_t,int_t], int_t)),
    (sub, fun_t([int_t,int_t], int_t)),
    (mul, fun_t([int_t,int_t], int_t)),
    (div, fun_t([int_t,int_t], int_t))].

%% commandes
bt_cmds(G,[X]) :- bt_stat(G,X).
bt_cmds(G,[T|L]) :- bt_def(G,T,G2), bt_cmds(G2,L).

%% instruction
bt_stat(G,echo(E)) :- bt_expr(G,E,int_t).

%% definitions
bt_def(G1,const(id(X),T,E),G2) :- bt_expr(G1,E,T), G2 = [(X,T)|G1].
bt_def(G1,fun_t(id(X),T,A,E),G2) :- ajoutRec(G1,A,G3), 
                                    bt_expr(G3,E,T),
                                    parcoursArg(A,TR),
                                    G2 = [(X,(TR,T))|G1].




%% expressions
bt_expr(_,num(_),int_t).
bt_expr(G,id(X),T) :- member((X,T),G).
bt_expr(G,if(E1,E2,E3),T) :- bt_expr(G,E1,bool_t), bt_expr(G,E2,T), bt_expr(G,E3,T).
bt_expr(G,and(E1,E2),bool_t) :- bt_expr(G,E1,bool_t), bt_expr(G,E2,bool_t).
bt_expr(G,or(E1,E2),bool_t) :- bt_expr(G,E1,bool_t), bt_expr(G,E2,bool_t).
bt_expr(G,app(id(F),A),T) :- parcoursArg(A,L2), member((F,(L2,T)), G).


ajoutRec(G,[],G).
ajoutRec(G, [(id(X),T)|L], G1) :- G2 = [(X,T)|G], ajoutRec(G2, L, G1).

parcoursArg([],_).
parcoursArg([(id(_),T1)|L], [T1|L2]) :- parcoursArg(L,[T1|L2]).


:-  read(The_program),
    bt_prog(The_program).