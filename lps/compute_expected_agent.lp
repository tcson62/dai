% clingo compute_expected_agent.lp domain.lp prob1_apn1.lp 1_agent.lp 1_occ_5.lp 1_enext_4.lp -c t=5 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr ' ' '\n'  > 1_enext_5.lp'

h(F, t) :- eh(F, t). 

h(X, t+1) :- action(AG, action(A)), occurs(AG, A, t), 
             postcondition(action(A), effect(unconditional), X, value(X, true)), not ab(A).
                   
h(X, t+1) :- % action(AG, action(A)), occurs(AG, A, t), 
             h(X, t), 
             not become_false(X, t+1). 
                   
become_false(X, t+1) :-  action(AG, action(A)), occurs(AG, A, t), h(X, t),                
             postcondition(action(A), effect(unconditional), X, value(X, false)), not ab(A).                  

executable(AG, A, t):-  action(AG, action(A)), 
             #count {1,X : precondition(action(A), X, value(X, true)), not h(X, t)} == 0. 

pre_relevant(AG, X, t) :- action(action(A)), occurs(AG, A, t), precondition(action(A), X, value(X, _)).

post_relevant(AG, X, t) :- action(action(A)), occurs(AG, A, t), postcondition(action(A), effect(unconditional), X, value(X, _)).

%relevant(AG, X, t+1) :- agent(_, AG), h(X, t+1).
%relevant(AG, X, t+1) :- agent(_, AG), variable(X), not h(X, t+1).
%relevant(AG, X) :- action(AG, action(A)), precondition(action(A), X, value(X, _)).
%relevant(AG, X) :- action(AG, action(A)), postcondition(action(A), effect(unconditional), X, value(X, _)).



#show. 
#show eh(F, t+1) : h(F, t+1).
#show pre_relevant/3.   
#show post_relevant/3.   
