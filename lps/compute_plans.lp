#include <incmode>.
 
h(X, 1) :- initialState(X, value(X, true)).
  
#program step(t).

time(t). 

0 {occurs(AG, A, t) : action(AG, action(A))} 1 :-  agent(AG), time(t).  

h(X, t) :- action(AG, action(A)), occurs(AG, A, t-1), 
             postcondition(action(A), effect(unconditional), X, value(X, true)), not ab(A).
                   
h(X, t) :- % action(AG, action(A)), occurs(AG, A, t-1), 
             h(X, t-1), 
             not become_false(X, t). 
                   
become_false(X, t) :-  action(AG, action(A)), occurs(AG, A, t-1), h(X, t-1),                
             postcondition(action(A), effect(unconditional), X, value(X, false)), not ab(A).                  

executable(AG, A, t):-  action(AG, action(A)), 
             #count {1,X : precondition(action(A), X, value(X, true)), not h(X, t)} == 0. 
             
:- occurs(AG, A, t), not executable(AG, A, t).              
             
:- action(Ag1, action(A)), action(Ag2, action(B)), Ag1!=Ag2,  interference(A, B), occurs(Ag1, A, t), occurs(Ag2, B, t).            

#program check(t).

% goal(variable(("at",  1, 0 ,3)), value(variable(("at", 1, 0 ,3)), true)).

:- query(t), goal(X, value(X, true)), not h(X, t). 

#show. 
#show occurs/3.   
#show time/1.