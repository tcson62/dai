h(X, t+1) :- action(AG, action(A)), occurs(AG, A, t), executable(AG, A, t), 
                   postcondition(action(A), effect(unconditional), X, value(X, true)), not ab(AG).
                   
h(X, t+1) :- action(AG, action(A)), occurs(AG, A, t), executable(AG, A, t), 
                   h(X, t), 
                   not become_false(X, t+1). 

is_executable:- action(AG, action(A)), occurs(AG, A, t), executable(AG, A, t).

h(X, t+1) :- h(X, t), not is_executable. 
                   
become_false(X, t+1) :-  action(AG, action(A)), occurs(AG, A, t), h(X, t), executable(AG, A, t),                
                   postcondition(action(A), effect(unconditional), X, value(X, false)), not ab(AG).                  

executable(AG, A, t):-  action(AG, action(A)), 
                    #count {1,X : precondition(action(A), X, value(X, true)), not h(X, t)} == 0. 

%:- action(AG, action(A)), occurs(AG, A, t), not executable(AG, A, t), not ab(A). 
%
%:- action(AG, action(A)), action(AG1, action(A1)), A!=A1, occurs(AG, A, t), occurs(AG1, A1,t), interference(A, A1), not ab(A). not ab(A1). 

#show. 
#show h(F, t+1) : h(F, t+1).
%#show executable/3. 