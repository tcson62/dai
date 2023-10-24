step(1..t).

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

% 2{ab(AG'); reason(AG, A, T, AG')} :- occurs(AG, A, T), good(AG, A, T), #count{A',T' : occurs(AG, A', T'), good(AG, A', T'), step(T'), T < T'} == 0, 
%               occurs(AG', B, t), step(T), conflict(A, B), ab(AG). 


ab(AG'):- occurs(AG, A, T), good(AG, A, T), #count{A',T' : occurs(AG, A', T'), good(AG, A', T'), step(T'), T < T'} == 0, 
               occurs(AG', B, t), step(T), conflict(A, B), ab(AG). 

#show. 
#show h(F, t+1) : h(F, t+1).
%#show executable/3. 
#show ab/1. 
% #show conflict/2. 
% #show reason/4. 
