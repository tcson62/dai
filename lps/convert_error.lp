%% domain dependent part 

%%% determining whether the agent is 'broken' 

ab(AG) :- occurs(AG, Act, t), ab(AG, T), T<=t. 

executable(AG, A, t):-  action(AG, action(A)), 
                    #count {1,X : precondition(action(A), X, value(X, true)), not h(X, t)} == 0. 

#show occurs/3.  
#show ab/1. 
% #show executable/3. 
#show good(AG, A, t) :  executable(AG, A, t), occurs(AG, A, t). 
