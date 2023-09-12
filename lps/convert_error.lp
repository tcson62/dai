%% domain dependent part 

%%% determining whether the agent is 'broken' 

ab(AG) :- occurs(AG, Act, t), ab(AG, T), T<=t. 

#show occurs/3.  
#show ab/1. 
 