%---------------------------------------
% domain
%---------------------------------------

% utils
boolean(true).
boolean(false).


% variables
variable(variable(("at", A, X1, X2))) :- agent(A), loc(X1, X2), not block(X1, X2).  
variable(variable(("occupied", X1, X2))) :- loc(X1, X2), not block(X1, X2).  

up((X1, X2), (X1, X2+1)) :- loc(X1, X2), loc(X1, X2+1), not block(X1, X2+1).
down((X1, X2), (X1, X2-1)) :- loc(X1, X2), loc(X1, X2-1), not block(X1, X2-1).
left((X1, X2), (X1-1, X2)) :- loc(X1, X2), loc(X1-1, X2), not block(X1-1, X2).
right((X1, X2), (X1+1, X2)) :- loc(X1, X2), loc(X1+1, X2), not block(X1+1, X2).

% actions
2{action(A, action(("up", A, X1, X2, X3))); action(action(("up", A, X1, X2, X3)))} :- agent(A), loc(X1, X2), up((X1,X2), (X1, X3)).   

precondition(action(("up", A, X1, X2, X3)), variable(("at", A, X1, X2)), value(variable(("at", A, X1, X2)), true)) :- action(A, action(("up", A, X1, X2, X3))).
precondition(action(("up", A, X1, X2, X3)), variable(("occupied", X1, X3)), value(variable(("occupied", X1, X3)), false)) :- action(A, action(("up", A, X1, X2, X3))).

postcondition(action(("up", A, X1, X2, X3)), effect(unconditional), variable(("at", A, X1, X2)), value(variable(("at", A, X1, X2)), false)) :- action(A, action(("up", A, X1, X2, X3))).
postcondition(action(("up", A, X1, X2, X3)), effect(unconditional), variable(("at", A, X1, X3)), value(variable(("at", A, X1, X3)), true)) :- action(A, action(("up", A, X1, X2, X3))).
postcondition(action(("up", A, X1, X2, X3)), effect(unconditional), variable(("occupied", X1, X2)), value(variable(("occupied", X1, X2)), false)) :- action(A, action(("up", A, X1, X2, X3))).
postcondition(action(("up", A, X1, X2, X3)), effect(unconditional), variable(("occupied", X1, X3)), value(variable(("occupied", X1, X3)), true)) :- action(A, action(("up", A, X1, X2, X3))).

% actions
2{action(A, action(("down", A, X1, X2, X3))); action(action(("down", X1, X2, X3)))} :- agent(A), loc(X1, X2), down((X1,X2), (X1, X3)).  
precondition(action(("down", A, X1, X2, X3)), variable(("at", A, X1, X2)), value(variable(("at", A, X1, X2)), true)) :- action(A, action(("down", A, X1, X2, X3))).
precondition(action(("down", A, X1, X2, X3)), variable(("occupied", X1, X3)), value(variable(("occupied", X1, X3)), false)) :- action(A, action(("down", A, X1, X2, X3))).

postcondition(action(("down", A, X1, X2, X3)), effect(unconditional), variable(("at", A, X1, X2)), value(variable(("at", A, X1, X2)), false)) :- action(A, action(("down", A, X1, X2, X3))).
postcondition(action(("down", A, X1, X2, X3)), effect(unconditional), variable(("at", A, X1, X3)), value(variable(("at", A, X1, X3)), true)) :- action(A, action(("down", A, X1, X2, X3))).
postcondition(action(("down", A, X1, X2, X3)), effect(unconditional), variable(("occupied", X1, X2)), value(variable(("occupied", X1, X2)), false)) :- action(A, action(("down", A, X1, X2, X3))).
postcondition(action(("down", A, X1, X2, X3)), effect(unconditional), variable(("occupied", X1, X3)), value(variable(("occupied", X1, X3)), true)) :- action(A, action(("down", A, X1, X2, X3))).



% actions
2{action(A, action(("left", A, X1, X2, X3))); action(action(("left", A, X1, X2, X3)))} :- agent(A), loc(X1, X2), left((X1,X2), (X3, X2)).   
precondition(action(("left", A, X1, X2, X3)), variable(("at", A, X1, X2)), value(variable(("at", A, X1, X2)), true)) :- action(A, action(("left", A, X1, X2, X3))).
precondition(action(("left", A, X1, X2, X3)), variable(("occupied", X1, X3)), value(variable(("occupied", X3, X2)), false)) :- action(A, action(("left", A, X1, X2, X3))).

postcondition(action(("left", A, X1, X2, X3)), effect(unconditional), variable(("at", A, X1, X2)), value(variable(("at", A, X1, X2)), false)) :- action(A, action(("left", A, X1, X2, X3))).
postcondition(action(("left", A, X1, X2, X3)), effect(unconditional), variable(("at", A, X3, X2)), value(variable(("at", A, X3, X2)), true)) :- action(A, action(("left", A, X1, X2, X3))).
postcondition(action(("left", A, X1, X2, X3)), effect(unconditional), variable(("occupied", X1, X2)), value(variable(("occupied", X1, X2)), false)) :- action(A, action(("left", A, X1, X2, X3))).
postcondition(action(("left", A, X1, X2, X3)), effect(unconditional), variable(("occupied", X3, X2)), value(variable(("occupied", X3, X2)), true)) :- action(A, action(("left", A, X1, X2, X3))).


% actions
2{action(A, action(("right", A, X1, X2, X3))); action(action(("right", A, X1, X2, X3)))} :- agent(A), loc(X1, X2), right((X1,X2), (X3, X2)).   
precondition(action(("right", A, X1, X2, X3)), variable(("at", A, X1, X2)), value(variable(("at", A, X1, X2)), true)) :- action(A, action(("right", A, X1, X2, X3))).
precondition(action(("right", A, X1, X2, X3)), variable(("occupied", X1, X3)), value(variable(("occupied", X3, X2)), false)) :- action(A, action(("left", A, X1, X2, X3))).

postcondition(action(("right", A, X1, X2, X3)), effect(unconditional), variable(("at", A, X1, X2)), value(variable(("at", A, X1, X2)), false)) :- action(A, action(("right", A, X1, X2, X3))).
postcondition(action(("right", A, X1, X2, X3)), effect(unconditional), variable(("at", A, X3, X2)), value(variable(("at", A, X3, X2)), true)) :- action(A, action(("right", A, X1, X2, X3))).
postcondition(action(("right", A, X1, X2, X3)), effect(unconditional), variable(("occupied", X1, X2)), value(variable(("occupied", X1, X2)), false)) :- action(A, action(("right", A, X1, X2, X3))).
postcondition(action(("right", A, X1, X2, X3)), effect(unconditional), variable(("occupied", X3, X2)), value(variable(("occupied", X3, X2)), true)) :- action(A, action(("right", A, X1, X2, X3))).

% actions
2{action(A, action(("stay", A, X1, X2))); action(action(("stay", A, X1, X2)))} :- agent(A), loc(X1, X2).  
precondition(action(("stay", A, X1, X2)), variable(("at", A, X1, X2)), value(variable(("at", A, X1, X2)), true)) :- action(A, action(("stay", A, X1, X2))).
postcondition(action(("stay", A, X1, X2)), effect(unconditional), variable(("at", A, X1, X2)), value(variable(("at", A, X1, X2)), true)) :- action(A, action(("stay", A, X1, X2))).

%%%%%%%%%% 
                          
interference(A, B) :- action(Ag1, action(A)), action(Ag2, action(B)), Ag1!=Ag2, 
                         postcondition(action(A), effect(unconditional), variable(("at", Ag1, X1, X2)), value(variable(("at", Ag1, X1, X2)), true)),
                         postcondition(action(B), effect(unconditional), variable(("at", Ag2, X1, X2)), value(variable(("at", Ag2, X1, X2)), true)).

interference(A, B) :- action(Ag1, action(A)), action(Ag2, action(B)), Ag1!=Ag2, 
                         precondition(action(A), variable(("at", Ag1, X1, X2)), value(variable(("at", Ag1, X1, X2)), true)),
                         postcondition(action(B), effect(unconditional), variable(("at", Ag2, X1, X2)), value(variable(("at", Ag2, X1, X2)), true)), 
                         precondition(action(B), variable(("at", Ag2, X3, X4)), value(variable(("at", Ag2, X3, X4)), true)),
                         postcondition(action(A), effect(unconditional), variable(("at", Ag1, X3, X4)), value(variable(("at", Ag1, X3, X4)), true)).

%% #show interference/2. 

%%%%%%%%%% 



