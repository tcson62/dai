loc(0..10, 0..10).
agent(4).
block(X,Y) :- loc(X,Y), X = 5, Y!= 5.

% initial state
%initialState(variable(("at", 1, 7, 6)), value(variable(("at", 1, 7, 6)), true)).
%initialState(variable(("at", 2, 3, 2)), value(variable(("at", 2, 3, 2)), true)).
%initialState(variable(("at", 3, 7, 5)), value(variable(("at", 3, 7, 5)), true)).
initialState(variable(("at", 4, 0, 5)), value(variable(("at", 4, 0, 5)), true)).
% initialState(variable(("occupied", 0, 5)), value(variable(("occupied", 0, 5)), true)).


%initialState(variable(("at", 5, 2, 4)), value(variable(("at", 5, 2, 4)), true)).

% goal
%goal(variable(("at",  1, 2 ,1)), value(variable(("at", 1, 2 ,1)), true)).
%goal(variable(("at",  2, 7 ,1)), value(variable(("at", 2, 7 ,1)), true)).
%goal(variable(("at",  3, 0 ,2)), value(variable(("at", 3, 0 ,2)), true)).
goal(variable(("at",  4, 8 ,0)), value(variable(("at", 4, 8 ,0)), true)).
%goal(variable(("at",  5, 6 ,5)), value(variable(("at", 5, 6 ,5)), true)).
