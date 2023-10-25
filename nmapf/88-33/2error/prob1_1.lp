loc(0..8, 0..8).
agent(1).
block(X,Y) :- loc(X,Y), X = 4, Y!= 4.

% initial state
initialState(variable(("at", 1, 7, 7)), value(variable(("at", 1, 7, 7)), true)).
%initialState(variable(("at", 2, 0, 0)), value(variable(("at", 2, 0, 0)), true)).
%initialState(variable(("at", 3, 6, 0)), value(variable(("at", 3, 6, 0)), true)).

% goal
goal(variable(("at",  1, 0 ,3)), value(variable(("at", 1, 0 ,3)), true)).
%goal(variable(("at",  2, 7 ,0)), value(variable(("at", 2, 7 ,0)), true)).
%goal(variable(("at",  3, 2 ,1)), value(variable(("at", 3, 2 ,1)), true)).
