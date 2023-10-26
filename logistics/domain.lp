%---------------------------------------
% domain
%---------------------------------------

% utils
boolean(true).
boolean(false).

% types
type(type("truck")).
inherits(type("truck"), type("vehicle")).
type(type("airplane")).
inherits(type("airplane"), type("vehicle")).
type(type("vehicle")).
inherits(type("vehicle"), type("physobj")).
type(type("package")).
inherits(type("package"), type("physobj")).
type(type("physobj")).
inherits(type("physobj"), type("object")).
type(type("airport")).
inherits(type("airport"), type("place")).
type(type("location")).
inherits(type("location"), type("place")).
type(type("place")).
inherits(type("place"), type("object")).
type(type("city")).
inherits(type("city"), type("object")).
type(type("object")).

has(X, type(T2)) :- has(X, type(T1)), inherits(type(T1), type(T2)).

% variables
variable(variable(("in-city", X1, X2))) :- has(X1, type("place")), has(X2, type("city")).
variable(variable(("at", X1, X2))) :- has(X1, type("physobj")), has(X2, type("place")).
variable(variable(("in", X1, X2))) :- has(X1, type("package")), has(X2, type("vehicle")).

contains(X, value(X, B)) :- variable(X), boolean(B).

% actions
2{action(X2, action(("load-truck", X1, X2, X3))); action(action(("load-truck", X1, X2, X3)))} :- has(X1, type("package")), has(X2, type("truck")), has(X3, type("place")).
precondition(action(("load-truck", X1, X2, X3)), variable(("at", X2, X3)), value(variable(("at", X2, X3)), true)) :- action(action(("load-truck", X1, X2, X3))).
precondition(action(("load-truck", X1, X2, X3)), variable(("at", X1, X3)), value(variable(("at", X1, X3)), true)) :- action(action(("load-truck", X1, X2, X3))).
postcondition(action(("load-truck", X1, X2, X3)), effect(unconditional), variable(("at", X1, X3)), value(variable(("at", X1, X3)), false)) :- action(action(("load-truck", X1, X2, X3))).
postcondition(action(("load-truck", X1, X2, X3)), effect(unconditional), variable(("in", X1, X2)), value(variable(("in", X1, X2)), true)) :- action(action(("load-truck", X1, X2, X3))).

2{action(X2, action(("load-airplane", X1, X2, X3))); action(action(("load-airplane", X1, X2, X3)))} :- has(X1, type("package")), has(X2, type("airplane")), has(X3, type("place")).
precondition(action(("load-airplane", X1, X2, X3)), variable(("at", X1, X3)), value(variable(("at", X1, X3)), true)) :- action(action(("load-airplane", X1, X2, X3))).
precondition(action(("load-airplane", X1, X2, X3)), variable(("at", X2, X3)), value(variable(("at", X2, X3)), true)) :- action(action(("load-airplane", X1, X2, X3))).
postcondition(action(("load-airplane", X1, X2, X3)), effect(unconditional), variable(("at", X1, X3)), value(variable(("at", X1, X3)), false)) :- action(action(("load-airplane", X1, X2, X3))).
postcondition(action(("load-airplane", X1, X2, X3)), effect(unconditional), variable(("in", X1, X2)), value(variable(("in", X1, X2)), true)) :- action(action(("load-airplane", X1, X2, X3))).

2{action(X2, action(("unload-truck", X1, X2, X3))); action(action(("unload-truck", X1, X2, X3)))} :- has(X1, type("package")), has(X2, type("truck")), has(X3, type("place")).
precondition(action(("unload-truck", X1, X2, X3)), variable(("at", X2, X3)), value(variable(("at", X2, X3)), true)) :- action(action(("unload-truck", X1, X2, X3))).
precondition(action(("unload-truck", X1, X2, X3)), variable(("in", X1, X2)), value(variable(("in", X1, X2)), true)) :- action(action(("unload-truck", X1, X2, X3))).
postcondition(action(("unload-truck", X1, X2, X3)), effect(unconditional), variable(("in", X1, X2)), value(variable(("in", X1, X2)), false)) :- action(action(("unload-truck", X1, X2, X3))).
postcondition(action(("unload-truck", X1, X2, X3)), effect(unconditional), variable(("at", X1, X3)), value(variable(("at", X1, X3)), true)) :- action(action(("unload-truck", X1, X2, X3))).

2{action(X2, action(("unload-airplane", X1, X2, X3))); action(action(("unload-airplane", X1, X2, X3)))} :- has(X1, type("package")), has(X2, type("airplane")), has(X3, type("place")).
precondition(action(("unload-airplane", X1, X2, X3)), variable(("in", X1, X2)), value(variable(("in", X1, X2)), true)) :- action(action(("unload-airplane", X1, X2, X3))).
precondition(action(("unload-airplane", X1, X2, X3)), variable(("at", X2, X3)), value(variable(("at", X2, X3)), true)) :- action(action(("unload-airplane", X1, X2, X3))).
postcondition(action(("unload-airplane", X1, X2, X3)), effect(unconditional), variable(("in", X1, X2)), value(variable(("in", X1, X2)), false)) :- action(action(("unload-airplane", X1, X2, X3))).
postcondition(action(("unload-airplane", X1, X2, X3)), effect(unconditional), variable(("at", X1, X3)), value(variable(("at", X1, X3)), true)) :- action(action(("unload-airplane", X1, X2, X3))).

2 {action(X1, action(("drive-truck", X1, X2, X3, X4)));  action(action(("drive-truck", X1, X2, X3, X4)))} :- has(X1, type("truck")), has(X2, type("place")), has(X3, type("place")), X2 != X3, has(X4, type("city")).
precondition(action(("drive-truck", X1, X2, X3, X4)), variable(("at", X1, X2)), value(variable(("at", X1, X2)), true)) :- action(action(("drive-truck", X1, X2, X3, X4))).
precondition(action(("drive-truck", X1, X2, X3, X4)), variable(("in-city", X2, X4)), value(variable(("in-city", X2, X4)), true)) :- action(action(("drive-truck", X1, X2, X3, X4))).
precondition(action(("drive-truck", X1, X2, X3, X4)), variable(("in-city", X3, X4)), value(variable(("in-city", X3, X4)), true)) :- action(action(("drive-truck", X1, X2, X3, X4))).
postcondition(action(("drive-truck", X1, X2, X3, X4)), effect(unconditional), variable(("at", X1, X2)), value(variable(("at", X1, X2)), false)) :- action(action(("drive-truck", X1, X2, X3, X4))).
postcondition(action(("drive-truck", X1, X2, X3, X4)), effect(unconditional), variable(("at", X1, X3)), value(variable(("at", X1, X3)), true)) :- action(action(("drive-truck", X1, X2, X3, X4))).

2 {action(X1, action(("fly-airplane", X1, X2, X3))); action(action(("fly-airplane", X1, X2, X3)))} :- has(X1, type("airplane")), has(X2, type("airport")), has(X3, type("airport")), X2 != X3.
precondition(action(("fly-airplane", X1, X2, X3)), variable(("at", X1, X2)), value(variable(("at", X1, X2)), true)) :- action(action(("fly-airplane", X1, X2, X3))).
postcondition(action(("fly-airplane", X1, X2, X3)), effect(unconditional), variable(("at", X1, X2)), value(variable(("at", X1, X2)), false)) :- action(action(("fly-airplane", X1, X2, X3))).
postcondition(action(("fly-airplane", X1, X2, X3)), effect(unconditional), variable(("at", X1, X3)), value(variable(("at", X1, X3)), true)) :- action(action(("fly-airplane", X1, X2, X3))).

%%%%%%%%%% 

agent(X) :- has(X, type("airplane")).
agent(X) :- has(X, type("truck")).

%conflict(A, B) :- action(Ag, A), action(Ag, B), A!=B. 

interference(A, B) :- action(Ag1, action(A)), action(Ag2, action(B)), Ag1!=Ag2, 
                         precondition(action(A), F, value(F, _)),
                         precondition(action(B), F, value(F, _)).
                          
interference(A, B) :- action(Ag1, action(A)), action(Ag2, action(B)), Ag1!=Ag2, 
                         postcondition(action(A), effect(unconditional), F, value(F, true)),
                         postcondition(action(B), effect(unconditional), F, value(F, false)).

%% #show interference/2. 

%%%%%%%%%% 



