%---------------------------------------
% problem
%---------------------------------------


% objects
 constant(constant("apn1")).
 has(constant("apn1"), type("airplane")).

constant(constant("apt1")).
has(constant("apt1"), type("airport")).

constant(constant("apt2")).
has(constant("apt2"), type("airport")).

%constant(constant("pos2")).
%has(constant("pos2"), type("location")).

constant(constant("pos1")).
has(constant("pos1"), type("location")).

constant(constant("cit2")).
has(constant("cit2"), type("city")).

constant(constant("cit1")).
has(constant("cit1"), type("city")).

%constant(constant("tru2")).
%has(constant("tru2"), type("truck")).

constant(constant("tru1")).
has(constant("tru1"), type("truck")).

constant(constant("obj23")).
has(constant("obj23"), type("package")).

constant(constant("obj22")).
has(constant("obj22"), type("package")).

constant(constant("obj21")).
has(constant("obj21"), type("package")).

constant(constant("obj13")).
has(constant("obj13"), type("package")).

constant(constant("obj12")).
has(constant("obj12"), type("package")).

constant(constant("obj11")).
has(constant("obj11"), type("package")).

% initial state
%initialState(variable(("at", constant("apn1"), constant("apt2"))), value(variable(("at", constant("apn1"), constant("apt2"))), true)).
initialState(variable(("at", constant("tru1"), constant("pos1"))), value(variable(("at", constant("tru1"), constant("pos1"))), true)).
initialState(variable(("at", constant("obj11"), constant("pos1"))), value(variable(("at", constant("obj11"), constant("pos1"))), true)).
initialState(variable(("at", constant("obj12"), constant("pos1"))), value(variable(("at", constant("obj12"), constant("pos1"))), true)).
initialState(variable(("at", constant("obj13"), constant("pos1"))), value(variable(("at", constant("obj13"), constant("pos1"))), true)).
% initialState(variable(("at", constant("tru2"), constant("pos2"))), value(variable(("at", constant("tru2"), constant("pos2"))), true)).
initialState(variable(("at", constant("obj21"), constant("pos2"))), value(variable(("at", constant("obj21"), constant("pos2"))), true)).
initialState(variable(("at", constant("obj22"), constant("pos2"))), value(variable(("at", constant("obj22"), constant("pos2"))), true)).
initialState(variable(("at", constant("obj23"), constant("pos2"))), value(variable(("at", constant("obj23"), constant("pos2"))), true)).
initialState(variable(("in-city", constant("pos1"), constant("cit1"))), value(variable(("in-city", constant("pos1"), constant("cit1"))), true)).
initialState(variable(("in-city", constant("apt1"), constant("cit1"))), value(variable(("in-city", constant("apt1"), constant("cit1"))), true)).
initialState(variable(("in-city", constant("pos2"), constant("cit2"))), value(variable(("in-city", constant("pos2"), constant("cit2"))), true)).
initialState(variable(("in-city", constant("apt2"), constant("cit2"))), value(variable(("in-city", constant("apt2"), constant("cit2"))), true)).

initialState(X, value(X, false)) :- variable(X), not initialState(X, value(X, true)).

% goal
%goal(variable(("at", constant("obj11"), constant("pos2"))), value(variable(("at", constant("obj11"), constant("pos2"))), true)).
goal(variable(("at", constant("obj23"), constant("pos1"))), value(variable(("at", constant("obj23"), constant("pos1"))), true)).
%goal(variable(("at", constant("obj13"), constant("apt1"))), value(variable(("at", constant("obj13"), constant("apt1"))), true)).
%goal(variable(("at", constant("obj21"), constant("pos1"))), value(variable(("at", constant("obj21"), constant("pos1"))), true)).

%#show agent/1. 
%#show action/2. 

