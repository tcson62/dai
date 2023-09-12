% clingo conflict_detection.lp 1_agent.lp 1_next_6.lp 1_enext_6.lp -c t=7 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr ' ' '\n' > 1_my_error_6.lp'

% expected to be false but true 
conflict(X, t) :- h(X, t), not eh(X, t).

% expected to be true but false    
conflict(X, t) :- eh(X, t), not h(X, t). 

% why X is true 
need_to_ask(AG, X, true) :- relevant(AG, X, t-1), conflict(X, t), h(X, t). 

% why X is false 
need_to_ask(AG, X, false) :- relevant(AG, X, t-1), conflict(X, t), not h(X, t). 

send_to_ask(X, V, t) :- agent(_, AG), need_to_ask(AG, X, V). 

#show. 
% #show conflict/2.
% #show need_to_ask/3.  
#show send_to_ask/3. 

