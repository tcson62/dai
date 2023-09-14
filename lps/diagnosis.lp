% clingo diagnosis.lp AGENT_DOMAIN AGENT_PROB AGENT_OCCRENCE CURRENT_STATE NEXT_STATE EXPECTED_STATE  
% TIME_STEP t is CURRENT_STEP+1 
% clingo diagnosis.lp domain.lp prob1_apn1.lp 1_occ_6.lp 
% 1_agent.lp 1_next_6.lp 1_enext_6.lp -c t=7 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr ' ' '\n' > 1_my_error_6.lp'

% clingo diagnosis.lp domain.lp prob1_apn1.lp 1_next_5.lp 1_occ_6.lp 1_agent.lp 1_next_6.lp 1_enext_6.lp -c t=7 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr ' ' '\n' | grep exe
% clingo diagnosis.lp domain.lp prob1_tru2.lp 3_next_0.lp 3_occ_1.lp 3_agent.lp 3_next_1.lp 3_enext_1.lp -c t=2 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr ' ' '\n' | grep exe

% question(other,("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),variable(("at",constant("obj23"),constant("apt2"))),false,4).

%% something is false 
%% the action is not executable 
%% then some precondition needs explanation 



complement(true, false). 
complement(false, true). 

% expected to be false but true 
conflict(X, t) :- h(X, t), not eh(X, t).

% expected to be true but false    
conflict(X, t) :- eh(X, t), not h(X, t). 

% why X is true 
need_to_ask(AG, X, true) :- relevant(AG, X, t-1), conflict(X, t), h(X, t). 

% why X is false 
need_to_ask(AG, X, false) :- relevant(AG, X, t-1), conflict(X, t), not h(X, t). 

relevant(AG, X, S) :- pre_relevant(AG, X, S). 

relevant(AG, X, S) :- post_relevant(AG, X, S). 

send_to_ask(X, V, t) :- agent(_, AG), need_to_ask(AG, X, V). 

executable(AG, A, S):-  action(AG, action(A)), S < t, 
                    occurs(AG, A, S),  
                    #count {1,X : precondition(action(A), X, value(X, true)), not h(X, S)} == 0. 

non_executable(AG, A, S):-  action(AG, action(A)), S < t, 
                    occurs(AG, A, S),  
                    precondition(action(A), X, value(X, true)), not h(X, S). 

non_executable(AG, A, X):-  action(AG, action(A)), S < t, 
                    occurs(AG, A, S),  
                    precondition(action(A), X, value(X, false)), h(X, S). 

question(self, A, X, false, t-1) :- agent(_, AG), 
  eh(X, t), 
  not h(X, t),
  action(AG, action(A)), postcondition(action(A), effect(unconditional), X, value(X, true)), 
  executable(AG, A, t-1), 
  occurs(AG, A, t-1).

question(self, A, X, true, t-1) :- agent(_, AG), 
  not eh(X, t), 
  h(X, t),
  action(AG, action(A)), postcondition(action(A), effect(unconditional), X, value(X, false)), 
  executable(AG, A, t-1), 
  occurs(AG, A, t-1).

question(self, A, X, false, t) :- agent(_, AG), 
  non_executable(AG, A, S), 
  action(AG, action(A)), postcondition(action(A), effect(unconditional), Y, value(X, true)), 
  not h(X, t), 
  occurs(AG, A, S), S < t.

question(self, A, X, true, t) :- agent(_, AG), 
  non_executable(AG, A, S), 
  action(AG, action(A)), postcondition(action(A), effect(unconditional), X, value(X, false)), 
  h(X, t-1), 
  occurs(AG, A, S), S < t.

question(other, A, X, false, t-1) :- agent(_, AG), 
  non_executable(AG, A, t-1), 
  precondition(action(A), X, value(X, true)), 
  not h(X, t-1), 
  occurs(AG, A, t-1).

question(other, A, X, true, t-1) :- agent(_, AG), 
  non_executable(AG, A, t-1), 
  precondition(action(A), X, value(X, false)), 
  h(X, t-1), 
  occurs(AG, A, t-1).

% question(other, no, X, true, t-1) :- agent(_, AG), 
%  not relevant(AG, X, t-1), 
%  conflict(X, t). 


%#show. 
%#show question/5.
% #show need_to_ask/3.  
%#show send_to_ask/3. 

