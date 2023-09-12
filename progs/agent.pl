:- use_module(library(tipc/tipc_linda)).
:- use_module(library(tipc/tipc)).
:- use_module(library(system)).
:- use_module(library(process)).
:- use_module(library(readutil)).

:- tipc:tipc_initialize.

:- dynamic plan/1.
:- dynamic send_to_ask/3. 
:- multifile send_to_ask/3.

:- dynamic need_to_ask/1. 

:- multifile question/5. 
:- dynamic question/5. 
:- dynamic answers/2. 
:- dynamic asked/4. 

:- dynamic mystop.
     
:- dynamic being_asked/5.        %%% asking agent or being_asked - agent - fluent - value - step [1/0, Ind, F , V, S]  

:- dynamic myCheck.

%%% Version 1

%% agent start with an index 

%%% agent(index, agent_name).
%%% The agent configuration file should contain 
%%% the file encoding the problem    *problem(file_name)* 
%%% the file encoding the domain     *domain(file_name)*
%%% a single atom of the form *agent(i, agent_name)* 

/* 

Message format msg(From, To, Type, Content)

Message type 

Sending: to 0 => 
         1 - action occurrence or none 
         3 - done 
         5 - need to wait for diagnosis 
         10 - done with current step 
         

Receiving: from 0 
         2 - done or next 
         4 - wait 

Receiving from others: question or answer 

	- question has the format: message id, type, fluent (f), value (v), time step (t)
	  message id is created by the agent 
	  type is 0 and 1 - 0 for originator and 1 for forwarder 
	  why fluent f has the value v at the step t 
	  when it is supposed to have value -v at the step t  
	  
	  if an agent receives a question with the same message id and it is 
	  an originator of the question then it will respond to the agent 
	  sending this question with unknown 

	- answer has the format: message id, agent id, and time step? 
	   * agent id = 
	     the id of the agent that should have set the value 
	     of f to v but failed 
	   * unknown 

Once an agent receives a question it tries to see if it was the 
reason for it then it will provide the answer. Otherwise, it will ask its 
neighbors. It will provide the first answer that it receives. 
If everyone returns unknown then it will provide unknown as answer.  

*/ 

%% turn on propagation of questioning process

set_diagnosis_mode(propagate).

%%% set_diagnosis_mode(single).

%%% Auxiliary predicates 

setMyDebug(true). 

myFormat(X, Y) :- 
   setMyDebug(Trace), 
   (Trace == true -> format(X,Y); true). 

myFormat(X) :- 
   setMyDebug(Trace), 
   (Trace == true -> length(X, NX), format('Print a list ~n: #elements~q ~n ~q ~n', [NX, X]); true). 


myErrorFile(AID, Step, ERRFile) :-  
     atom_concat(AID, '_my_error_', ERR1),
     atom_concat(ERR1, Step, ERR2),
     atom_concat(ERR2, '.lp', ERRFile).
     
myStepFile(AID, Step, FileName):- 
     atom_concat(AID, '_next_', PR1),
     atom_concat(PR1, Step, PR2),
     atom_concat(PR2, '.lp', FileName).

myOccurrenceFile(AID, Step, OCCFile) :- 
     atom_concat(AID, '_occ_',    OCC1),
     atom_concat(OCC1, Step,      OCC2),
     atom_concat(OCC2,'.lp',      OCCFile).     %1_occ_k.lp

myExpectedStep(AID, Step, EXPTFile):- 		
     atom_concat(AID,   '_enext_',    ENEXT1),
     atom_concat(ENEXT1, Step, ENEXT2),
     atom_concat(ENEXT2,'.lp', EXPTFile).      %1_enext_(k-1).lp

% Convert a list of strings to a command line 
     
toCmd(L, _):- length(L, N), N < 2, fail.

toCmd([A,B], X):- atom_concat(A, B, X).

toCmd([H|T], X):- length(T, L), L>=2, 
     toCmd(T, Y), 
     atom_concat(H, Y, X).     

% dump a list of string to a file 

dump_file([], _).  

dump_file([Head | Tail], FileHd):-
     assertz(occurrences(Head)),
     term_string(Head, ST), 
     write(FileHd, ST),
     write(FileHd, '.'), 
     nl(FileHd), 
     dump_file(Tail, FileHd).       

dump_to_file(List, File):- 
     open(File, write, Out), 
     findall(X, (member(X, List), X \== "none", X \= none), List1), 
     dump_file(List1, Out),
     nl(Out),  
     close(Out). 

dump_to_file_occ(Step, Fname):-
     open(File, write, Out),
     S1 is Step + 1, 
     findall(occurs(AG, A, T), (occurs(AG, A, T), T < S1), List1), 
     dump_file(List1, Out),
     nl(Out),  
     close(Out).      


%%% End auxiliary predicates 

%%% load information from agent configuration file,
%%% start the process 

start :-
    use_module(library(tipc/tipc_linda)),
    tipc:tipc_initialize, 
    linda_client(global).

load_agent_config(Config):-
     % read the configuation 
     start,  
     load_files(Config), 
     findall(Occs, occurs(_, Occs, _), LOccurs), 
     length(LOccurs, NSteps),
     domain(Domain), 
     problem(Problem), 
     format('Number of steps ~q~n Domain ~q~n Problem ~q~n', [NSteps, Domain, Problem]),
     % start the simulation process 
     agent(AID, Name),
     out(agent(AID, Name)),

     term_string(Name, AgentName),
     toCmd([AID, '_agent.lp'], FileName),
     toCmd([agent, '(',AID, ',', AgentName,')'], AAtom),
     term_string(TAtom, AAtom), 
     dump_to_file([TAtom], FileName),
     
     
     format('Done starting ~n',[]),
     preparation(AID, Domain, Problem),     
     agent_step(AID, Domain, Problem, 1),
     format('Done executing ~n',[]),
     out(msg(AID, 0, 3, done)), 
     findall(answers(X,Y), answers(X,Y), LAnswers), 
     myFormat(LAnswers),
     findall((P,Q,R,S), being_asked(_,P,Q,R,S), LAsks), 
     myFormat(LAsks).
%%% 

gettingOccurrence(Step, Occurrence):-  
    ( occurs(N, A, Step) 
       ->
       term_string(occurs(N, A, Step), Occurrence)
       ; 
       term_string(none, Occurrence)
     ). 
        
preparation(AID, Domain, Problem):-
     format('In preparation ~n',[]),

     getenv('SHELL', Shell),                
     myStepFile(AID, 0, StepFile),
     atom_concat(AID, '_fluents.lp', FluentFile), 

     myFormat('*********** ~n Computing initial state ~n **************~n ', []), 
        
     myFormat('Executing: clingo ../../../lps/compute_0.lp  ~q ~q --outf=0 -V0 --out-atomf=%s. | head -n1  > ~q ~n!', [Domain, Problem, StepFile]),  
     	
     process_create(Shell, ['-c', ['clingo ../../../lps/compute_0.lp ', Domain, ' ', Problem, ' --outf=0 -V0 --out-atomf=%s. | head -n1  > ', StepFile]], [process(P1)]),
     process_wait(P1,exit(_)),         
        
     myFormat('*********** ~n Computing the fluents ~n **************~n ', []), 
     
     process_create(Shell, ['-c', ['clingo ../../../lps/compute_fluents.lp ', Domain, ' ', Problem, ' --outf=0 -V0 --out-atomf=%s. | head -n1  > ', FluentFile]], [process(P2)]),
     process_wait(P2,exit(_)).     

% computing the next state 
% given the action occurrence and the current state of the world (observation) 

agent_next_state(AID, Step, Domain, Problem):-    
        myFormat('Agent computing next step and diagnosis ~q ~n',[Step]),
        Previous is Step - 1,
        Next is Step + 1,
        myFormat('In agent ~q solving step ~q ...~n ',[AID, Step]), 

        getenv('SHELL', Shell),                
        atom_concat(AID, '_agent.lp',AgentFile),   %1_agent.lp     
        myOccurrenceFile(AID, Step, OCCFile),  
        myExpectedStep(AID, Previous, ENEXTFile), 
        atom_concat(' -c t=', Step, COnstant),           %-c t=k 
	myExpectedStep(AID, Step, StepFile), 
  	myStepFile(AID, Step, NextFile), 
     		
        myFormat('Executing: clingo ../../../lps/compute_expected_agent.lp  
                  ~q ~q ~q ~q  ~q ~q  --outf=0 -V0 --out-atomf=%s. | head -n1  > ~q ~n!', 
		[Domain, Problem,  AgentFile, OCCFile,  ENEXTFile, COnstant, StepFile]),  
     	
        process_create(Shell, ['-c', ['clingo ../../../lps/compute_expected_agent.lp ', 
                              Domain, ' ', Problem, ' ', AgentFile,' ', OCCFile, ' ', 
                              ENEXTFile, COnstant, ' --outf=0 -V0 --out-atomf=%s. | head -n1  > ', 
                              StepFile]], [process(P1)]),
        process_wait(P1,exit(_)),     

        myErrorFile(AID, Step, ERRFile), 
        myStepFile(AID, Previous, PRFile),
        myStepFile(AID, Step, CRFile),
        atom_concat(' -c t=', Next, CONext), 
 
  	myFormat('Executing: clingo ../../../lps/diagnosis.lp  
  	          ~q ~q ~q ~q  ~q ~q  ~q ~q --outf=0 -V0 --out-atomf=%s. | head -n1  > ~q ~n!', 
		[Domain, Problem,  AgentFile, PRFile,  CRFile, OCCFile, StepFile, CONext, ERRFile]),  
     
        process_create(Shell, ['-c', ['clingo ../../../lps/diagnosis.lp ', Domain, ' ', Problem, ' ', 
        			      AgentFile,' ', PRFile, ' ', CRFile, ' ', OCCFile, ' ', 
        			      StepFile, ' ', CONext,  ' --outf=0 -V0 --out-atomf=%s. | head -n1  > ',
        			      ERRFile]], [process(P2)]),
        process_wait(P2,exit(_)).        

% activities of agent in each step 
% 

agent_step(AID, Domain, Problem, Step):-
       	format('Step ~q~n', [Step]),
       	gettingOccurrence(Step, Occurrence),
        term_string(Occ, Occurrence),
        out(msg(AID, 0, 1, Occ)),
        toCmd([AID, '_occ_', Step, '.lp'], Fname),
        dump_to_file_occ(Step, Fname),
        %% dump_to_file([Occ], Fname),        
        myFormat('Sent ~q~n and wait for response in step ~q ~n', [Occ, Step]),
        repeat, 
        in(msg(From, AID, Type, Content)),
        myFormat('Received - main loop: from ~q type ~q content ~q~n',[From, Type, Content]), 
        myFormat('Processing: from ~q type ~q content ~q~n',[From, Type, Content]), 
        processing_message(From, Type, Content, AID, Step, Domain, Problem),
        ((From, Type, Content) == (0, 2, next) 
         -> 
            Next is Step + 1, 
            myFormat('Will run step ~q when From and Type and Content are ~q ~q ~q ~n', [Next, From, Type, Content]),
            agent_step(AID, Domain, Problem, Next);
            true
         ),
         mystop, 
        !, 
        myFormat('Out from agent step ~q ~n',[Step]).

processing_message(0, 2, done, _, _, _, _):-
        assertz(mystop),  
	myFormat('All done ~n',[ ]).

processing_message(0, 2, next, AID, Step, Domain, Problem):-                  
        agent_next_state(AID, Step, Domain, Problem),
        agent_step_diagnosis(AID, Step, Domain, Problem),    
        do_wait_for_next(AID, Step, Domain, Problem), 
        myFormat('Done computing at step ~q ======================= ~n', [Step]).

processing_message(Other, 6, answer(F, V, S, A), AID, Step, Domain, Problem):-                  
        % answer for the (V)alue of (F)luent at (S)tep is A
        myFormat('Receiving answer(~q, ~q, ~q) from ~q! ~n', [F, V, S, A]),
        assertz(answers(question(Other, no, F, V, S), A)),
        retract(asked(Other, F, V, S)),
          (
            set_diagnosis_mode(propagate)
              ->
                  % need to check if all of my neighbors are asking about this question?
                  findall(XAg, being_asked(_, XAg, F, V, S), AQFrom),
                  myFormat(AQFrom),                     
                  findall(XAg, (being_asked(_, XAg, F, V, S), XAg \= Other), QFrom),
                  notify(QFrom, F, V, S, A)
              ;  
               true
          ) 
        .  
        %true. 

%           processing_message(From, Type, Content, AID, Step, Domain, Problem),

processing_message(Other, 7, question(F, V, S), AID, Step, Domain, Problem):-                  
    % need to find answer for the (V)alue of (F)luent at (S)tep
        myFormat(' ******** processing request from other ****** ~n', []), 
%        (myCheck ->
%            myFormat('The bad case ************~n', []),
%            retract(myCheck)
%            ;
%            myFormat('The normal case ******** ~n', [])
%        ), 
        myFormat('Receiving question(~q, ~q, ~q) from ~q! ~n', [F, S, V, Other]),
        assertz(being_asked(0, Other, F, V, S)),
        findall(answers(X,Y), answers(X,Y), LAnswers),
        myFormat(LAnswers),
        finding_answers(F, V, S, Me),
        (
          length(Me, 0) 
          ->
          myFormat('Did not find answer question(~q, ~q, ~q) from ~q! ~n', [F, S, V, Other]),
          (
            set_diagnosis_mode(propagate)
              ->
                  % need to check if all of my neighbors are asking about this question?                  
                  findall(XAg, being_asked(_, XAg, F, V, S), QFrom),
                  findall(XNew, (neighbor(XNew, _), \+ member(XNew, QFrom)), QAsked),
                  (length(QAsked, 0)
                   ->
                   out(msg(AID, Other, 6, answer(F, V, S, [no])));
                   myFormat('Asking my neighbors .... ', []),
                   send_to_my_neighbors(F, V, S, [Other])
                  )
            ;
            out(msg(AID, Other, 6, answer(F, V, S, [no])))
          )
          ;
          myFormat('Find answer question(~q, ~q, ~q) from ~q! ~n', [F, S, V, Other]),
          out(msg(AID, Other, 6, answer(F, V, S, Me)))
%
%          myFormat('Did not find answer question(~q, ~q, ~q) from ~q! ~n', [F, S, V, Other]),  
%          out(msg(AID, Other, 6, answer(F, V, S, [no])))
%          ;
%          myFormat('Find answer question(~q, ~q, ~q) from ~q! ~n', [F, S, V, Other]),  
%          out(msg(AID, Other, 6, answer(F, V, S, Me)))           
        ).         
%%true.

is_answers(F, V, S, Causes):-
    answers(question(_, _, F, V, T), Causes),     
    (T < S; T == S).
    


finding_answers(F, V, S, Me):-
        myFormat('==> in finding answers ~n', [ ]),     
        findall(Causes, is_answers(F, V, S, Causes), Me),
        myFormat('Reasons for ~q, ~q, ~q is ~q ~n', [F, V, S, Me]).


assertingAnswersSelf([]).
    
assertingAnswersSelf([question(self,Act,Fluent,Value,Step) | LErrorsSelf]):-
    assertz(answers(question(self, Act, Fluent, Value, Step), [self])),
    assertingAnswersSelf(LErrorsSelf).


agent_step_diagnosis(AID, Step, Domain, Problem):-                  
    (
        need_diagnosis(AID, Step) 
        -> 
        myFormat('I need some diagnoses at ~q Step!~n', [Step]),
        do_diagnosis(AID, Step, Domain, Problem)
        ; 
        myFormat('I do not need diagnoses at Step ~q!~n', [Step]),
        % do_wait_for_next(AID, Step, Domain, Problem) 
        out(msg(AID, 0, 10, Step))
    ).   

          
do_wait_for_next(AID, Step, Domain, Problem) :- 
        myFormat('Waiting for the next round to start ...~n',[]), 
        findall(asked(Ind1, Fluent1, Value1, Step1), asked(Ind1, Fluent1, Value1, Step1), LQuestions1), 
        myFormat('Still needs to resolve ~q~n',[LQuestions1]),    
	(
         length(LQuestions1, 0)
         -> 	
         out(msg(AID, 0, 10, Step));  
         true
        ), 
  	repeat, 
         in(msg(From, AID, Type, Content)),
         myFormat('Received: from ~q type ~q content ~q~n',[From, Type, Content]), 
         (
          (From, Type, Content) == (0, 10, Step) 
           -> 
               true
           ;
               myFormat('********   Need to process ~q ~q *~q* ~n ', [From, Type, Content]),
               assert(myCheck),
               (myCheck -> myFormat('Calling processing ~n',[]); true),

               processing_message(From, Type, Content, AID, Step, Domain, Problem),

               myFormat('Getting out from processing message ------------------ ~n', []),
               
           findall(asked(Ind, Fluent, Value, Step), asked(Ind, Fluent, Value, Step), LQuestions), 
           myFormat('Still needs to resolve xxxx ~q~n',[LQuestions]),    
           length(LQuestions, NQuestions), 
           (NQuestions == 0 -> out(msg(AID, 0, 10, Step)); true),
           NQuestions == 0
         ),
        !,
	myFormat('Receiving permission to send action occurrence for the next round ...~n',[]). 

 


% agent_solve: computing and diagnosis 
% check for the need to have a diagnosis 

need_diagnosis(AID, Step) :-
     Next is Step + 1, 
     toCmd([AID, '_my_error_', Step, '.lp'], FileName),      
     myFormat('File ~q~n', [FileName]),  
     load_files(FileName),
     findall(question(self,Act,Fluent,Value,Step), question(self,Act,Fluent,Value,Step), LErrorsSelf), 
     assertingAnswersSelf(LErrorsSelf),
     findall(question(other,Act,Fluent,Value,Step), (question(other,Act,Fluent,Value,Step), Act \== no), LErrors), 
     length(LErrors, NErrors),
     myFormat('Number of errors ~q ~q~n',[LErrors, NErrors]), 
     NErrors > 0.  
   
do_diagnosis(AID, Step, Domain, Problem):-
     % see if I am a problem 
     myFormat('I need some diagnoses at Step ~q!~n', [Step]), 
     findall(question(Who,Act,Fluent,Value,Step), (question(Who,Act,Fluent,Value,Step), Act \== no), LQuestions),
     myFormat('This needs to be resolved ~q at Step ~q!~n', [LQuestions, Step]),
     sending_request(LQuestions),
     true.
     %% do_wait_for_response(AID, Step, Domain, Problem).
     

do_wait_for_response(AID, Step, Domain, Problem)  :-   
     repeat,
        findall((NIn, Fluent, Value, Step), asked(NIn, Fluent, Value, Step), LQuestionsT),   
        (length(LQuestionsT, 0) -> 
           true;
           in(msg(OtherID, AID, 6, answer(F, V, S, Cause))),
           assertz(answers(question(OtherID, no, F, V, S), Cause)),
           retract(asked(OtherID, F, V, S)) 
        ),  
     !.

     
sending_request([]).

sending_request([question(AG, Act, Fluent, Value, Step) | LQuestions]) :-
    sending_request_individual(question(AG, Act, Fluent, Value, Step)),
    sending_request(LQuestions).

sending_request_individual(question(self, Act, Fluent, Value, Step)):-
    myFormat('Add some information for myself ~q ~q ~q ~q ~n', [Act, Fluent, Value, Step]),
    agent(AID, _Name), 
    assertz(answers(question(self, Act, Fluent, Value, Step), [(AID, Step)])).

sending_request_individual(question(other, Act, Fluent, Value, Step)):-
    myFormat('I need to look for answer for this ~q ~q ~q ~q ~n', [Act, Fluent, Value, Step]),
    findall((A, Fluent, Value, T), (question(self, A, Fluent, Value, T), T < Step), Me), 
    agent(AID, _Name), 
    length(Me, NMe), 
    (NMe > 0
       ->  
       myFormat('I found an answer for this ~q ~q ~q ~q ~n', [Act, Fluent, Value, Step]),
       assertz(answers(question(self, Act, Fluent, Value, Step), Me))
       ; 
       myFormat('I did not find answer for this ~q ~q ~q ~q ~n', [Act, Fluent, Value, Step]),
       send_to_my_neighbors(Fluent, Value, Step, [])
    ).    

send_to_my_neighbors(Fluent, Value, Step, Exception):-
    agent(AID, _), 
    myFormat('Need to ask for help from neighbor  ~n', [ ]),
    findall(X, (neighbor(X, _), \+ member(X, Exception)), Neighbors),
    (length(Neighbors, 0)   
            -> 
            true; 
            sending_request_neighbors(Neighbors, Fluent, Value, Step)
    ).

sending_request_neighbors([], _, _, _). 

sending_request_neighbors([Ind | Neighbors], Fluent, Value, Step):-
    agent(AID, _), 
    myFormat('I send a message to ~q for question(~q, ~q, ~q) !~n', [Ind, Fluent, Value, Step]),   
    out(msg(AID, Ind, 7, question(Fluent, Value, Step))),
    assertz(asked(Ind, Fluent, Value, Step)),
    assertz(being_asked(1, Ind, Fluent, Value, Step)),
    sending_request_neighbors(Neighbors, Fluent, Value, Step).  

notify([], _, _, _, _).

notify([ID | LAgents], Fluent, Value, Step, Ans):-
     out(diag(AID, ID, 2, (Fluent, Value, Step, Ans))),
     notify([ID | LAgents], Fluent, Value, Step, Ans).       

