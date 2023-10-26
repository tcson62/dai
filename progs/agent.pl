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
:- dynamic maxStep.

:- discontiguous processing_msg/6.

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
         1 - action occuA Garcez, M Gori, LC Lamb, L Serafini, M Spranger, and SN Tran. Neural-symbolic computing: An effective methodology for principled integration of machine learning and reasoning. Journal of Applied Logics, 6(4):611–632, 2019rrence or none 
         3 - done 
         5 - need to wait for diagnosis 
         10 - done with current step 
         

Receiving: from 0 
         2 - done 
         2 - or next(S) with S is the current step  
         10 -   

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
   (Trace == true -> length(X, NX), format('Print a list ~n: #elements is ~q ~n ~q ~n', [NX, X]); true). 


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

sameList([], []).

sameList([H | T], L):-
    member(H, L), 
    delete(L, H, L1), 
    sameList(T, L1).

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
     findall((Occs, S),  occurs(_, Occs, S), LOccurs), 
     length(LOccurs, NSteps),
     assert(maxStep(NSteps)), 
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
     agent_loop(AID, Domain, Problem),
     %% agent_step(AID, Domain, Problem, 1),
     format('Done executing ~n',[]),
     out(msg(AID, 0, 3, done)), 
     findall(answers(X,Y), answers(X,Y), LAnswers), 
     myFormat('List of answers ~n ~q ~n~n', [LAnswers]),
     findall((X,P,Q,R,S), being_asked(X,P,Q,R,S), LAsks), 
     myFormat('List of questions that I sent and received ~n ~q ~n~n', [LAsks]).
%%% 

gettingOccurrence(Step, Occurrence):-  
    ( occurs(N, A, Step) 
       ->
           term_string(occurs(N, A, Step), Occ)
       ; 
       term_string(none, Occ)
       ),
    term_string(Occurrence, Occ). 
        
preparation(AID, Domain, Problem):-
     format('In preparation ~n',[]),

     getenv('SHELL', Shell),                
     myStepFile(AID, 0, StepFile),
     atom_concat(AID, '_fluents.lp', FluentFile), 

     myFormat('*********** ~n Computing initial state ~n **************~n ', []), 
        
     myFormat('Executing: clingo ../../../lps/compute_0.lp  ~q ~q --outf=0 -V0 --out-atomf=%s. | head -n1  > ~q ~n!', [Domain, Problem, StepFile]),  
     	
     process_create(Shell, ['-c', ['clingo ../../../lps/compute_0.lp ', Domain, ' ', Problem, ' --outf=0 -V0 --out-atomf=%s. | head -n1  > ', StepFile]], [process(P1)]),
     process_wait(P1,exit(_)),         
        
     myExpectedStep(AID, 0, ENEXTFile), 
     
     myFormat('Setting up expected initial state~n clingo ~q -c t=0 --outf=0 -V0 --out-atomf=%s. | head -n1 >  ~q ~n', [StepFile,  ENEXTFile]),
     
     process_create(Shell, ['-c', ['clingo ../../../lps/compute_expected_agent.lp ', StepFile, '  -c t=0 --outf=0 -V0 --out-atomf=%s. | head -n1 > ', ENEXTFile]], [process(P10)]),
     process_wait(P10,exit(_)),   
        
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
        atom_concat(AID, '_agent.lp',AgentFile),          %1_agent.lp     
        myOccurrenceFile(AID, Step, OCCFile),           % occurrence 
        myExpectedStep(AID, Previous, ENEXTFile),  % expected next 
        atom_concat(' -c t=', Step, COnstant),               %-c t=k 
	myExpectedStep(AID, Step, StepFile),          
  	myStepFile(AID, Step, NextFile), 
     		
        myFormat('Executing: clingo ../../../lps/compute_expected_agent.lp ~q ~q ~q ~q  ~q ~q  --outf=0 -V0 --out-atomf=%s. | head -n1  > ~q ~n!', 
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

agent_loop(AID, Domain, Problem):-
    % Step starts from 1 to maxStep  
    % sending the action
    % getting the result
    % compute own state
    % diagnosis
    % wait for responses
    % waiiting for messages and then process it until there is no message from anyone 
    myFormat("In the loop ... ~n", []),
    repeat,
        in(msg(From, AID, Type, Content)),
        myFormat("Receiving ... From ~q  Type ~q Content ~q --- ~n", [From, Type, Content]), 
        processing_msg(From, Type, Content, AID, Domain, Problem), 
        (From, Type, Content) == (0, 2, done),
        mystop, 
    !, 
    myFormat('Out from agent loop ~n',[]).
    
    
%%% no more action to be executed 
%%% this needs to check for any other messages being unresponsive etc.     
    
processing_msg(0, 2, done, _, _, _):-
        assertz(mystop),  
	myFormat('All done ~n',[ ]).

%%% get the request for the action at Step 

processing_msg(0, 2, next(Step), AID, Domain, Problem):-
       	format('Step sending action ~q~n', [Step]),
       	gettingOccurrence(Step, Occurrence), 
       	toCmd([AID, '_occ_', Step, '.lp'], Fname),
        dump_to_file([Occurrence], Fname),        
       	maxStep(Length),  
       	(Step > Length             
       	   -> 
       	   myFormat('Done with executing all actions ~q ======================= ~n', [Step]),    
       	   out(msg(AID, 0, 1, Occurrence)), 
    	   true;
    	   out(msg(AID, 0, 1, Occurrence)),    
           myFormat('Done sending action ~q at step ~q ======================= ~n', [Occurrence, Step])
        ).    
    
%%% get the observations 
processing_msg(0, 10, next(Step), AID, Domain, Problem):-
       	format('Step processing observations ~q~n', [Step]),
       	maxStep(Length),  
       	(Step >  Length             
       	   -> 
       	   myFormat('Done with executing all actions ~q ======================= ~n', [Step]),    
    	   true;
      	   agent_next_state(AID, Step, Domain, Problem),
           agent_step_diagnosis(AID, Step, Domain, Problem),    
           myFormat('Done initializing the diagnosis at step ~q ======================= ~n', [Step])
        ).    
    

%%% message from a neighbor with an answer 

processing_msg(Other, 6, answer(F, V, Step, A), AID, _, _):-                  
        % answer for the (V)alue of (F)luent at (S)tep is A
        myFormat('Receiving answer(~q, ~q, ~q) is ~q from ~q! ~n', [F, V, Step, A, Other]),
        (answers(question(Other, no, F, V, Step), A)
         -> 
         true; 
         assertz(answers(question(Other, no, F, V, Step), A)),
         processing_answer(Other, 6, answer(F, V, Step, A), AID)
        ),
        retract(asked(Other, F, V, Step)).

% answer is [no]

processing_answer(Other, 6, answer(F, V, Step, [no]), AID):- 
        (received_all_no(AID, F, V, Step)
            ->
            myFormat('In propagation === received all NO ~n',[]),
            findall(XAg1, (being_asked(1, XAg1, F, V, Step), XAg1 \= Other), QTo),
            notify(AID, QTo, F, V, Step, [no]),
            retract(asked(_, F, V, Step))        
            ;
            retract(asked(Other, F, V, Step))
        ).        
        

% answer is not [no]

processing_answer(Other, 6, answer(F, V, Step, A), AID):-
        \+ sameList(A, [no]),
        % sending to those who have asked about this question 
        % or those that I have asked about this question 
        myFormat('In propagation with answer ~q  ~n',[A]),
        findall(XAg, (being_asked(1, XAg, F, V, Step), XAg \= Other), QFrom),
        myFormat('Notify ~q for the question(~q, ~q, ~q) with answer ~q ~n', [QFrom, F, V, Step, A]),
        notify(AID, QFrom, F, V, Step, A),
        retract(asked(_, F, V, Step)).    
    
processing_msg(Other, 7, question(F, V, Step), AID, _, _):-                  
        % need to find answer for the (V)alue of (F)luent at (S)tep
        myFormat('Receiving question(~q, ~q, ~q) from ~q! ~n', [F, V, Step, Other]),
        (being_asked(0, Other, F, V, Step)
           -> 
           myFormat('I already received the question(~q, ~q, ~q) from ~q! ~n', [F, V, Step, Other]),
           true;
           assertz(being_asked(0, Other, F, V, Step)),
           findall(answers(X,Y), answers(X,Y), LAnswers),
           myFormat('Current answers I stored: ~q~n', [LAnswers]),
           finding_answers(F, V, Step, Me),
           (
           length(Me, 0) 
            ->
             myFormat('Did not find answer to the question(~q, ~q, ~q) from ~q! ~n', [F, V, Step, Other]),
             processing_question_with_no_answer(Other, 7, question(F, V, Step), AID)
             ; 
             myFormat('Find answer question(~q, ~q, ~q) from ~q! ~n', [F, V, Step, Other]),
             myFormat('Sending ~q the answer  ~q! ~n', [Other, Me]),
             out(msg(AID, Other, 6, answer(F, V, Step, Me)))
           ) 
        ).                

processing_question_with_no_answer(Other, 7, question(F, V, Step), AID):- 
        % need to check if all of my neighbors are asking about this question?                  
        findall(XAg, being_asked(_, XAg, F, V, Step), QFrom),
        myFormat('Has been discussing with ~q about this question(~q, ~q, ~q) now is ~q ~n',[QFrom, F, V, Step, Other]),         
        (length(QFrom, 0) 
          ->   
          append([], [Other], Exceptions);
          append(QFrom, [Other], Exceptions)
        ), 
        findall(XNew, (neighbor(XNew, _), \+ member(XNew, Exceptions)), QAsked),
        myFormat('Being asked ~q ~nTo be asked ~q~n', [Exceptions, QAsked]),  
        (length(QAsked, 0)
           ->
           myFormat('Answer with no .... ', []),
           out(msg(AID, Other, 6, answer(F, V, Step, [no])))
           ;
           myFormat('Asking my neighbors .... ', []),
           send_to_my_neighbors(F, V, Step, Exceptions)
        ). 


received_all_no(AID, F, V, S) :-
    findall(X, answers(question(X, no, F, V, S), _), AnswerNo), 
    myFormat('List of people who answered with no to the question(~q,~q,~q) is ~q ~n', [F, V, S, AnswerNo]), 
    findall(Y, being_asked(1, Y, F, V, S), BeenAsked), 
    myFormat(BeenAsked), 
    findall(Z, (member(Z,BeenAsked), \+ member(Z, AnswerNo)), NotAnswers),
    length(NotAnswers, 0). 


is_answers(F, V, S, Causes):-
    answers(question(_, _, F, V, T), Causes),     
    (T < S; T == S).
    


finding_answers(F, V, S, Me):-
        myFormat('==> in finding answers ~n', [ ]),     
        findall(Causes, is_answers(F, V, S, Causes), Me),
        myFormat('Reasons for ~q, ~q, ~q is ~q ~n', [F, V, S, Me]).


assertingAnswersSelf([]).
    
assertingAnswersSelf([question(self,Act,Fluent,Value,Step) | LErrorsSelf]):-
    agent(_, Name), 
    assertz(answers(question(self, Act, Fluent, Value, Step), [(self,Name)])),
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
       findall(XAsked, being_asked(_, XAsked, Fluent, Value, Step), Exceptions), 
       myFormat('I already asked these people ~q for answer!~n', [Exceptions]), 
       send_to_my_neighbors(Fluent, Value, Step, Exceptions)
    ).    

send_to_my_neighbors(Fluent, Value, Step, Exception):-
    agent(AID, _), 
    findall(X, (neighbor(X, _), \+ member(X, Exception)), Neighbors),
    myFormat('Need to ask for help from neighbor ~q ~n', [Neighbors ]),
    (length(Neighbors, 0)   
            -> 
            true; 
            sending_request_neighbors(Neighbors, Fluent, Value, Step)
    ).

sending_request_neighbors([], _, _, _). 

sending_request_neighbors([Ind | Neighbors], Fluent, Value, Step):-
    agent(AID, _), 
    myFormat('I send a message to ~q for question(~q, ~q, ~q) !~n', [Ind, Fluent, Value, Step]),   
    assertz(asked(Ind, Fluent, Value, Step)),
    assertz(being_asked(1, Ind, Fluent, Value, Step)),
    out(msg(AID, Ind, 7, question(Fluent, Value, Step))),
    sending_request_neighbors(Neighbors, Fluent, Value, Step).  

notify(_, [], _, _, _, _).

notify(AID, [ID | LAgents], Fluent, Value, Step, Ans):-
     out(msg(AID, ID, 6, answer(Fluent, Value, Step, Ans))), 
     notify(AID, LAgents, Fluent, Value, Step, Ans).       

