%:- module(observer, [load_config/1, 
%                     setMyDebug/1, myFormat/2,
%                     send_all_agents/3]).

:- use_module(library(tipc/tipc_linda)).
:- use_module(library(tipc/tipc)).
:- use_module(library(system)).
:- use_module(library(process)). 
:- use_module(library(readutil)).

:- tipc:tipc_initialize.

:- dynamic def_agent/2.
:- dynamic from_agent/4. 
:- dynamic completed/4. 

setMyDebug(true). 

myFormat(X, Y) :- 
   setMyDebug(Trace), 
   (Trace == true -> format(X,Y); true). 


/* 

   Observations server 
   Wait for everyone to connect 
      agent from 1 to n, each with a domain and fluents
      if all agents send none => stop     
   Listening for all the information regardings 
   action occurrences of the form occurs() 
   Once the occurrences are there   
      compute the next state 
      split the state      
      send to each agent 
   
   Messages are sent in the format 'agent(From Agent, To Agent, Type of Message, Content)' 
      Agent 0 is observation server 
      Agent 1..n is one of the agent 
      Agent i will listen to messages of the form (From, i, Type, Content) 
      
*/

% create the partial view for each agents

env_nsa(_, []). 

env_nsa(Step, [AID | LA]):- 
     myFormat('Computing next state for agent ~q ~q ~n', [AID, LA]),
     % absolute_file_name(path('$SHELL'), Shell, []),
     getenv('SHELL', Shell), 
     atom_concat(AID,'_fluents.lp',Fluents),
     atom_concat('next_', Step, SFile),
     atom_concat(SFile, '.lp', StepFile),
     atom_concat(AID, '_next_', NFile),
     atom_concat(NFile, Step, NTmp),
     atom_concat(NTmp, '.lp', NextFile),
     myFormat('clingo ../../../lps/compute_next_agent.lp , ~q, , ~q, --outf=0 -V0 --out-atomf=%s. | head -n1  > ~q', [Fluents, StepFile, NextFile]),  
     process_create(Shell, ['-c', ['clingo ../../../lps/compute_next_agent.lp ', Fluents,' ', StepFile,' --outf=0 -V0 --out-atomf=%s. | head -n1  > ', NextFile]], [process(P1)]),
     process_wait(P1,exit(_)),     
     myFormat('Done with ~q~n', [AID]),
     env_nsa(Step, LA).

% preparation for start the system - computing the initial state 

preparation(Problem) :-
    toCmd(['clingo ../../../lps/compute_0.lp ', Problem, ' --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > next_0.lp'], Cmd0), 
    myFormat('*********** ~n Executing ~q~n **************~n ', [Cmd0]), 
    shell(Cmd0, _).
    
%%% Version 1 - Algorithm 2
% the configuration file should contain
% the file encoding the problem    *problem(file_name)*
% the file encoding the domain     *domain(file_name)*
% collection of  atoms of the form *agent(i, agent_name)*

load_config(Config):-
    % activate the linda server
    use_module(library(tipc/tipc_linda)),
    tipc:tipc_initialize, 
    linda,
    % read the configuation
    load_files(Config),
    number_of_agents(NAgents), 
    waiting_for_agents(NAgents),
    domain(Domain),
    problem(Problem),
    format('Number of agents ~q~n Domain ~q~n Problem ~q~n~n', [NAgents, Domain, Problem]), 
    % start the simulation process
    start_env(NAgents, Domain, Problem).

%% waiting for all agents to start 

waiting_for_agents(NAgents) :-
    repeat,
    in(agent(Index, Name)),
    (\+ def_agent(Index, Name)
     -> assertz(def_agent(Index, Name)); true),
    findall((I, N), def_agent(I, N), LAgents),
    length(LAgents, NAgents),
    !,
    myFormat('All agents has started successfully!~n', []).

%% start the environment computation process 

start_env(NAgents, Domain, Problem):- 
    format('Start processing ~n', []),
    preparation(Problem), 
    findall(ID, def_agent(ID, _), LAgents), 
    env_loop(NAgents, LAgents, Domain, Problem, 1),
    format('Done exeucting and wait for closing ~n', []),
    closing(NAgents).
    

env_loop(NAgents, LAgents, Domain, Problem, Step):-         
     format('Step ~q ~n', [Step]),
     myFormat('List of agents ~q ~n',[LAgents]), 
     send_all_agents(LAgents, 2, next(Step)),
     repeat, 
	format('Waiting ... ~n',[ ]),  
        in(msg(Agent, 0, Type, Content)),
        myFormat('Got ~q of type ~q (action occurrence) from agent ~q~n', [Content, Type, Agent]),
        (\+ from_agent(Type, Content, Agent, Step) 
            -> assertz(from_agent(Type, Content, Agent, Step)); true),
        findall((1, Cont1, At1, Step), from_agent(1, Cont1, At1, Step), LData),
        myFormat('List of action occurrences ~q~n', [LData]),
        length(LData, NReceived),
        (
           NReceived == NAgents 
            ->
            findall((1, none, Ag, Step), member((1, none, Ag, Step), LData), LDone),
            length(LDone, NDone),
            (NDone == NAgents
                 ->
                 format('Done ...~n', [ ]),
                 send_all_agents(LAgents, 2, done)
                 ;
                 findall(Occs, from_agent(1, Occs, _, Step), LOccurrences),  
                 myFormat('List of action that will be executed ~n   ~q~n', [LOccurrences]),
                 env_solve(Step, Domain, Problem, LOccurrences),                
                 myFormat('Done computing the next state ~n',[ ]),   

                 % send the output to the agents 
                 env_nsa(Step, LAgents),           
                 
                 send_all_agents(LAgents, 10, next(Step)),
                        
                 retractall(from_agent(_, _, _, _)), 
                 
                 Next is Step + 1,
                 env_loop(NAgents, LAgents, Domain, Problem, Next)             
            )
            ;
            myFormat('Do not receive all actions yet ~n',[ ])           
        ),
        NReceived == NAgents, 
     !, 
     myFormat('Exiting from Step ~q~n',[Step]).

send_all_agents([], Type, Content):-
     myFormat('Complete sending this message (Type, Content) ==> ~q ~q ~n',[Type, Content]).

send_all_agents([H | T], Type, Content):-
    myFormat('Sending agent ~q this message (Type, Content) ==> ~q ~q ~n',[H, Type, Content]),
    out(msg(0, H, Type,  Content)),
    send_all_agents(T, Type, Content).

closing(NAgents):-         
     repeat, 
	format('Waiting for closing ... ~n',[ ]),  
        in(msg(Agent, 0, 3, done)),
        myFormat('Got ack of type 3 (closing) from agent ~q~n', [Agent]),
        (\+ from_agent(3, done, Agent, -1) ->  
            assertz(from_agent(3, done, Agent, -1)); true),
        findall((3, Cont1, At1, -1), from_agent(3, Cont1, At1, -1), LData),
        length(LData, NReceived),
        NReceived == NAgents, 
        !.

%% compute the next state 
%% given the domain, the problem, and the current step 
%% assumed that the actions sent by the agents are collected in the file env_received_$current_step.lp
%% 

env_solve(CurrentStep, Domain, Problem, LOccurrences):-
     % dump the data to a file named env_received_$Step.lp
     myFormat('Computing next state at step ~q~n', [CurrentStep]),                 
     atom_concat(env_received_, CurrentStep, Ftmp),
     atom_concat(Ftmp, '.lp', File), 
     dump_to_file(LOccurrences, File), 
     Previous is CurrentStep - 1, 

     getenv('SHELL', Shell),            
     myFormat('clingo ../../../lps/convert_error.lp env_received_~q.lp errors.lp ~q next_~q.lp -c t=~q --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_~q.lp~n', [CurrentStep, Domain, Previous, CurrentStep, CurrentStep]), 

           
     process_create(Shell, ['-c', ['clingo ../../../lps/convert_error.lp env_received_', CurrentStep, '.lp errors.lp ', Domain, ' ' , Problem, ' next_', Previous, '.lp -c t=', CurrentStep, ' --outf=0 -V0 --out-atomf=%s. | head -n1   > occ_', CurrentStep, '.lp']], [process(P1)]),  
     process_wait(P1,exit(_)), 

     process_create(Shell, ['-c', ['cat occ_', CurrentStep, '.lp >> cumu_actions.lp ']], [process(P0)]),
     process_wait(P0,exit(_)),     
          
     myFormat('clingo  ../../../lps/compute_next.lp ~q ~q cumu_actions.lp occ_~q.lp next_~q.lp -c t=~q --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\' > next_~q.lp', [Domain, Problem, CurrentStep, Previous, CurrentStep, CurrentStep]),     
     
     process_create(Shell, ['-c', ['clingo ../../../lps/compute_next.lp ', Domain, ' ' , Problem, ' cumu_actions.lp occ_', CurrentStep, '.lp next_', Previous, '.lp -c t=', CurrentStep, ' --outf=0 -V0 --out-atomf=%s. | head -n1  > next_', CurrentStep, '.lp']], [process(P2)]),
     process_wait(P2,exit(_)).   

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
     findall(X, (member(X, List), X \== none), List1), 
     dump_file(List1, Out),
     nl(Out),  
     close(Out). 


