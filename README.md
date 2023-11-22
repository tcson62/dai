# dai

1. Requirements: the system needs SWI-Prolog (https://www.swi-prolog.org/) on Linux and  clingo (https://potassco.org/). For computing plans, the incremental version of clingo is needed. 

2. Structure: the folders are organized as follows

	* lps: ASP programs for various purposes, used by the simulator as well as the agent 
	* progs: SWI-Prolog programs. 
	  env.pl - the simulator 
	  agent.pl - the agent 
	* logistics - the logistics domain encoding and different problems (e.g., 3ptt-p2t-t12-t21-v1) 
	* mapf - MAPF problem 
	* nmapf - MAPF problem with different encoding 	

3. Content of a problem folder 

Each problem folder consists of a description about the problem in conf.pl with the information 

	* number_of_agents(4). 		=> number of agents PLUS 1 (simulator) 
	* domain('domain.lp').			=> the path to the domain description of the CMA-STRIPS problem 
	* problem('prob2.lp').			=> the path to the initial and goal description of the CMA-STRIPS problem 

For each agent, a configuration file with the information 

	* agent(ID, agentName) 			=> the agent name and id (ID is used for the simulator, name must be specified in CMA-STRIPS 	* description) 
	* neighbor(otherID, nameOther)	=> the ID and name of a neighbor 
	* ... 
	* domain('domain.lp').			=> the path to the domain description of the agent's CMA-STRIPS problem 
	* problem('prob2.lp').			=> the path to the initial and goal description of the agent's CMA-STRIPS problem 
	* ... 
	* occurs(agentID, action, timestep) 	=> the action that the agent needs to execute at timestep 
	* ... 

4. How to run the experiments: in each problem (e.g., logistics/3ptt-p2t-t12-t21-v1/1error), there is a run-[xxxx].sh that shows the commands for the problem. For example, 

The script 

	* swipl -l ../../../progs/env.pl -g "load_config('conf.pl'), halt." 		 [ > e_run.lp] 
	* swipl -l ../../../progs/agent.pl -g "load_agent_config('apn1.lp'), halt." 	 [> 1_run.lp] 
	* swipl -l ../../../progs/agent.pl -g "load_agent_config('tru1.lp'), halt." 	 [> 2_run.lp] 
	* swipl -l ../../../progs/agent.pl -g "load_agent_config('tru2.lp'), halt."	 [> 3_run.lp] 
	* swipl -l ../../../progs/agent.pl -g "load_agent_config('apn2.lp'), halt."	 [> 4_run.lp] 
  	    
runs the experiments with four agents.  The optional redirection [> ...] can be used to get the output of the computation to a file for examination. 


