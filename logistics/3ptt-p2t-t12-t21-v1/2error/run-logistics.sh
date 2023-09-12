
xterm -e swipl -l ../../../progs/env.pl -g "load_config('conf.pl'), halt." > e_run.lp & 
xterm -e swipl -l ../../../progs/agent.pl -g "load_agent_config('apn1.lp'), halt." > 1_run.lp & 
xterm -e swipl -l ../../../progs/agent.pl -g "load_agent_config('tru1.lp'), halt." > 2_run.lp &
xterm -e swipl -l ../../../progs/agent.pl -g "load_agent_config('tru2.lp'), halt." > 3_run.lp &

