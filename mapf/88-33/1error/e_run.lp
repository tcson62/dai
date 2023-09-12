TIPC Linda server now listening at: port_id('<0.0.0:1015523838>')
All agents has started successfully!
Number of agents 3
 Domain 'mapf_domain.lp'
 Problem 'prob1.lp'

Start processing 
*********** 
 Executing 'clingo ../../../lps/compute_0.lp prob1.lp --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > next_0.lp'
 **************
 Step 1 
List of agents [1,2,3] 
Waiting ... 
Got occurs(1,("down",1,7,7,6),1) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(1,("down",1,7,7,6),1),1,1)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("up",2,0,0,1),1) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(1,("down",1,7,7,6),1),1,1),(1,occurs(2,("up",2,0,0,1),1),2,1)]
Do not receive all actions yet 
Waiting ... 
Got occurs(3,("stay",3,6,0),1) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(1,("down",1,7,7,6),1),1,1),(1,occurs(2,("up",2,0,0,1),1),2,1),(1,occurs(3,("stay",3,6,0),1),3,1)]
List of action that will be executed 
   [occurs(1,("down",1,7,7,6),1),occurs(2,("up",2,0,0,1),1),occurs(3,("stay",3,6,0),1)]
Computing next state at step 1
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_1.lp errors.lp mapf_domain.lp next_0.lp -c t=1 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_1.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_1.lp next_0.lp -c t=1 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_1.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 1
Got completion notice from agent 1 at step 1
List of agents who have completed [(1,0,10,1)]
Got completion notice from agent 1 at step 1
List of agents who have completed [(1,0,10,1),(1,0,10,1)]
Got completion notice from agent 3 at step 1
List of agents who have completed [(1,0,10,1),(1,0,10,1),(3,0,10,1)]
All agents complete Step 1
Sending agent 1 this message (Type, Content) ==> 10 1 
Sending agent 2 this message (Type, Content) ==> 10 1 
Sending agent 3 this message (Type, Content) ==> 10 1 
Complete sending this message (Type, Content) ==> 10 1 
Step 2 
List of agents [1,2,3] 
Waiting ... 
Got 1 of type 10 (action occurrence) from agent 3
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got 1 of type 10 (action occurrence) from agent 2
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got 1 of type 10 (action occurrence) from agent 2
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("left",1,7,6,6),2) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(1,("left",1,7,6,6),2),1,2)]
Do not receive all actions yet 
Waiting ... 
Got occurs(3,("up",3,6,0,1),2) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(1,("left",1,7,6,6),2),1,2),(1,occurs(3,("up",3,6,0,1),2),3,2)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("up",2,0,1,2),2) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(1,("left",1,7,6,6),2),1,2),(1,occurs(3,("up",3,6,0,1),2),3,2),(1,occurs(2,("up",2,0,1,2),2),2,2)]
List of action that will be executed 
   [occurs(1,("left",1,7,6,6),2),occurs(3,("up",3,6,0,1),2),occurs(2,("up",2,0,1,2),2)]
Computing next state at step 2
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_2.lp errors.lp mapf_domain.lp next_1.lp -c t=2 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_2.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_2.lp next_1.lp -c t=2 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_2.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 2
Got completion notice from agent 3 at step 2
List of agents who have completed [(3,0,10,2)]
Got completion notice from agent 3 at step 2
List of agents who have completed [(3,0,10,2),(3,0,10,2)]
Got completion notice from agent 3 at step 2
List of agents who have completed [(3,0,10,2),(3,0,10,2),(3,0,10,2)]
All agents complete Step 2
Sending agent 1 this message (Type, Content) ==> 10 2 
Sending agent 2 this message (Type, Content) ==> 10 2 
Sending agent 3 this message (Type, Content) ==> 10 2 
Complete sending this message (Type, Content) ==> 10 2 
Step 3 
List of agents [1,2,3] 
Waiting ... 
Got occurs(3,("left",3,6,1,5),3) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(3,("left",3,6,1,5),3),3,3)]
Do not receive all actions yet 
Waiting ... 
Got 2 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("left",3,6,1,5),3),3,3)]
Do not receive all actions yet 
Waiting ... 
Got 2 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("left",3,6,1,5),3),3,3)]
Do not receive all actions yet 
Waiting ... 
Got 2 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("left",3,6,1,5),3),3,3)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("up",2,0,2,3),3) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("left",3,6,1,5),3),3,3),(1,occurs(2,("up",2,0,2,3),3),2,3)]
Do not receive all actions yet 
Waiting ... 
Got 2 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("left",3,6,1,5),3),3,3),(1,occurs(2,("up",2,0,2,3),3),2,3)]
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("down",1,6,6,5),3) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("left",3,6,1,5),3),3,3),(1,occurs(2,("up",2,0,2,3),3),2,3),(1,occurs(1,("down",1,6,6,5),3),1,3)]
List of action that will be executed 
   [occurs(3,("left",3,6,1,5),3),occurs(2,("up",2,0,2,3),3),occurs(1,("down",1,6,6,5),3)]
Computing next state at step 3
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_3.lp errors.lp mapf_domain.lp next_2.lp -c t=3 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_3.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_3.lp next_2.lp -c t=3 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_3.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 3
Got completion notice from agent 3 at step 3
List of agents who have completed [(3,0,10,3)]
Got completion notice from agent 3 at step 3
List of agents who have completed [(3,0,10,3),(3,0,10,3)]
Got completion notice from agent 3 at step 3
List of agents who have completed [(3,0,10,3),(3,0,10,3),(3,0,10,3)]
All agents complete Step 3
Sending agent 1 this message (Type, Content) ==> 10 3 
Sending agent 2 this message (Type, Content) ==> 10 3 
Sending agent 3 this message (Type, Content) ==> 10 3 
Complete sending this message (Type, Content) ==> 10 3 
Step 4 
List of agents [1,2,3] 
Waiting ... 
Got occurs(3,("stay",3,5,1),4) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(3,("stay",3,5,1),4),3,4)]
Do not receive all actions yet 
Waiting ... 
Got 3 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("stay",3,5,1),4),3,4)]
Do not receive all actions yet 
Waiting ... 
Got 3 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("stay",3,5,1),4),3,4)]
Do not receive all actions yet 
Waiting ... 
Got 3 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("stay",3,5,1),4),3,4)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("right",2,0,3,1),4) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("stay",3,5,1),4),3,4),(1,occurs(2,("right",2,0,3,1),4),2,4)]
Do not receive all actions yet 
Waiting ... 
Got 3 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("stay",3,5,1),4),3,4),(1,occurs(2,("right",2,0,3,1),4),2,4)]
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("down",1,6,5,4),4) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("stay",3,5,1),4),3,4),(1,occurs(2,("right",2,0,3,1),4),2,4),(1,occurs(1,("down",1,6,5,4),4),1,4)]
List of action that will be executed 
   [occurs(3,("stay",3,5,1),4),occurs(2,("right",2,0,3,1),4),occurs(1,("down",1,6,5,4),4)]
Computing next state at step 4
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_4.lp errors.lp mapf_domain.lp next_3.lp -c t=4 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_4.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_4.lp next_3.lp -c t=4 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_4.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 4
Got completion notice from agent 3 at step 4
List of agents who have completed [(3,0,10,4)]
Got completion notice from agent 3 at step 4
List of agents who have completed [(3,0,10,4),(3,0,10,4)]
Got completion notice from agent 2 at step 4
List of agents who have completed [(3,0,10,4),(3,0,10,4),(2,0,10,4)]
All agents complete Step 4
Sending agent 1 this message (Type, Content) ==> 10 4 
Sending agent 2 this message (Type, Content) ==> 10 4 
Sending agent 3 this message (Type, Content) ==> 10 4 
Complete sending this message (Type, Content) ==> 10 4 
Step 5 
List of agents [1,2,3] 
Waiting ... 
Got 4 of type 10 (action occurrence) from agent 2
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("stay",1,6,4),5) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(1,("stay",1,6,4),5),1,5)]
Do not receive all actions yet 
Waiting ... 
Got occurs(3,("up",3,5,1,2),5) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(1,("stay",1,6,4),5),1,5),(1,occurs(3,("up",3,5,1,2),5),3,5)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("right",2,1,3,2),5) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(1,("stay",1,6,4),5),1,5),(1,occurs(3,("up",3,5,1,2),5),3,5),(1,occurs(2,("right",2,1,3,2),5),2,5)]
List of action that will be executed 
   [occurs(1,("stay",1,6,4),5),occurs(3,("up",3,5,1,2),5),occurs(2,("right",2,1,3,2),5)]
Computing next state at step 5
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_5.lp errors.lp mapf_domain.lp next_4.lp -c t=5 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_5.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_5.lp next_4.lp -c t=5 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_5.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 5
Got completion notice from agent 2 at step 5
List of agents who have completed [(2,0,10,5)]
Got completion notice from agent 2 at step 5
List of agents who have completed [(2,0,10,5),(2,0,10,5)]
Got completion notice from agent 3 at step 5
List of agents who have completed [(2,0,10,5),(2,0,10,5),(3,0,10,5)]
All agents complete Step 5
Sending agent 1 this message (Type, Content) ==> 10 5 
Sending agent 2 this message (Type, Content) ==> 10 5 
Sending agent 3 this message (Type, Content) ==> 10 5 
Complete sending this message (Type, Content) ==> 10 5 
Step 6 
List of agents [1,2,3] 
Waiting ... 
Got 5 of type 10 (action occurrence) from agent 3
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got 5 of type 10 (action occurrence) from agent 3
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got occurs(3,("up",3,5,2,3),6) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(3,("up",3,5,2,3),6),3,6)]
Do not receive all actions yet 
Waiting ... 
Got 5 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("up",3,5,2,3),6),3,6)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("right",2,2,3,3),6) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("up",3,5,2,3),6),3,6),(1,occurs(2,("right",2,2,3,3),6),2,6)]
Do not receive all actions yet 
Waiting ... 
Got 5 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("up",3,5,2,3),6),3,6),(1,occurs(2,("right",2,2,3,3),6),2,6)]
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("up",1,6,4,5),6) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("up",3,5,2,3),6),3,6),(1,occurs(2,("right",2,2,3,3),6),2,6),(1,occurs(1,("up",1,6,4,5),6),1,6)]
List of action that will be executed 
   [occurs(3,("up",3,5,2,3),6),occurs(2,("right",2,2,3,3),6),occurs(1,("up",1,6,4,5),6)]
Computing next state at step 6
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_6.lp errors.lp mapf_domain.lp next_5.lp -c t=6 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_6.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_6.lp next_5.lp -c t=6 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_6.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 6
Got completion notice from agent 2 at step 6
List of agents who have completed [(2,0,10,6)]
Got completion notice from agent 2 at step 6
List of agents who have completed [(2,0,10,6),(2,0,10,6)]
Got completion notice from agent 1 at step 6
List of agents who have completed [(2,0,10,6),(2,0,10,6),(1,0,10,6)]
All agents complete Step 6
Sending agent 1 this message (Type, Content) ==> 10 6 
Sending agent 2 this message (Type, Content) ==> 10 6 
Sending agent 3 this message (Type, Content) ==> 10 6 
Complete sending this message (Type, Content) ==> 10 6 
Step 7 
List of agents [1,2,3] 
Waiting ... 
Got 6 of type 10 (action occurrence) from agent 3
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got 6 of type 10 (action occurrence) from agent 3
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("up",2,3,3,4),7) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(2,("up",2,3,3,4),7),2,7)]
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("down",1,6,5,4),7) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(2,("up",2,3,3,4),7),2,7),(1,occurs(1,("down",1,6,5,4),7),1,7)]
Do not receive all actions yet 
Waiting ... 
Got occurs(3,("up",3,5,3,4),7) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(2,("up",2,3,3,4),7),2,7),(1,occurs(1,("down",1,6,5,4),7),1,7),(1,occurs(3,("up",3,5,3,4),7),3,7)]
List of action that will be executed 
   [occurs(2,("up",2,3,3,4),7),occurs(1,("down",1,6,5,4),7),occurs(3,("up",3,5,3,4),7)]
Computing next state at step 7
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_7.lp errors.lp mapf_domain.lp next_6.lp -c t=7 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_7.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_7.lp next_6.lp -c t=7 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_7.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 7
Got completion notice from agent 1 at step 7
List of agents who have completed [(1,0,10,7)]
Got completion notice from agent 3 at step 7
List of agents who have completed [(1,0,10,7),(3,0,10,7)]
Got completion notice from agent 3 at step 7
List of agents who have completed [(1,0,10,7),(3,0,10,7),(3,0,10,7)]
All agents complete Step 7
Sending agent 1 this message (Type, Content) ==> 10 7 
Sending agent 2 this message (Type, Content) ==> 10 7 
Sending agent 3 this message (Type, Content) ==> 10 7 
Complete sending this message (Type, Content) ==> 10 7 
Step 8 
List of agents [1,2,3] 
Waiting ... 
Got 7 of type 10 (action occurrence) from agent 2
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got 7 of type 10 (action occurrence) from agent 2
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("right",2,3,4,4),8) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(2,("right",2,3,4,4),8),2,8)]
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("down",1,6,4,3),8) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(2,("right",2,3,4,4),8),2,8),(1,occurs(1,("down",1,6,4,3),8),1,8)]
Do not receive all actions yet 
Waiting ... 
Got occurs(3,("down",3,5,4,3),8) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(2,("right",2,3,4,4),8),2,8),(1,occurs(1,("down",1,6,4,3),8),1,8),(1,occurs(3,("down",3,5,4,3),8),3,8)]
List of action that will be executed 
   [occurs(2,("right",2,3,4,4),8),occurs(1,("down",1,6,4,3),8),occurs(3,("down",3,5,4,3),8)]
Computing next state at step 8
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_8.lp errors.lp mapf_domain.lp next_7.lp -c t=8 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_8.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_8.lp next_7.lp -c t=8 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_8.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 8
Got completion notice from agent 1 at step 8
List of agents who have completed [(1,0,10,8)]
Got completion notice from agent 3 at step 8
List of agents who have completed [(1,0,10,8),(3,0,10,8)]
Got completion notice from agent 3 at step 8
List of agents who have completed [(1,0,10,8),(3,0,10,8),(3,0,10,8)]
All agents complete Step 8
Sending agent 1 this message (Type, Content) ==> 10 8 
Sending agent 2 this message (Type, Content) ==> 10 8 
Sending agent 3 this message (Type, Content) ==> 10 8 
Complete sending this message (Type, Content) ==> 10 8 
Step 9 
List of agents [1,2,3] 
Waiting ... 
Got 8 of type 10 (action occurrence) from agent 2
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got 8 of type 10 (action occurrence) from agent 2
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("stay",1,6,3),9) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(1,("stay",1,6,3),9),1,9)]
Do not receive all actions yet 
Waiting ... 
Got occurs(3,("stay",3,5,3),9) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(1,("stay",1,6,3),9),1,9),(1,occurs(3,("stay",3,5,3),9),3,9)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("right",2,4,4,5),9) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(1,("stay",1,6,3),9),1,9),(1,occurs(3,("stay",3,5,3),9),3,9),(1,occurs(2,("right",2,4,4,5),9),2,9)]
List of action that will be executed 
   [occurs(1,("stay",1,6,3),9),occurs(3,("stay",3,5,3),9),occurs(2,("right",2,4,4,5),9)]
Computing next state at step 9
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_9.lp errors.lp mapf_domain.lp next_8.lp -c t=9 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_9.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_9.lp next_8.lp -c t=9 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_9.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 9
Got completion notice from agent 3 at step 9
List of agents who have completed [(3,0,10,9)]
Got completion notice from agent 3 at step 9
List of agents who have completed [(3,0,10,9),(3,0,10,9)]
Got completion notice from agent 3 at step 9
List of agents who have completed [(3,0,10,9),(3,0,10,9),(3,0,10,9)]
All agents complete Step 9
Sending agent 1 this message (Type, Content) ==> 10 9 
Sending agent 2 this message (Type, Content) ==> 10 9 
Sending agent 3 this message (Type, Content) ==> 10 9 
Complete sending this message (Type, Content) ==> 10 9 
Step 10 
List of agents [1,2,3] 
Waiting ... 
Got occurs(3,("up",3,5,3,4),10) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(3,("up",3,5,3,4),10),3,10)]
Do not receive all actions yet 
Waiting ... 
Got 9 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("up",3,5,3,4),10),3,10)]
Do not receive all actions yet 
Waiting ... 
Got 9 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("up",3,5,3,4),10),3,10)]
Do not receive all actions yet 
Waiting ... 
Got 9 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("up",3,5,3,4),10),3,10)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("up",2,5,4,5),10) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("up",3,5,3,4),10),3,10),(1,occurs(2,("up",2,5,4,5),10),2,10)]
Do not receive all actions yet 
Waiting ... 
Got 9 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("up",3,5,3,4),10),3,10),(1,occurs(2,("up",2,5,4,5),10),2,10)]
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("up",1,6,3,4),10) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("up",3,5,3,4),10),3,10),(1,occurs(2,("up",2,5,4,5),10),2,10),(1,occurs(1,("up",1,6,3,4),10),1,10)]
List of action that will be executed 
   [occurs(3,("up",3,5,3,4),10),occurs(2,("up",2,5,4,5),10),occurs(1,("up",1,6,3,4),10)]
Computing next state at step 10
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_10.lp errors.lp mapf_domain.lp next_9.lp -c t=10 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_10.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_10.lp next_9.lp -c t=10 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_10.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 10
Got completion notice from agent 3 at step 10
List of agents who have completed [(3,0,10,10)]
Got completion notice from agent 3 at step 10
List of agents who have completed [(3,0,10,10),(3,0,10,10)]
Got completion notice from agent 2 at step 10
List of agents who have completed [(3,0,10,10),(3,0,10,10),(2,0,10,10)]
All agents complete Step 10
Sending agent 1 this message (Type, Content) ==> 10 10 
Sending agent 2 this message (Type, Content) ==> 10 10 
Sending agent 3 this message (Type, Content) ==> 10 10 
Complete sending this message (Type, Content) ==> 10 10 
Step 11 
List of agents [1,2,3] 
Waiting ... 
Got 10 of type 10 (action occurrence) from agent 2
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got 10 of type 10 (action occurrence) from agent 1
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("left",1,6,4,5),11) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(1,("left",1,6,4,5),11),1,11)]
Do not receive all actions yet 
Waiting ... 
Got occurs(3,("left",3,5,4,4),11) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(1,("left",1,6,4,5),11),1,11),(1,occurs(3,("left",3,5,4,4),11),3,11)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("right",2,5,5,6),11) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(1,("left",1,6,4,5),11),1,11),(1,occurs(3,("left",3,5,4,4),11),3,11),(1,occurs(2,("right",2,5,5,6),11),2,11)]
List of action that will be executed 
   [occurs(1,("left",1,6,4,5),11),occurs(3,("left",3,5,4,4),11),occurs(2,("right",2,5,5,6),11)]
Computing next state at step 11
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_11.lp errors.lp mapf_domain.lp next_10.lp -c t=11 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_11.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_11.lp next_10.lp -c t=11 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_11.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 11
Got completion notice from agent 2 at step 11
List of agents who have completed [(2,0,10,11)]
Got completion notice from agent 2 at step 11
List of agents who have completed [(2,0,10,11),(2,0,10,11)]
Got completion notice from agent 1 at step 11
List of agents who have completed [(2,0,10,11),(2,0,10,11),(1,0,10,11)]
All agents complete Step 11
Sending agent 1 this message (Type, Content) ==> 10 11 
Sending agent 2 this message (Type, Content) ==> 10 11 
Sending agent 3 this message (Type, Content) ==> 10 11 
Complete sending this message (Type, Content) ==> 10 11 
Step 12 
List of agents [1,2,3] 
Waiting ... 
Got 11 of type 10 (action occurrence) from agent 3
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got 11 of type 10 (action occurrence) from agent 3
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got occurs(3,("left",3,4,4,3),12) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(3,("left",3,4,4,3),12),3,12)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("down",2,6,5,4),12) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("left",3,4,4,3),12),3,12),(1,occurs(2,("down",2,6,5,4),12),2,12)]
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("left",1,5,4,4),12) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("left",3,4,4,3),12),3,12),(1,occurs(2,("down",2,6,5,4),12),2,12),(1,occurs(1,("left",1,5,4,4),12),1,12)]
List of action that will be executed 
   [occurs(3,("left",3,4,4,3),12),occurs(2,("down",2,6,5,4),12),occurs(1,("left",1,5,4,4),12)]
Computing next state at step 12
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_12.lp errors.lp mapf_domain.lp next_11.lp -c t=12 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_12.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_12.lp next_11.lp -c t=12 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_12.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 12
Got completion notice from agent 3 at step 12
List of agents who have completed [(3,0,10,12)]
Got completion notice from agent 3 at step 12
List of agents who have completed [(3,0,10,12),(3,0,10,12)]
Got completion notice from agent 3 at step 12
List of agents who have completed [(3,0,10,12),(3,0,10,12),(3,0,10,12)]
All agents complete Step 12
Sending agent 1 this message (Type, Content) ==> 10 12 
Sending agent 2 this message (Type, Content) ==> 10 12 
Sending agent 3 this message (Type, Content) ==> 10 12 
Complete sending this message (Type, Content) ==> 10 12 
Step 13 
List of agents [1,2,3] 
Waiting ... 
Got occurs(3,("down",3,3,4,3),13) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(3,("down",3,3,4,3),13),3,13)]
Do not receive all actions yet 
Waiting ... 
Got 12 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("down",3,3,4,3),13),3,13)]
Do not receive all actions yet 
Waiting ... 
Got 12 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("down",3,3,4,3),13),3,13)]
Do not receive all actions yet 
Waiting ... 
Got 12 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("down",3,3,4,3),13),3,13)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("down",2,6,4,3),13) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("down",3,3,4,3),13),3,13),(1,occurs(2,("down",2,6,4,3),13),2,13)]
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("left",1,4,4,3),13) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("down",3,3,4,3),13),3,13),(1,occurs(2,("down",2,6,4,3),13),2,13),(1,occurs(1,("left",1,4,4,3),13),1,13)]
List of action that will be executed 
   [occurs(3,("down",3,3,4,3),13),occurs(2,("down",2,6,4,3),13),occurs(1,("left",1,4,4,3),13)]
Computing next state at step 13
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_13.lp errors.lp mapf_domain.lp next_12.lp -c t=13 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_13.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_13.lp next_12.lp -c t=13 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_13.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 13
Got completion notice from agent 3 at step 13
List of agents who have completed [(3,0,10,13)]
Got completion notice from agent 3 at step 13
List of agents who have completed [(3,0,10,13),(3,0,10,13)]
Got completion notice from agent 3 at step 13
List of agents who have completed [(3,0,10,13),(3,0,10,13),(3,0,10,13)]
All agents complete Step 13
Sending agent 1 this message (Type, Content) ==> 10 13 
Sending agent 2 this message (Type, Content) ==> 10 13 
Sending agent 3 this message (Type, Content) ==> 10 13 
Complete sending this message (Type, Content) ==> 10 13 
Step 14 
List of agents [1,2,3] 
Waiting ... 
Got occurs(3,("stay",3,3,3),14) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(3,("stay",3,3,3),14),3,14)]
Do not receive all actions yet 
Waiting ... 
Got 13 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("stay",3,3,3),14),3,14)]
Do not receive all actions yet 
Waiting ... 
Got 13 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("stay",3,3,3),14),3,14)]
Do not receive all actions yet 
Waiting ... 
Got 13 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("stay",3,3,3),14),3,14)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("down",2,6,3,2),14) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("stay",3,3,3),14),3,14),(1,occurs(2,("down",2,6,3,2),14),2,14)]
Do not receive all actions yet 
Waiting ... 
Got 13 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("stay",3,3,3),14),3,14),(1,occurs(2,("down",2,6,3,2),14),2,14)]
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("left",1,3,4,2),14) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("stay",3,3,3),14),3,14),(1,occurs(2,("down",2,6,3,2),14),2,14),(1,occurs(1,("left",1,3,4,2),14),1,14)]
List of action that will be executed 
   [occurs(3,("stay",3,3,3),14),occurs(2,("down",2,6,3,2),14),occurs(1,("left",1,3,4,2),14)]
Computing next state at step 14
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_14.lp errors.lp mapf_domain.lp next_13.lp -c t=14 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_14.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_14.lp next_13.lp -c t=14 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_14.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 14
Got completion notice from agent 3 at step 14
List of agents who have completed [(3,0,10,14)]
Got completion notice from agent 3 at step 14
List of agents who have completed [(3,0,10,14),(3,0,10,14)]
Got completion notice from agent 3 at step 14
List of agents who have completed [(3,0,10,14),(3,0,10,14),(3,0,10,14)]
All agents complete Step 14
Sending agent 1 this message (Type, Content) ==> 10 14 
Sending agent 2 this message (Type, Content) ==> 10 14 
Sending agent 3 this message (Type, Content) ==> 10 14 
Complete sending this message (Type, Content) ==> 10 14 
Step 15 
List of agents [1,2,3] 
Waiting ... 
Got occurs(3,("down",3,3,3,2),15) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(3,("down",3,3,3,2),15),3,15)]
Do not receive all actions yet 
Waiting ... 
Got 14 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("down",3,3,3,2),15),3,15)]
Do not receive all actions yet 
Waiting ... 
Got 14 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("down",3,3,3,2),15),3,15)]
Do not receive all actions yet 
Waiting ... 
Got 14 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("down",3,3,3,2),15),3,15)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("down",2,6,2,1),15) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("down",3,3,3,2),15),3,15),(1,occurs(2,("down",2,6,2,1),15),2,15)]
Do not receive all actions yet 
Waiting ... 
Got 14 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("down",3,3,3,2),15),3,15),(1,occurs(2,("down",2,6,2,1),15),2,15)]
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("down",1,2,4,3),15) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("down",3,3,3,2),15),3,15),(1,occurs(2,("down",2,6,2,1),15),2,15),(1,occurs(1,("down",1,2,4,3),15),1,15)]
List of action that will be executed 
   [occurs(3,("down",3,3,3,2),15),occurs(2,("down",2,6,2,1),15),occurs(1,("down",1,2,4,3),15)]
Computing next state at step 15
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_15.lp errors.lp mapf_domain.lp next_14.lp -c t=15 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_15.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_15.lp next_14.lp -c t=15 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_15.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 15
Got completion notice from agent 3 at step 15
List of agents who have completed [(3,0,10,15)]
Got completion notice from agent 3 at step 15
List of agents who have completed [(3,0,10,15),(3,0,10,15)]
Got completion notice from agent 3 at step 15
List of agents who have completed [(3,0,10,15),(3,0,10,15),(3,0,10,15)]
All agents complete Step 15
Sending agent 1 this message (Type, Content) ==> 10 15 
Sending agent 2 this message (Type, Content) ==> 10 15 
Sending agent 3 this message (Type, Content) ==> 10 15 
Complete sending this message (Type, Content) ==> 10 15 
Step 16 
List of agents [1,2,3] 
Waiting ... 
Got occurs(3,("down",3,3,2,1),16) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(3,("down",3,3,2,1),16),3,16)]
Do not receive all actions yet 
Waiting ... 
Got 15 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("down",3,3,2,1),16),3,16)]
Do not receive all actions yet 
Waiting ... 
Got 15 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("down",3,3,2,1),16),3,16)]
Do not receive all actions yet 
Waiting ... 
Got 15 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("down",3,3,2,1),16),3,16)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("down",2,6,1,0),16) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("down",3,3,2,1),16),3,16),(1,occurs(2,("down",2,6,1,0),16),2,16)]
Do not receive all actions yet 
Waiting ... 
Got 15 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("down",3,3,2,1),16),3,16),(1,occurs(2,("down",2,6,1,0),16),2,16)]
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("left",1,2,3,1),16) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("down",3,3,2,1),16),3,16),(1,occurs(2,("down",2,6,1,0),16),2,16),(1,occurs(1,("left",1,2,3,1),16),1,16)]
List of action that will be executed 
   [occurs(3,("down",3,3,2,1),16),occurs(2,("down",2,6,1,0),16),occurs(1,("left",1,2,3,1),16)]
Computing next state at step 16
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_16.lp errors.lp mapf_domain.lp next_15.lp -c t=16 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_16.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_16.lp next_15.lp -c t=16 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_16.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 16
Got completion notice from agent 2 at step 16
List of agents who have completed [(2,0,10,16)]
Got completion notice from agent 2 at step 16
List of agents who have completed [(2,0,10,16),(2,0,10,16)]
Got completion notice from agent 3 at step 16
List of agents who have completed [(2,0,10,16),(2,0,10,16),(3,0,10,16)]
All agents complete Step 16
Sending agent 1 this message (Type, Content) ==> 10 16 
Sending agent 2 this message (Type, Content) ==> 10 16 
Sending agent 3 this message (Type, Content) ==> 10 16 
Complete sending this message (Type, Content) ==> 10 16 
Step 17 
List of agents [1,2,3] 
Waiting ... 
Got 16 of type 10 (action occurrence) from agent 3
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got 16 of type 10 (action occurrence) from agent 3
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got occurs(3,("left",3,3,1,2),17) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(3,("left",3,3,1,2),17),3,17)]
Do not receive all actions yet 
Waiting ... 
Got 16 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("left",3,3,1,2),17),3,17)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("right",2,6,0,7),17) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("left",3,3,1,2),17),3,17),(1,occurs(2,("right",2,6,0,7),17),2,17)]
Do not receive all actions yet 
Waiting ... 
Got 16 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("left",3,3,1,2),17),3,17),(1,occurs(2,("right",2,6,0,7),17),2,17)]
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("left",1,1,3,0),17) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("left",3,3,1,2),17),3,17),(1,occurs(2,("right",2,6,0,7),17),2,17),(1,occurs(1,("left",1,1,3,0),17),1,17)]
List of action that will be executed 
   [occurs(3,("left",3,3,1,2),17),occurs(2,("right",2,6,0,7),17),occurs(1,("left",1,1,3,0),17)]
Computing next state at step 17
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_17.lp errors.lp mapf_domain.lp next_16.lp -c t=17 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_17.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_17.lp next_16.lp -c t=17 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_17.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 17
Got completion notice from agent 2 at step 17
List of agents who have completed [(2,0,10,17)]
Got completion notice from agent 2 at step 17
List of agents who have completed [(2,0,10,17),(2,0,10,17)]
Got completion notice from agent 3 at step 17
List of agents who have completed [(2,0,10,17),(2,0,10,17),(3,0,10,17)]
All agents complete Step 17
Sending agent 1 this message (Type, Content) ==> 10 17 
Sending agent 2 this message (Type, Content) ==> 10 17 
Sending agent 3 this message (Type, Content) ==> 10 17 
Complete sending this message (Type, Content) ==> 10 17 
Step 18 
List of agents [1,2,3] 
Waiting ... 
Got 17 of type 10 (action occurrence) from agent 3
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got 17 of type 10 (action occurrence) from agent 3
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got occurs(3,("right",3,2,1,3),18) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(3,("right",3,2,1,3),18),3,18)]
Do not receive all actions yet 
Waiting ... 
Got 17 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("right",3,2,1,3),18),3,18)]
Do not receive all actions yet 
Waiting ... 
Got occurs(2,("stay",2,7,0),18) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(3,("right",3,2,1,3),18),3,18),(1,occurs(2,("stay",2,7,0),18),2,18)]
Do not receive all actions yet 
Waiting ... 
Got occurs(1,("stay",1,0,3),18) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(3,("right",3,2,1,3),18),3,18),(1,occurs(2,("stay",2,7,0),18),2,18),(1,occurs(1,("stay",1,0,3),18),1,18)]
List of action that will be executed 
   [occurs(3,("right",3,2,1,3),18),occurs(2,("stay",2,7,0),18),occurs(1,("stay",1,0,3),18)]
Computing next state at step 18
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_18.lp errors.lp mapf_domain.lp next_17.lp -c t=18 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_18.lp'
 **************
 *********** 
 Executing 'clingo mapf_domain.lp prob1.lp ../../../lps/compute_next.lp occ_18.lp next_17.lp -c t=18 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_18.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
Done with 1
Computing next state for agent 2 [3] 
Done with 2
Computing next state for agent 3 [] 
Done with 3
Sending agent 1 this message (Type, Content) ==> 2 next 
Sending agent 2 this message (Type, Content) ==> 2 next 
Sending agent 3 this message (Type, Content) ==> 2 next 
Complete sending this message (Type, Content) ==> 2 next 
Waiting for agents to complete Step 18
Got completion notice from agent 3 at step 18
List of agents who have completed [(3,0,10,18)]
Got completion notice from agent 3 at step 18
List of agents who have completed [(3,0,10,18),(3,0,10,18)]
Got completion notice from agent 3 at step 18
List of agents who have completed [(3,0,10,18),(3,0,10,18),(3,0,10,18)]
All agents complete Step 18
Sending agent 1 this message (Type, Content) ==> 10 18 
Sending agent 2 this message (Type, Content) ==> 10 18 
Sending agent 3 this message (Type, Content) ==> 10 18 
Complete sending this message (Type, Content) ==> 10 18 
Step 19 
List of agents [1,2,3] 
Waiting ... 
Got none of type 1 (action occurrence) from agent 3
List of action occurrences [(1,none,3,19)]
Do not receive all actions yet 
Waiting ... 
Got 18 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,none,3,19)]
Do not receive all actions yet 
Waiting ... 
Got 18 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,none,3,19)]
Do not receive all actions yet 
Waiting ... 
Got 18 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,none,3,19)]
Do not receive all actions yet 
Waiting ... 
Got none of type 1 (action occurrence) from agent 2
List of action occurrences [(1,none,3,19),(1,none,2,19)]
Do not receive all actions yet 
Waiting ... 
Got none of type 1 (action occurrence) from agent 1
List of action occurrences [(1,none,3,19),(1,none,2,19),(1,none,1,19)]
Done ...
Sending agent 1 this message (Type, Content) ==> 2 done 
Sending agent 2 this message (Type, Content) ==> 2 done 
Sending agent 3 this message (Type, Content) ==> 2 done 
Complete sending this message (Type, Content) ==> 2 done 
Exiting from Step 19
Exiting from Step 18
Exiting from Step 17
Exiting from Step 16
Exiting from Step 15
Exiting from Step 14
Exiting from Step 13
Exiting from Step 12
Exiting from Step 11
Exiting from Step 10
Exiting from Step 9
Exiting from Step 8
Exiting from Step 7
Exiting from Step 6
Exiting from Step 5
Exiting from Step 4
Exiting from Step 3
Exiting from Step 2
Exiting from Step 1
Done exeucting and wait for closing 
Waiting for closing ... 
Got ack of type 3 (closing) from agent 2
Waiting for closing ... 
Got ack of type 3 (closing) from agent 1
Waiting for closing ... 
Got ack of type 3 (closing) from agent 3
