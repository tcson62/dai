TIPC Linda server now listening at: port_id('<0.0.0:1405336423>')
All agents has started successfully!
Number of agents 3
 Domain 'domain.lp'
 Problem 'prob1.lp'

Start processing 
*********** 
 Executing 'clingo ../../../lps/compute_0.lp prob1.lp --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > next_0.lp'
 **************
 Step 1 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(1) 
Sending agent 2 this message (Type, Content) ==> 2 next(1) 
Sending agent 3 this message (Type, Content) ==> 2 next(1) 
Complete sending this message (Type, Content) ==> 2 next(1) 
Waiting ... 
Got occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),1) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),1),1,1)]
Do not receive all actions yet 
Waiting ... 
Got none of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),1),1,1),(1,none,2,1)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru2"),("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),1) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),1),1,1),(1,none,2,1),(1,occurs(constant("tru2"),("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),1),3,1)]
List of action that will be executed 
   [occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),1),none,occurs(constant("tru2"),("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),1)]
Computing next state at step 1
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_1.lp errors.lp domain.lp next_0.lp -c t=1 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_1.lp'
 **************
 *********** 
 Executing 'clingo domain.lp prob1.lp ../../../lps/compute_next.lp occ_1.lp next_0.lp -c t=1 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_1.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
clingo ../../../lps/compute_next_agent.lp , '1_fluents.lp', , 'next_1.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_1.lp'Done with 1
Computing next state for agent 2 [3] 
clingo ../../../lps/compute_next_agent.lp , '2_fluents.lp', , 'next_1.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_1.lp'Done with 2
Computing next state for agent 3 [] 
clingo ../../../lps/compute_next_agent.lp , '3_fluents.lp', , 'next_1.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_1.lp'Done with 3
Sending agent 1 this message (Type, Content) ==> 10 next(1) 
Sending agent 2 this message (Type, Content) ==> 10 next(1) 
Sending agent 3 this message (Type, Content) ==> 10 next(1) 
Complete sending this message (Type, Content) ==> 10 next(1) 
Step 2 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(2) 
Sending agent 2 this message (Type, Content) ==> 2 next(2) 
Sending agent 3 this message (Type, Content) ==> 2 next(2) 
Complete sending this message (Type, Content) ==> 2 next(2) 
Waiting ... 
Got 1 of type 10 (action occurrence) from agent 1
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),2) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),2),1,2)]
Do not receive all actions yet 
Waiting ... 
Got 1 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),2),1,2)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru1"),("load-truck",constant("obj13"),constant("tru1"),constant("pos1")),2) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),2),1,2),(1,occurs(constant("tru1"),("load-truck",constant("obj13"),constant("tru1"),constant("pos1")),2),2,2)]
Do not receive all actions yet 
Waiting ... 
Got 1 of type 10 (action occurrence) from agent 3
List of action occurrences [(1,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),2),1,2),(1,occurs(constant("tru1"),("load-truck",constant("obj13"),constant("tru1"),constant("pos1")),2),2,2)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru2"),("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),2) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),2),1,2),(1,occurs(constant("tru1"),("load-truck",constant("obj13"),constant("tru1"),constant("pos1")),2),2,2),(1,occurs(constant("tru2"),("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),2),3,2)]
List of action that will be executed 
   [occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),2),occurs(constant("tru1"),("load-truck",constant("obj13"),constant("tru1"),constant("pos1")),2),occurs(constant("tru2"),("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),2)]
Computing next state at step 2
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_2.lp errors.lp domain.lp next_1.lp -c t=2 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_2.lp'
 **************
 *********** 
 Executing 'clingo domain.lp prob1.lp ../../../lps/compute_next.lp occ_2.lp next_1.lp -c t=2 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_2.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
clingo ../../../lps/compute_next_agent.lp , '1_fluents.lp', , 'next_2.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_2.lp'Done with 1
Computing next state for agent 2 [3] 
clingo ../../../lps/compute_next_agent.lp , '2_fluents.lp', , 'next_2.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_2.lp'Done with 2
Computing next state for agent 3 [] 
clingo ../../../lps/compute_next_agent.lp , '3_fluents.lp', , 'next_2.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_2.lp'Done with 3
Sending agent 1 this message (Type, Content) ==> 10 next(2) 
Sending agent 2 this message (Type, Content) ==> 10 next(2) 
Sending agent 3 this message (Type, Content) ==> 10 next(2) 
Complete sending this message (Type, Content) ==> 10 next(2) 
Step 3 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(3) 
Sending agent 2 this message (Type, Content) ==> 2 next(3) 
Sending agent 3 this message (Type, Content) ==> 2 next(3) 
Complete sending this message (Type, Content) ==> 2 next(3) 
Waiting ... 
Got 2 of type 10 (action occurrence) from agent 2
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru1"),("load-truck",constant("obj11"),constant("tru1"),constant("pos1")),3) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("tru1"),("load-truck",constant("obj11"),constant("tru1"),constant("pos1")),3),2,3)]
Do not receive all actions yet 
Waiting ... 
Got 2 of type 10 (action occurrence) from agent 3
List of action occurrences [(1,occurs(constant("tru1"),("load-truck",constant("obj11"),constant("tru1"),constant("pos1")),3),2,3)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),3) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(constant("tru1"),("load-truck",constant("obj11"),constant("tru1"),constant("pos1")),3),2,3),(1,occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),3),3,3)]
Do not receive all actions yet 
Waiting ... 
Got 2 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("tru1"),("load-truck",constant("obj11"),constant("tru1"),constant("pos1")),3),2,3),(1,occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),3),3,3)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),3) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("tru1"),("load-truck",constant("obj11"),constant("tru1"),constant("pos1")),3),2,3),(1,occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),3),3,3),(1,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),3),1,3)]
List of action that will be executed 
   [occurs(constant("tru1"),("load-truck",constant("obj11"),constant("tru1"),constant("pos1")),3),occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),3),occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),3)]
Computing next state at step 3
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_3.lp errors.lp domain.lp next_2.lp -c t=3 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_3.lp'
 **************
 *********** 
 Executing 'clingo domain.lp prob1.lp ../../../lps/compute_next.lp occ_3.lp next_2.lp -c t=3 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_3.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
clingo ../../../lps/compute_next_agent.lp , '1_fluents.lp', , 'next_3.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_3.lp'Done with 1
Computing next state for agent 2 [3] 
clingo ../../../lps/compute_next_agent.lp , '2_fluents.lp', , 'next_3.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_3.lp'Done with 2
Computing next state for agent 3 [] 
clingo ../../../lps/compute_next_agent.lp , '3_fluents.lp', , 'next_3.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_3.lp'Done with 3
Sending agent 1 this message (Type, Content) ==> 10 next(3) 
Sending agent 2 this message (Type, Content) ==> 10 next(3) 
Sending agent 3 this message (Type, Content) ==> 10 next(3) 
Complete sending this message (Type, Content) ==> 10 next(3) 
Step 4 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(4) 
Sending agent 2 this message (Type, Content) ==> 2 next(4) 
Sending agent 3 this message (Type, Content) ==> 2 next(4) 
Complete sending this message (Type, Content) ==> 2 next(4) 
Waiting ... 
Got 3 of type 10 (action occurrence) from agent 2
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),4) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),4),2,4)]
Do not receive all actions yet 
Waiting ... 
Got 3 of type 10 (action occurrence) from agent 3
List of action occurrences [(1,occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),4),2,4)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru2"),("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),4) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),4),2,4),(1,occurs(constant("tru2"),("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),4),3,4)]
Do not receive all actions yet 
Waiting ... 
Got 3 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),4),2,4),(1,occurs(constant("tru2"),("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),4),3,4)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),4) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),4),2,4),(1,occurs(constant("tru2"),("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),4),3,4),(1,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),4),1,4)]
List of action that will be executed 
   [occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),4),occurs(constant("tru2"),("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),4),occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),4)]
Computing next state at step 4
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_4.lp errors.lp domain.lp next_3.lp -c t=4 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_4.lp'
 **************
 *********** 
 Executing 'clingo domain.lp prob1.lp ../../../lps/compute_next.lp occ_4.lp next_3.lp -c t=4 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_4.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
clingo ../../../lps/compute_next_agent.lp , '1_fluents.lp', , 'next_4.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_4.lp'Done with 1
Computing next state for agent 2 [3] 
clingo ../../../lps/compute_next_agent.lp , '2_fluents.lp', , 'next_4.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_4.lp'Done with 2
Computing next state for agent 3 [] 
clingo ../../../lps/compute_next_agent.lp , '3_fluents.lp', , 'next_4.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_4.lp'Done with 3
Sending agent 1 this message (Type, Content) ==> 10 next(4) 
Sending agent 2 this message (Type, Content) ==> 10 next(4) 
Sending agent 3 this message (Type, Content) ==> 10 next(4) 
Complete sending this message (Type, Content) ==> 10 next(4) 
Step 5 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(5) 
Sending agent 2 this message (Type, Content) ==> 2 next(5) 
Sending agent 3 this message (Type, Content) ==> 2 next(5) 
Complete sending this message (Type, Content) ==> 2 next(5) 
Waiting ... 
Got occurs(constant("tru2"),("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),5) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(constant("tru2"),("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),5),3,5)]
Do not receive all actions yet 
Waiting ... 
Got 4 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("tru2"),("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),5),3,5)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("apn1"),("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),5) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("tru2"),("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),5),3,5),(1,occurs(constant("apn1"),("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),5),1,5)]
Do not receive all actions yet 
Waiting ... 
Got 4 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("tru2"),("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),5),3,5),(1,occurs(constant("apn1"),("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),5),1,5)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru1"),("unload-truck",constant("obj11"),constant("tru1"),constant("apt1")),5) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("tru2"),("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),5),3,5),(1,occurs(constant("apn1"),("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),5),1,5),(1,occurs(constant("tru1"),("unload-truck",constant("obj11"),constant("tru1"),constant("apt1")),5),2,5)]
List of action that will be executed 
   [occurs(constant("tru2"),("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),5),occurs(constant("apn1"),("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),5),occurs(constant("tru1"),("unload-truck",constant("obj11"),constant("tru1"),constant("apt1")),5)]
Computing next state at step 5
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_5.lp errors.lp domain.lp next_4.lp -c t=5 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_5.lp'
 **************
 *********** 
 Executing 'clingo domain.lp prob1.lp ../../../lps/compute_next.lp occ_5.lp next_4.lp -c t=5 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_5.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
clingo ../../../lps/compute_next_agent.lp , '1_fluents.lp', , 'next_5.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_5.lp'Done with 1
Computing next state for agent 2 [3] 
clingo ../../../lps/compute_next_agent.lp , '2_fluents.lp', , 'next_5.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_5.lp'Done with 2
Computing next state for agent 3 [] 
clingo ../../../lps/compute_next_agent.lp , '3_fluents.lp', , 'next_5.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_5.lp'Done with 3
Sending agent 1 this message (Type, Content) ==> 10 next(5) 
Sending agent 2 this message (Type, Content) ==> 10 next(5) 
Sending agent 3 this message (Type, Content) ==> 10 next(5) 
Complete sending this message (Type, Content) ==> 10 next(5) 
Step 6 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(6) 
Sending agent 2 this message (Type, Content) ==> 2 next(6) 
Sending agent 3 this message (Type, Content) ==> 2 next(6) 
Complete sending this message (Type, Content) ==> 2 next(6) 
Waiting ... 
Got occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),6) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),6),3,6)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("apn1"),("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),6) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),6),3,6),(1,occurs(constant("apn1"),("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),6),1,6)]
Do not receive all actions yet 
Waiting ... 
Got 5 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),6),3,6),(1,occurs(constant("apn1"),("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),6),1,6)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru1"),("unload-truck",constant("obj13"),constant("tru1"),constant("apt1")),6) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),6),3,6),(1,occurs(constant("apn1"),("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),6),1,6),(1,occurs(constant("tru1"),("unload-truck",constant("obj13"),constant("tru1"),constant("apt1")),6),2,6)]
List of action that will be executed 
   [occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),6),occurs(constant("apn1"),("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),6),occurs(constant("tru1"),("unload-truck",constant("obj13"),constant("tru1"),constant("apt1")),6)]
Computing next state at step 6
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_6.lp errors.lp domain.lp next_5.lp -c t=6 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_6.lp'
 **************
 *********** 
 Executing 'clingo domain.lp prob1.lp ../../../lps/compute_next.lp occ_6.lp next_5.lp -c t=6 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_6.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
clingo ../../../lps/compute_next_agent.lp , '1_fluents.lp', , 'next_6.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_6.lp'Done with 1
Computing next state for agent 2 [3] 
clingo ../../../lps/compute_next_agent.lp , '2_fluents.lp', , 'next_6.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_6.lp'Done with 2
Computing next state for agent 3 [] 
clingo ../../../lps/compute_next_agent.lp , '3_fluents.lp', , 'next_6.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_6.lp'Done with 3
Sending agent 1 this message (Type, Content) ==> 10 next(6) 
Sending agent 2 this message (Type, Content) ==> 10 next(6) 
Sending agent 3 this message (Type, Content) ==> 10 next(6) 
Complete sending this message (Type, Content) ==> 10 next(6) 
Step 7 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(7) 
Sending agent 2 this message (Type, Content) ==> 2 next(7) 
Sending agent 3 this message (Type, Content) ==> 2 next(7) 
Complete sending this message (Type, Content) ==> 2 next(7) 
Waiting ... 
Got none of type 1 (action occurrence) from agent 3
List of action occurrences [(1,none,3,7)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),7) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,none,3,7),(1,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),7),1,7)]
Do not receive all actions yet 
Waiting ... 
Got 6 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,none,3,7),(1,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),7),1,7)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),7) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,none,3,7),(1,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),7),1,7),(1,occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),7),2,7)]
List of action that will be executed 
   [none,occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),7),occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),7)]
Computing next state at step 7
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_7.lp errors.lp domain.lp next_6.lp -c t=7 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_7.lp'
 **************
 *********** 
 Executing 'clingo domain.lp prob1.lp ../../../lps/compute_next.lp occ_7.lp next_6.lp -c t=7 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_7.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
clingo ../../../lps/compute_next_agent.lp , '1_fluents.lp', , 'next_7.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_7.lp'Done with 1
Computing next state for agent 2 [3] 
clingo ../../../lps/compute_next_agent.lp , '2_fluents.lp', , 'next_7.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_7.lp'Done with 2
Computing next state for agent 3 [] 
clingo ../../../lps/compute_next_agent.lp , '3_fluents.lp', , 'next_7.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_7.lp'Done with 3
Sending agent 1 this message (Type, Content) ==> 10 next(7) 
Sending agent 2 this message (Type, Content) ==> 10 next(7) 
Sending agent 3 this message (Type, Content) ==> 10 next(7) 
Complete sending this message (Type, Content) ==> 10 next(7) 
Step 8 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(8) 
Sending agent 2 this message (Type, Content) ==> 2 next(8) 
Sending agent 3 this message (Type, Content) ==> 2 next(8) 
Complete sending this message (Type, Content) ==> 2 next(8) 
Waiting ... 
Got 7 of type 10 (action occurrence) from agent 3
List of action occurrences []
Do not receive all actions yet 
Waiting ... 
Got none of type 1 (action occurrence) from agent 3
List of action occurrences [(1,none,3,8)]
Do not receive all actions yet 
Waiting ... 
Got 7 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,none,3,8)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("apn1"),("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),8) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,none,3,8),(1,occurs(constant("apn1"),("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),8),1,8)]
Do not receive all actions yet 
Waiting ... 
Got 7 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,none,3,8),(1,occurs(constant("apn1"),("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),8),1,8)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),8) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,none,3,8),(1,occurs(constant("apn1"),("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),8),1,8),(1,occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),8),2,8)]
List of action that will be executed 
   [none,occurs(constant("apn1"),("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),8),occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),8)]
Computing next state at step 8
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_8.lp errors.lp domain.lp next_7.lp -c t=8 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_8.lp'
 **************
 *********** 
 Executing 'clingo domain.lp prob1.lp ../../../lps/compute_next.lp occ_8.lp next_7.lp -c t=8 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_8.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
clingo ../../../lps/compute_next_agent.lp , '1_fluents.lp', , 'next_8.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_8.lp'Done with 1
Computing next state for agent 2 [3] 
clingo ../../../lps/compute_next_agent.lp , '2_fluents.lp', , 'next_8.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_8.lp'Done with 2
Computing next state for agent 3 [] 
clingo ../../../lps/compute_next_agent.lp , '3_fluents.lp', , 'next_8.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_8.lp'Done with 3
Sending agent 1 this message (Type, Content) ==> 10 next(8) 
Sending agent 2 this message (Type, Content) ==> 10 next(8) 
Sending agent 3 this message (Type, Content) ==> 10 next(8) 
Complete sending this message (Type, Content) ==> 10 next(8) 
Step 9 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(9) 
Sending agent 2 this message (Type, Content) ==> 2 next(9) 
Sending agent 3 this message (Type, Content) ==> 2 next(9) 
Complete sending this message (Type, Content) ==> 2 next(9) 
Waiting ... 
Got occurs(constant("apn1"),("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),9) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("apn1"),("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),9),1,9)]
Do not receive all actions yet 
Waiting ... 
Got 8 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("apn1"),("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),9),1,9)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru1"),("load-truck",constant("obj21"),constant("tru1"),constant("apt1")),9) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("apn1"),("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),9),1,9),(1,occurs(constant("tru1"),("load-truck",constant("obj21"),constant("tru1"),constant("apt1")),9),2,9)]
Do not receive all actions yet 
Waiting ... 
Got none of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(constant("apn1"),("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),9),1,9),(1,occurs(constant("tru1"),("load-truck",constant("obj21"),constant("tru1"),constant("apt1")),9),2,9),(1,none,3,9)]
List of action that will be executed 
   [occurs(constant("apn1"),("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),9),occurs(constant("tru1"),("load-truck",constant("obj21"),constant("tru1"),constant("apt1")),9),none]
Computing next state at step 9
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_9.lp errors.lp domain.lp next_8.lp -c t=9 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_9.lp'
 **************
 *********** 
 Executing 'clingo domain.lp prob1.lp ../../../lps/compute_next.lp occ_9.lp next_8.lp -c t=9 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_9.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
clingo ../../../lps/compute_next_agent.lp , '1_fluents.lp', , 'next_9.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_9.lp'Done with 1
Computing next state for agent 2 [3] 
clingo ../../../lps/compute_next_agent.lp , '2_fluents.lp', , 'next_9.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_9.lp'Done with 2
Computing next state for agent 3 [] 
clingo ../../../lps/compute_next_agent.lp , '3_fluents.lp', , 'next_9.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_9.lp'Done with 3
Sending agent 1 this message (Type, Content) ==> 10 next(9) 
Sending agent 2 this message (Type, Content) ==> 10 next(9) 
Sending agent 3 this message (Type, Content) ==> 10 next(9) 
Complete sending this message (Type, Content) ==> 10 next(9) 
Step 10 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(10) 
Sending agent 2 this message (Type, Content) ==> 2 next(10) 
Sending agent 3 this message (Type, Content) ==> 2 next(10) 
Complete sending this message (Type, Content) ==> 2 next(10) 
Waiting ... 
Got occurs(constant("tru1"),("load-truck",constant("obj23"),constant("tru1"),constant("apt1")),10) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("tru1"),("load-truck",constant("obj23"),constant("tru1"),constant("apt1")),10),2,10)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("apn1"),("load-airplane",constant("obj13"),constant("apn1"),constant("apt1")),10) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("tru1"),("load-truck",constant("obj23"),constant("tru1"),constant("apt1")),10),2,10),(1,occurs(constant("apn1"),("load-airplane",constant("obj13"),constant("apn1"),constant("apt1")),10),1,10)]
Do not receive all actions yet 
Waiting ... 
Got none of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(constant("tru1"),("load-truck",constant("obj23"),constant("tru1"),constant("apt1")),10),2,10),(1,occurs(constant("apn1"),("load-airplane",constant("obj13"),constant("apn1"),constant("apt1")),10),1,10),(1,none,3,10)]
List of action that will be executed 
   [occurs(constant("tru1"),("load-truck",constant("obj23"),constant("tru1"),constant("apt1")),10),occurs(constant("apn1"),("load-airplane",constant("obj13"),constant("apn1"),constant("apt1")),10),none]
Computing next state at step 10
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_10.lp errors.lp domain.lp next_9.lp -c t=10 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_10.lp'
 **************
 *********** 
 Executing 'clingo domain.lp prob1.lp ../../../lps/compute_next.lp occ_10.lp next_9.lp -c t=10 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_10.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
clingo ../../../lps/compute_next_agent.lp , '1_fluents.lp', , 'next_10.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_10.lp'Done with 1
Computing next state for agent 2 [3] 
clingo ../../../lps/compute_next_agent.lp , '2_fluents.lp', , 'next_10.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_10.lp'Done with 2
Computing next state for agent 3 [] 
clingo ../../../lps/compute_next_agent.lp , '3_fluents.lp', , 'next_10.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_10.lp'Done with 3
Sending agent 1 this message (Type, Content) ==> 10 next(10) 
Sending agent 2 this message (Type, Content) ==> 10 next(10) 
Sending agent 3 this message (Type, Content) ==> 10 next(10) 
Complete sending this message (Type, Content) ==> 10 next(10) 
Step 11 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(11) 
Sending agent 2 this message (Type, Content) ==> 2 next(11) 
Sending agent 3 this message (Type, Content) ==> 2 next(11) 
Complete sending this message (Type, Content) ==> 2 next(11) 
Waiting ... 
Got none of type 1 (action occurrence) from agent 3
List of action occurrences [(1,none,3,11)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),11) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,none,3,11),(1,occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),11),2,11)]
Do not receive all actions yet 
Waiting ... 
Got 10 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,none,3,11),(1,occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),11),2,11)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("apn1"),("unload-airplane",constant("obj13"),constant("apn1"),constant("apt1")),11) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,none,3,11),(1,occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),11),2,11),(1,occurs(constant("apn1"),("unload-airplane",constant("obj13"),constant("apn1"),constant("apt1")),11),1,11)]
List of action that will be executed 
   [none,occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),11),occurs(constant("apn1"),("unload-airplane",constant("obj13"),constant("apn1"),constant("apt1")),11)]
Computing next state at step 11
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_11.lp errors.lp domain.lp next_10.lp -c t=11 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_11.lp'
 **************
 *********** 
 Executing 'clingo domain.lp prob1.lp ../../../lps/compute_next.lp occ_11.lp next_10.lp -c t=11 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_11.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
clingo ../../../lps/compute_next_agent.lp , '1_fluents.lp', , 'next_11.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_11.lp'Done with 1
Computing next state for agent 2 [3] 
clingo ../../../lps/compute_next_agent.lp , '2_fluents.lp', , 'next_11.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_11.lp'Done with 2
Computing next state for agent 3 [] 
clingo ../../../lps/compute_next_agent.lp , '3_fluents.lp', , 'next_11.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_11.lp'Done with 3
Sending agent 1 this message (Type, Content) ==> 10 next(11) 
Sending agent 2 this message (Type, Content) ==> 10 next(11) 
Sending agent 3 this message (Type, Content) ==> 10 next(11) 
Complete sending this message (Type, Content) ==> 10 next(11) 
Step 12 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(12) 
Sending agent 2 this message (Type, Content) ==> 2 next(12) 
Sending agent 3 this message (Type, Content) ==> 2 next(12) 
Complete sending this message (Type, Content) ==> 2 next(12) 
Waiting ... 
Got occurs(constant("tru2"),("load-truck",constant("obj22"),constant("tru2"),constant("pos2")),12) of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(constant("tru2"),("load-truck",constant("obj22"),constant("tru2"),constant("pos2")),12),3,12)]
Do not receive all actions yet 
Waiting ... 
Got 11 of type 10 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("tru2"),("load-truck",constant("obj22"),constant("tru2"),constant("pos2")),12),3,12)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("tru1"),("unload-truck",constant("obj21"),constant("tru1"),constant("pos1")),12) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("tru2"),("load-truck",constant("obj22"),constant("tru2"),constant("pos2")),12),3,12),(1,occurs(constant("tru1"),("unload-truck",constant("obj21"),constant("tru1"),constant("pos1")),12),2,12)]
Do not receive all actions yet 
Waiting ... 
Got 11 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("tru2"),("load-truck",constant("obj22"),constant("tru2"),constant("pos2")),12),3,12),(1,occurs(constant("tru1"),("unload-truck",constant("obj21"),constant("tru1"),constant("pos1")),12),2,12)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("apn1"),("load-airplane",constant("obj13"),constant("apn1"),constant("apt1")),12) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("tru2"),("load-truck",constant("obj22"),constant("tru2"),constant("pos2")),12),3,12),(1,occurs(constant("tru1"),("unload-truck",constant("obj21"),constant("tru1"),constant("pos1")),12),2,12),(1,occurs(constant("apn1"),("load-airplane",constant("obj13"),constant("apn1"),constant("apt1")),12),1,12)]
List of action that will be executed 
   [occurs(constant("tru2"),("load-truck",constant("obj22"),constant("tru2"),constant("pos2")),12),occurs(constant("tru1"),("unload-truck",constant("obj21"),constant("tru1"),constant("pos1")),12),occurs(constant("apn1"),("load-airplane",constant("obj13"),constant("apn1"),constant("apt1")),12)]
Computing next state at step 12
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_12.lp errors.lp domain.lp next_11.lp -c t=12 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_12.lp'
 **************
 *********** 
 Executing 'clingo domain.lp prob1.lp ../../../lps/compute_next.lp occ_12.lp next_11.lp -c t=12 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_12.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
clingo ../../../lps/compute_next_agent.lp , '1_fluents.lp', , 'next_12.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_12.lp'Done with 1
Computing next state for agent 2 [3] 
clingo ../../../lps/compute_next_agent.lp , '2_fluents.lp', , 'next_12.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_12.lp'Done with 2
Computing next state for agent 3 [] 
clingo ../../../lps/compute_next_agent.lp , '3_fluents.lp', , 'next_12.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_12.lp'Done with 3
Sending agent 1 this message (Type, Content) ==> 10 next(12) 
Sending agent 2 this message (Type, Content) ==> 10 next(12) 
Sending agent 3 this message (Type, Content) ==> 10 next(12) 
Complete sending this message (Type, Content) ==> 10 next(12) 
Step 13 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(13) 
Sending agent 2 this message (Type, Content) ==> 2 next(13) 
Sending agent 3 this message (Type, Content) ==> 2 next(13) 
Complete sending this message (Type, Content) ==> 2 next(13) 
Waiting ... 
Got occurs(constant("tru1"),("unload-truck",constant("obj23"),constant("tru1"),constant("pos1")),13) of type 1 (action occurrence) from agent 2
List of action occurrences [(1,occurs(constant("tru1"),("unload-truck",constant("obj23"),constant("tru1"),constant("pos1")),13),2,13)]
Do not receive all actions yet 
Waiting ... 
Got 12 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("tru1"),("unload-truck",constant("obj23"),constant("tru1"),constant("pos1")),13),2,13)]
Do not receive all actions yet 
Waiting ... 
Got occurs(constant("apn1"),("unload-airplane",constant("obj13"),constant("apn1"),constant("apt1")),13) of type 1 (action occurrence) from agent 1
List of action occurrences [(1,occurs(constant("tru1"),("unload-truck",constant("obj23"),constant("tru1"),constant("pos1")),13),2,13),(1,occurs(constant("apn1"),("unload-airplane",constant("obj13"),constant("apn1"),constant("apt1")),13),1,13)]
Do not receive all actions yet 
Waiting ... 
Got none of type 1 (action occurrence) from agent 3
List of action occurrences [(1,occurs(constant("tru1"),("unload-truck",constant("obj23"),constant("tru1"),constant("pos1")),13),2,13),(1,occurs(constant("apn1"),("unload-airplane",constant("obj13"),constant("apn1"),constant("apt1")),13),1,13),(1,none,3,13)]
List of action that will be executed 
   [occurs(constant("tru1"),("unload-truck",constant("obj23"),constant("tru1"),constant("pos1")),13),occurs(constant("apn1"),("unload-airplane",constant("obj13"),constant("apn1"),constant("apt1")),13),none]
Computing next state at step 13
*********** 
 Executing 'clingo ../../../lps/convert_error.lp env_received_13.lp errors.lp domain.lp next_12.lp -c t=13 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\\n\'  > occ_13.lp'
 **************
 *********** 
 Executing 'clingo domain.lp prob1.lp ../../../lps/compute_next.lp occ_13.lp next_12.lp -c t=13 --outf=0 -V0 --out-atomf=%s. | head -n1 | tr \' \' \'\n\' > next_13.lp'
 **************
 Done computing the next state 
Computing next state for agent 1 [2,3] 
clingo ../../../lps/compute_next_agent.lp , '1_fluents.lp', , 'next_13.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_13.lp'Done with 1
Computing next state for agent 2 [3] 
clingo ../../../lps/compute_next_agent.lp , '2_fluents.lp', , 'next_13.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_13.lp'Done with 2
Computing next state for agent 3 [] 
clingo ../../../lps/compute_next_agent.lp , '3_fluents.lp', , 'next_13.lp', --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_13.lp'Done with 3
Sending agent 1 this message (Type, Content) ==> 10 next(13) 
Sending agent 2 this message (Type, Content) ==> 10 next(13) 
Sending agent 3 this message (Type, Content) ==> 10 next(13) 
Complete sending this message (Type, Content) ==> 10 next(13) 
Step 14 
List of agents [1,2,3] 
Sending agent 1 this message (Type, Content) ==> 2 next(14) 
Sending agent 2 this message (Type, Content) ==> 2 next(14) 
Sending agent 3 this message (Type, Content) ==> 2 next(14) 
Complete sending this message (Type, Content) ==> 2 next(14) 
Waiting ... 
Got none of type 1 (action occurrence) from agent 2
List of action occurrences [(1,none,2,14)]
Do not receive all actions yet 
Waiting ... 
Got 13 of type 10 (action occurrence) from agent 1
List of action occurrences [(1,none,2,14)]
Do not receive all actions yet 
Waiting ... 
Got none of type 1 (action occurrence) from agent 1
List of action occurrences [(1,none,2,14),(1,none,1,14)]
Do not receive all actions yet 
Waiting ... 
Got none of type 1 (action occurrence) from agent 3
List of action occurrences [(1,none,2,14),(1,none,1,14),(1,none,3,14)]
Done ...
Sending agent 1 this message (Type, Content) ==> 2 done 
Sending agent 2 this message (Type, Content) ==> 2 done 
Sending agent 3 this message (Type, Content) ==> 2 done 
Complete sending this message (Type, Content) ==> 2 done 
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
