TIPC Linda server listening at: port_id('<0.0.0:2354444477>')
Number of steps 18
 Domain "mapf_domain.lp"
 Problem "prob1_2.lp"
Done starting 
In preparation 
*********** 
 Computing initial state 
 **************
 Executing: clingo ../../../lps/compute_0.lp  "mapf_domain.lp" "prob1_2.lp" --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_0.lp' 
!*********** 
 Computing the fluents 
 **************
 Step 1
Sent occurs(2,("up",2,0,0,1),1)
 and wait for response in step 1 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 1 
In agent 2 solving step 1 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_1.lp'  '2_enext_0.lp' ' -c t=1'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_1.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_0.lp'  '2_next_1.lp' '2_occ_1.lp'  '2_enext_1.lp' ' -c t=2' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_1.lp' 
!File '2_my_error_1.lp'
Number of errors [] 0
I do not need diagnoses at Step 1!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 0 type 10 content 1
Receiving permission to send action occurrence for the next round ...
Done computing at step 1 ======================= 
Will run step 2 when From and Type and Content are 0 2 next 
Step 2
Sent occurs(2,("up",2,0,1,2),2)
 and wait for response in step 2 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 2 
In agent 2 solving step 2 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_2.lp'  '2_enext_1.lp' ' -c t=2'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_2.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_1.lp'  '2_next_2.lp' '2_occ_2.lp'  '2_enext_2.lp' ' -c t=3' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_2.lp' 
!File '2_my_error_2.lp'
Number of errors [] 0
I do not need diagnoses at Step 2!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 1 type 7 content question(variable(("at",1,7,6)),false,2)
********   Need to process 1 7 *question(variable(("at",1,7,6)),false,2)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",1,7,6)), 2, false) from 1! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",1,7,6)), false, 2 is [] 
Did not find answer question(variable(("at",1,7,6)), 2, false) from 1! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 2 ======================= 
Will run step 3 when From and Type and Content are 0 2 next 
Step 3
Sent occurs(2,("up",2,0,2,3),3)
 and wait for response in step 3 
Received - main loop: from 0 type 10 content 2
Processing: from 0 type 10 content 2
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 3 
In agent 2 solving step 3 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_3.lp'  '2_enext_2.lp' ' -c t=3'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_3.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_2.lp'  '2_next_3.lp' '2_occ_3.lp'  '2_enext_3.lp' ' -c t=4' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_3.lp' 
!File '2_my_error_3.lp'
Number of errors [] 0
I do not need diagnoses at Step 3!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 1 type 7 content question(variable(("at",1,6,6)),false,3)
********   Need to process 1 7 *question(variable(("at",1,6,6)),false,3)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",1,6,6)), 3, false) from 1! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",1,6,6)), false, 3 is [] 
Did not find answer question(variable(("at",1,6,6)), 3, false) from 1! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 3 ======================= 
Will run step 4 when From and Type and Content are 0 2 next 
Step 4
Sent occurs(2,("right",2,0,3,1),4)
 and wait for response in step 4 
Received - main loop: from 0 type 10 content 3
Processing: from 0 type 10 content 3
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 4 
In agent 2 solving step 4 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_4.lp'  '2_enext_3.lp' ' -c t=4'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_4.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_3.lp'  '2_next_4.lp' '2_occ_4.lp'  '2_enext_4.lp' ' -c t=5' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_4.lp' 
!File '2_my_error_4.lp'
Number of errors [] 0
I do not need diagnoses at Step 4!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 1 type 7 content question(variable(("at",1,6,5)),false,4)
********   Need to process 1 7 *question(variable(("at",1,6,5)),false,4)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",1,6,5)), 4, false) from 1! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",1,6,5)), false, 4 is [] 
Did not find answer question(variable(("at",1,6,5)), 4, false) from 1! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 4 ======================= 
Will run step 5 when From and Type and Content are 0 2 next 
Step 5
Sent occurs(2,("right",2,1,3,2),5)
 and wait for response in step 5 
Received - main loop: from 0 type 10 content 4
Processing: from 0 type 10 content 4
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 5 
In agent 2 solving step 5 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_5.lp'  '2_enext_4.lp' ' -c t=5'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_5.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_4.lp'  '2_next_5.lp' '2_occ_5.lp'  '2_enext_5.lp' ' -c t=6' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_5.lp' 
!File '2_my_error_5.lp'
Number of errors [] 0
I do not need diagnoses at Step 5!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 1 type 7 content question(variable(("at",1,6,4)),false,5)
********   Need to process 1 7 *question(variable(("at",1,6,4)),false,5)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",1,6,4)), 5, false) from 1! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",1,6,4)), false, 5 is [] 
Did not find answer question(variable(("at",1,6,4)), 5, false) from 1! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 5 ======================= 
Will run step 6 when From and Type and Content are 0 2 next 
Step 6
Sent occurs(2,("right",2,2,3,3),6)
 and wait for response in step 6 
Received - main loop: from 0 type 10 content 5
Processing: from 0 type 10 content 5
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 6 
In agent 2 solving step 6 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_6.lp'  '2_enext_5.lp' ' -c t=6'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_6.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_5.lp'  '2_next_6.lp' '2_occ_6.lp'  '2_enext_6.lp' ' -c t=7' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_6.lp' 
!File '2_my_error_6.lp'
Number of errors [] 0
I do not need diagnoses at Step 6!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 0 type 10 content 6
Receiving permission to send action occurrence for the next round ...
Done computing at step 6 ======================= 
Will run step 7 when From and Type and Content are 0 2 next 
Step 7
Sent occurs(2,("up",2,3,3,4),7)
 and wait for response in step 7 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 7 
In agent 2 solving step 7 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_7.lp'  '2_enext_6.lp' ' -c t=7'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_7.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_6.lp'  '2_next_7.lp' '2_occ_7.lp'  '2_enext_7.lp' ' -c t=8' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_7.lp' 
!File '2_my_error_7.lp'
Number of errors [] 0
I do not need diagnoses at Step 7!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 3 type 7 content question(variable(("at",3,5,3)),false,7)
********   Need to process 3 7 *question(variable(("at",3,5,3)),false,7)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",3,5,3)), 7, false) from 3! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",3,5,3)), false, 7 is [] 
Did not find answer question(variable(("at",3,5,3)), 7, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 7 ======================= 
Will run step 8 when From and Type and Content are 0 2 next 
Step 8
Sent occurs(2,("right",2,3,4,4),8)
 and wait for response in step 8 
Received - main loop: from 0 type 10 content 7
Processing: from 0 type 10 content 7
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 8 
In agent 2 solving step 8 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_8.lp'  '2_enext_7.lp' ' -c t=8'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_8.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_7.lp'  '2_next_8.lp' '2_occ_8.lp'  '2_enext_8.lp' ' -c t=9' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_8.lp' 
!File '2_my_error_8.lp'
Number of errors [] 0
I do not need diagnoses at Step 8!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 3 type 7 content question(variable(("at",3,5,4)),false,8)
********   Need to process 3 7 *question(variable(("at",3,5,4)),false,8)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",3,5,4)), 8, false) from 3! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",3,5,4)), false, 8 is [] 
Did not find answer question(variable(("at",3,5,4)), 8, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 8 ======================= 
Will run step 9 when From and Type and Content are 0 2 next 
Step 9
Sent occurs(2,("right",2,4,4,5),9)
 and wait for response in step 9 
Received - main loop: from 0 type 10 content 8
Processing: from 0 type 10 content 8
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 9 
In agent 2 solving step 9 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_9.lp'  '2_enext_8.lp' ' -c t=9'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_9.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_8.lp'  '2_next_9.lp' '2_occ_9.lp'  '2_enext_9.lp' ' -c t=10' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_9.lp' 
!File '2_my_error_9.lp'
Number of errors [] 0
I do not need diagnoses at Step 9!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 1 type 7 content question(variable(("at",1,6,3)),false,9)
********   Need to process 1 7 *question(variable(("at",1,6,3)),false,9)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",1,6,3)), 9, false) from 1! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",1,6,3)), false, 9 is [] 
Did not find answer question(variable(("at",1,6,3)), 9, false) from 1! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 9 ======================= 
Will run step 10 when From and Type and Content are 0 2 next 
Step 10
Sent occurs(2,("up",2,5,4,5),10)
 and wait for response in step 10 
Received - main loop: from 0 type 10 content 9
Processing: from 0 type 10 content 9
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 10 
In agent 2 solving step 10 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_10.lp'  '2_enext_9.lp' ' -c t=10'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_10.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_9.lp'  '2_next_10.lp' '2_occ_10.lp'  '2_enext_10.lp' ' -c t=11' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_10.lp' 
!File '2_my_error_10.lp'
Number of errors [] 0
I do not need diagnoses at Step 10!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 0 type 10 content 10
Receiving permission to send action occurrence for the next round ...
Done computing at step 10 ======================= 
Will run step 11 when From and Type and Content are 0 2 next 
Step 11
Sent occurs(2,("right",2,5,5,6),11)
 and wait for response in step 11 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 11 
In agent 2 solving step 11 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_11.lp'  '2_enext_10.lp' ' -c t=11'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_11.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_10.lp'  '2_next_11.lp' '2_occ_11.lp'  '2_enext_11.lp' ' -c t=12' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_11.lp' 
!File '2_my_error_11.lp'
Number of errors [] 0
I do not need diagnoses at Step 11!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 0 type 10 content 11
Receiving permission to send action occurrence for the next round ...
Done computing at step 11 ======================= 
Will run step 12 when From and Type and Content are 0 2 next 
Step 12
Sent occurs(2,("down",2,6,5,4),12)
 and wait for response in step 12 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 12 
In agent 2 solving step 12 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_12.lp'  '2_enext_11.lp' ' -c t=12'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_12.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_11.lp'  '2_next_12.lp' '2_occ_12.lp'  '2_enext_12.lp' ' -c t=13' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_12.lp' 
!File '2_my_error_12.lp'
Number of errors [] 0
I do not need diagnoses at Step 12!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 1 type 7 content question(variable(("at",1,5,4)),false,12)
********   Need to process 1 7 *question(variable(("at",1,5,4)),false,12)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",1,5,4)), 12, false) from 1! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",1,5,4)), false, 12 is [] 
Did not find answer question(variable(("at",1,5,4)), 12, false) from 1! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 12 ======================= 
Will run step 13 when From and Type and Content are 0 2 next 
Step 13
Sent occurs(2,("down",2,6,4,3),13)
 and wait for response in step 13 
Received - main loop: from 3 type 7 content question(variable(("at",3,4,4)),false,12)
Processing: from 3 type 7 content question(variable(("at",3,4,4)),false,12)
 ******** processing request from other ****** 
The normal case ******** 
Receiving question(variable(("at",3,4,4)), 12, false) from 3! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",3,4,4)), false, 12 is [] 
Did not find answer question(variable(("at",3,4,4)), 12, false) from 3! 
Received - main loop: from 0 type 10 content 12
Processing: from 0 type 10 content 12
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 13 
In agent 2 solving step 13 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_13.lp'  '2_enext_12.lp' ' -c t=13'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_13.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_12.lp'  '2_next_13.lp' '2_occ_13.lp'  '2_enext_13.lp' ' -c t=14' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_13.lp' 
!File '2_my_error_13.lp'
Number of errors [] 0
I do not need diagnoses at Step 13!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 1 type 7 content question(variable(("at",1,4,4)),false,13)
********   Need to process 1 7 *question(variable(("at",1,4,4)),false,13)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",1,4,4)), 13, false) from 1! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",1,4,4)), false, 13 is [] 
Did not find answer question(variable(("at",1,4,4)), 13, false) from 1! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 13 ======================= 
Will run step 14 when From and Type and Content are 0 2 next 
Step 14
Sent occurs(2,("down",2,6,3,2),14)
 and wait for response in step 14 
Received - main loop: from 3 type 7 content question(variable(("at",3,3,4)),false,13)
Processing: from 3 type 7 content question(variable(("at",3,3,4)),false,13)
 ******** processing request from other ****** 
The normal case ******** 
Receiving question(variable(("at",3,3,4)), 13, false) from 3! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",3,3,4)), false, 13 is [] 
Did not find answer question(variable(("at",3,3,4)), 13, false) from 3! 
Received - main loop: from 0 type 10 content 13
Processing: from 0 type 10 content 13
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 14 
In agent 2 solving step 14 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_14.lp'  '2_enext_13.lp' ' -c t=14'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_14.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_13.lp'  '2_next_14.lp' '2_occ_14.lp'  '2_enext_14.lp' ' -c t=15' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_14.lp' 
!File '2_my_error_14.lp'
Number of errors [] 0
I do not need diagnoses at Step 14!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 1 type 7 content question(variable(("at",1,3,4)),false,14)
********   Need to process 1 7 *question(variable(("at",1,3,4)),false,14)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",1,3,4)), 14, false) from 1! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",1,3,4)), false, 14 is [] 
Did not find answer question(variable(("at",1,3,4)), 14, false) from 1! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 14 ======================= 
Will run step 15 when From and Type and Content are 0 2 next 
Step 15
Sent occurs(2,("down",2,6,2,1),15)
 and wait for response in step 15 
Received - main loop: from 3 type 7 content question(variable(("at",3,3,3)),false,14)
Processing: from 3 type 7 content question(variable(("at",3,3,3)),false,14)
 ******** processing request from other ****** 
The normal case ******** 
Receiving question(variable(("at",3,3,3)), 14, false) from 3! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",3,3,3)), false, 14 is [] 
Did not find answer question(variable(("at",3,3,3)), 14, false) from 3! 
Received - main loop: from 0 type 10 content 14
Processing: from 0 type 10 content 14
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 15 
In agent 2 solving step 15 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_15.lp'  '2_enext_14.lp' ' -c t=15'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_15.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_14.lp'  '2_next_15.lp' '2_occ_15.lp'  '2_enext_15.lp' ' -c t=16' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_15.lp' 
!File '2_my_error_15.lp'
Number of errors [] 0
I do not need diagnoses at Step 15!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 1 type 7 content question(variable(("at",1,2,4)),false,15)
********   Need to process 1 7 *question(variable(("at",1,2,4)),false,15)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",1,2,4)), 15, false) from 1! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",1,2,4)), false, 15 is [] 
Did not find answer question(variable(("at",1,2,4)), 15, false) from 1! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 15 ======================= 
Will run step 16 when From and Type and Content are 0 2 next 
Step 16
Sent occurs(2,("down",2,6,1,0),16)
 and wait for response in step 16 
Received - main loop: from 0 type 10 content 15
Processing: from 0 type 10 content 15
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 16 
In agent 2 solving step 16 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_16.lp'  '2_enext_15.lp' ' -c t=16'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_16.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_15.lp'  '2_next_16.lp' '2_occ_16.lp'  '2_enext_16.lp' ' -c t=17' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_16.lp' 
!File '2_my_error_16.lp'
Number of errors [] 0
I do not need diagnoses at Step 16!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 1 type 7 content question(variable(("at",1,2,3)),false,16)
********   Need to process 1 7 *question(variable(("at",1,2,3)),false,16)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",1,2,3)), 16, false) from 1! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",1,2,3)), false, 16 is [] 
Did not find answer question(variable(("at",1,2,3)), 16, false) from 1! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 16 ======================= 
Will run step 17 when From and Type and Content are 0 2 next 
Step 17
Sent occurs(2,("right",2,6,0,7),17)
 and wait for response in step 17 
Received - main loop: from 3 type 7 content question(variable(("at",3,3,2)),false,16)
Processing: from 3 type 7 content question(variable(("at",3,3,2)),false,16)
 ******** processing request from other ****** 
The normal case ******** 
Receiving question(variable(("at",3,3,2)), 16, false) from 3! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",3,3,2)), false, 16 is [] 
Did not find answer question(variable(("at",3,3,2)), 16, false) from 3! 
Received - main loop: from 0 type 10 content 16
Processing: from 0 type 10 content 16
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 17 
In agent 2 solving step 17 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_17.lp'  '2_enext_16.lp' ' -c t=17'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_17.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_16.lp'  '2_next_17.lp' '2_occ_17.lp'  '2_enext_17.lp' ' -c t=18' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_17.lp' 
!File '2_my_error_17.lp'
Number of errors [] 0
I do not need diagnoses at Step 17!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 3 type 7 content question(variable(("at",3,3,1)),false,17)
********   Need to process 3 7 *question(variable(("at",3,3,1)),false,17)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",3,3,1)), 17, false) from 3! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",3,3,1)), false, 17 is [] 
Did not find answer question(variable(("at",3,3,1)), 17, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 17 ======================= 
Will run step 18 when From and Type and Content are 0 2 next 
Step 18
Sent occurs(2,("stay",2,7,0),18)
 and wait for response in step 18 
Received - main loop: from 1 type 7 content question(variable(("at",1,1,3)),false,17)
Processing: from 1 type 7 content question(variable(("at",1,1,3)),false,17)
 ******** processing request from other ****** 
The normal case ******** 
Receiving question(variable(("at",1,1,3)), 17, false) from 1! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",1,1,3)), false, 17 is [] 
Did not find answer question(variable(("at",1,1,3)), 17, false) from 1! 
Received - main loop: from 0 type 10 content 17
Processing: from 0 type 10 content 17
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 18 
In agent 2 solving step 18 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_18.lp'  '2_enext_17.lp' ' -c t=18'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_18.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_17.lp'  '2_next_18.lp' '2_occ_18.lp'  '2_enext_18.lp' ' -c t=19' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_18.lp' 
!File '2_my_error_18.lp'
Number of errors [] 0
I do not need diagnoses at Step 18!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 1 type 7 content question(variable(("at",1,0,3)),false,18)
********   Need to process 1 7 *question(variable(("at",1,0,3)),false,18)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",1,0,3)), 18, false) from 1! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",1,0,3)), false, 18 is [] 
Did not find answer question(variable(("at",1,0,3)), 18, false) from 1! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 18 ======================= 
Will run step 19 when From and Type and Content are 0 2 next 
Step 19
Sent none
 and wait for response in step 19 
Received - main loop: from 3 type 7 content question(variable(("at",3,2,1)),false,18)
Processing: from 3 type 7 content question(variable(("at",3,2,1)),false,18)
 ******** processing request from other ****** 
The normal case ******** 
Receiving question(variable(("at",3,2,1)), 18, false) from 3! 
Print a list 
: [] 
==> in finding answers 
Reasons for variable(("at",3,2,1)), false, 18 is [] 
Did not find answer question(variable(("at",3,2,1)), 18, false) from 3! 
Received - main loop: from 0 type 10 content 18
Processing: from 0 type 10 content 18
Received - main loop: from 0 type 2 content done
Processing: from 0 type 2 content done
All done 
Out from agent step 19 
Out from agent step 18 
Out from agent step 17 
Out from agent step 16 
Out from agent step 15 
Out from agent step 14 
Out from agent step 13 
Out from agent step 12 
Out from agent step 11 
Out from agent step 10 
Out from agent step 9 
Out from agent step 8 
Out from agent step 7 
Out from agent step 6 
Out from agent step 5 
Out from agent step 4 
Out from agent step 3 
Out from agent step 2 
Out from agent step 1 
Done executing 
Print a list 
: [] 
Print a list 
: [] 
