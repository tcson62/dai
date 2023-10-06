TIPC Linda server listening at: port_id('<0.0.0:2354444477>')
Number of steps 18
 Domain "mapf_domain.lp"
 Problem "prob1_1.lp"
Done starting 
In preparation 
*********** 
 Computing initial state 
 **************
 Executing: clingo ../../../lps/compute_0.lp  "mapf_domain.lp" "prob1_1.lp" --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_0.lp' 
!*********** 
 Computing the fluents 
 **************
 Step 1
Sent occurs(1,("down",1,7,7,6),1)
 and wait for response in step 1 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 1 
In agent 1 solving step 1 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_1.lp'  '1_enext_0.lp' ' -c t=1'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_1.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_0.lp'  '1_next_1.lp' '1_occ_1.lp'  '1_enext_1.lp' ' -c t=2' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_1.lp' 
!File '1_my_error_1.lp'
Number of errors [] 0
I do not need diagnoses at Step 1!
Waiting for the next round to start ...
Still needs to resolve []
Received: from 0 type 10 content 1
Receiving permission to send action occurrence for the next round ...
Done computing at step 1 ======================= 
Will run step 2 when From and Type and Content are 0 2 next 
Step 2
Sent occurs(1,("left",1,7,6,6),2)
 and wait for response in step 2 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 2 
In agent 1 solving step 2 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_2.lp'  '1_enext_1.lp' ' -c t=2'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_2.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_1.lp'  '1_next_2.lp' '1_occ_2.lp'  '1_enext_2.lp' ' -c t=3' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_2.lp' 
!File '1_my_error_2.lp'
Number of errors [question(other,("left",1,7,6,6),variable(("at",1,7,6)),false,2)] 1
I need some diagnoses at 2 Step!
I need some diagnoses at Step 2!
This needs to be resolved [question(other,("left",1,7,6,6),variable(("at",1,7,6)),false,2)] at Step 2!
I need to look for answer for this "left",1,7,6,6 variable(("at",1,7,6)) false 2 
I did not find answer for this "left",1,7,6,6 variable(("at",1,7,6)) false 2 
Need to ask for help from neighbor  
I send a message to 2 for question(variable(("at",1,7,6)), false, 2) !
I send a message to 3 for question(variable(("at",1,7,6)), false, 2) !
Waiting for the next round to start ...
Still needs to resolve [asked(2,variable(("at",1,7,6)),false,2),asked(3,variable(("at",1,7,6)),false,2)]
Received: from 2 type 6 content answer(variable(("at",1,7,6)),false,2,[no])
********   Need to process 2 6 *answer(variable(("at",1,7,6)),false,2,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,7,6)), false, 2) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,7,6)),false,2)]
Received: from 3 type 6 content answer(variable(("at",1,7,6)),false,2,[no])
********   Need to process 3 6 *answer(variable(("at",1,7,6)),false,2,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,7,6)), false, 2) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 2 ======================= 
Will run step 3 when From and Type and Content are 0 2 next 
Step 3
Sent occurs(1,("down",1,6,6,5),3)
 and wait for response in step 3 
Received - main loop: from 0 type 10 content 2
Processing: from 0 type 10 content 2
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 3 
In agent 1 solving step 3 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_3.lp'  '1_enext_2.lp' ' -c t=3'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_3.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_2.lp'  '1_next_3.lp' '1_occ_3.lp'  '1_enext_3.lp' ' -c t=4' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_3.lp' 
!File '1_my_error_3.lp'
Number of errors [question(other,("down",1,6,6,5),variable(("at",1,6,6)),false,3)] 1
I need some diagnoses at 3 Step!
I need some diagnoses at Step 3!
This needs to be resolved [question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),question(other,("down",1,6,6,5),variable(("at",1,6,6)),false,3)] at Step 3!
Add some information for myself "left",1,7,6,6 variable(("at",1,6,6)) false 3 
I need to look for answer for this "down",1,6,6,5 variable(("at",1,6,6)) false 3 
I did not find answer for this "down",1,6,6,5 variable(("at",1,6,6)) false 3 
Need to ask for help from neighbor  
I send a message to 2 for question(variable(("at",1,6,6)), false, 3) !
I send a message to 3 for question(variable(("at",1,6,6)), false, 3) !
Waiting for the next round to start ...
Still needs to resolve [asked(2,variable(("at",1,6,6)),false,3),asked(3,variable(("at",1,6,6)),false,3)]
Received: from 2 type 6 content answer(variable(("at",1,6,6)),false,3,[no])
********   Need to process 2 6 *answer(variable(("at",1,6,6)),false,3,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,6,6)), false, 3) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,6,6)),false,3)]
Received: from 3 type 6 content answer(variable(("at",1,6,6)),false,3,[no])
********   Need to process 3 6 *answer(variable(("at",1,6,6)),false,3,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,6,6)), false, 3) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 3 ======================= 
Will run step 4 when From and Type and Content are 0 2 next 
Step 4
Sent occurs(1,("down",1,6,5,4),4)
 and wait for response in step 4 
Received - main loop: from 0 type 10 content 3
Processing: from 0 type 10 content 3
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 4 
In agent 1 solving step 4 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_4.lp'  '1_enext_3.lp' ' -c t=4'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_4.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_3.lp'  '1_next_4.lp' '1_occ_4.lp'  '1_enext_4.lp' ' -c t=5' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_4.lp' 
!File '1_my_error_4.lp'
Number of errors [question(other,("down",1,6,5,4),variable(("at",1,6,5)),false,4)] 1
I need some diagnoses at 4 Step!
I need some diagnoses at Step 4!
This needs to be resolved [question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),question(other,("down",1,6,5,4),variable(("at",1,6,5)),false,4)] at Step 4!
Add some information for myself "down",1,6,6,5 variable(("at",1,6,5)) false 4 
I need to look for answer for this "down",1,6,5,4 variable(("at",1,6,5)) false 4 
I did not find answer for this "down",1,6,5,4 variable(("at",1,6,5)) false 4 
Need to ask for help from neighbor  
I send a message to 2 for question(variable(("at",1,6,5)), false, 4) !
I send a message to 3 for question(variable(("at",1,6,5)), false, 4) !
Waiting for the next round to start ...
Still needs to resolve [asked(2,variable(("at",1,6,5)),false,4),asked(3,variable(("at",1,6,5)),false,4)]
Received: from 2 type 6 content answer(variable(("at",1,6,5)),false,4,[no])
********   Need to process 2 6 *answer(variable(("at",1,6,5)),false,4,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,6,5)), false, 4) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,6,5)),false,4)]
Received: from 3 type 6 content answer(variable(("at",1,6,5)),false,4,[no])
********   Need to process 3 6 *answer(variable(("at",1,6,5)),false,4,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,6,5)), false, 4) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 4 ======================= 
Will run step 5 when From and Type and Content are 0 2 next 
Step 5
Sent occurs(1,("stay",1,6,4),5)
 and wait for response in step 5 
Received - main loop: from 0 type 10 content 4
Processing: from 0 type 10 content 4
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 5 
In agent 1 solving step 5 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_5.lp'  '1_enext_4.lp' ' -c t=5'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_5.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_4.lp'  '1_next_5.lp' '1_occ_5.lp'  '1_enext_5.lp' ' -c t=6' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_5.lp' 
!File '1_my_error_5.lp'
Number of errors [question(other,("stay",1,6,4),variable(("at",1,6,4)),false,5)] 1
I need some diagnoses at 5 Step!
I need some diagnoses at Step 5!
This needs to be resolved [question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),question(other,("stay",1,6,4),variable(("at",1,6,4)),false,5)] at Step 5!
Add some information for myself "down",1,6,5,4 variable(("at",1,6,4)) false 5 
I need to look for answer for this "stay",1,6,4 variable(("at",1,6,4)) false 5 
I did not find answer for this "stay",1,6,4 variable(("at",1,6,4)) false 5 
Need to ask for help from neighbor  
I send a message to 2 for question(variable(("at",1,6,4)), false, 5) !
I send a message to 3 for question(variable(("at",1,6,4)), false, 5) !
Waiting for the next round to start ...
Still needs to resolve [asked(2,variable(("at",1,6,4)),false,5),asked(3,variable(("at",1,6,4)),false,5)]
Received: from 3 type 6 content answer(variable(("at",1,6,4)),false,5,[no])
********   Need to process 3 6 *answer(variable(("at",1,6,4)),false,5,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,6,4)), false, 5) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,6,4)),false,5)]
Received: from 0 type 10 content 5
Receiving permission to send action occurrence for the next round ...
Done computing at step 5 ======================= 
Will run step 6 when From and Type and Content are 0 2 next 
Step 6
Sent occurs(1,("up",1,6,4,5),6)
 and wait for response in step 6 
Received - main loop: from 2 type 6 content answer(variable(("at",1,6,4)),false,5,[no])
Processing: from 2 type 6 content answer(variable(("at",1,6,4)),false,5,[no])
Receiving answer(variable(("at",1,6,4)), false, 5) from [no]! 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 6 
In agent 1 solving step 6 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_6.lp'  '1_enext_5.lp' ' -c t=6'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_6.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_5.lp'  '1_next_6.lp' '1_occ_6.lp'  '1_enext_6.lp' ' -c t=7' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_6.lp' 
!File '1_my_error_6.lp'
Number of errors [question(other,("up",1,6,4,5),variable(("at",1,6,4)),false,6)] 1
I need some diagnoses at 6 Step!
I need some diagnoses at Step 6!
This needs to be resolved [question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),question(other,("up",1,6,4,5),variable(("at",1,6,4)),false,6)] at Step 6!
Add some information for myself "stay",1,6,4 variable(("at",1,6,4)) false 6 
I need to look for answer for this "up",1,6,4,5 variable(("at",1,6,4)) false 6 
I found an answer for this "up",1,6,4,5 variable(("at",1,6,4)) false 6 
Waiting for the next round to start ...
Still needs to resolve []
Received: from 0 type 10 content 6
Receiving permission to send action occurrence for the next round ...
Done computing at step 6 ======================= 
Will run step 7 when From and Type and Content are 0 2 next 
Step 7
Sent occurs(1,("down",1,6,5,4),7)
 and wait for response in step 7 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 7 
In agent 1 solving step 7 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_7.lp'  '1_enext_6.lp' ' -c t=7'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_7.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_6.lp'  '1_next_7.lp' '1_occ_7.lp'  '1_enext_7.lp' ' -c t=8' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_7.lp' 
!File '1_my_error_7.lp'
Number of errors [question(other,("down",1,6,5,4),variable(("at",1,6,5)),false,7)] 1
I need some diagnoses at 7 Step!
I need some diagnoses at Step 7!
This needs to be resolved [question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),question(other,("down",1,6,5,4),variable(("at",1,6,5)),false,7)] at Step 7!
Add some information for myself "up",1,6,4,5 variable(("at",1,6,5)) false 7 
I need to look for answer for this "down",1,6,5,4 variable(("at",1,6,5)) false 7 
I found an answer for this "down",1,6,5,4 variable(("at",1,6,5)) false 7 
Waiting for the next round to start ...
Still needs to resolve []
Received: from 3 type 7 content question(variable(("at",3,5,3)),false,7)
********   Need to process 3 7 *question(variable(("at",3,5,3)),false,7)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",3,5,3)), 7, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)])] 
==> in finding answers 
Reasons for variable(("at",3,5,3)), false, 7 is [] 
Did not find answer question(variable(("at",3,5,3)), 7, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 7 ======================= 
Will run step 8 when From and Type and Content are 0 2 next 
Step 8
Sent occurs(1,("down",1,6,4,3),8)
 and wait for response in step 8 
Received - main loop: from 0 type 10 content 7
Processing: from 0 type 10 content 7
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 8 
In agent 1 solving step 8 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_8.lp'  '1_enext_7.lp' ' -c t=8'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_8.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_7.lp'  '1_next_8.lp' '1_occ_8.lp'  '1_enext_8.lp' ' -c t=9' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_8.lp' 
!File '1_my_error_8.lp'
Number of errors [question(other,("down",1,6,4,3),variable(("at",1,6,4)),false,8)] 1
I need some diagnoses at 8 Step!
I need some diagnoses at Step 8!
This needs to be resolved [question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),question(other,("down",1,6,4,3),variable(("at",1,6,4)),false,8)] at Step 8!
Add some information for myself "down",1,6,5,4 variable(("at",1,6,4)) false 8 
I need to look for answer for this "down",1,6,4,3 variable(("at",1,6,4)) false 8 
I found an answer for this "down",1,6,4,3 variable(("at",1,6,4)) false 8 
Waiting for the next round to start ...
Still needs to resolve []
Received: from 3 type 7 content question(variable(("at",3,5,4)),false,8)
********   Need to process 3 7 *question(variable(("at",3,5,4)),false,8)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",3,5,4)), 8, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)])] 
==> in finding answers 
Reasons for variable(("at",3,5,4)), false, 8 is [] 
Did not find answer question(variable(("at",3,5,4)), 8, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 8 ======================= 
Will run step 9 when From and Type and Content are 0 2 next 
Step 9
Sent occurs(1,("stay",1,6,3),9)
 and wait for response in step 9 
Received - main loop: from 0 type 10 content 8
Processing: from 0 type 10 content 8
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 9 
In agent 1 solving step 9 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_9.lp'  '1_enext_8.lp' ' -c t=9'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_9.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_8.lp'  '1_next_9.lp' '1_occ_9.lp'  '1_enext_9.lp' ' -c t=10' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_9.lp' 
!File '1_my_error_9.lp'
Number of errors [question(other,("stay",1,6,3),variable(("at",1,6,3)),false,9)] 1
I need some diagnoses at 9 Step!
I need some diagnoses at Step 9!
This needs to be resolved [question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),question(other,("stay",1,6,3),variable(("at",1,6,3)),false,9)] at Step 9!
Add some information for myself "down",1,6,4,3 variable(("at",1,6,3)) false 9 
I need to look for answer for this "stay",1,6,3 variable(("at",1,6,3)) false 9 
I did not find answer for this "stay",1,6,3 variable(("at",1,6,3)) false 9 
Need to ask for help from neighbor  
I send a message to 2 for question(variable(("at",1,6,3)), false, 9) !
I send a message to 3 for question(variable(("at",1,6,3)), false, 9) !
Waiting for the next round to start ...
Still needs to resolve [asked(2,variable(("at",1,6,3)),false,9),asked(3,variable(("at",1,6,3)),false,9)]
Received: from 3 type 6 content answer(variable(("at",1,6,3)),false,9,[no])
********   Need to process 3 6 *answer(variable(("at",1,6,3)),false,9,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,6,3)), false, 9) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,6,3)),false,9)]
Received: from 0 type 10 content 9
Receiving permission to send action occurrence for the next round ...
Done computing at step 9 ======================= 
Will run step 10 when From and Type and Content are 0 2 next 
Step 10
Sent occurs(1,("up",1,6,3,4),10)
 and wait for response in step 10 
Received - main loop: from 2 type 6 content answer(variable(("at",1,6,3)),false,9,[no])
Processing: from 2 type 6 content answer(variable(("at",1,6,3)),false,9,[no])
Receiving answer(variable(("at",1,6,3)), false, 9) from [no]! 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 10 
In agent 1 solving step 10 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_10.lp'  '1_enext_9.lp' ' -c t=10'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_10.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_9.lp'  '1_next_10.lp' '1_occ_10.lp'  '1_enext_10.lp' ' -c t=11' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_10.lp' 
!File '1_my_error_10.lp'
Number of errors [question(other,("up",1,6,3,4),variable(("at",1,6,3)),false,10)] 1
I need some diagnoses at 10 Step!
I need some diagnoses at Step 10!
This needs to be resolved [question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),question(other,("up",1,6,3,4),variable(("at",1,6,3)),false,10)] at Step 10!
Add some information for myself "stay",1,6,3 variable(("at",1,6,3)) false 10 
I need to look for answer for this "up",1,6,3,4 variable(("at",1,6,3)) false 10 
I found an answer for this "up",1,6,3,4 variable(("at",1,6,3)) false 10 
Waiting for the next round to start ...
Still needs to resolve []
Received: from 0 type 10 content 10
Receiving permission to send action occurrence for the next round ...
Done computing at step 10 ======================= 
Will run step 11 when From and Type and Content are 0 2 next 
Step 11
Sent occurs(1,("left",1,6,4,5),11)
 and wait for response in step 11 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 11 
In agent 1 solving step 11 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_11.lp'  '1_enext_10.lp' ' -c t=11'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_11.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_10.lp'  '1_next_11.lp' '1_occ_11.lp'  '1_enext_11.lp' ' -c t=12' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_11.lp' 
!File '1_my_error_11.lp'
Number of errors [question(other,("left",1,6,4,5),variable(("at",1,6,4)),false,11)] 1
I need some diagnoses at 11 Step!
I need some diagnoses at Step 11!
This needs to be resolved [question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),question(other,("left",1,6,4,5),variable(("at",1,6,4)),false,11)] at Step 11!
Add some information for myself "up",1,6,3,4 variable(("at",1,6,4)) false 11 
I need to look for answer for this "left",1,6,4,5 variable(("at",1,6,4)) false 11 
I found an answer for this "left",1,6,4,5 variable(("at",1,6,4)) false 11 
Waiting for the next round to start ...
Still needs to resolve []
Received: from 0 type 10 content 11
Receiving permission to send action occurrence for the next round ...
Done computing at step 11 ======================= 
Will run step 12 when From and Type and Content are 0 2 next 
Step 12
Sent occurs(1,("left",1,5,4,4),12)
 and wait for response in step 12 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 12 
In agent 1 solving step 12 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_12.lp'  '1_enext_11.lp' ' -c t=12'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_12.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_11.lp'  '1_next_12.lp' '1_occ_12.lp'  '1_enext_12.lp' ' -c t=13' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_12.lp' 
!File '1_my_error_12.lp'
Number of errors [question(other,("left",1,5,4,4),variable(("at",1,5,4)),false,12)] 1
I need some diagnoses at 12 Step!
I need some diagnoses at Step 12!
This needs to be resolved [question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),question(other,("left",1,5,4,4),variable(("at",1,5,4)),false,12)] at Step 12!
Add some information for myself "left",1,6,4,5 variable(("at",1,5,4)) false 12 
I need to look for answer for this "left",1,5,4,4 variable(("at",1,5,4)) false 12 
I did not find answer for this "left",1,5,4,4 variable(("at",1,5,4)) false 12 
Need to ask for help from neighbor  
I send a message to 2 for question(variable(("at",1,5,4)), false, 12) !
I send a message to 3 for question(variable(("at",1,5,4)), false, 12) !
Waiting for the next round to start ...
Still needs to resolve [asked(2,variable(("at",1,5,4)),false,12),asked(3,variable(("at",1,5,4)),false,12)]
Received: from 2 type 6 content answer(variable(("at",1,5,4)),false,12,[no])
********   Need to process 2 6 *answer(variable(("at",1,5,4)),false,12,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,5,4)), false, 12) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,5,4)),false,12)]
Received: from 3 type 7 content question(variable(("at",3,4,4)),false,12)
********   Need to process 3 7 *question(variable(("at",3,4,4)),false,12)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",3,4,4)), 12, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no])] 
==> in finding answers 
Reasons for variable(("at",3,4,4)), false, 12 is [] 
Did not find answer question(variable(("at",3,4,4)), 12, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,5,4)),false,12)]
Receiving question(variable(("at",3,4,4)), 12, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no])] 
==> in finding answers 
Reasons for variable(("at",3,4,4)), false, 12 is [] 
Did not find answer question(variable(("at",3,4,4)), 12, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,5,4)),false,12)]
Receiving question(variable(("at",3,4,4)), 12, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no])] 
==> in finding answers 
Reasons for variable(("at",3,4,4)), false, 12 is [] 
Did not find answer question(variable(("at",3,4,4)), 12, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,5,4)),false,12)]
Receiving question(variable(("at",3,4,4)), 12, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no])] 
==> in finding answers 
Reasons for variable(("at",3,4,4)), false, 12 is [] 
Did not find answer question(variable(("at",3,4,4)), 12, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,5,4)),false,12)]
Receiving question(variable(("at",3,4,4)), 12, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no])] 
==> in finding answers 
Reasons for variable(("at",3,4,4)), false, 12 is [] 
Did not find answer question(variable(("at",3,4,4)), 12, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,5,4)),false,12)]
Receiving question(variable(("at",3,4,4)), 12, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no])] 
==> in finding answers 
Reasons for variable(("at",3,4,4)), false, 12 is [] 
Did not find answer question(variable(("at",3,4,4)), 12, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,5,4)),false,12)]
Receiving question(variable(("at",3,4,4)), 12, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no])] 
==> in finding answers 
Reasons for variable(("at",3,4,4)), false, 12 is [] 
Did not find answer question(variable(("at",3,4,4)), 12, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,5,4)),false,12)]
Receiving question(variable(("at",3,4,4)), 12, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no])] 
==> in finding answers 
Reasons for variable(("at",3,4,4)), false, 12 is [] 
Did not find answer question(variable(("at",3,4,4)), 12, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,5,4)),false,12)]
Receiving question(variable(("at",3,4,4)), 12, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no])] 
==> in finding answers 
Reasons for variable(("at",3,4,4)), false, 12 is [] 
Did not find answer question(variable(("at",3,4,4)), 12, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,5,4)),false,12)]
Receiving question(variable(("at",3,4,4)), 12, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no])] 
==> in finding answers 
Reasons for variable(("at",3,4,4)), false, 12 is [] 
Did not find answer question(variable(("at",3,4,4)), 12, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(3,variable(("at",1,5,4)),false,12)]
Received: from 3 type 6 content answer(variable(("at",1,5,4)),false,12,[no])
********   Need to process 3 6 *answer(variable(("at",1,5,4)),false,12,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,5,4)), false, 12) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 12 ======================= 
Will run step 13 when From and Type and Content are 0 2 next 
Step 13
Sent occurs(1,("left",1,4,4,3),13)
 and wait for response in step 13 
Received - main loop: from 0 type 10 content 12
Processing: from 0 type 10 content 12
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 13 
In agent 1 solving step 13 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_13.lp'  '1_enext_12.lp' ' -c t=13'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_13.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_12.lp'  '1_next_13.lp' '1_occ_13.lp'  '1_enext_13.lp' ' -c t=14' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_13.lp' 
!File '1_my_error_13.lp'
Number of errors [question(other,("left",1,4,4,3),variable(("at",1,4,4)),false,13)] 1
I need some diagnoses at 13 Step!
I need some diagnoses at Step 13!
This needs to be resolved [question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),question(other,("left",1,4,4,3),variable(("at",1,4,4)),false,13)] at Step 13!
Add some information for myself "left",1,5,4,4 variable(("at",1,4,4)) false 13 
I need to look for answer for this "left",1,4,4,3 variable(("at",1,4,4)) false 13 
I did not find answer for this "left",1,4,4,3 variable(("at",1,4,4)) false 13 
Need to ask for help from neighbor  
I send a message to 2 for question(variable(("at",1,4,4)), false, 13) !
I send a message to 3 for question(variable(("at",1,4,4)), false, 13) !
Waiting for the next round to start ...
Still needs to resolve [asked(2,variable(("at",1,4,4)),false,13),asked(3,variable(("at",1,4,4)),false,13)]
Received: from 3 type 7 content question(variable(("at",3,3,4)),false,13)
********   Need to process 3 7 *question(variable(("at",3,3,4)),false,13)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",3,3,4)), 13, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)])] 
==> in finding answers 
Reasons for variable(("at",3,3,4)), false, 13 is [] 
Did not find answer question(variable(("at",3,3,4)), 13, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,4,4)),false,13),asked(3,variable(("at",1,4,4)),false,13)]
Receiving question(variable(("at",3,3,4)), 13, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)])] 
==> in finding answers 
Reasons for variable(("at",3,3,4)), false, 13 is [] 
Did not find answer question(variable(("at",3,3,4)), 13, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,4,4)),false,13),asked(3,variable(("at",1,4,4)),false,13)]
Received: from 3 type 6 content answer(variable(("at",1,4,4)),false,13,[no])
********   Need to process 3 6 *answer(variable(("at",1,4,4)),false,13,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,4,4)), false, 13) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,4,4)),false,13)]
Received: from 2 type 6 content answer(variable(("at",1,4,4)),false,13,[no])
********   Need to process 2 6 *answer(variable(("at",1,4,4)),false,13,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,4,4)), false, 13) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 13 ======================= 
Will run step 14 when From and Type and Content are 0 2 next 
Step 14
Sent occurs(1,("left",1,3,4,2),14)
 and wait for response in step 14 
Received - main loop: from 0 type 10 content 13
Processing: from 0 type 10 content 13
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 14 
In agent 1 solving step 14 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_14.lp'  '1_enext_13.lp' ' -c t=14'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_14.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_13.lp'  '1_next_14.lp' '1_occ_14.lp'  '1_enext_14.lp' ' -c t=15' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_14.lp' 
!File '1_my_error_14.lp'
Number of errors [question(other,("left",1,3,4,2),variable(("at",1,3,4)),false,14)] 1
I need some diagnoses at 14 Step!
I need some diagnoses at Step 14!
This needs to be resolved [question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),question(other,("left",1,3,4,2),variable(("at",1,3,4)),false,14)] at Step 14!
Add some information for myself "left",1,4,4,3 variable(("at",1,3,4)) false 14 
I need to look for answer for this "left",1,3,4,2 variable(("at",1,3,4)) false 14 
I did not find answer for this "left",1,3,4,2 variable(("at",1,3,4)) false 14 
Need to ask for help from neighbor  
I send a message to 2 for question(variable(("at",1,3,4)), false, 14) !
I send a message to 3 for question(variable(("at",1,3,4)), false, 14) !
Waiting for the next round to start ...
Still needs to resolve [asked(2,variable(("at",1,3,4)),false,14),asked(3,variable(("at",1,3,4)),false,14)]
Received: from 3 type 7 content question(variable(("at",3,3,3)),false,14)
********   Need to process 3 7 *question(variable(("at",3,3,3)),false,14)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",3,3,3)), 14, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)])] 
==> in finding answers 
Reasons for variable(("at",3,3,3)), false, 14 is [] 
Did not find answer question(variable(("at",3,3,3)), 14, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,3,4)),false,14),asked(3,variable(("at",1,3,4)),false,14)]
Receiving question(variable(("at",3,3,3)), 14, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)])] 
==> in finding answers 
Reasons for variable(("at",3,3,3)), false, 14 is [] 
Did not find answer question(variable(("at",3,3,3)), 14, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,3,4)),false,14),asked(3,variable(("at",1,3,4)),false,14)]
Receiving question(variable(("at",3,3,3)), 14, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)])] 
==> in finding answers 
Reasons for variable(("at",3,3,3)), false, 14 is [] 
Did not find answer question(variable(("at",3,3,3)), 14, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,3,4)),false,14),asked(3,variable(("at",1,3,4)),false,14)]
Received: from 3 type 6 content answer(variable(("at",1,3,4)),false,14,[no])
********   Need to process 3 6 *answer(variable(("at",1,3,4)),false,14,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,3,4)), false, 14) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,3,4)),false,14)]
Received: from 3 type 6 content answer(variable(("at",1,3,4)),false,14,[no])
********   Need to process 3 6 *answer(variable(("at",1,3,4)),false,14,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,3,4)), false, 14) from [no]! 
Received: from 3 type 6 content answer(variable(("at",1,3,4)),false,14,[no])
********   Need to process 3 6 *answer(variable(("at",1,3,4)),false,14,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,3,4)), false, 14) from [no]! 
Received: from 2 type 6 content answer(variable(("at",1,3,4)),false,14,[no])
********   Need to process 2 6 *answer(variable(("at",1,3,4)),false,14,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,3,4)), false, 14) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 14 ======================= 
Will run step 15 when From and Type and Content are 0 2 next 
Step 15
Sent occurs(1,("down",1,2,4,3),15)
 and wait for response in step 15 
Received - main loop: from 0 type 10 content 14
Processing: from 0 type 10 content 14
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 15 
In agent 1 solving step 15 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_15.lp'  '1_enext_14.lp' ' -c t=15'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_15.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_14.lp'  '1_next_15.lp' '1_occ_15.lp'  '1_enext_15.lp' ' -c t=16' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_15.lp' 
!File '1_my_error_15.lp'
Number of errors [question(other,("down",1,2,4,3),variable(("at",1,2,4)),false,15)] 1
I need some diagnoses at 15 Step!
I need some diagnoses at Step 15!
This needs to be resolved [question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),question(other,("down",1,2,4,3),variable(("at",1,2,4)),false,15)] at Step 15!
Add some information for myself "left",1,3,4,2 variable(("at",1,2,4)) false 15 
I need to look for answer for this "down",1,2,4,3 variable(("at",1,2,4)) false 15 
I did not find answer for this "down",1,2,4,3 variable(("at",1,2,4)) false 15 
Need to ask for help from neighbor  
I send a message to 2 for question(variable(("at",1,2,4)), false, 15) !
I send a message to 3 for question(variable(("at",1,2,4)), false, 15) !
Waiting for the next round to start ...
Still needs to resolve [asked(2,variable(("at",1,2,4)),false,15),asked(3,variable(("at",1,2,4)),false,15)]
Received: from 3 type 6 content answer(variable(("at",1,2,4)),false,15,[no])
********   Need to process 3 6 *answer(variable(("at",1,2,4)),false,15,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,2,4)), false, 15) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,2,4)),false,15)]
Received: from 0 type 10 content 15
Receiving permission to send action occurrence for the next round ...
Done computing at step 15 ======================= 
Will run step 16 when From and Type and Content are 0 2 next 
Step 16
Sent occurs(1,("left",1,2,3,1),16)
 and wait for response in step 16 
Received - main loop: from 2 type 6 content answer(variable(("at",1,2,4)),false,15,[no])
Processing: from 2 type 6 content answer(variable(("at",1,2,4)),false,15,[no])
Receiving answer(variable(("at",1,2,4)), false, 15) from [no]! 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 16 
In agent 1 solving step 16 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_16.lp'  '1_enext_15.lp' ' -c t=16'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_16.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_15.lp'  '1_next_16.lp' '1_occ_16.lp'  '1_enext_16.lp' ' -c t=17' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_16.lp' 
!File '1_my_error_16.lp'
Number of errors [question(other,("left",1,2,3,1),variable(("at",1,2,3)),false,16)] 1
I need some diagnoses at 16 Step!
I need some diagnoses at Step 16!
This needs to be resolved [question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),question(other,("left",1,2,3,1),variable(("at",1,2,3)),false,16)] at Step 16!
Add some information for myself "down",1,2,4,3 variable(("at",1,2,3)) false 16 
I need to look for answer for this "left",1,2,3,1 variable(("at",1,2,3)) false 16 
I did not find answer for this "left",1,2,3,1 variable(("at",1,2,3)) false 16 
Need to ask for help from neighbor  
I send a message to 2 for question(variable(("at",1,2,3)), false, 16) !
I send a message to 3 for question(variable(("at",1,2,3)), false, 16) !
Waiting for the next round to start ...
Still needs to resolve [asked(2,variable(("at",1,2,3)),false,16),asked(3,variable(("at",1,2,3)),false,16)]
Received: from 3 type 7 content question(variable(("at",3,3,2)),false,16)
********   Need to process 3 7 *question(variable(("at",3,3,2)),false,16)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",3,3,2)), 16, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)])] 
==> in finding answers 
Reasons for variable(("at",3,3,2)), false, 16 is [] 
Did not find answer question(variable(("at",3,3,2)), 16, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,2,3)),false,16),asked(3,variable(("at",1,2,3)),false,16)]
Receiving question(variable(("at",3,3,2)), 16, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)])] 
==> in finding answers 
Reasons for variable(("at",3,3,2)), false, 16 is [] 
Did not find answer question(variable(("at",3,3,2)), 16, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,2,3)),false,16),asked(3,variable(("at",1,2,3)),false,16)]
Receiving question(variable(("at",3,3,2)), 16, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)])] 
==> in finding answers 
Reasons for variable(("at",3,3,2)), false, 16 is [] 
Did not find answer question(variable(("at",3,3,2)), 16, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,2,3)),false,16),asked(3,variable(("at",1,2,3)),false,16)]
Receiving question(variable(("at",3,3,2)), 16, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)])] 
==> in finding answers 
Reasons for variable(("at",3,3,2)), false, 16 is [] 
Did not find answer question(variable(("at",3,3,2)), 16, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,2,3)),false,16),asked(3,variable(("at",1,2,3)),false,16)]
Receiving question(variable(("at",3,3,2)), 16, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)])] 
==> in finding answers 
Reasons for variable(("at",3,3,2)), false, 16 is [] 
Did not find answer question(variable(("at",3,3,2)), 16, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,2,3)),false,16),asked(3,variable(("at",1,2,3)),false,16)]
Receiving question(variable(("at",3,3,2)), 16, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)])] 
==> in finding answers 
Reasons for variable(("at",3,3,2)), false, 16 is [] 
Did not find answer question(variable(("at",3,3,2)), 16, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,2,3)),false,16),asked(3,variable(("at",1,2,3)),false,16)]
Received: from 3 type 6 content answer(variable(("at",1,2,3)),false,16,[no])
********   Need to process 3 6 *answer(variable(("at",1,2,3)),false,16,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,2,3)), false, 16) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,2,3)),false,16)]
Received: from 3 type 6 content answer(variable(("at",1,2,3)),false,16,[no])
********   Need to process 3 6 *answer(variable(("at",1,2,3)),false,16,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,2,3)), false, 16) from [no]! 
Received: from 3 type 6 content answer(variable(("at",1,2,3)),false,16,[no])
********   Need to process 3 6 *answer(variable(("at",1,2,3)),false,16,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,2,3)), false, 16) from [no]! 
Received: from 2 type 6 content answer(variable(("at",1,2,3)),false,16,[no])
********   Need to process 2 6 *answer(variable(("at",1,2,3)),false,16,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,2,3)), false, 16) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 16 ======================= 
Will run step 17 when From and Type and Content are 0 2 next 
Step 17
Sent occurs(1,("left",1,1,3,0),17)
 and wait for response in step 17 
Received - main loop: from 0 type 10 content 16
Processing: from 0 type 10 content 16
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 17 
In agent 1 solving step 17 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_17.lp'  '1_enext_16.lp' ' -c t=17'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_17.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_16.lp'  '1_next_17.lp' '1_occ_17.lp'  '1_enext_17.lp' ' -c t=18' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_17.lp' 
!File '1_my_error_17.lp'
Number of errors [question(other,("left",1,1,3,0),variable(("at",1,1,3)),false,17)] 1
I need some diagnoses at 17 Step!
I need some diagnoses at Step 17!
This needs to be resolved [question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),question(other,("left",1,1,3,0),variable(("at",1,1,3)),false,17)] at Step 17!
Add some information for myself "left",1,2,3,1 variable(("at",1,1,3)) false 17 
I need to look for answer for this "left",1,1,3,0 variable(("at",1,1,3)) false 17 
I did not find answer for this "left",1,1,3,0 variable(("at",1,1,3)) false 17 
Need to ask for help from neighbor  
I send a message to 2 for question(variable(("at",1,1,3)), false, 17) !
I send a message to 3 for question(variable(("at",1,1,3)), false, 17) !
Waiting for the next round to start ...
Still needs to resolve [asked(2,variable(("at",1,1,3)),false,17),asked(3,variable(("at",1,1,3)),false,17)]
Received: from 3 type 7 content question(variable(("at",3,3,1)),false,17)
********   Need to process 3 7 *question(variable(("at",3,3,1)),false,17)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",3,3,1)), 17, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(2,no,variable(("at",1,2,3)),false,16),[no]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[self]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[(1,17)])] 
==> in finding answers 
Reasons for variable(("at",3,3,1)), false, 17 is [] 
Did not find answer question(variable(("at",3,3,1)), 17, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,1,3)),false,17),asked(3,variable(("at",1,1,3)),false,17)]
Receiving question(variable(("at",3,3,1)), 17, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(2,no,variable(("at",1,2,3)),false,16),[no]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[self]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[(1,17)])] 
==> in finding answers 
Reasons for variable(("at",3,3,1)), false, 17 is [] 
Did not find answer question(variable(("at",3,3,1)), 17, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,1,3)),false,17),asked(3,variable(("at",1,1,3)),false,17)]
Receiving question(variable(("at",3,3,1)), 17, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(2,no,variable(("at",1,2,3)),false,16),[no]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[self]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[(1,17)])] 
==> in finding answers 
Reasons for variable(("at",3,3,1)), false, 17 is [] 
Did not find answer question(variable(("at",3,3,1)), 17, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,1,3)),false,17),asked(3,variable(("at",1,1,3)),false,17)]
Receiving question(variable(("at",3,3,1)), 17, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(2,no,variable(("at",1,2,3)),false,16),[no]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[self]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[(1,17)])] 
==> in finding answers 
Reasons for variable(("at",3,3,1)), false, 17 is [] 
Did not find answer question(variable(("at",3,3,1)), 17, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,1,3)),false,17),asked(3,variable(("at",1,1,3)),false,17)]
Receiving question(variable(("at",3,3,1)), 17, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(2,no,variable(("at",1,2,3)),false,16),[no]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[self]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[(1,17)])] 
==> in finding answers 
Reasons for variable(("at",3,3,1)), false, 17 is [] 
Did not find answer question(variable(("at",3,3,1)), 17, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,1,3)),false,17),asked(3,variable(("at",1,1,3)),false,17)]
Received: from 3 type 6 content answer(variable(("at",1,1,3)),false,17,[no])
********   Need to process 3 6 *answer(variable(("at",1,1,3)),false,17,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,1,3)), false, 17) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,1,3)),false,17)]
Received: from 3 type 6 content answer(variable(("at",1,1,3)),false,17,[no])
********   Need to process 3 6 *answer(variable(("at",1,1,3)),false,17,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,1,3)), false, 17) from [no]! 
Received: from 3 type 6 content answer(variable(("at",1,1,3)),false,17,[no])
********   Need to process 3 6 *answer(variable(("at",1,1,3)),false,17,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,1,3)), false, 17) from [no]! 
Received: from 3 type 6 content answer(variable(("at",1,1,3)),false,17,[no])
********   Need to process 3 6 *answer(variable(("at",1,1,3)),false,17,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,1,3)), false, 17) from [no]! 
Received: from 0 type 10 content 17
Receiving permission to send action occurrence for the next round ...
Done computing at step 17 ======================= 
Will run step 18 when From and Type and Content are 0 2 next 
Step 18
Sent occurs(1,("stay",1,0,3),18)
 and wait for response in step 18 
Received - main loop: from 2 type 6 content answer(variable(("at",1,1,3)),false,17,[no])
Processing: from 2 type 6 content answer(variable(("at",1,1,3)),false,17,[no])
Receiving answer(variable(("at",1,1,3)), false, 17) from [no]! 
Received - main loop: from 0 type 2 content next
Processing: from 0 type 2 content next
Agent computing next step and diagnosis 18 
In agent 1 solving step 18 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp  
                  "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_occ_18.lp'  '1_enext_17.lp' ' -c t=18'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_18.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "mapf_domain.lp" "prob1_1.lp" '1_agent.lp' '1_next_17.lp'  '1_next_18.lp' '1_occ_18.lp'  '1_enext_18.lp' ' -c t=19' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_18.lp' 
!File '1_my_error_18.lp'
Number of errors [question(other,("stay",1,0,3),variable(("at",1,0,3)),false,18)] 1
I need some diagnoses at 18 Step!
I need some diagnoses at Step 18!
This needs to be resolved [question(self,("left",1,1,3,0),variable(("at",1,0,3)),false,18),question(other,("stay",1,0,3),variable(("at",1,0,3)),false,18)] at Step 18!
Add some information for myself "left",1,1,3,0 variable(("at",1,0,3)) false 18 
I need to look for answer for this "stay",1,0,3 variable(("at",1,0,3)) false 18 
I did not find answer for this "stay",1,0,3 variable(("at",1,0,3)) false 18 
Need to ask for help from neighbor  
I send a message to 2 for question(variable(("at",1,0,3)), false, 18) !
I send a message to 3 for question(variable(("at",1,0,3)), false, 18) !
Waiting for the next round to start ...
Still needs to resolve [asked(2,variable(("at",1,0,3)),false,18),asked(3,variable(("at",1,0,3)),false,18)]
Received: from 3 type 7 content question(variable(("at",3,2,1)),false,18)
********   Need to process 3 7 *question(variable(("at",3,2,1)),false,18)* 
 Calling processing 
 ******** processing request from other ****** 
The bad case ************
Receiving question(variable(("at",3,2,1)), 18, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(2,no,variable(("at",1,2,3)),false,16),[no]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[self]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[(1,17)]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(2,no,variable(("at",1,1,3)),false,17),[no]),answers(question(self,("left",1,1,3,0),variable(("at",1,0,3)),false,18),[self]),answers(question(self,("left",1,1,3,0),variable(("at",1,0,3)),false,18),[(1,18)])] 
==> in finding answers 
Reasons for variable(("at",3,2,1)), false, 18 is [] 
Did not find answer question(variable(("at",3,2,1)), 18, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,0,3)),false,18),asked(3,variable(("at",1,0,3)),false,18)]
Receiving question(variable(("at",3,2,1)), 18, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(2,no,variable(("at",1,2,3)),false,16),[no]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[self]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[(1,17)]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(2,no,variable(("at",1,1,3)),false,17),[no]),answers(question(self,("left",1,1,3,0),variable(("at",1,0,3)),false,18),[self]),answers(question(self,("left",1,1,3,0),variable(("at",1,0,3)),false,18),[(1,18)])] 
==> in finding answers 
Reasons for variable(("at",3,2,1)), false, 18 is [] 
Did not find answer question(variable(("at",3,2,1)), 18, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,0,3)),false,18),asked(3,variable(("at",1,0,3)),false,18)]
Receiving question(variable(("at",3,2,1)), 18, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(2,no,variable(("at",1,2,3)),false,16),[no]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[self]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[(1,17)]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(2,no,variable(("at",1,1,3)),false,17),[no]),answers(question(self,("left",1,1,3,0),variable(("at",1,0,3)),false,18),[self]),answers(question(self,("left",1,1,3,0),variable(("at",1,0,3)),false,18),[(1,18)])] 
==> in finding answers 
Reasons for variable(("at",3,2,1)), false, 18 is [] 
Did not find answer question(variable(("at",3,2,1)), 18, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,0,3)),false,18),asked(3,variable(("at",1,0,3)),false,18)]
Receiving question(variable(("at",3,2,1)), 18, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(2,no,variable(("at",1,2,3)),false,16),[no]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[self]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[(1,17)]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(2,no,variable(("at",1,1,3)),false,17),[no]),answers(question(self,("left",1,1,3,0),variable(("at",1,0,3)),false,18),[self]),answers(question(self,("left",1,1,3,0),variable(("at",1,0,3)),false,18),[(1,18)])] 
==> in finding answers 
Reasons for variable(("at",3,2,1)), false, 18 is [] 
Did not find answer question(variable(("at",3,2,1)), 18, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,0,3)),false,18),asked(3,variable(("at",1,0,3)),false,18)]
Receiving question(variable(("at",3,2,1)), 18, false) from 3! 
Print a list 
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(2,no,variable(("at",1,2,3)),false,16),[no]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[self]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[(1,17)]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(2,no,variable(("at",1,1,3)),false,17),[no]),answers(question(self,("left",1,1,3,0),variable(("at",1,0,3)),false,18),[self]),answers(question(self,("left",1,1,3,0),variable(("at",1,0,3)),false,18),[(1,18)])] 
==> in finding answers 
Reasons for variable(("at",3,2,1)), false, 18 is [] 
Did not find answer question(variable(("at",3,2,1)), 18, false) from 3! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,0,3)),false,18),asked(3,variable(("at",1,0,3)),false,18)]
Received: from 3 type 6 content answer(variable(("at",1,0,3)),false,18,[no])
********   Need to process 3 6 *answer(variable(("at",1,0,3)),false,18,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,0,3)), false, 18) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx [asked(2,variable(("at",1,0,3)),false,18)]
Received: from 3 type 6 content answer(variable(("at",1,0,3)),false,18,[no])
********   Need to process 3 6 *answer(variable(("at",1,0,3)),false,18,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,0,3)), false, 18) from [no]! 
Received: from 2 type 6 content answer(variable(("at",1,0,3)),false,18,[no])
********   Need to process 2 6 *answer(variable(("at",1,0,3)),false,18,[no])* 
 Calling processing 
Receiving answer(variable(("at",1,0,3)), false, 18) from [no]! 
Getting out from processing message ------------------ 
Still needs to resolve xxxx []
Receiving permission to send action occurrence for the next round ...
Done computing at step 18 ======================= 
Will run step 19 when From and Type and Content are 0 2 next 
Step 19
Sent none
 and wait for response in step 19 
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
: [answers(question(self,("down",1,7,7,6),variable(("at",1,7,7)),true,1),[self]),answers(question(2,no,variable(("at",1,7,6)),false,2),[no]),answers(question(3,no,variable(("at",1,7,6)),false,2),[no]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[self]),answers(question(self,("left",1,7,6,6),variable(("at",1,6,6)),false,3),[(1,3)]),answers(question(2,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[self]),answers(question(self,("down",1,6,6,5),variable(("at",1,6,5)),false,4),[(1,4)]),answers(question(2,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,5),[(1,5)]),answers(question(3,no,variable(("at",1,6,4)),false,5),[no]),answers(question(2,no,variable(("at",1,6,4)),false,5),[no]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[self]),answers(question(self,("stay",1,6,4),variable(("at",1,6,4)),false,6),[(1,6)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,4)),false,6),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5)]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[self]),answers(question(self,("up",1,6,4,5),variable(("at",1,6,5)),false,7),[(1,7)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,5)),false,7),[(("down",1,6,6,5),variable(("at",1,6,5)),false,4)]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[self]),answers(question(self,("down",1,6,5,4),variable(("at",1,6,4)),false,8),[(1,8)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,4)),false,8),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6)]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[self]),answers(question(self,("down",1,6,4,3),variable(("at",1,6,3)),false,9),[(1,9)]),answers(question(3,no,variable(("at",1,6,3)),false,9),[no]),answers(question(2,no,variable(("at",1,6,3)),false,9),[no]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[self]),answers(question(self,("stay",1,6,3),variable(("at",1,6,3)),false,10),[(1,10)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,3)),false,10),[(("down",1,6,4,3),variable(("at",1,6,3)),false,9)]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[self]),answers(question(self,("up",1,6,3,4),variable(("at",1,6,4)),false,11),[(1,11)]),answers(question(self,("left",1,6,4,5),variable(("at",1,6,4)),false,11),[(("down",1,6,5,4),variable(("at",1,6,4)),false,5),(("stay",1,6,4),variable(("at",1,6,4)),false,6),(("down",1,6,5,4),variable(("at",1,6,4)),false,8)]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[self]),answers(question(self,("left",1,6,4,5),variable(("at",1,5,4)),false,12),[(1,12)]),answers(question(2,no,variable(("at",1,5,4)),false,12),[no]),answers(question(3,no,variable(("at",1,5,4)),false,12),[no]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[self]),answers(question(self,("left",1,5,4,4),variable(("at",1,4,4)),false,13),[(1,13)]),answers(question(3,no,variable(("at",1,4,4)),false,13),[no]),answers(question(2,no,variable(("at",1,4,4)),false,13),[no]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[self]),answers(question(self,("left",1,4,4,3),variable(("at",1,3,4)),false,14),[(1,14)]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(3,no,variable(("at",1,3,4)),false,14),[no]),answers(question(2,no,variable(("at",1,3,4)),false,14),[no]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[self]),answers(question(self,("left",1,3,4,2),variable(("at",1,2,4)),false,15),[(1,15)]),answers(question(3,no,variable(("at",1,2,4)),false,15),[no]),answers(question(2,no,variable(("at",1,2,4)),false,15),[no]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[self]),answers(question(self,("down",1,2,4,3),variable(("at",1,2,3)),false,16),[(1,16)]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(3,no,variable(("at",1,2,3)),false,16),[no]),answers(question(2,no,variable(("at",1,2,3)),false,16),[no]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[self]),answers(question(self,("left",1,2,3,1),variable(("at",1,1,3)),false,17),[(1,17)]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(3,no,variable(("at",1,1,3)),false,17),[no]),answers(question(2,no,variable(("at",1,1,3)),false,17),[no]),answers(question(self,("left",1,1,3,0),variable(("at",1,0,3)),false,18),[self]),answers(question(self,("left",1,1,3,0),variable(("at",1,0,3)),false,18),[(1,18)]),answers(question(3,no,variable(("at",1,0,3)),false,18),[no]),answers(question(3,no,variable(("at",1,0,3)),false,18),[no]),answers(question(2,no,variable(("at",1,0,3)),false,18),[no])] 
Print a list 
: [] 
