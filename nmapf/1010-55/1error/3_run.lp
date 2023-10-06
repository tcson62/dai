TIPC Linda server listening at: port_id('<0.0.0:2289548605>')
Number of steps 15
 Domain "../../mapf_domain.lp"
 Problem "prob2_3.lp"
Done starting 
In preparation 
*********** 
 Computing initial state 
 **************
 Executing: clingo ../../../lps/compute_0.lp  "../../mapf_domain.lp" "prob2_3.lp" --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_0.lp' 
!Setting up expected initial state
 clingo '3_next_0.lp' -c t=0 --outf=0 -V0 --out-atomf=%s. | head -n1 >  '3_enext_0.lp' 
*********** 
 Computing the fluents 
 **************
 In the loop ... 
Receiving ... From 0  Type 2 Content next(1) --- 
Step sending action 1
Done sending action occurs(3,("left",3,7,5,6),1) at step 1 ======================= 
Receiving ... From 0  Type 10 Content next(1) --- 
Step processing observations 1
Agent computing next step and diagnosis 1 
In agent 3 solving step 1 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_1.lp'  '3_enext_0.lp' ' -c t=1'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_1.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_0.lp'  '3_next_1.lp' '3_occ_1.lp'  '3_enext_1.lp' ' -c t=2' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_1.lp' 
!File '3_my_error_1.lp'
Number of errors [] 0
I do not need diagnoses at Step 1!
Done initializing the diagnosis at step 1 ======================= 
Receiving ... From 0  Type 2 Content next(2) --- 
Step sending action 2
Done sending action occurs(3,("left",3,6,5,5),2) at step 2 ======================= 
Receiving ... From 0  Type 10 Content next(2) --- 
Step processing observations 2
Agent computing next step and diagnosis 2 
In agent 3 solving step 2 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_2.lp'  '3_enext_1.lp' ' -c t=2'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_2.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_1.lp'  '3_next_2.lp' '3_occ_2.lp'  '3_enext_2.lp' ' -c t=3' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_2.lp' 
!File '3_my_error_2.lp'
Number of errors [] 0
I do not need diagnoses at Step 2!
Done initializing the diagnosis at step 2 ======================= 
Receiving ... From 0  Type 2 Content next(3) --- 
Step sending action 3
Done sending action occurs(3,("left",3,5,5,4),3) at step 3 ======================= 
Receiving ... From 0  Type 10 Content next(3) --- 
Step processing observations 3
Agent computing next step and diagnosis 3 
In agent 3 solving step 3 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_3.lp'  '3_enext_2.lp' ' -c t=3'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_3.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_2.lp'  '3_next_3.lp' '3_occ_3.lp'  '3_enext_3.lp' ' -c t=4' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_3.lp' 
!File '3_my_error_3.lp'
Number of errors [] 0
I do not need diagnoses at Step 3!
Done initializing the diagnosis at step 3 ======================= 
Receiving ... From 0  Type 2 Content next(4) --- 
Step sending action 4
Done sending action occurs(3,("down",3,4,5,4),4) at step 4 ======================= 
Receiving ... From 0  Type 10 Content next(4) --- 
Step processing observations 4
Agent computing next step and diagnosis 4 
In agent 3 solving step 4 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_4.lp'  '3_enext_3.lp' ' -c t=4'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_4.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_3.lp'  '3_next_4.lp' '3_occ_4.lp'  '3_enext_4.lp' ' -c t=5' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_4.lp' 
!File '3_my_error_4.lp'
Number of errors [] 0
I do not need diagnoses at Step 4!
Done initializing the diagnosis at step 4 ======================= 
Receiving ... From 0  Type 2 Content next(5) --- 
Step sending action 5
Done sending action occurs(3,("left",3,4,4,3),5) at step 5 ======================= 
Receiving ... From 0  Type 10 Content next(5) --- 
Step processing observations 5
Agent computing next step and diagnosis 5 
In agent 3 solving step 5 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_5.lp'  '3_enext_4.lp' ' -c t=5'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_5.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_4.lp'  '3_next_5.lp' '3_occ_5.lp'  '3_enext_5.lp' ' -c t=6' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_5.lp' 
!File '3_my_error_5.lp'
Number of errors [question(other,("left",3,4,4,3),variable(("at",3,4,4)),false,5)] 1
I need some diagnoses at 5 Step!
I need some diagnoses at Step 5!
This needs to be resolved [question(other,("left",3,4,4,3),variable(("at",3,4,4)),false,5)] at Step 5!
I need to look for answer for this "left",3,4,4,3 variable(("at",3,4,4)) false 5 
I found an answer for this "left",3,4,4,3 variable(("at",3,4,4)) false 5 
Done initializing the diagnosis at step 5 ======================= 
Receiving ... From 0  Type 2 Content next(6) --- 
Step sending action 6
Done sending action occurs(3,("left",3,3,4,2),6) at step 6 ======================= 
Receiving ... From 0  Type 10 Content next(6) --- 
Step processing observations 6
Agent computing next step and diagnosis 6 
In agent 3 solving step 6 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_6.lp'  '3_enext_5.lp' ' -c t=6'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_6.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_5.lp'  '3_next_6.lp' '3_occ_6.lp'  '3_enext_6.lp' ' -c t=7' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_6.lp' 
!File '3_my_error_6.lp'
Number of errors [question(other,("left",3,3,4,2),variable(("at",3,3,4)),false,6)] 1
I need some diagnoses at 6 Step!
I need some diagnoses at Step 6!
This needs to be resolved [question(self,("left",3,4,4,3),variable(("at",3,3,4)),false,6),question(self,("left",3,4,4,3),variable(("occupied",3,4)),false,6),question(other,("left",3,3,4,2),variable(("at",3,3,4)),false,6)] at Step 6!
Add some information for myself "left",3,4,4,3 variable(("at",3,3,4)) false 6 
Add some information for myself "left",3,4,4,3 variable(("occupied",3,4)) false 6 
I need to look for answer for this "left",3,3,4,2 variable(("at",3,3,4)) false 6 
I did not find answer for this "left",3,3,4,2 variable(("at",3,3,4)) false 6 
I already asked these people [] for answer!
Need to ask for help from neighbor [2,5,4,1] 
I send a message to 2 for question(variable(("at",3,3,4)), false, 6) !
I send a message to 5 for question(variable(("at",3,3,4)), false, 6) !
I send a message to 4 for question(variable(("at",3,3,4)), false, 6) !
I send a message to 1 for question(variable(("at",3,3,4)), false, 6) !
Done initializing the diagnosis at step 6 ======================= 
Receiving ... From 0  Type 2 Content next(7) --- 
Step sending action 7
Done sending action occurs(3,("down",3,2,4,3),7) at step 7 ======================= 
Receiving ... From 0  Type 10 Content next(7) --- 
Step processing observations 7
Agent computing next step and diagnosis 7 
In agent 3 solving step 7 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_7.lp'  '3_enext_6.lp' ' -c t=7'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_7.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_6.lp'  '3_next_7.lp' '3_occ_7.lp'  '3_enext_7.lp' ' -c t=8' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_7.lp' 
!File '3_my_error_7.lp'
Number of errors [question(other,("down",3,2,4,3),variable(("at",3,2,4)),false,7)] 1
I need some diagnoses at 7 Step!
I need some diagnoses at Step 7!
This needs to be resolved [question(self,("left",3,3,4,2),variable(("at",3,2,4)),false,7),question(self,("left",3,3,4,2),variable(("occupied",2,4)),false,7),question(other,("down",3,2,4,3),variable(("at",3,2,4)),false,7)] at Step 7!
Add some information for myself "left",3,3,4,2 variable(("at",3,2,4)) false 7 
Add some information for myself "left",3,3,4,2 variable(("occupied",2,4)) false 7 
I need to look for answer for this "down",3,2,4,3 variable(("at",3,2,4)) false 7 
I did not find answer for this "down",3,2,4,3 variable(("at",3,2,4)) false 7 
I already asked these people [] for answer!
Need to ask for help from neighbor [2,5,4,1] 
I send a message to 2 for question(variable(("at",3,2,4)), false, 7) !
I send a message to 5 for question(variable(("at",3,2,4)), false, 7) !
I send a message to 4 for question(variable(("at",3,2,4)), false, 7) !
I send a message to 1 for question(variable(("at",3,2,4)), false, 7) !
Done initializing the diagnosis at step 7 ======================= 
Receiving ... From 0  Type 2 Content next(8) --- 
Step sending action 8
Done sending action occurs(3,("left",3,2,3,1),8) at step 8 ======================= 
Receiving ... From 0  Type 10 Content next(8) --- 
Step processing observations 8
Agent computing next step and diagnosis 8 
In agent 3 solving step 8 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_8.lp'  '3_enext_7.lp' ' -c t=8'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_8.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_7.lp'  '3_next_8.lp' '3_occ_8.lp'  '3_enext_8.lp' ' -c t=9' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_8.lp' 
!File '3_my_error_8.lp'
Number of errors [question(other,("left",3,2,3,1),variable(("at",3,2,3)),false,8)] 1
I need some diagnoses at 8 Step!
I need some diagnoses at Step 8!
This needs to be resolved [question(self,("down",3,2,4,3),variable(("at",3,2,3)),false,8),question(self,("down",3,2,4,3),variable(("occupied",2,3)),false,8),question(other,("left",3,2,3,1),variable(("at",3,2,3)),false,8)] at Step 8!
Add some information for myself "down",3,2,4,3 variable(("at",3,2,3)) false 8 
Add some information for myself "down",3,2,4,3 variable(("occupied",2,3)) false 8 
I need to look for answer for this "left",3,2,3,1 variable(("at",3,2,3)) false 8 
I did not find answer for this "left",3,2,3,1 variable(("at",3,2,3)) false 8 
I already asked these people [] for answer!
Need to ask for help from neighbor [2,5,4,1] 
I send a message to 2 for question(variable(("at",3,2,3)), false, 8) !
I send a message to 5 for question(variable(("at",3,2,3)), false, 8) !
I send a message to 4 for question(variable(("at",3,2,3)), false, 8) !
I send a message to 1 for question(variable(("at",3,2,3)), false, 8) !
Done initializing the diagnosis at step 8 ======================= 
Receiving ... From 0  Type 2 Content next(9) --- 
Step sending action 9
Done sending action occurs(3,("stay",3,1,3),9) at step 9 ======================= 
Receiving ... From 0  Type 10 Content next(9) --- 
Step processing observations 9
Agent computing next step and diagnosis 9 
In agent 3 solving step 9 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_9.lp'  '3_enext_8.lp' ' -c t=9'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_9.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_8.lp'  '3_next_9.lp' '3_occ_9.lp'  '3_enext_9.lp' ' -c t=10' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_9.lp' 
!File '3_my_error_9.lp'
Number of errors [question(other,("stay",3,1,3),variable(("at",3,1,3)),false,9)] 1
I need some diagnoses at 9 Step!
I need some diagnoses at Step 9!
This needs to be resolved [question(self,("left",3,2,3,1),variable(("at",3,1,3)),false,9),question(self,("left",3,2,3,1),variable(("occupied",1,3)),false,9),question(other,("stay",3,1,3),variable(("at",3,1,3)),false,9)] at Step 9!
Add some information for myself "left",3,2,3,1 variable(("at",3,1,3)) false 9 
Add some information for myself "left",3,2,3,1 variable(("occupied",1,3)) false 9 
I need to look for answer for this "stay",3,1,3 variable(("at",3,1,3)) false 9 
I did not find answer for this "stay",3,1,3 variable(("at",3,1,3)) false 9 
I already asked these people [] for answer!
Need to ask for help from neighbor [2,5,4,1] 
I send a message to 2 for question(variable(("at",3,1,3)), false, 9) !
I send a message to 5 for question(variable(("at",3,1,3)), false, 9) !
I send a message to 4 for question(variable(("at",3,1,3)), false, 9) !
I send a message to 1 for question(variable(("at",3,1,3)), false, 9) !
Done initializing the diagnosis at step 9 ======================= 
Receiving ... From 0  Type 2 Content next(10) --- 
Step sending action 10
Done sending action occurs(3,("stay",3,1,3),10) at step 10 ======================= 
Receiving ... From 0  Type 10 Content next(10) --- 
Step processing observations 10
Agent computing next step and diagnosis 10 
In agent 3 solving step 10 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_10.lp'  '3_enext_9.lp' ' -c t=10'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_10.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_9.lp'  '3_next_10.lp' '3_occ_10.lp'  '3_enext_10.lp' ' -c t=11' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_10.lp' 
!File '3_my_error_10.lp'
Number of errors [question(other,("stay",3,1,3),variable(("at",3,1,3)),false,10)] 1
I need some diagnoses at 10 Step!
I need some diagnoses at Step 10!
This needs to be resolved [question(self,("stay",3,1,3),variable(("occupied",1,3)),false,10),question(other,("stay",3,1,3),variable(("at",3,1,3)),false,10)] at Step 10!
Add some information for myself "stay",3,1,3 variable(("occupied",1,3)) false 10 
I need to look for answer for this "stay",3,1,3 variable(("at",3,1,3)) false 10 
I found an answer for this "stay",3,1,3 variable(("at",3,1,3)) false 10 
Done initializing the diagnosis at step 10 ======================= 
Receiving ... From 0  Type 2 Content next(11) --- 
Step sending action 11
Done sending action occurs(3,("down",3,1,3,2),11) at step 11 ======================= 
Receiving ... From 0  Type 10 Content next(11) --- 
Step processing observations 11
Agent computing next step and diagnosis 11 
In agent 3 solving step 11 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_11.lp'  '3_enext_10.lp' ' -c t=11'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_11.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_10.lp'  '3_next_11.lp' '3_occ_11.lp'  '3_enext_11.lp' ' -c t=12' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_11.lp' 
!File '3_my_error_11.lp'
Number of errors [question(other,("down",3,1,3,2),variable(("at",3,1,3)),false,11)] 1
I need some diagnoses at 11 Step!
I need some diagnoses at Step 11!
This needs to be resolved [question(self,("stay",3,1,3),variable(("occupied",1,3)),false,11),question(other,("down",3,1,3,2),variable(("at",3,1,3)),false,11)] at Step 11!
Add some information for myself "stay",3,1,3 variable(("occupied",1,3)) false 11 
I need to look for answer for this "down",3,1,3,2 variable(("at",3,1,3)) false 11 
I found an answer for this "down",3,1,3,2 variable(("at",3,1,3)) false 11 
Done initializing the diagnosis at step 11 ======================= 
Receiving ... From 0  Type 2 Content next(12) --- 
Step sending action 12
Done sending action occurs(3,("left",3,1,2,0),12) at step 12 ======================= 
Receiving ... From 0  Type 10 Content next(12) --- 
Step processing observations 12
Agent computing next step and diagnosis 12 
In agent 3 solving step 12 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_12.lp'  '3_enext_11.lp' ' -c t=12'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_12.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_11.lp'  '3_next_12.lp' '3_occ_12.lp'  '3_enext_12.lp' ' -c t=13' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_12.lp' 
!File '3_my_error_12.lp'
Number of errors [question(other,("left",3,1,2,0),variable(("at",3,1,2)),false,12)] 1
I need some diagnoses at 12 Step!
I need some diagnoses at Step 12!
This needs to be resolved [question(self,("down",3,1,3,2),variable(("at",3,1,2)),false,12),question(self,("down",3,1,3,2),variable(("occupied",1,2)),false,12),question(other,("left",3,1,2,0),variable(("at",3,1,2)),false,12)] at Step 12!
Add some information for myself "down",3,1,3,2 variable(("at",3,1,2)) false 12 
Add some information for myself "down",3,1,3,2 variable(("occupied",1,2)) false 12 
I need to look for answer for this "left",3,1,2,0 variable(("at",3,1,2)) false 12 
I did not find answer for this "left",3,1,2,0 variable(("at",3,1,2)) false 12 
I already asked these people [] for answer!
Need to ask for help from neighbor [2,5,4,1] 
I send a message to 2 for question(variable(("at",3,1,2)), false, 12) !
I send a message to 5 for question(variable(("at",3,1,2)), false, 12) !
I send a message to 4 for question(variable(("at",3,1,2)), false, 12) !
I send a message to 1 for question(variable(("at",3,1,2)), false, 12) !
Done initializing the diagnosis at step 12 ======================= 
Receiving ... From 0  Type 2 Content next(13) --- 
Step sending action 13
Done sending action occurs(3,("down",3,0,2,1),13) at step 13 ======================= 
Receiving ... From 0  Type 10 Content next(13) --- 
Step processing observations 13
Agent computing next step and diagnosis 13 
In agent 3 solving step 13 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_13.lp'  '3_enext_12.lp' ' -c t=13'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_13.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_12.lp'  '3_next_13.lp' '3_occ_13.lp'  '3_enext_13.lp' ' -c t=14' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_13.lp' 
!File '3_my_error_13.lp'
Number of errors [question(other,("down",3,0,2,1),variable(("at",3,0,2)),false,13)] 1
I need some diagnoses at 13 Step!
I need some diagnoses at Step 13!
This needs to be resolved [question(self,("left",3,1,2,0),variable(("at",3,0,2)),false,13),question(self,("left",3,1,2,0),variable(("occupied",0,2)),false,13),question(other,("down",3,0,2,1),variable(("at",3,0,2)),false,13)] at Step 13!
Add some information for myself "left",3,1,2,0 variable(("at",3,0,2)) false 13 
Add some information for myself "left",3,1,2,0 variable(("occupied",0,2)) false 13 
I need to look for answer for this "down",3,0,2,1 variable(("at",3,0,2)) false 13 
I did not find answer for this "down",3,0,2,1 variable(("at",3,0,2)) false 13 
I already asked these people [] for answer!
Need to ask for help from neighbor [2,5,4,1] 
I send a message to 2 for question(variable(("at",3,0,2)), false, 13) !
I send a message to 5 for question(variable(("at",3,0,2)), false, 13) !
I send a message to 4 for question(variable(("at",3,0,2)), false, 13) !
I send a message to 1 for question(variable(("at",3,0,2)), false, 13) !
Done initializing the diagnosis at step 13 ======================= 
Receiving ... From 0  Type 2 Content next(14) --- 
Step sending action 14
Done sending action occurs(3,("up",3,0,1,2),14) at step 14 ======================= 
Receiving ... From 0  Type 10 Content next(14) --- 
Step processing observations 14
Agent computing next step and diagnosis 14 
In agent 3 solving step 14 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_14.lp'  '3_enext_13.lp' ' -c t=14'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_14.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_13.lp'  '3_next_14.lp' '3_occ_14.lp'  '3_enext_14.lp' ' -c t=15' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_14.lp' 
!File '3_my_error_14.lp'
Number of errors [question(other,("up",3,0,1,2),variable(("at",3,0,1)),false,14)] 1
I need some diagnoses at 14 Step!
I need some diagnoses at Step 14!
This needs to be resolved [question(self,("down",3,0,2,1),variable(("at",3,0,1)),false,14),question(self,("down",3,0,2,1),variable(("occupied",0,1)),false,14),question(other,("up",3,0,1,2),variable(("at",3,0,1)),false,14)] at Step 14!
Add some information for myself "down",3,0,2,1 variable(("at",3,0,1)) false 14 
Add some information for myself "down",3,0,2,1 variable(("occupied",0,1)) false 14 
I need to look for answer for this "up",3,0,1,2 variable(("at",3,0,1)) false 14 
I did not find answer for this "up",3,0,1,2 variable(("at",3,0,1)) false 14 
I already asked these people [] for answer!
Need to ask for help from neighbor [2,5,4,1] 
I send a message to 2 for question(variable(("at",3,0,1)), false, 14) !
I send a message to 5 for question(variable(("at",3,0,1)), false, 14) !
I send a message to 4 for question(variable(("at",3,0,1)), false, 14) !
I send a message to 1 for question(variable(("at",3,0,1)), false, 14) !
Done initializing the diagnosis at step 14 ======================= 
Receiving ... From 0  Type 2 Content next(15) --- 
Step sending action 15
Done sending action occurs(3,("stay",3,0,2),15) at step 15 ======================= 
Receiving ... From 0  Type 10 Content next(15) --- 
Step processing observations 15
Agent computing next step and diagnosis 15 
In agent 3 solving step 15 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_occ_15.lp'  '3_enext_14.lp' ' -c t=15'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_15.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_3.lp" '3_agent.lp' '3_next_14.lp'  '3_next_15.lp' '3_occ_15.lp'  '3_enext_15.lp' ' -c t=16' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_15.lp' 
!File '3_my_error_15.lp'
Number of errors [question(other,("stay",3,0,2),variable(("at",3,0,2)),false,15)] 1
I need some diagnoses at 15 Step!
I need some diagnoses at Step 15!
This needs to be resolved [question(self,("up",3,0,1,2),variable(("at",3,0,2)),false,15),question(self,("up",3,0,1,2),variable(("occupied",0,2)),false,15),question(other,("stay",3,0,2),variable(("at",3,0,2)),false,15)] at Step 15!
Add some information for myself "up",3,0,1,2 variable(("at",3,0,2)) false 15 
Add some information for myself "up",3,0,1,2 variable(("occupied",0,2)) false 15 
I need to look for answer for this "stay",3,0,2 variable(("at",3,0,2)) false 15 
I found an answer for this "stay",3,0,2 variable(("at",3,0,2)) false 15 
Done initializing the diagnosis at step 15 ======================= 
Receiving ... From 0  Type 2 Content next(16) --- 
Step sending action 16
Done with executing all actions 16 ======================= 
Receiving ... From 0  Type 2 Content done --- 
All done 
Out from agent loop 
Done executing 
List of answers 
 [answers(question(self,("left",3,6,5,5),variable(("occupied",6,5)),true,2),[self]),answers(question(self,("left",3,5,5,4),variable(("occupied",5,5)),true,3),[self]),answers(question(self,("down",3,4,5,4),variable(("at",3,4,4)),false,4),[self]),answers(question(self,("down",3,4,5,4),variable(("occupied",4,4)),false,4),[self]),answers(question(self,("down",3,4,5,4),variable(("at",3,4,5)),true,4),[self]),answers(question(self,("down",3,4,5,4),variable(("occupied",4,5)),true,4),[self]),answers(question(self,("left",3,4,4,3),variable(("at",3,4,4)),false,5),[(("down",3,4,5,4),variable(("at",3,4,4)),false,4)]),answers(question(self,("left",3,4,4,3),variable(("at",3,3,4)),false,6),[self]),answers(question(self,("left",3,4,4,3),variable(("occupied",3,4)),false,6),[self]),answers(question(self,("left",3,4,4,3),variable(("at",3,3,4)),false,6),[(3,6)]),answers(question(self,("left",3,4,4,3),variable(("occupied",3,4)),false,6),[(3,6)]),answers(question(self,("left",3,3,4,2),variable(("at",3,2,4)),false,7),[self]),answers(question(self,("left",3,3,4,2),variable(("occupied",2,4)),false,7),[self]),answers(question(self,("left",3,3,4,2),variable(("at",3,2,4)),false,7),[(3,7)]),answers(question(self,("left",3,3,4,2),variable(("occupied",2,4)),false,7),[(3,7)]),answers(question(self,("down",3,2,4,3),variable(("at",3,2,3)),false,8),[self]),answers(question(self,("down",3,2,4,3),variable(("occupied",2,3)),false,8),[self]),answers(question(self,("down",3,2,4,3),variable(("at",3,2,3)),false,8),[(3,8)]),answers(question(self,("down",3,2,4,3),variable(("occupied",2,3)),false,8),[(3,8)]),answers(question(self,("left",3,2,3,1),variable(("at",3,1,3)),false,9),[self]),answers(question(self,("left",3,2,3,1),variable(("occupied",1,3)),false,9),[self]),answers(question(self,("left",3,2,3,1),variable(("at",3,1,3)),false,9),[(3,9)]),answers(question(self,("left",3,2,3,1),variable(("occupied",1,3)),false,9),[(3,9)]),answers(question(self,("stay",3,1,3),variable(("occupied",1,3)),false,10),[self]),answers(question(self,("stay",3,1,3),variable(("occupied",1,3)),false,10),[(3,10)]),answers(question(self,("stay",3,1,3),variable(("at",3,1,3)),false,10),[(("left",3,2,3,1),variable(("at",3,1,3)),false,9)]),answers(question(self,("stay",3,1,3),variable(("occupied",1,3)),false,11),[self]),answers(question(self,("stay",3,1,3),variable(("occupied",1,3)),false,11),[(3,11)]),answers(question(self,("down",3,1,3,2),variable(("at",3,1,3)),false,11),[(("left",3,2,3,1),variable(("at",3,1,3)),false,9)]),answers(question(self,("down",3,1,3,2),variable(("at",3,1,2)),false,12),[self]),answers(question(self,("down",3,1,3,2),variable(("occupied",1,2)),false,12),[self]),answers(question(self,("down",3,1,3,2),variable(("at",3,1,2)),false,12),[(3,12)]),answers(question(self,("down",3,1,3,2),variable(("occupied",1,2)),false,12),[(3,12)]),answers(question(self,("left",3,1,2,0),variable(("at",3,0,2)),false,13),[self]),answers(question(self,("left",3,1,2,0),variable(("occupied",0,2)),false,13),[self]),answers(question(self,("left",3,1,2,0),variable(("at",3,0,2)),false,13),[(3,13)]),answers(question(self,("left",3,1,2,0),variable(("occupied",0,2)),false,13),[(3,13)]),answers(question(self,("down",3,0,2,1),variable(("at",3,0,1)),false,14),[self]),answers(question(self,("down",3,0,2,1),variable(("occupied",0,1)),false,14),[self]),answers(question(self,("down",3,0,2,1),variable(("at",3,0,1)),false,14),[(3,14)]),answers(question(self,("down",3,0,2,1),variable(("occupied",0,1)),false,14),[(3,14)]),answers(question(self,("up",3,0,1,2),variable(("at",3,0,2)),false,15),[self]),answers(question(self,("up",3,0,1,2),variable(("occupied",0,2)),false,15),[self]),answers(question(self,("up",3,0,1,2),variable(("at",3,0,2)),false,15),[(3,15)]),answers(question(self,("up",3,0,1,2),variable(("occupied",0,2)),false,15),[(3,15)]),answers(question(self,("stay",3,0,2),variable(("at",3,0,2)),false,15),[(("left",3,1,2,0),variable(("at",3,0,2)),false,13)])] 

List of questions that I sent and received 
 [(1,2,variable(("at",3,3,4)),false,6),(1,5,variable(("at",3,3,4)),false,6),(1,4,variable(("at",3,3,4)),false,6),(1,1,variable(("at",3,3,4)),false,6),(1,2,variable(("at",3,2,4)),false,7),(1,5,variable(("at",3,2,4)),false,7),(1,4,variable(("at",3,2,4)),false,7),(1,1,variable(("at",3,2,4)),false,7),(1,2,variable(("at",3,2,3)),false,8),(1,5,variable(("at",3,2,3)),false,8),(1,4,variable(("at",3,2,3)),false,8),(1,1,variable(("at",3,2,3)),false,8),(1,2,variable(("at",3,1,3)),false,9),(1,5,variable(("at",3,1,3)),false,9),(1,4,variable(("at",3,1,3)),false,9),(1,1,variable(("at",3,1,3)),false,9),(1,2,variable(("at",3,1,2)),false,12),(1,5,variable(("at",3,1,2)),false,12),(1,4,variable(("at",3,1,2)),false,12),(1,1,variable(("at",3,1,2)),false,12),(1,2,variable(("at",3,0,2)),false,13),(1,5,variable(("at",3,0,2)),false,13),(1,4,variable(("at",3,0,2)),false,13),(1,1,variable(("at",3,0,2)),false,13),(1,2,variable(("at",3,0,1)),false,14),(1,5,variable(("at",3,0,1)),false,14),(1,4,variable(("at",3,0,1)),false,14),(1,1,variable(("at",3,0,1)),false,14)] 

