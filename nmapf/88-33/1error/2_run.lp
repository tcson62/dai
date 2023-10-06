TIPC Linda server listening at: port_id('<0.0.0:4062545366>')
Number of steps 17
 Domain "../../mapf_domain.lp"
 Problem "prob1_2.lp"
Done starting 
In preparation 
*********** 
 Computing initial state 
 **************
 Executing: clingo ../../../lps/compute_0.lp  "../../mapf_domain.lp" "prob1_2.lp" --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_0.lp' 
!Setting up expected initial state
 clingo '2_next_0.lp' -c t=0 --outf=0 -V0 --out-atomf=%s. | head -n1 >  '2_enext_0.lp' 
*********** 
 Computing the fluents 
 **************
 In the loop ... 
Receiving ... From 0  Type 2 Content next(1) --- 
Step sending action 1
Done sending action occurs(2,("up",2,0,0,1),1) at step 1 ======================= 
Receiving ... From 0  Type 10 Content next(1) --- 
Step processing observations 1
Agent computing next step and diagnosis 1 
In agent 2 solving step 1 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_1.lp'  '2_enext_0.lp' ' -c t=1'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_1.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_0.lp'  '2_next_1.lp' '2_occ_1.lp'  '2_enext_1.lp' ' -c t=2' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_1.lp' 
!File '2_my_error_1.lp'
Number of errors [] 0
I do not need diagnoses at Step 1!
Done initializing the diagnosis at step 1 ======================= 
Receiving ... From 0  Type 2 Content next(2) --- 
Step sending action 2
Done sending action occurs(2,("right",2,0,1,1),2) at step 2 ======================= 
Receiving ... From 0  Type 10 Content next(2) --- 
Step processing observations 2
Agent computing next step and diagnosis 2 
In agent 2 solving step 2 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_2.lp'  '2_enext_1.lp' ' -c t=2'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_2.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_1.lp'  '2_next_2.lp' '2_occ_2.lp'  '2_enext_2.lp' ' -c t=3' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_2.lp' 
!File '2_my_error_2.lp'
Number of errors [] 0
I do not need diagnoses at Step 2!
Done initializing the diagnosis at step 2 ======================= 
Receiving ... From 0  Type 2 Content next(3) --- 
Step sending action 3
Done sending action occurs(2,("up",2,1,1,2),3) at step 3 ======================= 
Receiving ... From 0  Type 10 Content next(3) --- 
Step processing observations 3
Agent computing next step and diagnosis 3 
In agent 2 solving step 3 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_3.lp'  '2_enext_2.lp' ' -c t=3'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_3.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_2.lp'  '2_next_3.lp' '2_occ_3.lp'  '2_enext_3.lp' ' -c t=4' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_3.lp' 
!File '2_my_error_3.lp'
Number of errors [] 0
I do not need diagnoses at Step 3!
Done initializing the diagnosis at step 3 ======================= 
Receiving ... From 0  Type 2 Content next(4) --- 
Step sending action 4
Done sending action occurs(2,("right",2,1,2,2),4) at step 4 ======================= 
Receiving ... From 1  Type 7 Content question(variable(("at",1,6,6)),false,3) --- 
Receiving question(variable(("at",1,6,6)), false, 3) from 1! 
Current answers I stored: []
==> in finding answers 
Reasons for variable(("at",1,6,6)), false, 3 is [] 
Did not find answer to the question(variable(("at",1,6,6)), false, 3) from 1! 
Has been discussing with [1] about this question(variable(("at",1,6,6)), false, 3) now is 1 
Being asked [1,1] 
To be asked [3]
Asking my neighbors .... Need to ask for help from neighbor [3] 
I send a message to 3 for question(variable(("at",1,6,6)), false, 3) !
Receiving ... From 3  Type 7 Content question(variable(("at",1,6,6)),false,3) --- 
Receiving question(variable(("at",1,6,6)), false, 3) from 3! 
Current answers I stored: []
==> in finding answers 
Reasons for variable(("at",1,6,6)), false, 3 is [] 
Did not find answer to the question(variable(("at",1,6,6)), false, 3) from 3! 
Has been discussing with [1,3,3] about this question(variable(("at",1,6,6)), false, 3) now is 3 
Being asked [1,3,3,3] 
To be asked []
Answer with no .... Receiving ... From 3  Type 6 Content answer(variable(("at",1,6,6)),false,3,[no]) --- 
Receiving answer(variable(("at",1,6,6)), false, 3) is [no] from 3! 
List of people who answered with no to the question(variable(("at",1,6,6)),false,3) is [3] 
Print a list 
: #elements is 1 
 [3] 
In propagation === received all NO 
Receiving ... From 0  Type 10 Content next(4) --- 
Step processing observations 4
Agent computing next step and diagnosis 4 
In agent 2 solving step 4 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_4.lp'  '2_enext_3.lp' ' -c t=4'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_4.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_3.lp'  '2_next_4.lp' '2_occ_4.lp'  '2_enext_4.lp' ' -c t=5' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_4.lp' 
!File '2_my_error_4.lp'
Number of errors [] 0
I do not need diagnoses at Step 4!
Done initializing the diagnosis at step 4 ======================= 
Receiving ... From 0  Type 2 Content next(5) --- 
Step sending action 5
Done sending action occurs(2,("up",2,2,2,3),5) at step 5 ======================= 
Receiving ... From 1  Type 7 Content question(variable(("at",1,6,5)),false,4) --- 
Receiving question(variable(("at",1,6,5)), false, 4) from 1! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no])]
==> in finding answers 
Reasons for variable(("at",1,6,5)), false, 4 is [] 
Did not find answer to the question(variable(("at",1,6,5)), false, 4) from 1! 
Has been discussing with [1] about this question(variable(("at",1,6,5)), false, 4) now is 1 
Being asked [1,1] 
To be asked [3]
Asking my neighbors .... Need to ask for help from neighbor [3] 
I send a message to 3 for question(variable(("at",1,6,5)), false, 4) !
Receiving ... From 3  Type 7 Content question(variable(("at",1,6,5)),false,4) --- 
Receiving question(variable(("at",1,6,5)), false, 4) from 3! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no])]
==> in finding answers 
Reasons for variable(("at",1,6,5)), false, 4 is [] 
Did not find answer to the question(variable(("at",1,6,5)), false, 4) from 3! 
Has been discussing with [1,3,3] about this question(variable(("at",1,6,5)), false, 4) now is 3 
Being asked [1,3,3,3] 
To be asked []
Answer with no .... Receiving ... From 3  Type 6 Content answer(variable(("at",1,6,5)),false,4,[no]) --- 
Receiving answer(variable(("at",1,6,5)), false, 4) is [no] from 3! 
List of people who answered with no to the question(variable(("at",1,6,5)),false,4) is [3] 
Print a list 
: #elements is 1 
 [3] 
In propagation === received all NO 
Receiving ... From 0  Type 10 Content next(5) --- 
Step processing observations 5
Agent computing next step and diagnosis 5 
In agent 2 solving step 5 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_5.lp'  '2_enext_4.lp' ' -c t=5'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_5.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_4.lp'  '2_next_5.lp' '2_occ_5.lp'  '2_enext_5.lp' ' -c t=6' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_5.lp' 
!File '2_my_error_5.lp'
Number of errors [] 0
I do not need diagnoses at Step 5!
Done initializing the diagnosis at step 5 ======================= 
Receiving ... From 0  Type 2 Content next(6) --- 
Step sending action 6
Done sending action occurs(2,("right",2,2,3,3),6) at step 6 ======================= 
Receiving ... From 0  Type 10 Content next(6) --- 
Step processing observations 6
Agent computing next step and diagnosis 6 
In agent 2 solving step 6 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_6.lp'  '2_enext_5.lp' ' -c t=6'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_6.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_5.lp'  '2_next_6.lp' '2_occ_6.lp'  '2_enext_6.lp' ' -c t=7' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_6.lp' 
!File '2_my_error_6.lp'
Number of errors [] 0
I do not need diagnoses at Step 6!
Done initializing the diagnosis at step 6 ======================= 
Receiving ... From 0  Type 2 Content next(7) --- 
Step sending action 7
Done sending action occurs(2,("stay",2,3,3),7) at step 7 ======================= 
Receiving ... From 1  Type 7 Content question(variable(("at",1,6,4)),false,6) --- 
Receiving question(variable(("at",1,6,4)), false, 6) from 1! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no])]
==> in finding answers 
Reasons for variable(("at",1,6,4)), false, 6 is [] 
Did not find answer to the question(variable(("at",1,6,4)), false, 6) from 1! 
Has been discussing with [1] about this question(variable(("at",1,6,4)), false, 6) now is 1 
Being asked [1,1] 
To be asked [3]
Asking my neighbors .... Need to ask for help from neighbor [3] 
I send a message to 3 for question(variable(("at",1,6,4)), false, 6) !
Receiving ... From 3  Type 7 Content question(variable(("at",1,6,4)),false,6) --- 
Receiving question(variable(("at",1,6,4)), false, 6) from 3! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no])]
==> in finding answers 
Reasons for variable(("at",1,6,4)), false, 6 is [] 
Did not find answer to the question(variable(("at",1,6,4)), false, 6) from 3! 
Has been discussing with [1,3,3] about this question(variable(("at",1,6,4)), false, 6) now is 3 
Being asked [1,3,3,3] 
To be asked []
Answer with no .... Receiving ... From 0  Type 10 Content next(7) --- 
Step processing observations 7
Agent computing next step and diagnosis 7 
In agent 2 solving step 7 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_7.lp'  '2_enext_6.lp' ' -c t=7'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_7.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_6.lp'  '2_next_7.lp' '2_occ_7.lp'  '2_enext_7.lp' ' -c t=8' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_7.lp' 
!File '2_my_error_7.lp'
Number of errors [] 0
I do not need diagnoses at Step 7!
Done initializing the diagnosis at step 7 ======================= 
Receiving ... From 0  Type 2 Content next(8) --- 
Step sending action 8
Done sending action occurs(2,("stay",2,3,3),8) at step 8 ======================= 
Receiving ... From 3  Type 6 Content answer(variable(("at",1,6,4)),false,6,[no]) --- 
Receiving answer(variable(("at",1,6,4)), false, 6) is [no] from 3! 
List of people who answered with no to the question(variable(("at",1,6,4)),false,6) is [3] 
Print a list 
: #elements is 1 
 [3] 
In propagation === received all NO 
Receiving ... From 1  Type 7 Content question(variable(("at",1,5,4)),false,7) --- 
Receiving question(variable(("at",1,5,4)), false, 7) from 1! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no])]
==> in finding answers 
Reasons for variable(("at",1,5,4)), false, 7 is [] 
Did not find answer to the question(variable(("at",1,5,4)), false, 7) from 1! 
Has been discussing with [1] about this question(variable(("at",1,5,4)), false, 7) now is 1 
Being asked [1,1] 
To be asked [3]
Asking my neighbors .... Need to ask for help from neighbor [3] 
I send a message to 3 for question(variable(("at",1,5,4)), false, 7) !
Receiving ... From 3  Type 7 Content question(variable(("at",1,5,4)),false,7) --- 
Receiving question(variable(("at",1,5,4)), false, 7) from 3! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no])]
==> in finding answers 
Reasons for variable(("at",1,5,4)), false, 7 is [] 
Did not find answer to the question(variable(("at",1,5,4)), false, 7) from 3! 
Has been discussing with [1,3,3] about this question(variable(("at",1,5,4)), false, 7) now is 3 
Being asked [1,3,3,3] 
To be asked []
Answer with no .... Receiving ... From 3  Type 6 Content answer(variable(("at",1,5,4)),false,7,[no]) --- 
Receiving answer(variable(("at",1,5,4)), false, 7) is [no] from 3! 
List of people who answered with no to the question(variable(("at",1,5,4)),false,7) is [3] 
Print a list 
: #elements is 1 
 [3] 
In propagation === received all NO 
Receiving ... From 0  Type 10 Content next(8) --- 
Step processing observations 8
Agent computing next step and diagnosis 8 
In agent 2 solving step 8 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_8.lp'  '2_enext_7.lp' ' -c t=8'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_8.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_7.lp'  '2_next_8.lp' '2_occ_8.lp'  '2_enext_8.lp' ' -c t=9' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_8.lp' 
!File '2_my_error_8.lp'
Number of errors [] 0
I do not need diagnoses at Step 8!
Done initializing the diagnosis at step 8 ======================= 
Receiving ... From 0  Type 2 Content next(9) --- 
Step sending action 9
Done sending action occurs(2,("up",2,3,3,4),9) at step 9 ======================= 
Receiving ... From 1  Type 7 Content question(variable(("at",1,4,4)),false,8) --- 
Receiving question(variable(("at",1,4,4)), false, 8) from 1! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no])]
==> in finding answers 
Reasons for variable(("at",1,4,4)), false, 8 is [] 
Did not find answer to the question(variable(("at",1,4,4)), false, 8) from 1! 
Has been discussing with [1] about this question(variable(("at",1,4,4)), false, 8) now is 1 
Being asked [1,1] 
To be asked [3]
Asking my neighbors .... Need to ask for help from neighbor [3] 
I send a message to 3 for question(variable(("at",1,4,4)), false, 8) !
Receiving ... From 3  Type 7 Content question(variable(("at",1,4,4)),false,8) --- 
Receiving question(variable(("at",1,4,4)), false, 8) from 3! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no])]
==> in finding answers 
Reasons for variable(("at",1,4,4)), false, 8 is [] 
Did not find answer to the question(variable(("at",1,4,4)), false, 8) from 3! 
Has been discussing with [1,3,3] about this question(variable(("at",1,4,4)), false, 8) now is 3 
Being asked [1,3,3,3] 
To be asked []
Answer with no .... Receiving ... From 3  Type 6 Content answer(variable(("at",1,4,4)),false,8,[no]) --- 
Receiving answer(variable(("at",1,4,4)), false, 8) is [no] from 3! 
List of people who answered with no to the question(variable(("at",1,4,4)),false,8) is [3] 
Print a list 
: #elements is 1 
 [3] 
In propagation === received all NO 
Receiving ... From 0  Type 10 Content next(9) --- 
Step processing observations 9
Agent computing next step and diagnosis 9 
In agent 2 solving step 9 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_9.lp'  '2_enext_8.lp' ' -c t=9'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_9.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_8.lp'  '2_next_9.lp' '2_occ_9.lp'  '2_enext_9.lp' ' -c t=10' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_9.lp' 
!File '2_my_error_9.lp'
Number of errors [] 0
I do not need diagnoses at Step 9!
Done initializing the diagnosis at step 9 ======================= 
Receiving ... From 0  Type 2 Content next(10) --- 
Step sending action 10
Done sending action occurs(2,("right",2,3,4,4),10) at step 10 ======================= 
Receiving ... From 1  Type 7 Content question(variable(("at",1,3,4)),false,9) --- 
Receiving question(variable(("at",1,3,4)), false, 9) from 1! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no])]
==> in finding answers 
Reasons for variable(("at",1,3,4)), false, 9 is [] 
Did not find answer to the question(variable(("at",1,3,4)), false, 9) from 1! 
Has been discussing with [1] about this question(variable(("at",1,3,4)), false, 9) now is 1 
Being asked [1,1] 
To be asked [3]
Asking my neighbors .... Need to ask for help from neighbor [3] 
I send a message to 3 for question(variable(("at",1,3,4)), false, 9) !
Receiving ... From 3  Type 7 Content question(variable(("at",1,3,4)),false,9) --- 
Receiving question(variable(("at",1,3,4)), false, 9) from 3! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no])]
==> in finding answers 
Reasons for variable(("at",1,3,4)), false, 9 is [] 
Did not find answer to the question(variable(("at",1,3,4)), false, 9) from 3! 
Has been discussing with [1,3,3] about this question(variable(("at",1,3,4)), false, 9) now is 3 
Being asked [1,3,3,3] 
To be asked []
Answer with no .... Receiving ... From 0  Type 10 Content next(10) --- 
Step processing observations 10
Agent computing next step and diagnosis 10 
In agent 2 solving step 10 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_10.lp'  '2_enext_9.lp' ' -c t=10'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_10.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_9.lp'  '2_next_10.lp' '2_occ_10.lp'  '2_enext_10.lp' ' -c t=11' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_10.lp' 
!File '2_my_error_10.lp'
Number of errors [] 0
I do not need diagnoses at Step 10!
Done initializing the diagnosis at step 10 ======================= 
Receiving ... From 0  Type 2 Content next(11) --- 
Step sending action 11
Done sending action occurs(2,("right",2,4,4,5),11) at step 11 ======================= 
Receiving ... From 3  Type 6 Content answer(variable(("at",1,3,4)),false,9,[no]) --- 
Receiving answer(variable(("at",1,3,4)), false, 9) is [no] from 3! 
List of people who answered with no to the question(variable(("at",1,3,4)),false,9) is [3] 
Print a list 
: #elements is 1 
 [3] 
In propagation === received all NO 
Receiving ... From 1  Type 7 Content question(variable(("at",1,2,4)),false,10) --- 
Receiving question(variable(("at",1,2,4)), false, 10) from 1! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no]),answers(question(3,no,variable(("at",1,3,4)),false,9),[no])]
==> in finding answers 
Reasons for variable(("at",1,2,4)), false, 10 is [] 
Did not find answer to the question(variable(("at",1,2,4)), false, 10) from 1! 
Has been discussing with [1] about this question(variable(("at",1,2,4)), false, 10) now is 1 
Being asked [1,1] 
To be asked [3]
Asking my neighbors .... Need to ask for help from neighbor [3] 
I send a message to 3 for question(variable(("at",1,2,4)), false, 10) !
Receiving ... From 3  Type 7 Content question(variable(("at",1,2,4)),false,10) --- 
Receiving question(variable(("at",1,2,4)), false, 10) from 3! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no]),answers(question(3,no,variable(("at",1,3,4)),false,9),[no])]
==> in finding answers 
Reasons for variable(("at",1,2,4)), false, 10 is [] 
Did not find answer to the question(variable(("at",1,2,4)), false, 10) from 3! 
Has been discussing with [1,3,3] about this question(variable(("at",1,2,4)), false, 10) now is 3 
Being asked [1,3,3,3] 
To be asked []
Answer with no .... Receiving ... From 3  Type 6 Content answer(variable(("at",1,2,4)),false,10,[no]) --- 
Receiving answer(variable(("at",1,2,4)), false, 10) is [no] from 3! 
List of people who answered with no to the question(variable(("at",1,2,4)),false,10) is [3] 
Print a list 
: #elements is 1 
 [3] 
In propagation === received all NO 
Receiving ... From 0  Type 10 Content next(11) --- 
Step processing observations 11
Agent computing next step and diagnosis 11 
In agent 2 solving step 11 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_11.lp'  '2_enext_10.lp' ' -c t=11'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_11.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_10.lp'  '2_next_11.lp' '2_occ_11.lp'  '2_enext_11.lp' ' -c t=12' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_11.lp' 
!File '2_my_error_11.lp'
Number of errors [] 0
I do not need diagnoses at Step 11!
Done initializing the diagnosis at step 11 ======================= 
Receiving ... From 0  Type 2 Content next(12) --- 
Step sending action 12
Done sending action occurs(2,("right",2,5,4,6),12) at step 12 ======================= 
Receiving ... From 0  Type 10 Content next(12) --- 
Step processing observations 12
Agent computing next step and diagnosis 12 
In agent 2 solving step 12 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_12.lp'  '2_enext_11.lp' ' -c t=12'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_12.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_11.lp'  '2_next_12.lp' '2_occ_12.lp'  '2_enext_12.lp' ' -c t=13' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_12.lp' 
!File '2_my_error_12.lp'
Number of errors [] 0
I do not need diagnoses at Step 12!
Done initializing the diagnosis at step 12 ======================= 
Receiving ... From 0  Type 2 Content next(13) --- 
Step sending action 13
Done sending action occurs(2,("down",2,6,4,3),13) at step 13 ======================= 
Receiving ... From 1  Type 7 Content question(variable(("at",1,2,5)),false,12) --- 
Receiving question(variable(("at",1,2,5)), false, 12) from 1! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no]),answers(question(3,no,variable(("at",1,3,4)),false,9),[no]),answers(question(3,no,variable(("at",1,2,4)),false,10),[no])]
==> in finding answers 
Reasons for variable(("at",1,2,5)), false, 12 is [] 
Did not find answer to the question(variable(("at",1,2,5)), false, 12) from 1! 
Has been discussing with [1] about this question(variable(("at",1,2,5)), false, 12) now is 1 
Being asked [1,1] 
To be asked [3]
Asking my neighbors .... Need to ask for help from neighbor [3] 
I send a message to 3 for question(variable(("at",1,2,5)), false, 12) !
Receiving ... From 3  Type 7 Content question(variable(("at",1,2,5)),false,12) --- 
Receiving question(variable(("at",1,2,5)), false, 12) from 3! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no]),answers(question(3,no,variable(("at",1,3,4)),false,9),[no]),answers(question(3,no,variable(("at",1,2,4)),false,10),[no])]
==> in finding answers 
Reasons for variable(("at",1,2,5)), false, 12 is [] 
Did not find answer to the question(variable(("at",1,2,5)), false, 12) from 3! 
Has been discussing with [1,3,3] about this question(variable(("at",1,2,5)), false, 12) now is 3 
Being asked [1,3,3,3] 
To be asked []
Answer with no .... Receiving ... From 3  Type 6 Content answer(variable(("at",1,2,5)),false,12,[no]) --- 
Receiving answer(variable(("at",1,2,5)), false, 12) is [no] from 3! 
List of people who answered with no to the question(variable(("at",1,2,5)),false,12) is [3] 
Print a list 
: #elements is 1 
 [3] 
In propagation === received all NO 
Receiving ... From 0  Type 10 Content next(13) --- 
Step processing observations 13
Agent computing next step and diagnosis 13 
In agent 2 solving step 13 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_13.lp'  '2_enext_12.lp' ' -c t=13'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_13.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_12.lp'  '2_next_13.lp' '2_occ_13.lp'  '2_enext_13.lp' ' -c t=14' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_13.lp' 
!File '2_my_error_13.lp'
Number of errors [] 0
I do not need diagnoses at Step 13!
Done initializing the diagnosis at step 13 ======================= 
Receiving ... From 0  Type 2 Content next(14) --- 
Step sending action 14
Done sending action occurs(2,("down",2,6,3,2),14) at step 14 ======================= 
Receiving ... From 0  Type 10 Content next(14) --- 
Step processing observations 14
Agent computing next step and diagnosis 14 
In agent 2 solving step 14 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_14.lp'  '2_enext_13.lp' ' -c t=14'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_14.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_13.lp'  '2_next_14.lp' '2_occ_14.lp'  '2_enext_14.lp' ' -c t=15' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_14.lp' 
!File '2_my_error_14.lp'
Number of errors [] 0
I do not need diagnoses at Step 14!
Done initializing the diagnosis at step 14 ======================= 
Receiving ... From 0  Type 2 Content next(15) --- 
Step sending action 15
Done sending action occurs(2,("down",2,6,2,1),15) at step 15 ======================= 
Receiving ... From 1  Type 7 Content question(variable(("at",1,2,3)),false,14) --- 
Receiving question(variable(("at",1,2,3)), false, 14) from 1! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no]),answers(question(3,no,variable(("at",1,3,4)),false,9),[no]),answers(question(3,no,variable(("at",1,2,4)),false,10),[no]),answers(question(3,no,variable(("at",1,2,5)),false,12),[no])]
==> in finding answers 
Reasons for variable(("at",1,2,3)), false, 14 is [] 
Did not find answer to the question(variable(("at",1,2,3)), false, 14) from 1! 
Has been discussing with [1] about this question(variable(("at",1,2,3)), false, 14) now is 1 
Being asked [1,1] 
To be asked [3]
Asking my neighbors .... Need to ask for help from neighbor [3] 
I send a message to 3 for question(variable(("at",1,2,3)), false, 14) !
Receiving ... From 3  Type 7 Content question(variable(("at",1,2,3)),false,14) --- 
Receiving question(variable(("at",1,2,3)), false, 14) from 3! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no]),answers(question(3,no,variable(("at",1,3,4)),false,9),[no]),answers(question(3,no,variable(("at",1,2,4)),false,10),[no]),answers(question(3,no,variable(("at",1,2,5)),false,12),[no])]
==> in finding answers 
Reasons for variable(("at",1,2,3)), false, 14 is [] 
Did not find answer to the question(variable(("at",1,2,3)), false, 14) from 3! 
Has been discussing with [1,3,3] about this question(variable(("at",1,2,3)), false, 14) now is 3 
Being asked [1,3,3,3] 
To be asked []
Answer with no .... Receiving ... From 0  Type 10 Content next(15) --- 
Step processing observations 15
Agent computing next step and diagnosis 15 
In agent 2 solving step 15 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_15.lp'  '2_enext_14.lp' ' -c t=15'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_15.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_14.lp'  '2_next_15.lp' '2_occ_15.lp'  '2_enext_15.lp' ' -c t=16' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_15.lp' 
!File '2_my_error_15.lp'
Number of errors [] 0
I do not need diagnoses at Step 15!
Done initializing the diagnosis at step 15 ======================= 
Receiving ... From 0  Type 2 Content next(16) --- 
Step sending action 16
Done sending action occurs(2,("down",2,6,1,0),16) at step 16 ======================= 
Receiving ... From 3  Type 6 Content answer(variable(("at",1,2,3)),false,14,[no]) --- 
Receiving answer(variable(("at",1,2,3)), false, 14) is [no] from 3! 
List of people who answered with no to the question(variable(("at",1,2,3)),false,14) is [3] 
Print a list 
: #elements is 1 
 [3] 
In propagation === received all NO 
Receiving ... From 1  Type 7 Content question(variable(("at",1,1,3)),false,15) --- 
Receiving question(variable(("at",1,1,3)), false, 15) from 1! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no]),answers(question(3,no,variable(("at",1,3,4)),false,9),[no]),answers(question(3,no,variable(("at",1,2,4)),false,10),[no]),answers(question(3,no,variable(("at",1,2,5)),false,12),[no]),answers(question(3,no,variable(("at",1,2,3)),false,14),[no])]
==> in finding answers 
Reasons for variable(("at",1,1,3)), false, 15 is [] 
Did not find answer to the question(variable(("at",1,1,3)), false, 15) from 1! 
Has been discussing with [1] about this question(variable(("at",1,1,3)), false, 15) now is 1 
Being asked [1,1] 
To be asked [3]
Asking my neighbors .... Need to ask for help from neighbor [3] 
I send a message to 3 for question(variable(("at",1,1,3)), false, 15) !
Receiving ... From 3  Type 7 Content question(variable(("at",1,1,3)),false,15) --- 
Receiving question(variable(("at",1,1,3)), false, 15) from 3! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no]),answers(question(3,no,variable(("at",1,3,4)),false,9),[no]),answers(question(3,no,variable(("at",1,2,4)),false,10),[no]),answers(question(3,no,variable(("at",1,2,5)),false,12),[no]),answers(question(3,no,variable(("at",1,2,3)),false,14),[no])]
==> in finding answers 
Reasons for variable(("at",1,1,3)), false, 15 is [] 
Did not find answer to the question(variable(("at",1,1,3)), false, 15) from 3! 
Has been discussing with [1,3,3] about this question(variable(("at",1,1,3)), false, 15) now is 3 
Being asked [1,3,3,3] 
To be asked []
Answer with no .... Receiving ... From 3  Type 6 Content answer(variable(("at",1,1,3)),false,15,[no]) --- 
Receiving answer(variable(("at",1,1,3)), false, 15) is [no] from 3! 
List of people who answered with no to the question(variable(("at",1,1,3)),false,15) is [3] 
Print a list 
: #elements is 1 
 [3] 
In propagation === received all NO 
Receiving ... From 0  Type 10 Content next(16) --- 
Step processing observations 16
Agent computing next step and diagnosis 16 
In agent 2 solving step 16 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_16.lp'  '2_enext_15.lp' ' -c t=16'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_16.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_15.lp'  '2_next_16.lp' '2_occ_16.lp'  '2_enext_16.lp' ' -c t=17' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_16.lp' 
!File '2_my_error_16.lp'
Number of errors [] 0
I do not need diagnoses at Step 16!
Done initializing the diagnosis at step 16 ======================= 
Receiving ... From 0  Type 2 Content next(17) --- 
Step sending action 17
Done sending action occurs(2,("right",2,6,0,7),17) at step 17 ======================= 
Receiving ... From 1  Type 7 Content question(variable(("at",1,0,3)),false,16) --- 
Receiving question(variable(("at",1,0,3)), false, 16) from 1! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no]),answers(question(3,no,variable(("at",1,3,4)),false,9),[no]),answers(question(3,no,variable(("at",1,2,4)),false,10),[no]),answers(question(3,no,variable(("at",1,2,5)),false,12),[no]),answers(question(3,no,variable(("at",1,2,3)),false,14),[no]),answers(question(3,no,variable(("at",1,1,3)),false,15),[no])]
==> in finding answers 
Reasons for variable(("at",1,0,3)), false, 16 is [] 
Did not find answer to the question(variable(("at",1,0,3)), false, 16) from 1! 
Has been discussing with [1] about this question(variable(("at",1,0,3)), false, 16) now is 1 
Being asked [1,1] 
To be asked [3]
Asking my neighbors .... Need to ask for help from neighbor [3] 
I send a message to 3 for question(variable(("at",1,0,3)), false, 16) !
Receiving ... From 3  Type 7 Content question(variable(("at",1,0,3)),false,16) --- 
Receiving question(variable(("at",1,0,3)), false, 16) from 3! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no]),answers(question(3,no,variable(("at",1,3,4)),false,9),[no]),answers(question(3,no,variable(("at",1,2,4)),false,10),[no]),answers(question(3,no,variable(("at",1,2,5)),false,12),[no]),answers(question(3,no,variable(("at",1,2,3)),false,14),[no]),answers(question(3,no,variable(("at",1,1,3)),false,15),[no])]
==> in finding answers 
Reasons for variable(("at",1,0,3)), false, 16 is [] 
Did not find answer to the question(variable(("at",1,0,3)), false, 16) from 3! 
Has been discussing with [1,3,3] about this question(variable(("at",1,0,3)), false, 16) now is 3 
Being asked [1,3,3,3] 
To be asked []
Answer with no .... Receiving ... From 0  Type 10 Content next(17) --- 
Step processing observations 17
Agent computing next step and diagnosis 17 
In agent 2 solving step 17 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_occ_17.lp'  '2_enext_16.lp' ' -c t=17'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_17.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob1_2.lp" '2_agent.lp' '2_next_16.lp'  '2_next_17.lp' '2_occ_17.lp'  '2_enext_17.lp' ' -c t=18' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_17.lp' 
!File '2_my_error_17.lp'
Number of errors [] 0
I do not need diagnoses at Step 17!
Done initializing the diagnosis at step 17 ======================= 
Receiving ... From 0  Type 2 Content next(18) --- 
Step sending action 18
Done with executing all actions 18 ======================= 
Receiving ... From 3  Type 6 Content answer(variable(("at",1,0,3)),false,16,[no]) --- 
Receiving answer(variable(("at",1,0,3)), false, 16) is [no] from 3! 
List of people who answered with no to the question(variable(("at",1,0,3)),false,16) is [3] 
Print a list 
: #elements is 1 
 [3] 
In propagation === received all NO 
Receiving ... From 1  Type 7 Content question(variable(("at",1,0,2)),false,17) --- 
Receiving question(variable(("at",1,0,2)), false, 17) from 1! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no]),answers(question(3,no,variable(("at",1,3,4)),false,9),[no]),answers(question(3,no,variable(("at",1,2,4)),false,10),[no]),answers(question(3,no,variable(("at",1,2,5)),false,12),[no]),answers(question(3,no,variable(("at",1,2,3)),false,14),[no]),answers(question(3,no,variable(("at",1,1,3)),false,15),[no]),answers(question(3,no,variable(("at",1,0,3)),false,16),[no])]
==> in finding answers 
Reasons for variable(("at",1,0,2)), false, 17 is [] 
Did not find answer to the question(variable(("at",1,0,2)), false, 17) from 1! 
Has been discussing with [1] about this question(variable(("at",1,0,2)), false, 17) now is 1 
Being asked [1,1] 
To be asked [3]
Asking my neighbors .... Need to ask for help from neighbor [3] 
I send a message to 3 for question(variable(("at",1,0,2)), false, 17) !
Receiving ... From 3  Type 7 Content question(variable(("at",1,0,2)),false,17) --- 
Receiving question(variable(("at",1,0,2)), false, 17) from 3! 
Current answers I stored: [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no]),answers(question(3,no,variable(("at",1,3,4)),false,9),[no]),answers(question(3,no,variable(("at",1,2,4)),false,10),[no]),answers(question(3,no,variable(("at",1,2,5)),false,12),[no]),answers(question(3,no,variable(("at",1,2,3)),false,14),[no]),answers(question(3,no,variable(("at",1,1,3)),false,15),[no]),answers(question(3,no,variable(("at",1,0,3)),false,16),[no])]
==> in finding answers 
Reasons for variable(("at",1,0,2)), false, 17 is [] 
Did not find answer to the question(variable(("at",1,0,2)), false, 17) from 3! 
Has been discussing with [1,3,3] about this question(variable(("at",1,0,2)), false, 17) now is 3 
Being asked [1,3,3,3] 
To be asked []
Answer with no .... Receiving ... From 0  Type 10 Content next(18) --- 
Step processing observations 18
Done with executing all actions 18 ======================= 
Receiving ... From 0  Type 2 Content next(19) --- 
Step sending action 19
Done with executing all actions 19 ======================= 
Receiving ... From 3  Type 6 Content answer(variable(("at",1,0,2)),false,17,[no]) --- 
Receiving answer(variable(("at",1,0,2)), false, 17) is [no] from 3! 
List of people who answered with no to the question(variable(("at",1,0,2)),false,17) is [3] 
Print a list 
: #elements is 1 
 [3] 
In propagation === received all NO 
Receiving ... From 0  Type 2 Content done --- 
All done 
Out from agent loop 
Done executing 
List of answers 
 [answers(question(3,no,variable(("at",1,6,6)),false,3),[no]),answers(question(3,no,variable(("at",1,6,5)),false,4),[no]),answers(question(3,no,variable(("at",1,6,4)),false,6),[no]),answers(question(3,no,variable(("at",1,5,4)),false,7),[no]),answers(question(3,no,variable(("at",1,4,4)),false,8),[no]),answers(question(3,no,variable(("at",1,3,4)),false,9),[no]),answers(question(3,no,variable(("at",1,2,4)),false,10),[no]),answers(question(3,no,variable(("at",1,2,5)),false,12),[no]),answers(question(3,no,variable(("at",1,2,3)),false,14),[no]),answers(question(3,no,variable(("at",1,1,3)),false,15),[no]),answers(question(3,no,variable(("at",1,0,3)),false,16),[no]),answers(question(3,no,variable(("at",1,0,2)),false,17),[no])] 

List of questions that I sent and received 
 [(0,1,variable(("at",1,6,6)),false,3),(1,3,variable(("at",1,6,6)),false,3),(0,3,variable(("at",1,6,6)),false,3),(0,1,variable(("at",1,6,5)),false,4),(1,3,variable(("at",1,6,5)),false,4),(0,3,variable(("at",1,6,5)),false,4),(0,1,variable(("at",1,6,4)),false,6),(1,3,variable(("at",1,6,4)),false,6),(0,3,variable(("at",1,6,4)),false,6),(0,1,variable(("at",1,5,4)),false,7),(1,3,variable(("at",1,5,4)),false,7),(0,3,variable(("at",1,5,4)),false,7),(0,1,variable(("at",1,4,4)),false,8),(1,3,variable(("at",1,4,4)),false,8),(0,3,variable(("at",1,4,4)),false,8),(0,1,variable(("at",1,3,4)),false,9),(1,3,variable(("at",1,3,4)),false,9),(0,3,variable(("at",1,3,4)),false,9),(0,1,variable(("at",1,2,4)),false,10),(1,3,variable(("at",1,2,4)),false,10),(0,3,variable(("at",1,2,4)),false,10),(0,1,variable(("at",1,2,5)),false,12),(1,3,variable(("at",1,2,5)),false,12),(0,3,variable(("at",1,2,5)),false,12),(0,1,variable(("at",1,2,3)),false,14),(1,3,variable(("at",1,2,3)),false,14),(0,3,variable(("at",1,2,3)),false,14),(0,1,variable(("at",1,1,3)),false,15),(1,3,variable(("at",1,1,3)),false,15),(0,3,variable(("at",1,1,3)),false,15),(0,1,variable(("at",1,0,3)),false,16),(1,3,variable(("at",1,0,3)),false,16),(0,3,variable(("at",1,0,3)),false,16),(0,1,variable(("at",1,0,2)),false,17),(1,3,variable(("at",1,0,2)),false,17),(0,3,variable(("at",1,0,2)),false,17)] 

