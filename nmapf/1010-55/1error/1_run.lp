TIPC Linda server listening at: port_id('<0.0.0:2289548605>')
Number of steps 15
 Domain "../../mapf_domain.lp"
 Problem "prob2_1.lp"
Done starting 
In preparation 
*********** 
 Computing initial state 
 **************
 Executing: clingo ../../../lps/compute_0.lp  "../../mapf_domain.lp" "prob2_1.lp" --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_0.lp' 
!Setting up expected initial state
 clingo '1_next_0.lp' -c t=0 --outf=0 -V0 --out-atomf=%s. | head -n1 >  '1_enext_0.lp' 
*********** 
 Computing the fluents 
 **************
 In the loop ... 
Receiving ... From 0  Type 2 Content next(1) --- 
Step sending action 1
Done sending action occurs(1,("left",1,7,6,6),1) at step 1 ======================= 
Receiving ... From 0  Type 10 Content next(1) --- 
Step processing observations 1
Agent computing next step and diagnosis 1 
In agent 1 solving step 1 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_1.lp'  '1_enext_0.lp' ' -c t=1'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_1.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_0.lp'  '1_next_1.lp' '1_occ_1.lp'  '1_enext_1.lp' ' -c t=2' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_1.lp' 
!File '1_my_error_1.lp'
Number of errors [] 0
I do not need diagnoses at Step 1!
Done initializing the diagnosis at step 1 ======================= 
Receiving ... From 0  Type 2 Content next(2) --- 
Step sending action 2
Done sending action occurs(1,("down",1,6,6,5),2) at step 2 ======================= 
Receiving ... From 0  Type 10 Content next(2) --- 
Step processing observations 2
Agent computing next step and diagnosis 2 
In agent 1 solving step 2 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_2.lp'  '1_enext_1.lp' ' -c t=2'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_2.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_1.lp'  '1_next_2.lp' '1_occ_2.lp'  '1_enext_2.lp' ' -c t=3' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_2.lp' 
!File '1_my_error_2.lp'
Number of errors [] 0
I do not need diagnoses at Step 2!
Done initializing the diagnosis at step 2 ======================= 
Receiving ... From 0  Type 2 Content next(3) --- 
Step sending action 3
Done sending action occurs(1,("left",1,6,5,5),3) at step 3 ======================= 
Receiving ... From 0  Type 10 Content next(3) --- 
Step processing observations 3
Agent computing next step and diagnosis 3 
In agent 1 solving step 3 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_3.lp'  '1_enext_2.lp' ' -c t=3'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_3.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_2.lp'  '1_next_3.lp' '1_occ_3.lp'  '1_enext_3.lp' ' -c t=4' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_3.lp' 
!File '1_my_error_3.lp'
Number of errors [] 0
I do not need diagnoses at Step 3!
Done initializing the diagnosis at step 3 ======================= 
Receiving ... From 0  Type 2 Content next(4) --- 
Step sending action 4
Done sending action occurs(1,("left",1,5,5,4),4) at step 4 ======================= 
Receiving ... From 0  Type 10 Content next(4) --- 
Step processing observations 4
Agent computing next step and diagnosis 4 
In agent 1 solving step 4 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_4.lp'  '1_enext_3.lp' ' -c t=4'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_4.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_3.lp'  '1_next_4.lp' '1_occ_4.lp'  '1_enext_4.lp' ' -c t=5' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_4.lp' 
!File '1_my_error_4.lp'
Number of errors [] 0
I do not need diagnoses at Step 4!
Done initializing the diagnosis at step 4 ======================= 
Receiving ... From 0  Type 2 Content next(5) --- 
Step sending action 5
Done sending action occurs(1,("up",1,4,5,6),5) at step 5 ======================= 
Receiving ... From 0  Type 10 Content next(5) --- 
Step processing observations 5
Agent computing next step and diagnosis 5 
In agent 1 solving step 5 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_5.lp'  '1_enext_4.lp' ' -c t=5'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_5.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_4.lp'  '1_next_5.lp' '1_occ_5.lp'  '1_enext_5.lp' ' -c t=6' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_5.lp' 
!File '1_my_error_5.lp'
Number of errors [] 0
I do not need diagnoses at Step 5!
Done initializing the diagnosis at step 5 ======================= 
Receiving ... From 0  Type 2 Content next(6) --- 
Step sending action 6
Done sending action occurs(1,("left",1,4,6,3),6) at step 6 ======================= 
Receiving ... From 0  Type 10 Content next(6) --- 
Step processing observations 6
Agent computing next step and diagnosis 6 
In agent 1 solving step 6 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_6.lp'  '1_enext_5.lp' ' -c t=6'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_6.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_5.lp'  '1_next_6.lp' '1_occ_6.lp'  '1_enext_6.lp' ' -c t=7' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_6.lp' 
!File '1_my_error_6.lp'
Number of errors [] 0
I do not need diagnoses at Step 6!
Done initializing the diagnosis at step 6 ======================= 
Receiving ... From 0  Type 2 Content next(7) --- 
Step sending action 7
Done sending action occurs(1,("stay",1,3,6),7) at step 7 ======================= 
Receiving ... From 3  Type 7 Content question(variable(("at",3,3,4)),false,6) --- 
Receiving question(variable(("at",3,3,4)), false, 6) from 3! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self])]
==> in finding answers 
Reasons for variable(("at",3,3,4)), false, 6 is [] 
Did not find answer to the question(variable(("at",3,3,4)), false, 6) from 3! 
Has been discussing with [3] about this question(variable(("at",3,3,4)), false, 6) now is 3 
Being asked [3,3] 
To be asked [2,4,5]
Asking my neighbors .... Need to ask for help from neighbor [2,4,5] 
I send a message to 2 for question(variable(("at",3,3,4)), false, 6) !
I send a message to 4 for question(variable(("at",3,3,4)), false, 6) !
I send a message to 5 for question(variable(("at",3,3,4)), false, 6) !
Receiving ... From 5  Type 7 Content question(variable(("at",3,3,4)),false,6) --- 
Receiving question(variable(("at",3,3,4)), false, 6) from 5! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self])]
==> in finding answers 
Reasons for variable(("at",3,3,4)), false, 6 is [] 
Did not find answer to the question(variable(("at",3,3,4)), false, 6) from 5! 
Has been discussing with [3,2,4,5,5] about this question(variable(("at",3,3,4)), false, 6) now is 5 
Being asked [3,2,4,5,5,5] 
To be asked []
Answer with no .... Receiving ... From 4  Type 7 Content question(variable(("at",3,3,4)),false,6) --- 
Receiving question(variable(("at",3,3,4)), false, 6) from 4! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self])]
==> in finding answers 
Reasons for variable(("at",3,3,4)), false, 6 is [] 
Did not find answer to the question(variable(("at",3,3,4)), false, 6) from 4! 
Has been discussing with [3,2,4,5,5,4] about this question(variable(("at",3,3,4)), false, 6) now is 4 
Being asked [3,2,4,5,5,4,4] 
To be asked []
Answer with no .... Receiving ... From 2  Type 7 Content question(variable(("at",3,3,4)),false,6) --- 
Receiving question(variable(("at",3,3,4)), false, 6) from 2! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self])]
==> in finding answers 
Reasons for variable(("at",3,3,4)), false, 6 is [] 
Did not find answer to the question(variable(("at",3,3,4)), false, 6) from 2! 
Has been discussing with [3,2,4,5,5,4,2] about this question(variable(("at",3,3,4)), false, 6) now is 2 
Being asked [3,2,4,5,5,4,2,2] 
To be asked []
Answer with no .... Receiving ... From 5  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no],[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no],[no]] from 5! 
In propagation with answer [[no],[no]]  
Notify [2,4] for the question(variable(("at",3,3,4)), false, 6) with answer [[no],[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no]] from 4! 
In propagation with answer [[no]]  
Notify [2,5] for the question(variable(("at",3,3,4)), false, 6) with answer [[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,3,4)),false,6,[no]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [no] from 4! 
List of people who answered with no to the question(variable(("at",3,3,4)),false,6) is [5,4,4] 
Print a list 
: #elements is 3 
 [2,4,5] 
Receiving ... From 0  Type 10 Content next(7) --- 
Step processing observations 7
Agent computing next step and diagnosis 7 
In agent 1 solving step 7 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_7.lp'  '1_enext_6.lp' ' -c t=7'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_7.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_6.lp'  '1_next_7.lp' '1_occ_7.lp'  '1_enext_7.lp' ' -c t=8' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_7.lp' 
!File '1_my_error_7.lp'
Number of errors [] 0
I do not need diagnoses at Step 7!
Done initializing the diagnosis at step 7 ======================= 
Receiving ... From 0  Type 2 Content next(8) --- 
Step sending action 8
Done sending action occurs(1,("down",1,3,6,5),8) at step 8 ======================= 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,3,4)),false,6,[no]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [no] from 2! 
List of people who answered with no to the question(variable(("at",3,3,4)),false,6) is [5,4,4,2] 
Print a list 
: #elements is 3 
 [2,4,5] 
In propagation === received all NO 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no]] from 2! 
In propagation with answer [[no]]  
Notify [4,5] for the question(variable(("at",3,3,4)), false, 6) with answer [[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,3,4)),false,6,[no]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [no] from 2! 
Receiving ... From 3  Type 7 Content question(variable(("at",3,2,4)),false,7) --- 
Receiving question(variable(("at",3,2,4)), false, 7) from 3! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]])]
==> in finding answers 
Reasons for variable(("at",3,2,4)), false, 7 is [] 
Did not find answer to the question(variable(("at",3,2,4)), false, 7) from 3! 
Has been discussing with [3] about this question(variable(("at",3,2,4)), false, 7) now is 3 
Being asked [3,3] 
To be asked [2,4,5]
Asking my neighbors .... Need to ask for help from neighbor [2,4,5] 
I send a message to 2 for question(variable(("at",3,2,4)), false, 7) !
I send a message to 4 for question(variable(("at",3,2,4)), false, 7) !
I send a message to 5 for question(variable(("at",3,2,4)), false, 7) !
Receiving ... From 5  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no],[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no],[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no]] from 5! 
In propagation with answer [[no]]  
Notify [2,4] for the question(variable(("at",3,3,4)), false, 6) with answer [[no]] 
Receiving ... From 5  Type 7 Content question(variable(("at",3,2,4)),false,7) --- 
Receiving question(variable(("at",3,2,4)), false, 7) from 5! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]])]
==> in finding answers 
Reasons for variable(("at",3,2,4)), false, 7 is [] 
Did not find answer to the question(variable(("at",3,2,4)), false, 7) from 5! 
Has been discussing with [3,2,4,5,5] about this question(variable(("at",3,2,4)), false, 7) now is 5 
Being asked [3,2,4,5,5,5] 
To be asked []
Answer with no .... Receiving ... From 4  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no]] from 4! 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no],[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no],[no]] from 4! 
In propagation with answer [[no],[no]]  
Notify [2,5] for the question(variable(("at",3,3,4)), false, 6) with answer [[no],[no]] 
Receiving ... From 4  Type 7 Content question(variable(("at",3,2,4)),false,7) --- 
Receiving question(variable(("at",3,2,4)), false, 7) from 4! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]])]
==> in finding answers 
Reasons for variable(("at",3,2,4)), false, 7 is [] 
Did not find answer to the question(variable(("at",3,2,4)), false, 7) from 4! 
Has been discussing with [3,2,4,5,5,4] about this question(variable(("at",3,2,4)), false, 7) now is 4 
Being asked [3,2,4,5,5,4,4] 
To be asked []
Answer with no .... Receiving ... From 4  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no]] from 4! 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no],[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no],[no]] from 4! 
Receiving ... From 2  Type 7 Content question(variable(("at",3,2,4)),false,7) --- 
Receiving question(variable(("at",3,2,4)), false, 7) from 2! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]])]
==> in finding answers 
Reasons for variable(("at",3,2,4)), false, 7 is [] 
Did not find answer to the question(variable(("at",3,2,4)), false, 7) from 2! 
Has been discussing with [3,2,4,5,5,4,2] about this question(variable(("at",3,2,4)), false, 7) now is 2 
Being asked [3,2,4,5,5,4,2,2] 
To be asked []
Answer with no .... Receiving ... From 2  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no],[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no],[no]] from 2! 
In propagation with answer [[no],[no]]  
Notify [4,5] for the question(variable(("at",3,3,4)), false, 6) with answer [[no],[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no]] from 2! 
Receiving ... From 0  Type 10 Content next(8) --- 
Step processing observations 8
Agent computing next step and diagnosis 8 
In agent 1 solving step 8 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_8.lp'  '1_enext_7.lp' ' -c t=8'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_8.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_7.lp'  '1_next_8.lp' '1_occ_8.lp'  '1_enext_8.lp' ' -c t=9' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_8.lp' 
!File '1_my_error_8.lp'
Number of errors [] 0
I do not need diagnoses at Step 8!
Done initializing the diagnosis at step 8 ======================= 
Receiving ... From 0  Type 2 Content next(9) --- 
Step sending action 9
Done sending action occurs(1,("stay",1,3,5),9) at step 9 ======================= 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no],[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no],[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no],[no]] from 5! 
In propagation with answer [[no],[no]]  
Notify [2,4] for the question(variable(("at",3,2,4)), false, 7) with answer [[no],[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no]] from 4! 
In propagation with answer [[no]]  
Notify [2,5] for the question(variable(("at",3,2,4)), false, 7) with answer [[no]] 
Receiving ... From 3  Type 7 Content question(variable(("at",3,2,3)),false,8) --- 
Receiving question(variable(("at",3,2,3)), false, 8) from 3! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]])]
==> in finding answers 
Reasons for variable(("at",3,2,3)), false, 8 is [] 
Did not find answer to the question(variable(("at",3,2,3)), false, 8) from 3! 
Has been discussing with [3] about this question(variable(("at",3,2,3)), false, 8) now is 3 
Being asked [3,3] 
To be asked [2,4,5]
Asking my neighbors .... Need to ask for help from neighbor [2,4,5] 
I send a message to 2 for question(variable(("at",3,2,3)), false, 8) !
I send a message to 4 for question(variable(("at",3,2,3)), false, 8) !
I send a message to 5 for question(variable(("at",3,2,3)), false, 8) !
Receiving ... From 5  Type 7 Content question(variable(("at",3,2,3)),false,8) --- 
Receiving question(variable(("at",3,2,3)), false, 8) from 5! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]])]
==> in finding answers 
Reasons for variable(("at",3,2,3)), false, 8 is [] 
Did not find answer to the question(variable(("at",3,2,3)), false, 8) from 5! 
Has been discussing with [3,2,4,5,5] about this question(variable(("at",3,2,3)), false, 8) now is 5 
Being asked [3,2,4,5,5,5] 
To be asked []
Answer with no .... Receiving ... From 4  Type 6 Content answer(variable(("at",3,2,4)),false,7,[no]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [no] from 4! 
List of people who answered with no to the question(variable(("at",3,2,4)),false,7) is [5,4,4] 
Print a list 
: #elements is 3 
 [2,4,5] 
Receiving ... From 4  Type 7 Content question(variable(("at",3,2,3)),false,8) --- 
Receiving question(variable(("at",3,2,3)), false, 8) from 4! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no])]
==> in finding answers 
Reasons for variable(("at",3,2,3)), false, 8 is [] 
Did not find answer to the question(variable(("at",3,2,3)), false, 8) from 4! 
Has been discussing with [3,2,4,5,5,4] about this question(variable(("at",3,2,3)), false, 8) now is 4 
Being asked [3,2,4,5,5,4,4] 
To be asked []
Answer with no .... Receiving ... From 2  Type 6 Content answer(variable(("at",3,2,4)),false,7,[no]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [no] from 2! 
List of people who answered with no to the question(variable(("at",3,2,4)),false,7) is [5,4,4,2] 
Print a list 
: #elements is 3 
 [2,4,5] 
In propagation === received all NO 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,3,4)),false,6,[[no],[no]]) --- 
Receiving answer(variable(("at",3,3,4)), false, 6) is [[no],[no]] from 2! 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no]] from 2! 
In propagation with answer [[no]]  
Notify [4,5] for the question(variable(("at",3,2,4)), false, 7) with answer [[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,2,4)),false,7,[no]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [no] from 2! 
Receiving ... From 2  Type 7 Content question(variable(("at",3,2,3)),false,8) --- 
Receiving question(variable(("at",3,2,3)), false, 8) from 2! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]])]
==> in finding answers 
Reasons for variable(("at",3,2,3)), false, 8 is [] 
Did not find answer to the question(variable(("at",3,2,3)), false, 8) from 2! 
Has been discussing with [3,2,4,5,5,4,2] about this question(variable(("at",3,2,3)), false, 8) now is 2 
Being asked [3,2,4,5,5,4,2,2] 
To be asked []
Answer with no .... Receiving ... From 5  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no],[no]] from 5! 
In propagation with answer [[no],[no]]  
Notify [2,4] for the question(variable(("at",3,2,3)), false, 8) with answer [[no],[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no]] from 4! 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no]] from 4! 
In propagation with answer [[no]]  
Notify [2,5] for the question(variable(("at",3,2,3)), false, 8) with answer [[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,2,3)),false,8,[no]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [no] from 4! 
List of people who answered with no to the question(variable(("at",3,2,3)),false,8) is [5,4,4] 
Print a list 
: #elements is 3 
 [2,4,5] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no]] from 4! 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,2,3)),false,8,[no]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [no] from 2! 
List of people who answered with no to the question(variable(("at",3,2,3)),false,8) is [5,4,4,2] 
Print a list 
: #elements is 3 
 [2,4,5] 
In propagation === received all NO 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no]] from 2! 
In propagation with answer [[no]]  
Notify [4,5] for the question(variable(("at",3,2,3)), false, 8) with answer [[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,2,3)),false,8,[no]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [no] from 2! 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no],[no]] from 2! 
In propagation with answer [[no],[no]]  
Notify [4,5] for the question(variable(("at",3,2,4)), false, 7) with answer [[no],[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no]] from 2! 
Receiving ... From 0  Type 10 Content next(9) --- 
Step processing observations 9
Agent computing next step and diagnosis 9 
In agent 1 solving step 9 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_9.lp'  '1_enext_8.lp' ' -c t=9'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_9.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_8.lp'  '1_next_9.lp' '1_occ_9.lp'  '1_enext_9.lp' ' -c t=10' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_9.lp' 
!File '1_my_error_9.lp'
Number of errors [] 0
I do not need diagnoses at Step 9!
Done initializing the diagnosis at step 9 ======================= 
Receiving ... From 0  Type 2 Content next(10) --- 
Step sending action 10
Done sending action occurs(1,("down",1,3,5,4),10) at step 10 ======================= 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no]] from 5! 
In propagation with answer [[no]]  
Notify [2,4] for the question(variable(("at",3,2,4)), false, 7) with answer [[no]] 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no],[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no],[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no]] from 5! 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no],[no]] from 4! 
In propagation with answer [[no],[no]]  
Notify [2,5] for the question(variable(("at",3,2,4)), false, 7) with answer [[no],[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no]] from 4! 
Receiving ... From 3  Type 7 Content question(variable(("at",3,1,3)),false,9) --- 
Receiving question(variable(("at",3,1,3)), false, 9) from 3! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]])]
==> in finding answers 
Reasons for variable(("at",3,1,3)), false, 9 is [] 
Did not find answer to the question(variable(("at",3,1,3)), false, 9) from 3! 
Has been discussing with [3] about this question(variable(("at",3,1,3)), false, 9) now is 3 
Being asked [3,3] 
To be asked [2,4,5]
Asking my neighbors .... Need to ask for help from neighbor [2,4,5] 
I send a message to 2 for question(variable(("at",3,1,3)), false, 9) !
I send a message to 4 for question(variable(("at",3,1,3)), false, 9) !
I send a message to 5 for question(variable(("at",3,1,3)), false, 9) !
Receiving ... From 5  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no]] from 5! 
In propagation with answer [[no]]  
Notify [2,4] for the question(variable(("at",3,2,3)), false, 8) with answer [[no]] 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no],[no]] from 5! 
Receiving ... From 5  Type 7 Content question(variable(("at",3,1,3)),false,9) --- 
Receiving question(variable(("at",3,1,3)), false, 9) from 5! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]])]
==> in finding answers 
Reasons for variable(("at",3,1,3)), false, 9 is [] 
Did not find answer to the question(variable(("at",3,1,3)), false, 9) from 5! 
Has been discussing with [3,2,4,5,5] about this question(variable(("at",3,1,3)), false, 9) now is 5 
Being asked [3,2,4,5,5,5] 
To be asked []
Answer with no .... Receiving ... From 4  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no],[no]] from 4! 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no]] from 4! 
Receiving ... From 4  Type 7 Content question(variable(("at",3,1,3)),false,9) --- 
Receiving question(variable(("at",3,1,3)), false, 9) from 4! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]])]
==> in finding answers 
Reasons for variable(("at",3,1,3)), false, 9 is [] 
Did not find answer to the question(variable(("at",3,1,3)), false, 9) from 4! 
Has been discussing with [3,2,4,5,5,4] about this question(variable(("at",3,1,3)), false, 9) now is 4 
Being asked [3,2,4,5,5,4,4] 
To be asked []
Answer with no .... Receiving ... From 2  Type 6 Content answer(variable(("at",3,2,4)),false,7,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,4)), false, 7) is [[no],[no]] from 2! 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no],[no]] from 2! 
In propagation with answer [[no],[no]]  
Notify [4,5] for the question(variable(("at",3,2,3)), false, 8) with answer [[no],[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no]] from 2! 
Receiving ... From 2  Type 7 Content question(variable(("at",3,1,3)),false,9) --- 
Receiving question(variable(("at",3,1,3)), false, 9) from 2! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]])]
==> in finding answers 
Reasons for variable(("at",3,1,3)), false, 9 is [] 
Did not find answer to the question(variable(("at",3,1,3)), false, 9) from 2! 
Has been discussing with [3,2,4,5,5,4,2] about this question(variable(("at",3,1,3)), false, 9) now is 2 
Being asked [3,2,4,5,5,4,2,2] 
To be asked []
Answer with no .... Receiving ... From 2  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no],[no]] from 2! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no],[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no],[no]] from 5! 
In propagation with answer [[no],[no]]  
Notify [2,4] for the question(variable(("at",3,1,3)), false, 9) with answer [[no],[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no],[no]] from 4! 
In propagation with answer [[no],[no]]  
Notify [2,5] for the question(variable(("at",3,2,3)), false, 8) with answer [[no],[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no]] from 4! 
In propagation with answer [[no]]  
Notify [2,5] for the question(variable(("at",3,1,3)), false, 9) with answer [[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,1,3)),false,9,[no]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [no] from 4! 
List of people who answered with no to the question(variable(("at",3,1,3)),false,9) is [5,4,4] 
Print a list 
: #elements is 3 
 [2,4,5] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,2,3)),false,8,[[no],[no]]) --- 
Receiving answer(variable(("at",3,2,3)), false, 8) is [[no],[no]] from 4! 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,1,3)),false,9,[no]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [no] from 2! 
List of people who answered with no to the question(variable(("at",3,1,3)),false,9) is [5,4,4,2] 
Print a list 
: #elements is 3 
 [2,4,5] 
In propagation === received all NO 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no]] from 2! 
In propagation with answer [[no]]  
Notify [4,5] for the question(variable(("at",3,1,3)), false, 9) with answer [[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,1,3)),false,9,[no]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [no] from 2! 
Receiving ... From 0  Type 10 Content next(10) --- 
Step processing observations 10
Agent computing next step and diagnosis 10 
In agent 1 solving step 10 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_10.lp'  '1_enext_9.lp' ' -c t=10'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_10.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_9.lp'  '1_next_10.lp' '1_occ_10.lp'  '1_enext_10.lp' ' -c t=11' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_10.lp' 
!File '1_my_error_10.lp'
Number of errors [] 0
I do not need diagnoses at Step 10!
Done initializing the diagnosis at step 10 ======================= 
Receiving ... From 0  Type 2 Content next(11) --- 
Step sending action 11
Done sending action occurs(1,("down",1,3,4,3),11) at step 11 ======================= 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no]] from 4! 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no]] from 4! 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no],[no]] from 2! 
In propagation with answer [[no],[no]]  
Notify [4,5] for the question(variable(("at",3,1,3)), false, 9) with answer [[no],[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no]] from 2! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no]] from 5! 
In propagation with answer [[no]]  
Notify [2,4] for the question(variable(("at",3,1,3)), false, 9) with answer [[no]] 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no],[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no],[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no]] from 5! 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no],[no]] from 4! 
In propagation with answer [[no],[no]]  
Notify [2,5] for the question(variable(("at",3,1,3)), false, 9) with answer [[no],[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no],[no]] from 4! 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,1,3)),false,9,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,3)), false, 9) is [[no],[no]] from 2! 
Receiving ... From 0  Type 10 Content next(11) --- 
Step processing observations 11
Agent computing next step and diagnosis 11 
In agent 1 solving step 11 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_11.lp'  '1_enext_10.lp' ' -c t=11'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_11.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_10.lp'  '1_next_11.lp' '1_occ_11.lp'  '1_enext_11.lp' ' -c t=12' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_11.lp' 
!File '1_my_error_11.lp'
Number of errors [] 0
I do not need diagnoses at Step 11!
Done initializing the diagnosis at step 11 ======================= 
Receiving ... From 0  Type 2 Content next(12) --- 
Step sending action 12
Done sending action occurs(1,("down",1,3,3,2),12) at step 12 ======================= 
Receiving ... From 0  Type 10 Content next(12) --- 
Step processing observations 12
Agent computing next step and diagnosis 12 
In agent 1 solving step 12 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_12.lp'  '1_enext_11.lp' ' -c t=12'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_12.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_11.lp'  '1_next_12.lp' '1_occ_12.lp'  '1_enext_12.lp' ' -c t=13' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_12.lp' 
!File '1_my_error_12.lp'
Number of errors [] 0
I do not need diagnoses at Step 12!
Done initializing the diagnosis at step 12 ======================= 
Receiving ... From 0  Type 2 Content next(13) --- 
Step sending action 13
Done sending action occurs(1,("down",1,3,2,1),13) at step 13 ======================= 
Receiving ... From 3  Type 7 Content question(variable(("at",3,1,2)),false,12) --- 
Receiving question(variable(("at",3,1,2)), false, 12) from 3! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no],[no]])]
==> in finding answers 
Reasons for variable(("at",3,1,2)), false, 12 is [] 
Did not find answer to the question(variable(("at",3,1,2)), false, 12) from 3! 
Has been discussing with [3] about this question(variable(("at",3,1,2)), false, 12) now is 3 
Being asked [3,3] 
To be asked [2,4,5]
Asking my neighbors .... Need to ask for help from neighbor [2,4,5] 
I send a message to 2 for question(variable(("at",3,1,2)), false, 12) !
I send a message to 4 for question(variable(("at",3,1,2)), false, 12) !
I send a message to 5 for question(variable(("at",3,1,2)), false, 12) !
Receiving ... From 5  Type 7 Content question(variable(("at",3,1,2)),false,12) --- 
Receiving question(variable(("at",3,1,2)), false, 12) from 5! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no],[no]])]
==> in finding answers 
Reasons for variable(("at",3,1,2)), false, 12 is [] 
Did not find answer to the question(variable(("at",3,1,2)), false, 12) from 5! 
Has been discussing with [3,2,4,5,5] about this question(variable(("at",3,1,2)), false, 12) now is 5 
Being asked [3,2,4,5,5,5] 
To be asked []
Answer with no .... Receiving ... From 4  Type 7 Content question(variable(("at",3,1,2)),false,12) --- 
Receiving question(variable(("at",3,1,2)), false, 12) from 4! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no],[no]])]
==> in finding answers 
Reasons for variable(("at",3,1,2)), false, 12 is [] 
Did not find answer to the question(variable(("at",3,1,2)), false, 12) from 4! 
Has been discussing with [3,2,4,5,5,4] about this question(variable(("at",3,1,2)), false, 12) now is 4 
Being asked [3,2,4,5,5,4,4] 
To be asked []
Answer with no .... Receiving ... From 2  Type 7 Content question(variable(("at",3,1,2)),false,12) --- 
Receiving question(variable(("at",3,1,2)), false, 12) from 2! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no],[no]])]
==> in finding answers 
Reasons for variable(("at",3,1,2)), false, 12 is [] 
Did not find answer to the question(variable(("at",3,1,2)), false, 12) from 2! 
Has been discussing with [3,2,4,5,5,4,2] about this question(variable(("at",3,1,2)), false, 12) now is 2 
Being asked [3,2,4,5,5,4,2,2] 
To be asked []
Answer with no .... Receiving ... From 5  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no],[no]] from 5! 
In propagation with answer [[no],[no]]  
Notify [2,4] for the question(variable(("at",3,1,2)), false, 12) with answer [[no],[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no]] from 4! 
In propagation with answer [[no]]  
Notify [2,5] for the question(variable(("at",3,1,2)), false, 12) with answer [[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,1,2)),false,12,[no]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [no] from 4! 
List of people who answered with no to the question(variable(("at",3,1,2)),false,12) is [5,4,4] 
Print a list 
: #elements is 3 
 [2,4,5] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,1,2)),false,12,[no]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [no] from 2! 
List of people who answered with no to the question(variable(("at",3,1,2)),false,12) is [5,4,4,2] 
Print a list 
: #elements is 3 
 [2,4,5] 
In propagation === received all NO 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no]] from 2! 
In propagation with answer [[no]]  
Notify [4,5] for the question(variable(("at",3,1,2)), false, 12) with answer [[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,1,2)),false,12,[no]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [no] from 2! 
Receiving ... From 0  Type 10 Content next(13) --- 
Step processing observations 13
Agent computing next step and diagnosis 13 
In agent 1 solving step 13 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_13.lp'  '1_enext_12.lp' ' -c t=13'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_13.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_12.lp'  '1_next_13.lp' '1_occ_13.lp'  '1_enext_13.lp' ' -c t=14' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_13.lp' 
!File '1_my_error_13.lp'
Number of errors [] 0
I do not need diagnoses at Step 13!
Done initializing the diagnosis at step 13 ======================= 
Receiving ... From 0  Type 2 Content next(14) --- 
Step sending action 14
Done sending action occurs(1,("left",1,3,1,2),14) at step 14 ======================= 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no]] from 4! 
Receiving ... From 3  Type 7 Content question(variable(("at",3,0,2)),false,13) --- 
Receiving question(variable(("at",3,0,2)), false, 13) from 3! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no]])]
==> in finding answers 
Reasons for variable(("at",3,0,2)), false, 13 is [] 
Did not find answer to the question(variable(("at",3,0,2)), false, 13) from 3! 
Has been discussing with [3] about this question(variable(("at",3,0,2)), false, 13) now is 3 
Being asked [3,3] 
To be asked [2,4,5]
Asking my neighbors .... Need to ask for help from neighbor [2,4,5] 
I send a message to 2 for question(variable(("at",3,0,2)), false, 13) !
I send a message to 4 for question(variable(("at",3,0,2)), false, 13) !
I send a message to 5 for question(variable(("at",3,0,2)), false, 13) !
Receiving ... From 5  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no]] from 5! 
In propagation with answer [[no]]  
Notify [2,4] for the question(variable(("at",3,1,2)), false, 12) with answer [[no]] 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no],[no]] from 5! 
Receiving ... From 5  Type 7 Content question(variable(("at",3,0,2)),false,13) --- 
Receiving question(variable(("at",3,0,2)), false, 13) from 5! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no]])]
==> in finding answers 
Reasons for variable(("at",3,0,2)), false, 13 is [] 
Did not find answer to the question(variable(("at",3,0,2)), false, 13) from 5! 
Has been discussing with [3,2,4,5,5] about this question(variable(("at",3,0,2)), false, 13) now is 5 
Being asked [3,2,4,5,5,5] 
To be asked []
Answer with no .... Receiving ... From 4  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no]] from 4! 
Receiving ... From 4  Type 7 Content question(variable(("at",3,0,2)),false,13) --- 
Receiving question(variable(("at",3,0,2)), false, 13) from 4! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no]])]
==> in finding answers 
Reasons for variable(("at",3,0,2)), false, 13 is [] 
Did not find answer to the question(variable(("at",3,0,2)), false, 13) from 4! 
Has been discussing with [3,2,4,5,5,4] about this question(variable(("at",3,0,2)), false, 13) now is 4 
Being asked [3,2,4,5,5,4,4] 
To be asked []
Answer with no .... Receiving ... From 2  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no],[no]] from 2! 
In propagation with answer [[no],[no]]  
Notify [4,5] for the question(variable(("at",3,1,2)), false, 12) with answer [[no],[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no]] from 2! 
Receiving ... From 2  Type 7 Content question(variable(("at",3,0,2)),false,13) --- 
Receiving question(variable(("at",3,0,2)), false, 13) from 2! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no],[no]])]
==> in finding answers 
Reasons for variable(("at",3,0,2)), false, 13 is [] 
Did not find answer to the question(variable(("at",3,0,2)), false, 13) from 2! 
Has been discussing with [3,2,4,5,5,4,2] about this question(variable(("at",3,0,2)), false, 13) now is 2 
Being asked [3,2,4,5,5,4,2,2] 
To be asked []
Answer with no .... Receiving ... From 2  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no],[no]] from 2! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no],[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no],[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no],[no]] from 5! 
In propagation with answer [[no],[no]]  
Notify [2,4] for the question(variable(("at",3,0,2)), false, 13) with answer [[no],[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no],[no]] from 4! 
In propagation with answer [[no],[no]]  
Notify [2,5] for the question(variable(("at",3,1,2)), false, 12) with answer [[no],[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no]] from 4! 
In propagation with answer [[no]]  
Notify [2,5] for the question(variable(("at",3,0,2)), false, 13) with answer [[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,0,2)),false,13,[no]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [no] from 4! 
List of people who answered with no to the question(variable(("at",3,0,2)),false,13) is [5,4,4] 
Print a list 
: #elements is 3 
 [2,4,5] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,1,2)),false,12,[[no],[no]]) --- 
Receiving answer(variable(("at",3,1,2)), false, 12) is [[no],[no]] from 4! 
Receiving ... From 0  Type 10 Content next(14) --- 
Step processing observations 14
Agent computing next step and diagnosis 14 
In agent 1 solving step 14 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_14.lp'  '1_enext_13.lp' ' -c t=14'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_14.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_13.lp'  '1_next_14.lp' '1_occ_14.lp'  '1_enext_14.lp' ' -c t=15' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_14.lp' 
!File '1_my_error_14.lp'
Number of errors [] 0
I do not need diagnoses at Step 14!
Done initializing the diagnosis at step 14 ======================= 
Receiving ... From 0  Type 2 Content next(15) --- 
Step sending action 15
Done sending action occurs(1,("stay",1,2,1),15) at step 15 ======================= 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,0,2)),false,13,[no]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [no] from 2! 
List of people who answered with no to the question(variable(("at",3,0,2)),false,13) is [5,4,4,2] 
Print a list 
: #elements is 3 
 [2,4,5] 
In propagation === received all NO 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no]] from 2! 
In propagation with answer [[no]]  
Notify [4,5] for the question(variable(("at",3,0,2)), false, 13) with answer [[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,0,2)),false,13,[no]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [no] from 2! 
Receiving ... From 3  Type 7 Content question(variable(("at",3,0,1)),false,14) --- 
Receiving question(variable(("at",3,0,1)), false, 14) from 3! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(5,no,variable(("at",3,0,2)),false,13),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,0,2)),false,13),[[no]]),answers(question(4,no,variable(("at",3,0,2)),false,13),[no]),answers(question(2,no,variable(("at",3,0,2)),false,13),[no]),answers(question(2,no,variable(("at",3,0,2)),false,13),[[no]])]
==> in finding answers 
Reasons for variable(("at",3,0,1)), false, 14 is [] 
Did not find answer to the question(variable(("at",3,0,1)), false, 14) from 3! 
Has been discussing with [3] about this question(variable(("at",3,0,1)), false, 14) now is 3 
Being asked [3,3] 
To be asked [2,4,5]
Asking my neighbors .... Need to ask for help from neighbor [2,4,5] 
I send a message to 2 for question(variable(("at",3,0,1)), false, 14) !
I send a message to 4 for question(variable(("at",3,0,1)), false, 14) !
I send a message to 5 for question(variable(("at",3,0,1)), false, 14) !
Receiving ... From 5  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no],[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no],[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no]] from 5! 
In propagation with answer [[no]]  
Notify [2,4] for the question(variable(("at",3,0,2)), false, 13) with answer [[no]] 
Receiving ... From 5  Type 7 Content question(variable(("at",3,0,1)),false,14) --- 
Receiving question(variable(("at",3,0,1)), false, 14) from 5! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(5,no,variable(("at",3,0,2)),false,13),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,0,2)),false,13),[[no]]),answers(question(4,no,variable(("at",3,0,2)),false,13),[no]),answers(question(2,no,variable(("at",3,0,2)),false,13),[no]),answers(question(2,no,variable(("at",3,0,2)),false,13),[[no]]),answers(question(5,no,variable(("at",3,0,2)),false,13),[[no]])]
==> in finding answers 
Reasons for variable(("at",3,0,1)), false, 14 is [] 
Did not find answer to the question(variable(("at",3,0,1)), false, 14) from 5! 
Has been discussing with [3,2,4,5,5] about this question(variable(("at",3,0,1)), false, 14) now is 5 
Being asked [3,2,4,5,5,5] 
To be asked []
Answer with no .... Receiving ... From 4  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no]] from 4! 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no],[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no],[no]] from 4! 
In propagation with answer [[no],[no]]  
Notify [2,5] for the question(variable(("at",3,0,2)), false, 13) with answer [[no],[no]] 
Receiving ... From 4  Type 7 Content question(variable(("at",3,0,1)),false,14) --- 
Receiving question(variable(("at",3,0,1)), false, 14) from 4! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(5,no,variable(("at",3,0,2)),false,13),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,0,2)),false,13),[[no]]),answers(question(4,no,variable(("at",3,0,2)),false,13),[no]),answers(question(2,no,variable(("at",3,0,2)),false,13),[no]),answers(question(2,no,variable(("at",3,0,2)),false,13),[[no]]),answers(question(5,no,variable(("at",3,0,2)),false,13),[[no]]),answers(question(4,no,variable(("at",3,0,2)),false,13),[[no],[no]])]
==> in finding answers 
Reasons for variable(("at",3,0,1)), false, 14 is [] 
Did not find answer to the question(variable(("at",3,0,1)), false, 14) from 4! 
Has been discussing with [3,2,4,5,5,4] about this question(variable(("at",3,0,1)), false, 14) now is 4 
Being asked [3,2,4,5,5,4,4] 
To be asked []
Answer with no .... Receiving ... From 4  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no]] from 4! 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no],[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no],[no]] from 4! 
Receiving ... From 2  Type 7 Content question(variable(("at",3,0,1)),false,14) --- 
Receiving question(variable(("at",3,0,1)), false, 14) from 2! 
Current answers I stored: [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(5,no,variable(("at",3,0,2)),false,13),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,0,2)),false,13),[[no]]),answers(question(4,no,variable(("at",3,0,2)),false,13),[no]),answers(question(2,no,variable(("at",3,0,2)),false,13),[no]),answers(question(2,no,variable(("at",3,0,2)),false,13),[[no]]),answers(question(5,no,variable(("at",3,0,2)),false,13),[[no]]),answers(question(4,no,variable(("at",3,0,2)),false,13),[[no],[no]])]
==> in finding answers 
Reasons for variable(("at",3,0,1)), false, 14 is [] 
Did not find answer to the question(variable(("at",3,0,1)), false, 14) from 2! 
Has been discussing with [3,2,4,5,5,4,2] about this question(variable(("at",3,0,1)), false, 14) now is 2 
Being asked [3,2,4,5,5,4,2,2] 
To be asked []
Answer with no .... Receiving ... From 2  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no],[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no],[no]] from 2! 
In propagation with answer [[no],[no]]  
Notify [4,5] for the question(variable(("at",3,0,2)), false, 13) with answer [[no],[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no]] from 2! 
Receiving ... From 0  Type 10 Content next(15) --- 
Step processing observations 15
Agent computing next step and diagnosis 15 
In agent 1 solving step 15 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_occ_15.lp'  '1_enext_14.lp' ' -c t=15'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_15.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "../../mapf_domain.lp" "prob2_1.lp" '1_agent.lp' '1_next_14.lp'  '1_next_15.lp' '1_occ_15.lp'  '1_enext_15.lp' ' -c t=16' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_15.lp' 
!File '1_my_error_15.lp'
Number of errors [] 0
I do not need diagnoses at Step 15!
Done initializing the diagnosis at step 15 ======================= 
Receiving ... From 0  Type 2 Content next(16) --- 
Step sending action 16
Done with executing all actions 16 ======================= 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no],[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no],[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,0,1)),false,14,[[no],[no]]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [[no],[no]] from 5! 
In propagation with answer [[no],[no]]  
Notify [2,4] for the question(variable(("at",3,0,1)), false, 14) with answer [[no],[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,0,1)),false,14,[[no]]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [[no]] from 4! 
In propagation with answer [[no]]  
Notify [2,5] for the question(variable(("at",3,0,1)), false, 14) with answer [[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,0,1)),false,14,[no]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [no] from 4! 
List of people who answered with no to the question(variable(("at",3,0,1)),false,14) is [5,4,4] 
Print a list 
: #elements is 3 
 [2,4,5] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,0,1)),false,14,[no]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [no] from 2! 
List of people who answered with no to the question(variable(("at",3,0,1)),false,14) is [5,4,4,2] 
Print a list 
: #elements is 3 
 [2,4,5] 
In propagation === received all NO 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,0,2)),false,13,[[no],[no]]) --- 
Receiving answer(variable(("at",3,0,2)), false, 13) is [[no],[no]] from 2! 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,0,1)),false,14,[[no]]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [[no]] from 2! 
In propagation with answer [[no]]  
Notify [4,5] for the question(variable(("at",3,0,1)), false, 14) with answer [[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,0,1)),false,14,[no]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [no] from 2! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,0,1)),false,14,[[no],[no]]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [[no],[no]] from 5! 
Receiving ... From 5  Type 6 Content answer(variable(("at",3,0,1)),false,14,[[no]]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [[no]] from 5! 
In propagation with answer [[no]]  
Notify [2,4] for the question(variable(("at",3,0,1)), false, 14) with answer [[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,0,1)),false,14,[[no]]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [[no]] from 4! 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,0,1)),false,14,[[no],[no]]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [[no],[no]] from 4! 
In propagation with answer [[no],[no]]  
Notify [2,5] for the question(variable(("at",3,0,1)), false, 14) with answer [[no],[no]] 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,0,1)),false,14,[[no]]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [[no]] from 4! 
Receiving ... From 4  Type 6 Content answer(variable(("at",3,0,1)),false,14,[[no],[no]]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [[no],[no]] from 4! 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,0,1)),false,14,[[no],[no]]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [[no],[no]] from 2! 
In propagation with answer [[no],[no]]  
Notify [4,5] for the question(variable(("at",3,0,1)), false, 14) with answer [[no],[no]] 
Receiving ... From 2  Type 6 Content answer(variable(("at",3,0,1)),false,14,[[no]]) --- 
Receiving answer(variable(("at",3,0,1)), false, 14) is [[no]] from 2! 
Receiving ... From 0  Type 2 Content done --- 
All done 
Out from agent loop 
Done executing 
List of answers 
 [answers(question(self,("up",1,4,5,6),variable(("occupied",4,5)),true,5),[self]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[no]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(5,no,variable(("at",3,3,4)),false,6),[[no]]),answers(question(4,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(2,no,variable(("at",3,3,4)),false,6),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[no]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[no]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,4)),false,7),[[no]]),answers(question(4,no,variable(("at",3,2,4)),false,7),[[no],[no]]),answers(question(5,no,variable(("at",3,2,3)),false,8),[[no]]),answers(question(2,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(4,no,variable(("at",3,2,3)),false,8),[[no],[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[no]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(2,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,3)),false,9),[[no]]),answers(question(4,no,variable(("at",3,1,3)),false,9),[[no],[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[no]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(5,no,variable(("at",3,1,2)),false,12),[[no]]),answers(question(2,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(5,no,variable(("at",3,0,2)),false,13),[[no],[no]]),answers(question(4,no,variable(("at",3,1,2)),false,12),[[no],[no]]),answers(question(4,no,variable(("at",3,0,2)),false,13),[[no]]),answers(question(4,no,variable(("at",3,0,2)),false,13),[no]),answers(question(2,no,variable(("at",3,0,2)),false,13),[no]),answers(question(2,no,variable(("at",3,0,2)),false,13),[[no]]),answers(question(5,no,variable(("at",3,0,2)),false,13),[[no]]),answers(question(4,no,variable(("at",3,0,2)),false,13),[[no],[no]]),answers(question(2,no,variable(("at",3,0,2)),false,13),[[no],[no]]),answers(question(5,no,variable(("at",3,0,1)),false,14),[[no],[no]]),answers(question(4,no,variable(("at",3,0,1)),false,14),[[no]]),answers(question(4,no,variable(("at",3,0,1)),false,14),[no]),answers(question(2,no,variable(("at",3,0,1)),false,14),[no]),answers(question(2,no,variable(("at",3,0,1)),false,14),[[no]]),answers(question(5,no,variable(("at",3,0,1)),false,14),[[no]]),answers(question(4,no,variable(("at",3,0,1)),false,14),[[no],[no]]),answers(question(2,no,variable(("at",3,0,1)),false,14),[[no],[no]])] 

List of questions that I sent and received 
 [(0,3,variable(("at",3,3,4)),false,6),(1,2,variable(("at",3,3,4)),false,6),(1,4,variable(("at",3,3,4)),false,6),(1,5,variable(("at",3,3,4)),false,6),(0,5,variable(("at",3,3,4)),false,6),(0,4,variable(("at",3,3,4)),false,6),(0,2,variable(("at",3,3,4)),false,6),(0,3,variable(("at",3,2,4)),false,7),(1,2,variable(("at",3,2,4)),false,7),(1,4,variable(("at",3,2,4)),false,7),(1,5,variable(("at",3,2,4)),false,7),(0,5,variable(("at",3,2,4)),false,7),(0,4,variable(("at",3,2,4)),false,7),(0,2,variable(("at",3,2,4)),false,7),(0,3,variable(("at",3,2,3)),false,8),(1,2,variable(("at",3,2,3)),false,8),(1,4,variable(("at",3,2,3)),false,8),(1,5,variable(("at",3,2,3)),false,8),(0,5,variable(("at",3,2,3)),false,8),(0,4,variable(("at",3,2,3)),false,8),(0,2,variable(("at",3,2,3)),false,8),(0,3,variable(("at",3,1,3)),false,9),(1,2,variable(("at",3,1,3)),false,9),(1,4,variable(("at",3,1,3)),false,9),(1,5,variable(("at",3,1,3)),false,9),(0,5,variable(("at",3,1,3)),false,9),(0,4,variable(("at",3,1,3)),false,9),(0,2,variable(("at",3,1,3)),false,9),(0,3,variable(("at",3,1,2)),false,12),(1,2,variable(("at",3,1,2)),false,12),(1,4,variable(("at",3,1,2)),false,12),(1,5,variable(("at",3,1,2)),false,12),(0,5,variable(("at",3,1,2)),false,12),(0,4,variable(("at",3,1,2)),false,12),(0,2,variable(("at",3,1,2)),false,12),(0,3,variable(("at",3,0,2)),false,13),(1,2,variable(("at",3,0,2)),false,13),(1,4,variable(("at",3,0,2)),false,13),(1,5,variable(("at",3,0,2)),false,13),(0,5,variable(("at",3,0,2)),false,13),(0,4,variable(("at",3,0,2)),false,13),(0,2,variable(("at",3,0,2)),false,13),(0,3,variable(("at",3,0,1)),false,14),(1,2,variable(("at",3,0,1)),false,14),(1,4,variable(("at",3,0,1)),false,14),(1,5,variable(("at",3,0,1)),false,14),(0,5,variable(("at",3,0,1)),false,14),(0,4,variable(("at",3,0,1)),false,14),(0,2,variable(("at",3,0,1)),false,14)] 

