TIPC Linda server listening at: port_id('<0.0.0:1405336423>')
Number of steps 12
 Domain "domain.lp"
 Problem "prob1_tru1.lp"
Done starting 
In preparation 
*********** 
 Computing initial state 
 **************
 Executing: clingo ../../../lps/compute_0.lp  "domain.lp" "prob1_tru1.lp" --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_next_0.lp' 
!Setting up expected initial state
 clingo '2_next_0.lp' -c t=0 --outf=0 -V0 --out-atomf=%s. | head -n1 >  '2_enext_0.lp' 
*********** 
 Computing the fluents 
 **************
 In the loop ... 
Receiving ... From 0  Type 2 Content next(1) --- 
Step sending action 1
Done sending action none at step 1 ======================= 
Receiving ... From 0  Type 10 Content next(1) --- 
Step processing observations 1
Agent computing next step and diagnosis 1 
In agent 2 solving step 1 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_occ_1.lp'  '2_enext_0.lp' ' -c t=1'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_1.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_next_0.lp'  '2_next_1.lp' '2_occ_1.lp'  '2_enext_1.lp' ' -c t=2' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_1.lp' 
!File '2_my_error_1.lp'
Number of errors [] 0
I do not need diagnoses at Step 1!
Done initializing the diagnosis at step 1 ======================= 
Receiving ... From 0  Type 2 Content next(2) --- 
Step sending action 2
Done sending action occurs(constant("tru1"),("load-truck",constant("obj13"),constant("tru1"),constant("pos1")),2) at step 2 ======================= 
Receiving ... From 0  Type 10 Content next(2) --- 
Step processing observations 2
Agent computing next step and diagnosis 2 
In agent 2 solving step 2 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_occ_2.lp'  '2_enext_1.lp' ' -c t=2'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_2.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_next_1.lp'  '2_next_2.lp' '2_occ_2.lp'  '2_enext_2.lp' ' -c t=3' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_2.lp' 
!File '2_my_error_2.lp'
Number of errors [] 0
I do not need diagnoses at Step 2!
Done initializing the diagnosis at step 2 ======================= 
Receiving ... From 0  Type 2 Content next(3) --- 
Step sending action 3
Done sending action occurs(constant("tru1"),("load-truck",constant("obj11"),constant("tru1"),constant("pos1")),3) at step 3 ======================= 
Receiving ... From 0  Type 10 Content next(3) --- 
Step processing observations 3
Agent computing next step and diagnosis 3 
In agent 2 solving step 3 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_occ_3.lp'  '2_enext_2.lp' ' -c t=3'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_3.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_next_2.lp'  '2_next_3.lp' '2_occ_3.lp'  '2_enext_3.lp' ' -c t=4' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_3.lp' 
!File '2_my_error_3.lp'
Number of errors [] 0
I do not need diagnoses at Step 3!
Done initializing the diagnosis at step 3 ======================= 
Receiving ... From 0  Type 2 Content next(4) --- 
Step sending action 4
Done sending action occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),4) at step 4 ======================= 
Receiving ... From 0  Type 10 Content next(4) --- 
Step processing observations 4
Agent computing next step and diagnosis 4 
In agent 2 solving step 4 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_occ_4.lp'  '2_enext_3.lp' ' -c t=4'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_4.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_next_3.lp'  '2_next_4.lp' '2_occ_4.lp'  '2_enext_4.lp' ' -c t=5' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_4.lp' 
!File '2_my_error_4.lp'
Number of errors [] 0
I do not need diagnoses at Step 4!
Done initializing the diagnosis at step 4 ======================= 
Receiving ... From 0  Type 2 Content next(5) --- 
Step sending action 5
Done sending action occurs(constant("tru1"),("unload-truck",constant("obj11"),constant("tru1"),constant("apt1")),5) at step 5 ======================= 
Receiving ... From 0  Type 10 Content next(5) --- 
Step processing observations 5
Agent computing next step and diagnosis 5 
In agent 2 solving step 5 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_occ_5.lp'  '2_enext_4.lp' ' -c t=5'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_5.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_next_4.lp'  '2_next_5.lp' '2_occ_5.lp'  '2_enext_5.lp' ' -c t=6' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_5.lp' 
!File '2_my_error_5.lp'
Number of errors [] 0
I do not need diagnoses at Step 5!
Done initializing the diagnosis at step 5 ======================= 
Receiving ... From 0  Type 2 Content next(6) --- 
Step sending action 6
Done sending action occurs(constant("tru1"),("unload-truck",constant("obj13"),constant("tru1"),constant("apt1")),6) at step 6 ======================= 
Receiving ... From 1  Type 7 Content question(variable(("at",constant("obj23"),constant("apt2"))),false,5) --- 
Receiving question(variable(("at",constant("obj23"),constant("apt2"))), false, 5) from 1! 
Current answers I stored: []
==> in finding answers 
Reasons for variable(("at",constant("obj23"),constant("apt2"))), false, 5 is [] 
Did not find answer to the question(variable(("at",constant("obj23"),constant("apt2"))), false, 5) from 1! 
Has been discussing with [1] about this question(variable(("at",constant("obj23"),constant("apt2"))), false, 5) now is 1 
Being asked [1,1] 
To be asked []
Answer with no .... Receiving ... From 1  Type 6 Content answer(variable(("at",constant("obj23"),constant("apt2"))),false,5,[[self],[(3,5)]]) --- 
Receiving answer(variable(("at",constant("obj23"),constant("apt2"))), false, 5) is [[self],[(3,5)]] from 1! 
In propagation with answer [[self],[(3,5)]]  
Notify [] for the question(variable(("at",constant("obj23"),constant("apt2"))), false, 5) with answer [[self],[(3,5)]] 
Receiving ... From 0  Type 10 Content next(6) --- 
Step processing observations 6
Agent computing next step and diagnosis 6 
In agent 2 solving step 6 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_occ_6.lp'  '2_enext_5.lp' ' -c t=6'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_6.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_next_5.lp'  '2_next_6.lp' '2_occ_6.lp'  '2_enext_6.lp' ' -c t=7' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_6.lp' 
!File '2_my_error_6.lp'
Number of errors [] 0
I do not need diagnoses at Step 6!
Done initializing the diagnosis at step 6 ======================= 
Receiving ... From 0  Type 2 Content next(7) --- 
Step sending action 7
Done sending action occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),7) at step 7 ======================= 
Receiving ... From 1  Type 7 Content question(variable(("at",constant("obj21"),constant("apt2"))),false,6) --- 
Receiving question(variable(("at",constant("obj21"),constant("apt2"))), false, 6) from 1! 
Current answers I stored: [answers(question(1,no,variable(("at",constant("obj23"),constant("apt2"))),false,5),[[self],[(3,5)]])]
==> in finding answers 
Reasons for variable(("at",constant("obj21"),constant("apt2"))), false, 6 is [] 
Did not find answer to the question(variable(("at",constant("obj21"),constant("apt2"))), false, 6) from 1! 
Has been discussing with [1] about this question(variable(("at",constant("obj21"),constant("apt2"))), false, 6) now is 1 
Being asked [1,1] 
To be asked []
Answer with no .... Receiving ... From 0  Type 10 Content next(7) --- 
Step processing observations 7
Agent computing next step and diagnosis 7 
In agent 2 solving step 7 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_occ_7.lp'  '2_enext_6.lp' ' -c t=7'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_7.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_next_6.lp'  '2_next_7.lp' '2_occ_7.lp'  '2_enext_7.lp' ' -c t=8' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_7.lp' 
!File '2_my_error_7.lp'
Number of errors [] 0
I do not need diagnoses at Step 7!
Done initializing the diagnosis at step 7 ======================= 
Receiving ... From 0  Type 2 Content next(8) --- 
Step sending action 8
Done sending action occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),8) at step 8 ======================= 
Receiving ... From 1  Type 6 Content answer(variable(("at",constant("obj21"),constant("apt2"))),false,6,[[self],[(3,6)]]) --- 
Receiving answer(variable(("at",constant("obj21"),constant("apt2"))), false, 6) is [[self],[(3,6)]] from 1! 
In propagation with answer [[self],[(3,6)]]  
Notify [] for the question(variable(("at",constant("obj21"),constant("apt2"))), false, 6) with answer [[self],[(3,6)]] 
Receiving ... From 0  Type 10 Content next(8) --- 
Step processing observations 8
Agent computing next step and diagnosis 8 
In agent 2 solving step 8 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_occ_8.lp'  '2_enext_7.lp' ' -c t=8'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_8.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_next_7.lp'  '2_next_8.lp' '2_occ_8.lp'  '2_enext_8.lp' ' -c t=9' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_8.lp' 
!File '2_my_error_8.lp'
Number of errors [] 0
I do not need diagnoses at Step 8!
Done initializing the diagnosis at step 8 ======================= 
Receiving ... From 0  Type 2 Content next(9) --- 
Step sending action 9
Done sending action occurs(constant("tru1"),("load-truck",constant("obj21"),constant("tru1"),constant("apt1")),9) at step 9 ======================= 
Receiving ... From 0  Type 10 Content next(9) --- 
Step processing observations 9
Agent computing next step and diagnosis 9 
In agent 2 solving step 9 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_occ_9.lp'  '2_enext_8.lp' ' -c t=9'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_9.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_next_8.lp'  '2_next_9.lp' '2_occ_9.lp'  '2_enext_9.lp' ' -c t=10' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_9.lp' 
!File '2_my_error_9.lp'
Number of errors [question(other,("load-truck",constant("obj21"),constant("tru1"),constant("apt1")),variable(("at",constant("obj21"),constant("apt1"))),false,9)] 1
I need some diagnoses at 9 Step!
I need some diagnoses at Step 9!
This needs to be resolved [question(other,("load-truck",constant("obj21"),constant("tru1"),constant("apt1")),variable(("at",constant("obj21"),constant("apt1"))),false,9)] at Step 9!
I need to look for answer for this "load-truck",constant("obj21"),constant("tru1"),constant("apt1") variable(("at",constant("obj21"),constant("apt1"))) false 9 
I did not find answer for this "load-truck",constant("obj21"),constant("tru1"),constant("apt1") variable(("at",constant("obj21"),constant("apt1"))) false 9 
I already asked these people [] for answer!
Need to ask for help from neighbor [1] 
I send a message to 1 for question(variable(("at",constant("obj21"),constant("apt1"))), false, 9) !
Done initializing the diagnosis at step 9 ======================= 
Receiving ... From 0  Type 2 Content next(10) --- 
Step sending action 10
Done sending action occurs(constant("tru1"),("load-truck",constant("obj23"),constant("tru1"),constant("apt1")),10) at step 10 ======================= 
Receiving ... From 1  Type 6 Content answer(variable(("at",constant("obj21"),constant("apt1"))),false,9,[[self],[(1,9)]]) --- 
Receiving answer(variable(("at",constant("obj21"),constant("apt1"))), false, 9) is [[self],[(1,9)]] from 1! 
In propagation with answer [[self],[(1,9)]]  
Notify [] for the question(variable(("at",constant("obj21"),constant("apt1"))), false, 9) with answer [[self],[(1,9)]] 
Receiving ... From 0  Type 10 Content next(10) --- 
Step processing observations 10
Agent computing next step and diagnosis 10 
In agent 2 solving step 10 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_occ_10.lp'  '2_enext_9.lp' ' -c t=10'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_10.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_next_9.lp'  '2_next_10.lp' '2_occ_10.lp'  '2_enext_10.lp' ' -c t=11' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_10.lp' 
!File '2_my_error_10.lp'
Number of errors [question(other,("load-truck",constant("obj23"),constant("tru1"),constant("apt1")),variable(("at",constant("obj23"),constant("apt1"))),false,10)] 1
I need some diagnoses at 10 Step!
I need some diagnoses at Step 10!
This needs to be resolved [question(self,("load-truck",constant("obj21"),constant("tru1"),constant("apt1")),variable(("in",constant("obj21"),constant("tru1"))),false,10),question(other,("load-truck",constant("obj23"),constant("tru1"),constant("apt1")),variable(("at",constant("obj23"),constant("apt1"))),false,10)] at Step 10!
Add some information for myself "load-truck",constant("obj21"),constant("tru1"),constant("apt1") variable(("in",constant("obj21"),constant("tru1"))) false 10 
I need to look for answer for this "load-truck",constant("obj23"),constant("tru1"),constant("apt1") variable(("at",constant("obj23"),constant("apt1"))) false 10 
I did not find answer for this "load-truck",constant("obj23"),constant("tru1"),constant("apt1") variable(("at",constant("obj23"),constant("apt1"))) false 10 
I already asked these people [] for answer!
Need to ask for help from neighbor [1] 
I send a message to 1 for question(variable(("at",constant("obj23"),constant("apt1"))), false, 10) !
Done initializing the diagnosis at step 10 ======================= 
Receiving ... From 0  Type 2 Content next(11) --- 
Step sending action 11
Done sending action occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),11) at step 11 ======================= 
Receiving ... From 1  Type 6 Content answer(variable(("at",constant("obj23"),constant("apt1"))),false,10,[[self]]) --- 
Receiving answer(variable(("at",constant("obj23"),constant("apt1"))), false, 10) is [[self]] from 1! 
In propagation with answer [[self]]  
Notify [] for the question(variable(("at",constant("obj23"),constant("apt1"))), false, 10) with answer [[self]] 
Receiving ... From 0  Type 10 Content next(11) --- 
Step processing observations 11
Agent computing next step and diagnosis 11 
In agent 2 solving step 11 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_occ_11.lp'  '2_enext_10.lp' ' -c t=11'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_11.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_next_10.lp'  '2_next_11.lp' '2_occ_11.lp'  '2_enext_11.lp' ' -c t=12' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_11.lp' 
!File '2_my_error_11.lp'
Number of errors [] 0
I do not need diagnoses at Step 11!
Done initializing the diagnosis at step 11 ======================= 
Receiving ... From 0  Type 2 Content next(12) --- 
Step sending action 12
Done sending action occurs(constant("tru1"),("unload-truck",constant("obj21"),constant("tru1"),constant("pos1")),12) at step 12 ======================= 
Receiving ... From 0  Type 10 Content next(12) --- 
Step processing observations 12
Agent computing next step and diagnosis 12 
In agent 2 solving step 12 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_occ_12.lp'  '2_enext_11.lp' ' -c t=12'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_enext_12.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru1.lp" '2_agent.lp' '2_next_11.lp'  '2_next_12.lp' '2_occ_12.lp'  '2_enext_12.lp' ' -c t=13' --outf=0 -V0 --out-atomf=%s. | head -n1  > '2_my_error_12.lp' 
!File '2_my_error_12.lp'
Number of errors [question(other,("unload-truck",constant("obj21"),constant("tru1"),constant("pos1")),variable(("in",constant("obj21"),constant("tru1"))),false,12)] 1
I need some diagnoses at 12 Step!
I need some diagnoses at Step 12!
This needs to be resolved [question(other,("unload-truck",constant("obj21"),constant("tru1"),constant("pos1")),variable(("in",constant("obj21"),constant("tru1"))),false,12)] at Step 12!
I need to look for answer for this "unload-truck",constant("obj21"),constant("tru1"),constant("pos1") variable(("in",constant("obj21"),constant("tru1"))) false 12 
I found an answer for this "unload-truck",constant("obj21"),constant("tru1"),constant("pos1") variable(("in",constant("obj21"),constant("tru1"))) false 12 
Done initializing the diagnosis at step 12 ======================= 
Receiving ... From 0  Type 2 Content next(13) --- 
Step sending action 13
Done with executing all actions 13 ======================= 
Receiving ... From 0  Type 10 Content next(13) --- 
Step processing observations 13
Done with executing all actions 13 ======================= 
Receiving ... From 0  Type 2 Content next(14) --- 
Step sending action 14
Done with executing all actions 14 ======================= 
Receiving ... From 0  Type 2 Content done --- 
All done 
Out from agent loop 
Done executing 
List of answers 
 [answers(question(1,no,variable(("at",constant("obj23"),constant("apt2"))),false,5),[[self],[(3,5)]]),answers(question(1,no,variable(("at",constant("obj21"),constant("apt2"))),false,6),[[self],[(3,6)]]),answers(question(1,no,variable(("at",constant("obj21"),constant("apt1"))),false,9),[[self],[(1,9)]]),answers(question(self,("load-truck",constant("obj21"),constant("tru1"),constant("apt1")),variable(("in",constant("obj21"),constant("tru1"))),false,10),[self]),answers(question(self,("load-truck",constant("obj21"),constant("tru1"),constant("apt1")),variable(("in",constant("obj21"),constant("tru1"))),false,10),[(2,10)]),answers(question(1,no,variable(("at",constant("obj23"),constant("apt1"))),false,10),[[self]]),answers(question(self,("load-truck",constant("obj23"),constant("tru1"),constant("apt1")),variable(("in",constant("obj23"),constant("tru1"))),false,11),[self]),answers(question(self,("unload-truck",constant("obj21"),constant("tru1"),constant("pos1")),variable(("in",constant("obj21"),constant("tru1"))),false,12),[(("load-truck",constant("obj21"),constant("tru1"),constant("apt1")),variable(("in",constant("obj21"),constant("tru1"))),false,10)])] 

List of questions that I sent and received 
 [(0,1,variable(("at",constant("obj23"),constant("apt2"))),false,5),(0,1,variable(("at",constant("obj21"),constant("apt2"))),false,6),(1,1,variable(("at",constant("obj21"),constant("apt1"))),false,9),(1,1,variable(("at",constant("obj23"),constant("apt1"))),false,10)] 

