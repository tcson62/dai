TIPC Linda server listening at: port_id('<0.0.0:1405336423>')
Number of steps 13
 Domain "domain.lp"
 Problem "prob1_apn1.lp"
Done starting 
In preparation 
*********** 
 Computing initial state 
 **************
 Executing: clingo ../../../lps/compute_0.lp  "domain.lp" "prob1_apn1.lp" --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_next_0.lp' 
!Setting up expected initial state
 clingo '1_next_0.lp' -c t=0 --outf=0 -V0 --out-atomf=%s. | head -n1 >  '1_enext_0.lp' 
*********** 
 Computing the fluents 
 **************
 In the loop ... 
Receiving ... From 0  Type 2 Content next(1) --- 
Step sending action 1
Done sending action occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),1) at step 1 ======================= 
Receiving ... From 0  Type 10 Content next(1) --- 
Step processing observations 1
Agent computing next step and diagnosis 1 
In agent 1 solving step 1 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_occ_1.lp'  '1_enext_0.lp' ' -c t=1'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_1.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_next_0.lp'  '1_next_1.lp' '1_occ_1.lp'  '1_enext_1.lp' ' -c t=2' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_1.lp' 
!File '1_my_error_1.lp'
Number of errors [] 0
I do not need diagnoses at Step 1!
Done initializing the diagnosis at step 1 ======================= 
Receiving ... From 0  Type 2 Content next(2) --- 
Step sending action 2
Done sending action occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),2) at step 2 ======================= 
Receiving ... From 0  Type 10 Content next(2) --- 
Step processing observations 2
Agent computing next step and diagnosis 2 
In agent 1 solving step 2 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_occ_2.lp'  '1_enext_1.lp' ' -c t=2'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_2.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_next_1.lp'  '1_next_2.lp' '1_occ_2.lp'  '1_enext_2.lp' ' -c t=3' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_2.lp' 
!File '1_my_error_2.lp'
Number of errors [] 0
I do not need diagnoses at Step 2!
Done initializing the diagnosis at step 2 ======================= 
Receiving ... From 0  Type 2 Content next(3) --- 
Step sending action 3
Done sending action occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),3) at step 3 ======================= 
Receiving ... From 0  Type 10 Content next(3) --- 
Step processing observations 3
Agent computing next step and diagnosis 3 
In agent 1 solving step 3 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_occ_3.lp'  '1_enext_2.lp' ' -c t=3'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_3.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_next_2.lp'  '1_next_3.lp' '1_occ_3.lp'  '1_enext_3.lp' ' -c t=4' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_3.lp' 
!File '1_my_error_3.lp'
Number of errors [] 0
I do not need diagnoses at Step 3!
Done initializing the diagnosis at step 3 ======================= 
Receiving ... From 0  Type 2 Content next(4) --- 
Step sending action 4
Done sending action occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),4) at step 4 ======================= 
Receiving ... From 0  Type 10 Content next(4) --- 
Step processing observations 4
Agent computing next step and diagnosis 4 
In agent 1 solving step 4 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_occ_4.lp'  '1_enext_3.lp' ' -c t=4'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_4.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_next_3.lp'  '1_next_4.lp' '1_occ_4.lp'  '1_enext_4.lp' ' -c t=5' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_4.lp' 
!File '1_my_error_4.lp'
Number of errors [] 0
I do not need diagnoses at Step 4!
Done initializing the diagnosis at step 4 ======================= 
Receiving ... From 0  Type 2 Content next(5) --- 
Step sending action 5
Done sending action occurs(constant("apn1"),("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),5) at step 5 ======================= 
Receiving ... From 0  Type 10 Content next(5) --- 
Step processing observations 5
Agent computing next step and diagnosis 5 
In agent 1 solving step 5 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_occ_5.lp'  '1_enext_4.lp' ' -c t=5'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_5.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_next_4.lp'  '1_next_5.lp' '1_occ_5.lp'  '1_enext_5.lp' ' -c t=6' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_5.lp' 
!File '1_my_error_5.lp'
Number of errors [question(other,("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),variable(("at",constant("obj23"),constant("apt2"))),false,5)] 1
I need some diagnoses at 5 Step!
I need some diagnoses at Step 5!
This needs to be resolved [question(other,("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),variable(("at",constant("obj23"),constant("apt2"))),false,5)] at Step 5!
I need to look for answer for this "load-airplane",constant("obj23"),constant("apn1"),constant("apt2") variable(("at",constant("obj23"),constant("apt2"))) false 5 
I did not find answer for this "load-airplane",constant("obj23"),constant("apn1"),constant("apt2") variable(("at",constant("obj23"),constant("apt2"))) false 5 
I already asked these people [] for answer!
Need to ask for help from neighbor [2,3] 
I send a message to 2 for question(variable(("at",constant("obj23"),constant("apt2"))), false, 5) !
I send a message to 3 for question(variable(("at",constant("obj23"),constant("apt2"))), false, 5) !
Done initializing the diagnosis at step 5 ======================= 
Receiving ... From 0  Type 2 Content next(6) --- 
Step sending action 6
Done sending action occurs(constant("apn1"),("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),6) at step 6 ======================= 
Receiving ... From 2  Type 6 Content answer(variable(("at",constant("obj23"),constant("apt2"))),false,5,[no]) --- 
Receiving answer(variable(("at",constant("obj23"),constant("apt2"))), false, 5) is [no] from 2! 
List of people who answered with no to the question(variable(("at",constant("obj23"),constant("apt2"))),false,5) is [2] 
Print a list 
: #elements is 2 
 [2,3] 
Receiving ... From 3  Type 6 Content answer(variable(("at",constant("obj23"),constant("apt2"))),false,5,[[self],[(3,5)]]) --- 
Receiving answer(variable(("at",constant("obj23"),constant("apt2"))), false, 5) is [[self],[(3,5)]] from 3! 
In propagation with answer [[self],[(3,5)]]  
Notify [2] for the question(variable(("at",constant("obj23"),constant("apt2"))), false, 5) with answer [[self],[(3,5)]] 
Receiving ... From 0  Type 10 Content next(6) --- 
Step processing observations 6
Agent computing next step and diagnosis 6 
In agent 1 solving step 6 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_occ_6.lp'  '1_enext_5.lp' ' -c t=6'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_6.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_next_5.lp'  '1_next_6.lp' '1_occ_6.lp'  '1_enext_6.lp' ' -c t=7' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_6.lp' 
!File '1_my_error_6.lp'
Number of errors [question(other,("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),variable(("at",constant("obj21"),constant("apt2"))),false,6)] 1
I need some diagnoses at 6 Step!
I need some diagnoses at Step 6!
This needs to be resolved [question(self,("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),variable(("in",constant("obj23"),constant("apn1"))),false,6),question(other,("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),variable(("at",constant("obj21"),constant("apt2"))),false,6)] at Step 6!
Add some information for myself "load-airplane",constant("obj23"),constant("apn1"),constant("apt2") variable(("in",constant("obj23"),constant("apn1"))) false 6 
I need to look for answer for this "load-airplane",constant("obj21"),constant("apn1"),constant("apt2") variable(("at",constant("obj21"),constant("apt2"))) false 6 
I did not find answer for this "load-airplane",constant("obj21"),constant("apn1"),constant("apt2") variable(("at",constant("obj21"),constant("apt2"))) false 6 
I already asked these people [] for answer!
Need to ask for help from neighbor [2,3] 
I send a message to 2 for question(variable(("at",constant("obj21"),constant("apt2"))), false, 6) !
I send a message to 3 for question(variable(("at",constant("obj21"),constant("apt2"))), false, 6) !
Done initializing the diagnosis at step 6 ======================= 
Receiving ... From 0  Type 2 Content next(7) --- 
Step sending action 7
Done sending action occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),7) at step 7 ======================= 
Receiving ... From 2  Type 6 Content answer(variable(("at",constant("obj21"),constant("apt2"))),false,6,[no]) --- 
Receiving answer(variable(("at",constant("obj21"),constant("apt2"))), false, 6) is [no] from 2! 
List of people who answered with no to the question(variable(("at",constant("obj21"),constant("apt2"))),false,6) is [2] 
Print a list 
: #elements is 2 
 [2,3] 
Receiving ... From 0  Type 10 Content next(7) --- 
Step processing observations 7
Agent computing next step and diagnosis 7 
In agent 1 solving step 7 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_occ_7.lp'  '1_enext_6.lp' ' -c t=7'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_7.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_next_6.lp'  '1_next_7.lp' '1_occ_7.lp'  '1_enext_7.lp' ' -c t=8' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_7.lp' 
!File '1_my_error_7.lp'
Number of errors [] 0
I do not need diagnoses at Step 7!
Done initializing the diagnosis at step 7 ======================= 
Receiving ... From 0  Type 2 Content next(8) --- 
Step sending action 8
Done sending action occurs(constant("apn1"),("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),8) at step 8 ======================= 
Receiving ... From 3  Type 6 Content answer(variable(("at",constant("obj21"),constant("apt2"))),false,6,[[self],[(3,6)]]) --- 
Receiving answer(variable(("at",constant("obj21"),constant("apt2"))), false, 6) is [[self],[(3,6)]] from 3! 
In propagation with answer [[self],[(3,6)]]  
Notify [2] for the question(variable(("at",constant("obj21"),constant("apt2"))), false, 6) with answer [[self],[(3,6)]] 
Receiving ... From 0  Type 10 Content next(8) --- 
Step processing observations 8
Agent computing next step and diagnosis 8 
In agent 1 solving step 8 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_occ_8.lp'  '1_enext_7.lp' ' -c t=8'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_8.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_next_7.lp'  '1_next_8.lp' '1_occ_8.lp'  '1_enext_8.lp' ' -c t=9' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_8.lp' 
!File '1_my_error_8.lp'
Number of errors [question(other,("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),variable(("in",constant("obj21"),constant("apn1"))),false,8)] 1
I need some diagnoses at 8 Step!
I need some diagnoses at Step 8!
This needs to be resolved [question(other,("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),variable(("in",constant("obj21"),constant("apn1"))),false,8)] at Step 8!
I need to look for answer for this "unload-airplane",constant("obj21"),constant("apn1"),constant("apt1") variable(("in",constant("obj21"),constant("apn1"))) false 8 
I found an answer for this "unload-airplane",constant("obj21"),constant("apn1"),constant("apt1") variable(("in",constant("obj21"),constant("apn1"))) false 8 
Done initializing the diagnosis at step 8 ======================= 
Receiving ... From 0  Type 2 Content next(9) --- 
Step sending action 9
Done sending action occurs(constant("apn1"),("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),9) at step 9 ======================= 
Receiving ... From 0  Type 10 Content next(9) --- 
Step processing observations 9
Agent computing next step and diagnosis 9 
In agent 1 solving step 9 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_occ_9.lp'  '1_enext_8.lp' ' -c t=9'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_9.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_next_8.lp'  '1_next_9.lp' '1_occ_9.lp'  '1_enext_9.lp' ' -c t=10' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_9.lp' 
!File '1_my_error_9.lp'
Number of errors [question(other,("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),variable(("in",constant("obj23"),constant("apn1"))),false,9)] 1
I need some diagnoses at 9 Step!
I need some diagnoses at Step 9!
This needs to be resolved [question(self,("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),variable(("at",constant("obj21"),constant("apt1"))),false,9),question(other,("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),variable(("in",constant("obj23"),constant("apn1"))),false,9)] at Step 9!
Add some information for myself "unload-airplane",constant("obj21"),constant("apn1"),constant("apt1") variable(("at",constant("obj21"),constant("apt1"))) false 9 
I need to look for answer for this "unload-airplane",constant("obj23"),constant("apn1"),constant("apt1") variable(("in",constant("obj23"),constant("apn1"))) false 9 
I found an answer for this "unload-airplane",constant("obj23"),constant("apn1"),constant("apt1") variable(("in",constant("obj23"),constant("apn1"))) false 9 
Done initializing the diagnosis at step 9 ======================= 
Receiving ... From 0  Type 2 Content next(10) --- 
Step sending action 10
Done sending action occurs(constant("apn1"),("load-airplane",constant("obj13"),constant("apn1"),constant("apt1")),10) at step 10 ======================= 
Receiving ... From 2  Type 7 Content question(variable(("at",constant("obj21"),constant("apt1"))),false,9) --- 
Receiving question(variable(("at",constant("obj21"),constant("apt1"))), false, 9) from 2! 
Current answers I stored: [answers(question(2,no,variable(("at",constant("obj23"),constant("apt2"))),false,5),[no]),answers(question(3,no,variable(("at",constant("obj23"),constant("apt2"))),false,5),[[self],[(3,5)]]),answers(question(self,("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),variable(("in",constant("obj23"),constant("apn1"))),false,6),[self]),answers(question(self,("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),variable(("in",constant("obj23"),constant("apn1"))),false,6),[(1,6)]),answers(question(2,no,variable(("at",constant("obj21"),constant("apt2"))),false,6),[no]),answers(question(self,("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),variable(("in",constant("obj21"),constant("apn1"))),false,7),[self]),answers(question(3,no,variable(("at",constant("obj21"),constant("apt2"))),false,6),[[self],[(3,6)]]),answers(question(self,("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),variable(("in",constant("obj21"),constant("apn1"))),false,8),[(("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),variable(("in",constant("obj21"),constant("apn1"))),false,7)]),answers(question(self,("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),variable(("at",constant("obj21"),constant("apt1"))),false,9),[self]),answers(question(self,("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),variable(("at",constant("obj21"),constant("apt1"))),false,9),[(1,9)]),answers(question(self,("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),variable(("in",constant("obj23"),constant("apn1"))),false,9),[(("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),variable(("in",constant("obj23"),constant("apn1"))),false,6)])]
==> in finding answers 
Reasons for variable(("at",constant("obj21"),constant("apt1"))), false, 9 is [[self],[(1,9)]] 
Find answer question(variable(("at",constant("obj21"),constant("apt1"))), false, 9) from 2! 
Sending 2 the answer  [[self],[(1,9)]]! 
Receiving ... From 0  Type 10 Content next(10) --- 
Step processing observations 10
Agent computing next step and diagnosis 10 
In agent 1 solving step 10 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_occ_10.lp'  '1_enext_9.lp' ' -c t=10'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_10.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_next_9.lp'  '1_next_10.lp' '1_occ_10.lp'  '1_enext_10.lp' ' -c t=11' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_10.lp' 
!File '1_my_error_10.lp'
Number of errors [] 0
I do not need diagnoses at Step 10!
Done initializing the diagnosis at step 10 ======================= 
Receiving ... From 0  Type 2 Content next(11) --- 
Step sending action 11
Done sending action occurs(constant("apn1"),("unload-airplane",constant("obj13"),constant("apn1"),constant("apt1")),11) at step 11 ======================= 
Receiving ... From 2  Type 7 Content question(variable(("at",constant("obj23"),constant("apt1"))),false,10) --- 
Receiving question(variable(("at",constant("obj23"),constant("apt1"))), false, 10) from 2! 
Current answers I stored: [answers(question(2,no,variable(("at",constant("obj23"),constant("apt2"))),false,5),[no]),answers(question(3,no,variable(("at",constant("obj23"),constant("apt2"))),false,5),[[self],[(3,5)]]),answers(question(self,("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),variable(("in",constant("obj23"),constant("apn1"))),false,6),[self]),answers(question(self,("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),variable(("in",constant("obj23"),constant("apn1"))),false,6),[(1,6)]),answers(question(2,no,variable(("at",constant("obj21"),constant("apt2"))),false,6),[no]),answers(question(self,("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),variable(("in",constant("obj21"),constant("apn1"))),false,7),[self]),answers(question(3,no,variable(("at",constant("obj21"),constant("apt2"))),false,6),[[self],[(3,6)]]),answers(question(self,("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),variable(("in",constant("obj21"),constant("apn1"))),false,8),[(("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),variable(("in",constant("obj21"),constant("apn1"))),false,7)]),answers(question(self,("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),variable(("at",constant("obj21"),constant("apt1"))),false,9),[self]),answers(question(self,("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),variable(("at",constant("obj21"),constant("apt1"))),false,9),[(1,9)]),answers(question(self,("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),variable(("in",constant("obj23"),constant("apn1"))),false,9),[(("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),variable(("in",constant("obj23"),constant("apn1"))),false,6)]),answers(question(self,("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),variable(("at",constant("obj23"),constant("apt1"))),false,10),[self])]
==> in finding answers 
Reasons for variable(("at",constant("obj23"),constant("apt1"))), false, 10 is [[self]] 
Find answer question(variable(("at",constant("obj23"),constant("apt1"))), false, 10) from 2! 
Sending 2 the answer  [[self]]! 
Receiving ... From 0  Type 10 Content next(11) --- 
Step processing observations 11
Agent computing next step and diagnosis 11 
In agent 1 solving step 11 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_occ_11.lp'  '1_enext_10.lp' ' -c t=11'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_11.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_next_10.lp'  '1_next_11.lp' '1_occ_11.lp'  '1_enext_11.lp' ' -c t=12' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_11.lp' 
!File '1_my_error_11.lp'
Number of errors [] 0
I do not need diagnoses at Step 11!
Done initializing the diagnosis at step 11 ======================= 
Receiving ... From 0  Type 2 Content next(12) --- 
Step sending action 12
Done sending action occurs(constant("apn1"),("load-airplane",constant("obj13"),constant("apn1"),constant("apt1")),12) at step 12 ======================= 
Receiving ... From 0  Type 10 Content next(12) --- 
Step processing observations 12
Agent computing next step and diagnosis 12 
In agent 1 solving step 12 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_occ_12.lp'  '1_enext_11.lp' ' -c t=12'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_12.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_next_11.lp'  '1_next_12.lp' '1_occ_12.lp'  '1_enext_12.lp' ' -c t=13' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_12.lp' 
!File '1_my_error_12.lp'
Number of errors [] 0
I do not need diagnoses at Step 12!
Done initializing the diagnosis at step 12 ======================= 
Receiving ... From 0  Type 2 Content next(13) --- 
Step sending action 13
Done sending action occurs(constant("apn1"),("unload-airplane",constant("obj13"),constant("apn1"),constant("apt1")),13) at step 13 ======================= 
Receiving ... From 0  Type 10 Content next(13) --- 
Step processing observations 13
Agent computing next step and diagnosis 13 
In agent 1 solving step 13 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_occ_13.lp'  '1_enext_12.lp' ' -c t=13'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_enext_13.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_apn1.lp" '1_agent.lp' '1_next_12.lp'  '1_next_13.lp' '1_occ_13.lp'  '1_enext_13.lp' ' -c t=14' --outf=0 -V0 --out-atomf=%s. | head -n1  > '1_my_error_13.lp' 
!File '1_my_error_13.lp'
Number of errors [] 0
I do not need diagnoses at Step 13!
Done initializing the diagnosis at step 13 ======================= 
Receiving ... From 0  Type 2 Content next(14) --- 
Step sending action 14
Done with executing all actions 14 ======================= 
Receiving ... From 0  Type 2 Content done --- 
All done 
Out from agent loop 
Done executing 
List of answers 
 [answers(question(2,no,variable(("at",constant("obj23"),constant("apt2"))),false,5),[no]),answers(question(3,no,variable(("at",constant("obj23"),constant("apt2"))),false,5),[[self],[(3,5)]]),answers(question(self,("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),variable(("in",constant("obj23"),constant("apn1"))),false,6),[self]),answers(question(self,("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),variable(("in",constant("obj23"),constant("apn1"))),false,6),[(1,6)]),answers(question(2,no,variable(("at",constant("obj21"),constant("apt2"))),false,6),[no]),answers(question(self,("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),variable(("in",constant("obj21"),constant("apn1"))),false,7),[self]),answers(question(3,no,variable(("at",constant("obj21"),constant("apt2"))),false,6),[[self],[(3,6)]]),answers(question(self,("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),variable(("in",constant("obj21"),constant("apn1"))),false,8),[(("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),variable(("in",constant("obj21"),constant("apn1"))),false,7)]),answers(question(self,("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),variable(("at",constant("obj21"),constant("apt1"))),false,9),[self]),answers(question(self,("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),variable(("at",constant("obj21"),constant("apt1"))),false,9),[(1,9)]),answers(question(self,("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),variable(("in",constant("obj23"),constant("apn1"))),false,9),[(("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),variable(("in",constant("obj23"),constant("apn1"))),false,6)]),answers(question(self,("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),variable(("at",constant("obj23"),constant("apt1"))),false,10),[self])] 

List of questions that I sent and received 
 [(1,2,variable(("at",constant("obj23"),constant("apt2"))),false,5),(1,3,variable(("at",constant("obj23"),constant("apt2"))),false,5),(1,2,variable(("at",constant("obj21"),constant("apt2"))),false,6),(1,3,variable(("at",constant("obj21"),constant("apt2"))),false,6),(0,2,variable(("at",constant("obj21"),constant("apt1"))),false,9),(0,2,variable(("at",constant("obj23"),constant("apt1"))),false,10)] 

