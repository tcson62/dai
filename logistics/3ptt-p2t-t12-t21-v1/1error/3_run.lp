TIPC Linda server listening at: port_id('<0.0.0:1405336423>')
Number of steps 7
 Domain "domain.lp"
 Problem "prob1_tru2.lp"
Done starting 
In preparation 
*********** 
 Computing initial state 
 **************
 Executing: clingo ../../../lps/compute_0.lp  "domain.lp" "prob1_tru2.lp" --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_next_0.lp' 
!Setting up expected initial state
 clingo '3_next_0.lp' -c t=0 --outf=0 -V0 --out-atomf=%s. | head -n1 >  '3_enext_0.lp' 
*********** 
 Computing the fluents 
 **************
 In the loop ... 
Receiving ... From 0  Type 2 Content next(1) --- 
Step sending action 1
Done sending action occurs(constant("tru2"),("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),1) at step 1 ======================= 
Receiving ... From 0  Type 10 Content next(1) --- 
Step processing observations 1
Agent computing next step and diagnosis 1 
In agent 3 solving step 1 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_occ_1.lp'  '3_enext_0.lp' ' -c t=1'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_1.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_next_0.lp'  '3_next_1.lp' '3_occ_1.lp'  '3_enext_1.lp' ' -c t=2' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_1.lp' 
!File '3_my_error_1.lp'
Number of errors [] 0
I do not need diagnoses at Step 1!
Done initializing the diagnosis at step 1 ======================= 
Receiving ... From 0  Type 2 Content next(2) --- 
Step sending action 2
Done sending action occurs(constant("tru2"),("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),2) at step 2 ======================= 
Receiving ... From 0  Type 10 Content next(2) --- 
Step processing observations 2
Agent computing next step and diagnosis 2 
In agent 3 solving step 2 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_occ_2.lp'  '3_enext_1.lp' ' -c t=2'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_2.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_next_1.lp'  '3_next_2.lp' '3_occ_2.lp'  '3_enext_2.lp' ' -c t=3' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_2.lp' 
!File '3_my_error_2.lp'
Number of errors [] 0
I do not need diagnoses at Step 2!
Done initializing the diagnosis at step 2 ======================= 
Receiving ... From 0  Type 2 Content next(3) --- 
Step sending action 3
Done sending action occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),3) at step 3 ======================= 
Receiving ... From 0  Type 10 Content next(3) --- 
Step processing observations 3
Agent computing next step and diagnosis 3 
In agent 3 solving step 3 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_occ_3.lp'  '3_enext_2.lp' ' -c t=3'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_3.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_next_2.lp'  '3_next_3.lp' '3_occ_3.lp'  '3_enext_3.lp' ' -c t=4' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_3.lp' 
!File '3_my_error_3.lp'
Number of errors [] 0
I do not need diagnoses at Step 3!
Done initializing the diagnosis at step 3 ======================= 
Receiving ... From 0  Type 2 Content next(4) --- 
Step sending action 4
Done sending action occurs(constant("tru2"),("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),4) at step 4 ======================= 
Receiving ... From 0  Type 10 Content next(4) --- 
Step processing observations 4
Agent computing next step and diagnosis 4 
In agent 3 solving step 4 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_occ_4.lp'  '3_enext_3.lp' ' -c t=4'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_4.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_next_3.lp'  '3_next_4.lp' '3_occ_4.lp'  '3_enext_4.lp' ' -c t=5' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_4.lp' 
!File '3_my_error_4.lp'
Number of errors [question(other,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("at",constant("tru2"),constant("apt2"))),false,4),question(other,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("in",constant("obj23"),constant("tru2"))),false,4)] 2
I need some diagnoses at 4 Step!
I need some diagnoses at Step 4!
This needs to be resolved [question(other,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("at",constant("tru2"),constant("apt2"))),false,4),question(other,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("in",constant("obj23"),constant("tru2"))),false,4)] at Step 4!
I need to look for answer for this "unload-truck",constant("obj23"),constant("tru2"),constant("apt2") variable(("at",constant("tru2"),constant("apt2"))) false 4 
I found an answer for this "unload-truck",constant("obj23"),constant("tru2"),constant("apt2") variable(("at",constant("tru2"),constant("apt2"))) false 4 
I need to look for answer for this "unload-truck",constant("obj23"),constant("tru2"),constant("apt2") variable(("in",constant("obj23"),constant("tru2"))) false 4 
I found an answer for this "unload-truck",constant("obj23"),constant("tru2"),constant("apt2") variable(("in",constant("obj23"),constant("tru2"))) false 4 
Done initializing the diagnosis at step 4 ======================= 
Receiving ... From 0  Type 2 Content next(5) --- 
Step sending action 5
Done sending action occurs(constant("tru2"),("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),5) at step 5 ======================= 
Receiving ... From 0  Type 10 Content next(5) --- 
Step processing observations 5
Agent computing next step and diagnosis 5 
In agent 3 solving step 5 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_occ_5.lp'  '3_enext_4.lp' ' -c t=5'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_5.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_next_4.lp'  '3_next_5.lp' '3_occ_5.lp'  '3_enext_5.lp' ' -c t=6' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_5.lp' 
!File '3_my_error_5.lp'
Number of errors [question(other,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("at",constant("tru2"),constant("apt2"))),false,5),question(other,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("in",constant("obj21"),constant("tru2"))),false,5)] 2
I need some diagnoses at 5 Step!
I need some diagnoses at Step 5!
This needs to be resolved [question(self,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("at",constant("obj23"),constant("apt2"))),false,5),question(other,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("at",constant("tru2"),constant("apt2"))),false,5),question(other,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("in",constant("obj21"),constant("tru2"))),false,5)] at Step 5!
Add some information for myself "unload-truck",constant("obj23"),constant("tru2"),constant("apt2") variable(("at",constant("obj23"),constant("apt2"))) false 5 
I need to look for answer for this "unload-truck",constant("obj21"),constant("tru2"),constant("apt2") variable(("at",constant("tru2"),constant("apt2"))) false 5 
I found an answer for this "unload-truck",constant("obj21"),constant("tru2"),constant("apt2") variable(("at",constant("tru2"),constant("apt2"))) false 5 
I need to look for answer for this "unload-truck",constant("obj21"),constant("tru2"),constant("apt2") variable(("in",constant("obj21"),constant("tru2"))) false 5 
I found an answer for this "unload-truck",constant("obj21"),constant("tru2"),constant("apt2") variable(("in",constant("obj21"),constant("tru2"))) false 5 
Done initializing the diagnosis at step 5 ======================= 
Receiving ... From 0  Type 2 Content next(6) --- 
Step sending action 6
Done sending action occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),6) at step 6 ======================= 
Receiving ... From 1  Type 7 Content question(variable(("at",constant("obj23"),constant("apt2"))),false,5) --- 
Receiving question(variable(("at",constant("obj23"),constant("apt2"))), false, 5) from 1! 
Current answers I stored: [answers(question(self,("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),variable(("in",constant("obj23"),constant("tru2"))),false,1),[self]),answers(question(self,("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),variable(("at",constant("obj23"),constant("pos2"))),true,1),[self]),answers(question(self,("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),variable(("in",constant("obj21"),constant("tru2"))),false,2),[self]),answers(question(self,("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),variable(("at",constant("obj21"),constant("pos2"))),true,2),[self]),answers(question(self,("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,3),[self]),answers(question(self,("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("pos2"))),true,3),[self]),answers(question(self,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("at",constant("tru2"),constant("apt2"))),false,4),[(("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,3)]),answers(question(self,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("in",constant("obj23"),constant("tru2"))),false,4),[(("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),variable(("in",constant("obj23"),constant("tru2"))),false,1)]),answers(question(self,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("at",constant("obj23"),constant("apt2"))),false,5),[self]),answers(question(self,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("at",constant("obj23"),constant("apt2"))),false,5),[(3,5)]),answers(question(self,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("at",constant("tru2"),constant("apt2"))),false,5),[(("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,3)]),answers(question(self,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("in",constant("obj21"),constant("tru2"))),false,5),[(("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),variable(("in",constant("obj21"),constant("tru2"))),false,2)])]
==> in finding answers 
Reasons for variable(("at",constant("obj23"),constant("apt2"))), false, 5 is [[self],[(3,5)]] 
Find answer question(variable(("at",constant("obj23"),constant("apt2"))), false, 5) from 1! 
Sending 1 the answer  [[self],[(3,5)]]! 
Receiving ... From 0  Type 10 Content next(6) --- 
Step processing observations 6
Agent computing next step and diagnosis 6 
In agent 3 solving step 6 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_occ_6.lp'  '3_enext_5.lp' ' -c t=6'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_6.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_next_5.lp'  '3_next_6.lp' '3_occ_6.lp'  '3_enext_6.lp' ' -c t=7' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_6.lp' 
!File '3_my_error_6.lp'
Number of errors [question(other,("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,6)] 1
I need some diagnoses at 6 Step!
I need some diagnoses at Step 6!
This needs to be resolved [question(self,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("at",constant("obj21"),constant("apt2"))),false,6),question(other,("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,6)] at Step 6!
Add some information for myself "unload-truck",constant("obj21"),constant("tru2"),constant("apt2") variable(("at",constant("obj21"),constant("apt2"))) false 6 
I need to look for answer for this "drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2") variable(("at",constant("tru2"),constant("apt2"))) false 6 
I found an answer for this "drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2") variable(("at",constant("tru2"),constant("apt2"))) false 6 
Done initializing the diagnosis at step 6 ======================= 
Receiving ... From 0  Type 2 Content next(7) --- 
Step sending action 7
Done sending action none at step 7 ======================= 
Receiving ... From 1  Type 7 Content question(variable(("at",constant("obj21"),constant("apt2"))),false,6) --- 
Receiving question(variable(("at",constant("obj21"),constant("apt2"))), false, 6) from 1! 
Current answers I stored: [answers(question(self,("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),variable(("in",constant("obj23"),constant("tru2"))),false,1),[self]),answers(question(self,("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),variable(("at",constant("obj23"),constant("pos2"))),true,1),[self]),answers(question(self,("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),variable(("in",constant("obj21"),constant("tru2"))),false,2),[self]),answers(question(self,("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),variable(("at",constant("obj21"),constant("pos2"))),true,2),[self]),answers(question(self,("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,3),[self]),answers(question(self,("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("pos2"))),true,3),[self]),answers(question(self,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("at",constant("tru2"),constant("apt2"))),false,4),[(("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,3)]),answers(question(self,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("in",constant("obj23"),constant("tru2"))),false,4),[(("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),variable(("in",constant("obj23"),constant("tru2"))),false,1)]),answers(question(self,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("at",constant("obj23"),constant("apt2"))),false,5),[self]),answers(question(self,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("at",constant("obj23"),constant("apt2"))),false,5),[(3,5)]),answers(question(self,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("at",constant("tru2"),constant("apt2"))),false,5),[(("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,3)]),answers(question(self,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("in",constant("obj21"),constant("tru2"))),false,5),[(("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),variable(("in",constant("obj21"),constant("tru2"))),false,2)]),answers(question(self,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("at",constant("obj21"),constant("apt2"))),false,6),[self]),answers(question(self,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("at",constant("obj21"),constant("apt2"))),false,6),[(3,6)]),answers(question(self,("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,6),[(("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,3)])]
==> in finding answers 
Reasons for variable(("at",constant("obj21"),constant("apt2"))), false, 6 is [[self],[(3,6)]] 
Find answer question(variable(("at",constant("obj21"),constant("apt2"))), false, 6) from 1! 
Sending 1 the answer  [[self],[(3,6)]]! 
Receiving ... From 0  Type 10 Content next(7) --- 
Step processing observations 7
Agent computing next step and diagnosis 7 
In agent 3 solving step 7 ...
 Executing: clingo ../../../lps/compute_expected_agent.lp "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_occ_7.lp'  '3_enext_6.lp' ' -c t=7'  --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_enext_7.lp' 
!Executing: clingo ../../../lps/diagnosis.lp  
  	          "domain.lp" "prob1_tru2.lp" '3_agent.lp' '3_next_6.lp'  '3_next_7.lp' '3_occ_7.lp'  '3_enext_7.lp' ' -c t=8' --outf=0 -V0 --out-atomf=%s. | head -n1  > '3_my_error_7.lp' 
!File '3_my_error_7.lp'
Number of errors [] 0
I do not need diagnoses at Step 7!
Done initializing the diagnosis at step 7 ======================= 
Receiving ... From 0  Type 2 Content next(8) --- 
Step sending action 8
Done with executing all actions 8 ======================= 
Receiving ... From 0  Type 10 Content next(8) --- 
Step processing observations 8
Done with executing all actions 8 ======================= 
Receiving ... From 0  Type 2 Content next(9) --- 
Step sending action 9
Done with executing all actions 9 ======================= 
Receiving ... From 0  Type 10 Content next(9) --- 
Step processing observations 9
Done with executing all actions 9 ======================= 
Receiving ... From 0  Type 2 Content next(10) --- 
Step sending action 10
Done with executing all actions 10 ======================= 
Receiving ... From 0  Type 10 Content next(10) --- 
Step processing observations 10
Done with executing all actions 10 ======================= 
Receiving ... From 0  Type 2 Content next(11) --- 
Step sending action 11
Done with executing all actions 11 ======================= 
Receiving ... From 0  Type 10 Content next(11) --- 
Step processing observations 11
Done with executing all actions 11 ======================= 
Receiving ... From 0  Type 2 Content next(12) --- 
Step sending action 12
Done with executing all actions 12 ======================= 
Receiving ... From 0  Type 10 Content next(12) --- 
Step processing observations 12
Done with executing all actions 12 ======================= 
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
 [answers(question(self,("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),variable(("in",constant("obj23"),constant("tru2"))),false,1),[self]),answers(question(self,("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),variable(("at",constant("obj23"),constant("pos2"))),true,1),[self]),answers(question(self,("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),variable(("in",constant("obj21"),constant("tru2"))),false,2),[self]),answers(question(self,("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),variable(("at",constant("obj21"),constant("pos2"))),true,2),[self]),answers(question(self,("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,3),[self]),answers(question(self,("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("pos2"))),true,3),[self]),answers(question(self,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("at",constant("tru2"),constant("apt2"))),false,4),[(("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,3)]),answers(question(self,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("in",constant("obj23"),constant("tru2"))),false,4),[(("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),variable(("in",constant("obj23"),constant("tru2"))),false,1)]),answers(question(self,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("at",constant("obj23"),constant("apt2"))),false,5),[self]),answers(question(self,("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),variable(("at",constant("obj23"),constant("apt2"))),false,5),[(3,5)]),answers(question(self,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("at",constant("tru2"),constant("apt2"))),false,5),[(("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,3)]),answers(question(self,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("in",constant("obj21"),constant("tru2"))),false,5),[(("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),variable(("in",constant("obj21"),constant("tru2"))),false,2)]),answers(question(self,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("at",constant("obj21"),constant("apt2"))),false,6),[self]),answers(question(self,("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),variable(("at",constant("obj21"),constant("apt2"))),false,6),[(3,6)]),answers(question(self,("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,6),[(("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),variable(("at",constant("tru2"),constant("apt2"))),false,3)])] 

List of questions that I sent and received 
 [(0,1,variable(("at",constant("obj23"),constant("apt2"))),false,5),(0,1,variable(("at",constant("obj21"),constant("apt2"))),false,6)] 

