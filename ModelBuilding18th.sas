libname project 'C:\Users\bryso\OneDrive - North Carolina State University\HWTeam (Fall 1)\LR Project';

proc surveyselect data=project.construction 
  method=srs rate=0.2 out=project.validation outall;
  run;

  data project.train project.vali;
   set project.validation;
   if win_bid = 'Yes' then win = 1;
   Else win = 0;
   if Selected=1 then output project.vali;
   else output project.train;
   drop win_bid;
	run;

proc contents data = project.construction;
run;

proc logistic data=project.train;
	class Sector Competitor_A Competitor_B Competitor_C Competitor_D 
				Competitor_E Competitor_F Competitor_G Competitor_H Competitor_I Competitor_J /param=ref;
	model win(event='1') = Bid_Price__Millions_ Competitor_A Competitor_B Competitor_C Competitor_D 
								Competitor_E Competitor_F Competitor_G Competitor_H Competitor_I 
								Competitor_J Cost_After_Engineering_Estimate_ Estimated_Cost__Millions_
								Estimated_Years_to_Complete Number_of_Competitor_Bids 
								Sector Winning_Bid_Price__Millions_ / rsq;
	title 'generalized r2';
run;

proc freq data=project.train;
run;

proc logistic data=project.train;
	class Sector/param=ref;
	model win(event='1') = Bid_Price__Millions_ Cost_After_Engineering_Estimate_ Estimated_Cost__Millions_
								Estimated_Years_to_Complete Number_of_Competitor_Bids 
								Sector  / rsq;
	title 'generalized r2';
run;
