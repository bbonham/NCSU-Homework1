libname project 'C:\Users\bryso\OneDrive - North Carolina State University\HWTeam (Fall 1)\LR Project';

/*Creating 2 datasets*/

proc surveyselect data=project.construction 
  method=srs rate=0.2 out=project.validation outall;
  run;

/*Subsets for win = 1*/

  data project.train project.vali;
   set project.validation;
   if win_bid = 'Yes' then win = 1;
   Else win = 0;
   if Selected=1 then output project.vali;
   else output project.train;
   drop win_bid;
	run;

/*Contents of the data file*/

proc contents data = project.construction;
run;

/*Creates easier to read Numb var from Number of Competitor Bids*/

data project.trainnumbid;
	set project.train;
	Numb = Number_of_Competitor_Bids;
run;

/*Model Building without Interaction*/

proc logistic data=project.trainnumbid;
	class Sector Region_of_Country
		Competitor_A Competitor_B Competitor_C Competitor_D 
		Competitor_E Competitor_F Competitor_G Competitor_H Competitor_I Competitor_J /param=ref;
	model win(event='1') = Competitor_A Competitor_B Competitor_C Competitor_D 
								Competitor_E Competitor_F Competitor_G Competitor_H Competitor_I Competitor_J

								Region_of_Country
								Number_of_Competitor_Bids 
								Sector 

/ rsq firth;
	title 'generalized r2';
run;

/*Discovering interaction with Region of Country and Competitor F*/

proc freq data=project.trainnumbid;
    tables Region_of_Country*Competitor_F*win / all BDT plots(only)=oddsratioplot(stats);
	    title1 'Region of Country and Competitor F interaction';
run;

/*Model Building with Interaction*/

proc logistic data=project.trainnumbid;
	class Sector Region_of_Country
		Competitor_A Competitor_B Competitor_C Competitor_D 
		Competitor_E Competitor_F Competitor_G Competitor_H Competitor_I Competitor_J /param=ref;
	model win(event='1') = Competitor_A Competitor_B Competitor_C Competitor_D 
								Competitor_E Competitor_F Competitor_G Competitor_H Competitor_I Competitor_J
								Number_of_Competitor_Bids 
								Region_of_Country
								Sector 
Sector*Numb
Region_of_Country*Numb
Competitor_F*Region_of_Country
/*Competitor_A*Numb Competitor_B*Numb */
/*Competitor_C*numb */
/*Competitor_D*numb*/
/*Competitor_E*Numb Competitor_F*Numb Competitor_G*Numb Competitor_H*Numb */
/*Competitor_I*Numb Competitor_J*Numb*/
/ rsq firth;
	title 'generalized r2';
run;

/*Model with interaction removed since number of competitor is not known in advanced!*/

proc logistic data=project.train;
	class Sector Region_of_Country
		Competitor_A Competitor_B Competitor_C Competitor_D 
		Competitor_E Competitor_F Competitor_G Competitor_H Competitor_I Competitor_J /param=ref;
	model win(event='1') = Competitor_A Competitor_B Competitor_C Competitor_D 
								Competitor_E Competitor_F Competitor_G Competitor_H Competitor_I Competitor_J

								Region_of_Country
								Sector 
Competitor_F*Region_of_Country
/ rsq firth;
	title 'generalized r2';
run;
