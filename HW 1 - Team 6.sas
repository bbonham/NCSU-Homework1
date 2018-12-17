libname HWlog 'C:\Users\ehhester\Documents\MSA\Fall\Logistic Regression\HW 1 Data';

/******************************************/
/*Checking distribution and missing values*/
/******************************************/

/* Distribution */
proc univariate data=HWlog.insurance_t noprint;
	histogram;
run;

/*Missing values*/
proc means data=HWlog.insurance_t nmiss;
run;

/******************************************/
/*Checking for interaction and confounding*/
/******************************************/

/* checking interaction -- just add in interaction term */
proc logistic data=HWLog.Insurance_t;
    model Ins(event='1') = sav mm / CLODDS = Wald;
    title 'checking interaction';
    
run;
/* checking for confounding -- compare estimate of "ins" with and without "money market" */
proc logistic data=HWlog.insurance_t;
    model INS(event='1') = sav / plcl plrl;
    title 'confounding1';
run;
proc logistic data=HWlog.insurance_t;
    model INS(event='1') = sav mm / plcl plrl;
    title 'confounding2';
run;
/*freq table for commmon odds*/
proc freq data=HWlog.insurance_t nlevels;
   tables sav*INS / all cl plots(only)=freqplot 
                              (type=barchart scale=percent orient=vertical twoway=stacked);
   tables mm*sav*ins / all bdt cl plots(only)=oddsratioplot(stats);
   /* - confounder goes first in "tables" statement!
   - "bdt" = Breslow-Day-Tarone test
   - for small samples, "exact zelen" will give zelen's exact tests
   - or use mc option as shown above */
   title 'stratified analysis for interactions and confounding';
run;

/******************************************/
/************Model Selection***************/
/******************************************/

%let intervallines =  	DDABAL | DEP | DEPAMT | CHECKS | NSFAMT | 
						TELLER | SAVBAL | ATMAMT | CDBAL | IRABAL | 
						LOCBAL | ILSBAL | MMBAL | MTGBAL ;
%let interval =  		DDABAL DEP DEPAMT CHECKS NSFAMT 
						TELLER SAVBAL ATMAMT CDBAL IRABAL 
						LOCBAL ILSBAL MMBAL MTGBAL;
%let cat = 		DDA DIRDEP NSF SAV ATM 
				CD IRA LOC ILS MM 
				SDB BRANCH RES;
%let catlines = DDA | DIRDEP | NSF | SAV | ATM | 
				CD | IRA | LOC | ILS | MM | 
				SDB| BRANCH | RES;

*No interaction with no missing values - MODEL WE CHOSE;
proc logistic data=HWlog.insurance_t plots(only)=(effect oddsratio);         
	class DDA(ref='0') DIRDEP(ref='0') NSF(ref='0') SAV(ref='0') ATM(ref='0') CD(ref='0') 
		IRA(ref='0') LOC(ref='0') ILS(ref='0') MM(ref='0') BRANCH(ref='B9') RES(ref='U')/param=ref;
	model INS(event='1')= &intervallines | &catlines @1
		/ selection=stepwise clodds=pl slstay=0.05; 
	title 'No interaction with no missing values';
run;

*No interaction with missing values below 10% - acctage and crscore;
proc logistic data=HWlog.insurance_t plots(only)=(effect oddsratio);         
	class DDA(ref='0') DIRDEP(ref='0') NSF(ref='0') SAV(ref='0') ATM(ref='0') CD(ref='0') 
		IRA(ref='0') LOC(ref='0') ILS(ref='0') MM(ref='0') BRANCH(ref='B9') RES(ref='U')/param=ref;
	model INS(event='1')= &intervallines | &catlines | ACCTAGE | CRSCORE @1
		/ selection=stepwise clodds=pl slstay=0.05; 
	title 'No interaction with missing values below 10% - acctage and crscore';
run;
*Interaction with no missing values - BEST % CONCORDANCE; 
proc logistic data=HWlog.insurance_t plots(only)=(effect oddsratio);         
	class DDA(ref='0') DIRDEP(ref='0') NSF(ref='0') SAV(ref='0') ATM(ref='0') CD(ref='0') 
		IRA(ref='0') LOC(ref='0') ILS(ref='0') MM(ref='0') BRANCH(ref='B9') RES(ref='U')/ param=ref;
	model INS(event='1')= &intervallines | &catlines @2
	/ selection=stepwise clodds=pl slstay=0.05;
	title 'Interaction with no missing values';
run;
*Interaction with missing values below 10% - acctage and crscore; 
proc logistic data=HWlog.insurance_t plots(only)=(effect oddsratio);         
	class DDA(ref='0') DIRDEP(ref='0') NSF(ref='0') SAV(ref='0') ATM(ref='0') CD(ref='0') 
		IRA(ref='0') LOC(ref='0') ILS(ref='0') MM(ref='0') BRANCH(ref='B9') RES(ref='U')/ param=ref;
	model INS(event='1')= &intervallines | &catlines | ACCTAGE | CRSCORE @2
	/ selection=stepwise clodds=pl slstay=0.05;
	title 'Interaction with missing values below 10% - acctage and crscore';
run;

proc freq data=HWlog.insurance_t;
tables res branch;
run;
