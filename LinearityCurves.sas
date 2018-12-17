libname logistic 'C:\Users\bryso\OneDrive - North Carolina State University\Logistic Regression\HW2';

%let cat = DIRDEP BRANCH ASSNUM DEBTNUM;

%let cont = ACCTAGE DEPAMT ATMAMT CRSCORE FREQ DEBTAMT;


proc logistic data=logistic.train1 plots(only label)=(dpc dfbetas phat);
	class &cat / param=ref ref = FIRST;
	model INS(event='1') = DEBTAMT &cat;
	output out=diags xbeta=eta stdxbeta=etasd p=p;
run;

/* checking linearity again */
/* partial residual plot with loess */
data diags2;
set diags;
ppart = (INS-p)/(p*(1-p));

rDEBTAMT = DEBTAMT *3.61e-7+ ppart;
run;
proc sgplot data=diags2;
scatter x=DEBTAMT y=rDEBTAMT;
loess x=DEBTAMT y=rDEBTAMT;
run;

ods graphics / loessmaxobs=10000;

ACCTAGE  0.00452 
DEPAMT   0.000033 
ATMAMT   0.000047 
CRSCORE  -0.00031 
FREQ   -0.0309 
DEBTAMT   3.61E-7;

proc logistic data=logistic.train1 plots(only label)=(dpc dfbetas phat);
	class &cat / param=ref ref = FIRST;
	model INS(event='1') = ACCTAGE &cat;
	output out=diags xbeta=eta stdxbeta=etasd p=p;
run;

/* checking linearity again */
/* partial residual plot with loess */
data diags2;
set diags;
ppart = (INS-p)/(p*(1-p));

rACCTAGE = ACCTAGE *.00452+ ppart;
run;
proc sgplot data=diags2;
scatter x=ACCTAGE y=rACCTAGE;
loess x=ACCTAGE y=rACCTAGE;
run;




proc logistic data=logistic.train1 plots(only label)=(dpc dfbetas phat);
	class &cat / param=ref ref = FIRST;
	model INS(event='1') = DEPAMT;
	output out=diags xbeta=eta stdxbeta=etasd p=p;
run;

/* checking linearity again */
/* partial residual plot with loess */
data diags2;
set diags;
ppart = (INS-p)/(p*(1-p));

rDEPAMT = DEPAMT*.000033+ ppart;
run;
proc sgplot data=diags2;
scatter x=DEPAMT y=rDEPAMT;
loess x=DEPAMT y=rDEPAMT;
run;



proc logistic data=logistic.train1 plots(only label)=(dpc dfbetas phat);
	class &cat / param=ref ref = FIRST;
	model INS(event='1') = CRSCORE &cat;
	output out=diags xbeta=eta stdxbeta=etasd p=p;
run;

/* checking linearity again */
/* partial residual plot with loess */
data diags2;
set diags;
ppart = (INS-p)/(p*(1-p));

rCRSCORE = CRSCORE *-.00031+ ppart;
run;
proc sgplot data=diags2;
scatter x=CRSCORE y=rCRSCORE;
loess x=CRSCORE y=rCRSCORE;
run;
