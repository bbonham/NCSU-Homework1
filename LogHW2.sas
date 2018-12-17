libname logs 'C:\Users\bryso\OneDrive - North Carolina State University\HWTeam (Fall 1)\LR HW2';

%let cat = DIRDEP BRANCH ASSNUM DEBTNUM;

%let cont = ACCTAGE DEPAMT ATMAMT CRSCORE FREQ DEBTAMT;
%let wlog = wACCTAGE wDEPAMT wATMAMT wCRSCORE wFREQ wDEBTAMPT;

proc logistic data=logs.train1 plots(only label)=(dpc dfbetas phat);
	class &cat / param=ref ref = FIRST;
	model INS(event='1') = DEBTAMT &cat DEBTAMT*log(DEBTAMT);
	output out=diags xbeta=eta stdxbeta=etasd p=p;
run;

data logs.wlogw;
	set logistic.train1;
	wACCTAGE = ACCTAGE * log(ACCTAGE);
	wDEPAMT = DEPAMT * log(DEPAMT);
	wATMAMT = ATMAMT * log(ATMAMT);
	wCRSCORE = CRSCORE * log(CRSCORE);
	wFREQ = FREQ * log(FREQ);
	wDEBTAMPT = DEBTAMT * log(DEBTAMT);
run;


proc logistic data = logs.wlogw;
	class &cat / param=ref ref = FIRST;
	model INS(event='1') = &cont &cat &wlog / plcl plrl;
	output out=WlowOut xbeta=eta stdxbeta=etasd p=p;
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


proc logistic data=logs.train1 plots(only)=(effect oddsratio) plots(maxpoints=none);         
	class DIRDEP(ref='0') BRANCH ASSNUM(ref='0') DEBTNUM(ref='0')/param=ref ;
	model INS(event='1')= DIRDEP BRANCH ASSNUM DEBTNUM DEPAMT ATMAMT FREQ
		/ clodds=pl; 
run;

proc means data=logs.train1;
var INS;
output out=obsprop;
run;

/*BRIER SCORE*/

proc logistic data=logs.train1;
	class DIRDEP(ref='0') BRANCH ASSNUM(ref='0') DEBTNUM(ref='0')/param=ref ;
	model INS(event='1')= DIRDEP BRANCH ASSNUM DEBTNUM DEPAMT ATMAMT FREQ
		/ clodds=pl; 
	/* here i'm using the original dataset
	so these are the metrics on the training data */
	score data=logs.Validate1 fitstat;
	/* output table with brier score */
	ods output scorefitstat=fit;
	title 'brier score';
run;


data brier;
/* now pick out mean and brier score */
set obsprop(keep=INS firstobs=4 obs=4);
set fit(keep=BrierScore);
/* max = p*(1-p)^2 + (1-p)*p^2 */
briermax = (low)*(1-INS)**2 + (1-INS)*(INS**2);
brierscaled = 1 - BrierScore/briermax;
/* I have absolutely no idea how
to do the z-score for calibration here.
sorry y'all. */
run;
proc print data=brier;
/* this matches what I got in R so I assume I did all this correctly */
run;

/*YOUNDENS*/

proc logistic data=logs.train1;
	class DIRDEP(ref='0') BRANCH ASSNUM(ref='0') DEBTNUM(ref='0')/param=ref ;
	model INS(event='1')= DIRDEP BRANCH ASSNUM DEBTNUM DEPAMT ATMAMT FREQ
		/ clodds=pl ctable; 
	score data=logs.Validate1 fitstat;
	ods output classification=classtable;
	title 'classification table';
run;

/* Youden's J statistic */
data classtable;
set classtable;
/* using 100 because sas gives these in percentages */
youden = sensitivity + specificity - 1;
run;

proc sort data=classtable;
by descending youden;
run;

proc print data=classtable;
run;

proc logistic data=logs.train1 outmodel=logs.trainmodel;
	class DIRDEP(ref='0') BRANCH ASSNUM(ref='0') DEBTNUM(ref='0')/param=ref ;
	model INS(event='1')= DIRDEP BRANCH ASSNUM DEBTNUM DEPAMT ATMAMT FREQ
		/ clodds=pl; 
   score data=logs.validate1 out=Score2;
   store trainmodel;
run;

proc logistic inmodel=logs.trainmodel;
   score data=logs.validate1 out=Score3;
   ods output classification=classtable;
run;

proc print data=score3;
run;


/*ROC CURVE*/
      ods graphics on;
      proc logistic data=logs.train1;
	class DIRDEP(ref='0') BRANCH ASSNUM(ref='0') DEBTNUM(ref='0')/param=ref ;
	model INS(event='1')= DIRDEP BRANCH ASSNUM DEBTNUM DEPAMT ATMAMT FREQ
		/ clodds=pl; 
        score data=logs.validate1 out=valpred outroc=vroc;
        roc; roccontrast;
        run;

proc print data=score3;
run;

data classtable;
set classtable;
/* using 100 because sas gives these in percentages */
youden = sensitivity + specificity - 1;
run;

proc sort data=classtable;
by descending youden;
run;

proc print data=classtable;
run;


/*Classification*/

proc logistic inmodel=logs.trainmodel rocoptions(crossvalidate);
   score data=logs.validate1 out=Score3;
	/* output table */
	title 'classification table';
run;








/*YOUDEN*/

proc logistic data=logs.train1;
	class DIRDEP(ref='0') BRANCH ASSNUM(ref='0') DEBTNUM(ref='0')/param=ref ;
	model INS(event='1')= DIRDEP BRANCH ASSNUM DEBTNUM DEPAMT ATMAMT FREQ
		/ clodds=pl ctable pprob = 0 to 0.98 by 0.02; 
	score data=logs.Validate1 fitstat;
	ods output classification=classtable3;
	title 'classification table';
run;

/* Youden's J statistic */
data classtable3;
set classtable3;
/* using 100 because sas gives these in percentages */
youden = sensitivity + specificity - 1;
run;

proc sort data=classtable3;
by descending youden;
run;

proc print data=classtable3;
run;


/*Better ROC CURVE*/

proc logistic data=logs.train1;
	class DIRDEP(ref='0') BRANCH ASSNUM(ref='0') DEBTNUM(ref='0')/param=ref ;
	model INS(event='1')= DIRDEP BRANCH ASSNUM DEBTNUM DEPAMT ATMAMT FREQ
		/ clodds=pl ctable pprob = 0 to 0.98 by 0.02 ; 
	ods output classification=classtable2;
    score data=logs.validate1 out=valpred outroc=vroc fitstat out=class;
	title 'classification table';
run;


/*BRIER SCORE*/

proc logistic data=logs.train1;
	class DIRDEP(ref='0') BRANCH ASSNUM(ref='0') DEBTNUM(ref='0')/param=ref ;
	model INS(event='1')= DIRDEP BRANCH ASSNUM DEBTNUM DEPAMT ATMAMT FREQ
		/ clodds=pl; 
	/* here i'm using the original dataset
	so these are the metrics on the training data */
	score data=logs.Validate1 fitstat;
	/* output table with brier score */
	ods output scorefitstat=fit;
	title 'brier score';
run;

proc contents data=Vroc;
run;

data logs.ClassTableVal;
	set Vroc;
	Probability_Level = round(_prob_,.01);
	Specificity = 1-_1MSPEC_;
	Percent_Correct = (_POS_ + _NEG_) / 2124;
	Percent_False_Pos = _FALPOS_ / (_FALPOS_ + _POS_);
	Percent_False_Neg = _FALNEG_ / (_FALNEG_ + _NEG_);
	drop _prob_ _1MSPEC_;
run;

proc print data=logs.classtableval;
run;


proc SQL;
create table work.test as
	SELECT DISTINCT Probability_Level, _POS_, _NEG_, _FALPOS_, _FALNEG_, Percent_Correct, _SENSIT_, Specificity, Percent_False_Pos, Percent_False_Neg
	FROM logs.classtableval;
quit;
run;

proc export data=work.test
	outfile = "C:\Users\bryso\OneDrive - North Carolina State University\Logistic Regression\HW2\Temp.csv"
	dbms=csv;
run;



proc SQL;
	SELECT DISTINCT Probability_Level
	FROM work.test
quit;
run;


data logs.ClassTableVal2;
	set work.test;
	retain probability_level
	if probability_level 
