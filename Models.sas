libname project 'C:\Users\ehhester\Documents\MSA\Fall\Logistic Regression\Project';
%let interval = Cost_After_Engineering_Estimate_ Estimated_Cost__Millions_ Estimated_Years_to_Complete Number_of_Competitor_Bids Winning_Bid_Price__Millions_; *Bid_Price__Millions_ is removed - only winning bid included;
%let cat = Competitor_A Competitor_B Competitor_C Competitor_D Competitor_E Competitor_F Competitor_G Competitor_H Competitor_I Competitor_J  Sector Region_of_Country;
	
*	simple model	;
data bintrain; *23 mill to 550;
	set project.train;
	if  winning_bid_price__millions_ >= 274.9310 then winbin = 2; *from averages of winning big by win;
	else if 155.6083 > winning_bid_price__millions_ < 274.9310 then winbin = 1;
	else winbin = 0;
run;
data binvalidation;
	set project.vali;
	if  winning_bid_price__millions_ >= 274.9310 then winbin = 2; *from averages of winning big by win;
	else if 155.6083 > winning_bid_price__millions_ < 274.9310 then winbin = 1;
	else winbin = 0;
run;
proc logistic data=bintrain plots(only) = ROC(id=prob);
	class &cat /param=ref;
	model win(event='1') =  &cat Number_of_Competitor_Bids winbin / ctable pprob = 0 to 0.98 by 0.02;
	title 'Simple Model - Sector, Region, and Winbin';
	store out = simplebid;
	ods output classification = classtablesimple;
	score data=binvalidation out=valpredsimple fitstat;
run;
data classtablesimple;
	set classtablesimple;
	youden = sensitivity + specificity -100;
run;
proc sort data=classtablesimple;
	by descending youden;
run;
proc print data=classtablesimple;
run;
proc plm restore=simplebid;
	score data=binvalidation out=scored_simple predicted/ILINK;
run;
proc print data=scored_simple;
run;

*	Alternate models	;
*	All	;
proc logistic data=project.train plots(only) = ROC(id=prob);
	class &cat /param=ref;
	model win(event='1') = &cat &interval / ctable pprob = 0 to 0.98 by 0.02  clodds=pl;
	title 'Alternate Model - All Predictors except Bid_Price__Millions_';
	store out = altall;
	ods output classification = classtablealtall;
	score data=project.vali out=valpredaltall fitstat;
run;
data classtablealtall;
	set classtablealtall;
	youden = sensitivity + specificity -100;
run;
proc sort data=classtablealtall;
	by descending youden;
run;

proc plm restore=altall;
	score data=project.vali out=scored_altall predicted / ILINK;
run;
proc print data=scored_altall;
run;

*	All with stepwise	;
proc logistic data=project.train plots(only) = ROC(id=prob);
	class &cat /param=ref;
	model win(event='1') = &cat &interval / selection = stepwise ctable pprob = 0 to 0.98 by 0.02;
	title 'Alternate Model - All with Stepwise';
	store out = altstep;
	ods output classification = classtablealtstep;
	score data=project.vali out=valpredaltstep fitstat;
run;
data classtablealtstep;
	set classtablealtstep;
	youden = sensitivity + specificity -100;
run;
proc sort data=classtablealtstep;
	by descending youden;
run;

proc plm restore=altstep;
	score data=project.vali out=scored_altstep predicted / ILINK;
run;
proc print data=scored_altstep;
run;

*	All but winbin instead of winning bin	;
proc logistic data=bintrain plots(only) = ROC(id=prob);
	class &cat winbin /param=ref;
	model win(event='1') =  &cat winbin Cost_After_Engineering_Estimate_ Estimated_Cost__Millions_ Estimated_Years_to_Complete / ctable pprob = 0 to 0.98 by 0.02;
	title 'Alternate Model - All with Winbin';
	store out = altbin;
	ods output classification = classtablealtbin;
	score data=binvalidation out=valpredaltbin fitstat;
run;
data classtablealtbin;
	set classtablealtbin;
	youden = sensitivity + specificity -100;
run;
proc sort data=classtablealtbin;
	by descending youden;
run;
proc plm restore=altbin;
	score data=binvalidation out=scored_altbin predicted / ILINK;
run;
proc print data=scored_altbin;
run;

*	No estimate info CHOSEN MODEL	;
proc logistic data=project.train plots(only) = ROC(id=prob);
	class &cat /param=ref;
	model win(event='1') =  &cat  Competitor_F* Region_of_country / ctable pprob = 0 to 0.98 by 0.02 clodds=pl firth;
	title 'Alternate Model - No Estimate Info';
	store out = altnoest;
	ods output classification = classtablealtnoest;
	score data=project.vali out=valpredaltnoest fitstat;
run;
data classtablealtnoest;
	set classtablealtnoest;
	youden = sensitivity + specificity -100;
run;
proc sort data=classtablealtnoest;
	by descending youden;
run;
proc plm restore=altnoest;
	score data=project.vali out=scored_altnoest predicted / ILINK;
run;

proc print data=scored_altnoest;
run;


*	Assessment;
proc sgplot data=scored_simple;
	histogram predicted /	group = win;
	title 'Simple Model - Region, Sector, Winbin';
run;
proc npar1way data=scored_simple d plot=edfplot;
	class win;
	var predicted;
run;
proc sgplot data=scored_altall;
	histogram predicted / group = win;
	title 'Alternate Model - All';
run;
proc npar1way data=scored_altall d plot=edfplot;
	class win;
	var predicted;
run;
proc sgplot data=scored_altstep;
	histogram predicted /group = win;
	title 'Alternate Model - Stepwise';
run;
proc npar1way data=scored_altstep d plot=edfplot;
	class win;
	var predicted;
run;
proc sgplot data=scored_altbin;
	histogram predicted / group = win;
	title 'Alternate Model - Winbin';
run;
proc npar1way data=scored_altbin d plot=edfplot;
	class win;
	var predicted;
run;
proc sgplot data=scored_altnoest;
	histogram predicted / group = win;
	title 'Alternate Model - No Estimates';
run;
proc npar1way data=scored_altnoest d plot=edfplot;
	class win;
	var predicted;
run;
*	recall and precision	;
proc sql;
title 'Simple Model - Region, Sector, Winbin';
select positive, count(*) as Freq, win 
from (	select case when predicted > 0.16 then 1 else 0 end as positive,win
		from scored_simple) group by win, positive;
quit;
proc sql;
title 'Alternate Model - All';
select positive, count(*) as Freq, win 
from (	select case when predicted > 0.18 then 1 else 0 end as positive,win
		from scored_altall) group by win, positive;
quit;
proc sql;
title 'Alternate Model - Stepwise';
select positive, count(*) as Freq, win 
from (	select case when predicted > 0.14 then 1 else 0 end as positive,win
		from scored_altstep) group by win, positive;
quit;
proc sql;
title 'Alternate Model - Winbin';
select positive, count(*) as Freq, win 
from (	select case when predicted > 0.26 then 1 else 0 end as positive,win
		from scored_altbin) group by win, positive;
quit;
proc sql;
title 'Alternate Model - No Estimates';
select positive, count(*) as Freq, win 
from (	select case when predicted > 0.34 then 1 else 0 end as positive,win
		from scored_altnoest) group by win, positive;
quit;

*Youden index's;
proc print data=classtablesimple (obs=5);
title 'Simple Model - Region, Sector, Winbin';
run;
proc print data=classtablealtall (obs=5);
title 'Alternate Model - All';
run;
proc print data=classtablealtstep (obs=5);
title 'Alternate Model - Stepwise';
run;
proc print data=classtablealtbin (obs=5);
title 'Alternate Model - Winbin';
run;
proc print data=classtablealtnoest (obs=5);
title 'Alternate Model - No Estimates';
run;