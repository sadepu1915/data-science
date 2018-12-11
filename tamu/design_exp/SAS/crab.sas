* crab.sas;
* The relationship between different habitats and the population densities of
Hermit crabs. There are 6 sites. At each site 25 transects are run and the
number of crabs are counted.;
ods html; ods graphics on;
option ls=70 ps=50 nocenter nodate;
title 'Hermit Crab Density';
data count;
infile 'u:\meth2\kuehl\expl4-1.dat';
input Y Site;
label Y = 'Crab Count';
*generate BoxPlots and Tests of Normality;
proc boxplot;
plot y*site/boxstyle=schematic;
run;
*analysis of variance and residual analysis;
proc glm data=count;
class Site;
model  Y = Site;
*Levine's Test;
means Site/hovtest=bf;
means Site/ LSD tukey snk;
output out=ASSUMP r=RESID p=MEANS STUDENT=SR;
DATA TRANSRESID; SET ASSUMP; TSR=SQRT(ABS(SR)); 
proc univariate def=5 plot normal; var RESID;
proc gplot data=assump; plot resid*means;
PROC gPLOT DATA=TRANSRESID; PLOT TSR*MEANS;
RUN;

*analysis of transformed data;
data trans;
set count;
label TY = 'Transformed Count';
TY=log(Y+1/6);
proc boxplot;
plot ty*site;
proc glm data=trans;
class Site;
model  TY = Site;
*Levine's Test;
means Site/hovtest=bf;
means Site/ tukey ;
output out=ASSUMP2 r=RESID2 p=MEANS2 STUDENT=SR2;
data TRANSRESID2; set ASSUMP2; TSR2 = sqrt(abs(SR2));	
proc univariate def=5 plot normal; var RESID2;
proc gplot data=assump2; plot resid2*means2;
proc gplot; plot TSR2*MEANS2;
run;
*computation of the Kruskal-Wallis Test;
proc npar1way anova wilcoxon;
var Y;
class Site;
run;
proc npar1way anova wilcoxon;
var TY;
class Site;
run;
ods graphics off; ods html close;
