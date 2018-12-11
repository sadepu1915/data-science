options ps=55 ls=70 nocenter nodate;
*SAS Program to compute sample size for experiments with random effects.
* Fix number of reps, r, then determine number of treatments, t.;
data;
input  t @@;
r=6;
u1=t-1;
u2=t*(r-1);
gamma=.80;
alpha=.01;
tau=(2.5)**2/(2.0)**2;
lambda=sqrt(1+(r)*(tau));
Fcr=finv(1-alpha,u1,u2);
C=(1/lambda**2)*Fcr;
power=1-probf(C,u1,u2);
cards;
3 4 5 6 7 8 9  10 11 12 13
run;
proc print; var t u1 u2 Fcr lambda power;
run;
