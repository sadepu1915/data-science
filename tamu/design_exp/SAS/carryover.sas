* ~longneck/meth2/sas/carryover.sas;
option ls=75 ps=58 nocenter nodate;
title 'Crossover Design Example';

data raw;
input T S P D C $ Y @@;
cards;
1 1 1 1 N  50   2 1 1 2 1  61  3 1 1 3 2  53
1 1 2 1 N  55   2 1 2 2 1  63  3 1 2 3 2  57
1 2 1 2 N  44   2 2 1 3 2  42  3 2 1 1 3  57
1 2 2 2 N  51   2 2 2 3 2  45  3 2 2 1 3  59
1 3 1 3 N  35   2 3 1 1 3  55  3 3 1 2 1  47
1 3 2 3 N  41   2 3 2 1 3  56  3 3 2 2 1  50
1 4 1 1 N  54   2 4 1 3 1  48  3 4 1 2 3  51
1 4 2 1 N  58   2 4 2 3 1  51  3 4 2 2 3  54
1 5 1 2 N  50   2 5 1 1 2  57  3 5 1 3 1  51
1 5 2 2 N  55   2 5 2 1 2  59  3 5 2 3 1  55
1 6 1 3 N  41   2 6 1 2 3  56  3 6 1 1 2  58
1 6 2 3 N  46   2 6 2 2 3  58  3 6 2 1 2  61
run;
proc mixed cl alpha=.05;
class T S P D C;
model  Y = S  D C;
RANDOM P(S) T;
LSMEANS D/PDIFF;
RUN;
proc mixed cl alpha=.05;
class T S P D C;
model  Y = S  C;
random P(s) t;
proc mixed cl alpha=.05;
class T S P D C;
model  Y = S  D;
random P(s) t;
run;
proc GLM;
class T S P D C;
model  Y = S P(S)T D C;
RANDOM P(S);
LSMEANS D/adjust=tukey;
RUN;
