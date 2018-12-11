options ls=72 ps=58 nocenter nodate;
* This is Example 15.1 on page 192 in the
"Analysis of Messy Data, Vol I", by G. Milliken and D. Johnson;
*This experiment is a CR 3x3 factorial with 4 reps but there
is missing reps for some treatments and 0 reps for 2 treatments;
options pagesize=55 linesize=120;
data raw;
input trt fat surf fl1-fl4;
*drop fl1-fl4;
sv=fl1;output;
sv=fl2;output;
sv=fl3;output;
sv=fl4;output;
cards;
1 1  1  6.7 4.3 5.7  .
2 1  2  7.1  .  5.9 5.6
3 1  3   .   .   .  .
4 2  1   .  5.9 7.4 7.1
5 2  2   .   .   .   .
6 2  3  6.4 5.1 6.2 6.3
7 3  1  7.1 5.9  .   .
8 3  2  7.3 6.6 8.1 6.8
9 3  3   .  7.5 9.1  .
;
title 'Analysis as a CR 3x3 factorial';
proc glm;
class trt;
model sv = trt / ss1 ss2 ss3 ss4;
estimate 'Main1 of Fat'       trt  1   1  0  0 -1 -1  0;
estimate 'Main2 of Fat'       trt  0   0  1  1 -1  0 -1;
estimate 'Main1 of Surf'      trt  0   0  0  0  0  1 -1;
estimate 'Main2 of Surf'      trt  0   0  1 -1  1  0 -1;
estimate 'Inter1 of Fat&Surf' trt  1  -1  0  0 -1  1  0;
estimate 'Inter2 of Fat&Surf' trt  0   0  1 -1 -1  0  1;

contrast 'Main of Fat'        trt  1   1  0  0 -1 -1  0,
                              trt  0   0  1  1 -1  0 -1;
contrast 'Main of Surf'       trt  0   0  0  0  0  1 -1,
                              trt  0   0  1 -1  1  0 -1;
contrast 'Inter of Fat&Surf' trt  1  -1  0  0 -1  1  0,
                              trt  0   0  1 -1 -1  0  1;

means trt/tukey;    
lsmeans trt / stderr pdiff adjust=tukey ;
run;
