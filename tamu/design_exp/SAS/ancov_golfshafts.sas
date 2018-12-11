*ancov_golfshafts.sas;
OPTION LS=120 PS=55 NOCENTER NODATE; 
TITLE 'ANALYSIS OF COVARIATES WITH TWO COVARIATES'; 
DATA;        
INPUT WT HT DIST SHAFT $ @@;  
CARDS;   
212  71  205 1S 214  73  215 G 152  78  198 2S
220  71  218 1S 186  75  249 G 206  72  178 2S
176  76  224 1S 183  69  166 G 211  78  199 2S
204  77  238 1S 202  74  232 G 203  69  178 2S
152  74  211 1S 195  73  195 G 183  71  182 2S
205  69  189 1S 185  77  243 G 163  73  163 2S
173  69  182 1S 195  76  255 G 160  73  169 2S
196  76  231 1S 198  78  258 G 216  74  200 2S
202  69  183 1S 206  68  174 G 205  69  179 2S
171  72  181 1S 205  69  170 G 199  68  155 2S
run;
proc sort; by SHAFT;
PROC means; Var DIST HT WT ; by SHAFT;
proc means;var DIST HT WT;
run;
PROC PLOT;  
PLOT DIST*HT=SHAFT; 
PLOT DIST*WT=SHAFT;
* HT means;
PROC GLM;  
CLASS SHAFT;
MODEL HT=SHAFT /SOLUTION;  
MEANS SHAFT;
RUN;
* WT means;
PROC GLM;  
CLASS SHAFT;
MODEL WT=SHAFT /SOLUTION;  
MEANS SHAFT;
RUN;
*Model 0: No Covariate in Model;
PROC GLM;  
CLASS SHAFT;
MODEL DIST=SHAFT /SOLUTION; 
MEANS SHAFT; 
LSMEANS SHAFT/ PDIFF STDERR ADJUST=TUKEY;
RUN;
TITLE 'MODEL ALLOWING DIFFERENT SLOPES AND INTERCEPTS'; 
* EQUATIONS OF LINES;
PROC GLM;  
CLASS SHAFT;
MODEL DIST = SHAFT HT*SHAFT WT*SHAFT / NOINT SOLUTION;  
RUN;
* TEST OF EQUAL SLOPES FOR BOTH HT AND WT;
PROC GLM;  
CLASS SHAFT;
MODEL DIST = SHAFT HT HT*SHAFT WT WT*SHAFT / SOLUTION;  
LSMEANS SHAFT/ PDIFF STDERR ADJUST=TUKEY;
RUN;
* MODEL WITH DIFF SLOPES FOR HT BUT SAME SLOPES FOR WT;
PROC GLM;  
CLASS SHAFT;
MODEL DIST = SHAFT HT HT*SHAFT WT  / SOLUTION;  
LSMEANS SHAFT/ PDIFF STDERR ADJUST=TUKEY;
LSMEANS SHAFT/ PDIFF STDERR ADJUST=TUKEY AT (HT WT)=(68 192.6);
LSMEANS SHAFT/ PDIFF STDERR ADJUST=TUKEY AT (HT WT)=(73 192.6);
LSMEANS SHAFT/ PDIFF STDERR ADJUST=TUKEY AT (HT WT)=(78 192.6);
RUN;
* TEST OF EQUAL SLOPES FOR BOTH HT AND WT;
PROC GLM;  
CLASS SHAFT;
MODEL DIST = SHAFT HT HT*SHAFT WT WT*SHAFT  /noint SOLUTION;  
LSMEANS SHAFT/ PDIFF STDERR ADJUST=TUKEY;
RUN;

PROC GLM;  
CLASS SHAFT;
MODEL DIST = SHAFT HT HT*SHAFT WT WT*SHAFT HT*WT HT*WT*SHAFT/ SOLUTION;  
LSMEANS SHAFT/ PDIFF STDERR ADJUST=TUKEY;
RUN;
