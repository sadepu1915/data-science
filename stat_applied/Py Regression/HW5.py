#!/usr/bin/env python3

import math
import pandas as pd
import numpy  as np
import statsmodels.formula.api as sm
import statsmodels.api as av
import matplotlib.pyplot as plt
import os

##### Ex1###########

datafile = os.path.join(os.getcwd(), "SheatherData/overdue.txt")
df   = pd.read_csv(datafile, sep="\t")

newcol = []
#for i in range(df.shape[0]):
#   if i < 48:
#       newcol.append("RESIDENTIAL")
#    else:
#       newcol.append("COMMERCIAL")

for i in range(df.shape[0]):
    if i < 48:
       newcol.append(1)
    else:
       newcol.append(0)

       
df['TYPE']=newcol
       

# Fit Simple Linear Regression Model
#unrelated regression line 

model = sm.ols('LATE ~ BILL + TYPE + TYPE:BILL', data=df)
full_results = model.fit()
print(full_results.summary(), "\n")
av.stats.anova_lm(full_results)

reduced_model = sm.ols('LATE ~ BILL', data=df)
red_results = reduced_model.fit()
print(red_results.summary(), "\n")
av.stats.anova_lm(red_results)

########### Ex2 ###############

datafile = os.path.join(os.getcwd(), "SheatherData/HoustonChronicle.csv")
df   = pd.read_csv(datafile)

model1 = sm.ols('Repeating1stGrade ~ LowincomeStudents', data=df)
results = model1.fit()
print(results.summary(), "\n")
av.stats.anova_lm(results)

#assuming year change the size of %lowincome students on  repeating frstgrade  and additive change in
# repeating first grade by year by year
df.loc[(df.Year == 2004),'Year'] = 1
df.loc[(df.Year == 1994),'Year'] = 0

model3 = sm.ols('Repeating1stGrade ~ LowincomeStudents + Year + LowincomeStudents:Year', data=df)
m3_results = model3.fit()
print(m3_results.summary(), "\n")
av.stats.anova_lm(m3_results)

av.stats.anova_lm(results, m3_results)

