#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import pandas as pd
import numpy  as np
from sklearn.linear_model import LogisticRegression
# Statsmodel
import statsmodels.api as sm
import statsmodels.discrete.discrete_model as smd
#  classes provided for the course
from AdvancedAnalytics import ReplaceImputeEncode, logreg
import os
from statsmodels.stats.outliers_influence import variance_inflation_factor
import seaborn as sns


print("***** Read Data for Exercise 4, Chapter 8 - Heart Disease Data ***")
datafile = os.path.join(os.getcwd(), "SheatherData/spambase.csv")
df   = pd.read_csv(datafile)

y   = np.asarray(df['Spam'])
X   = np.asarray(df.drop('Spam', axis=1))
col = list(df.columns)[:-1]

lr = LogisticRegression(solver='lbfgs')
lr.fit(X, y)
print("\nLogistic Regression")
nx = X.shape[1]
ny = 2
logreg.display_coef(lr, nx, ny, col)
logreg.display_binary_metrics(lr, X, y)

VX   = df.drop('Spam', axis=1)
vif = pd.DataFrame()
vif["VIF Factor"] = [variance_inflation_factor(VX.values, i) for i in range(VX.shape[1])]
vif["features"] = VX.columns


print("\nStats Model Fit:\n")
Xc = sm.add_constant(X)
logit_model = smd.Logit(y, Xc)
results   = logit_model.fit()
print(results.summary())

#dropcols=['all','our','over','internet', 'order', 'mail', 'report','your','font','money','W_857', 'W_415','W_1999','C(','C!', 'people', 'telnet', 'CAP_avg','receive', 'direct', 'labs', 'george', 'hp', 'remove', 'C$', 'free', 'edu', 'meeting', 're:', 'W_000', 'cs', 'business','conference', 'CAP_long', 'W_3d', 'credit','data','technology']
dropcols=['over','your','C!', 'Spam']
XMod=df.drop(dropcols, axis=1)

###### working
#XMod=df.drop('Spam', axis=1)

XMod=df.drop(['Spam','all','our','over','internet', 'order', 'mail', 'report','your','font','money','W_857', 'W_415'], axis=1)
#XMod=df.drop(['all','our','over','internet', 'order', 'mail', 'report','your','font','W_857', 'W_415'], axis=1)
#XMod=df.drop(['all','our','internet', 'order', 'mail', 'report','your','W_857', 'W_415'], axis=1)

#XMod=df.drop(['all','our','internet', 'order', 'mail', 'report','your'], axis=1)


LogAll=(df['all']+1).apply(np.log)
LogOur=(df['our']+1).apply(np.log)
LogInternet=(df['internet']+1).apply(np.log)
LogOrder=(df['order']+1).apply(np.log)
LogMail=(df['mail']+1).apply(np.log)
LogReport=(df['report']+1).apply(np.log)
LogYour=(df['your']+1).apply(np.log)

LogFont=(df['font']+1).apply(np.log)
LogMoney=(df['money']+1).apply(np.log)

LogW1999=df['W_1999'].apply(np.sqrt)
LogCPar=df['C('].apply(np.sqrt)
LogCEx=df['C!'].apply(np.sqrt)

LogPeople=df['people'].apply(np.log)
LogTelnet=df['telnet'].apply(np.log)
LogCAPAvg=df['CAP_avg'].apply(np.log)
LogReceive=df['receive'].apply(np.log)
LogDirect=df['direct'].apply(np.log)
LogLabs=df['labs'].apply(np.log)

LogOver=df['over'].apply(np.log)
LogYour=df['your'].apply(np.log)
LogCEx=df['C!'].apply(np.log)

###### working
LogOver=np.log(df['over']+1)
LogYour=np.log(df['your']+1)
LogCEx=np.log(df['C!']+1)



#df2=pd.concat([LogAll,LogOur,LogOver,LogInternet,LogOrder,LogMail,LogReport,LogYour,LogFont,LogMoney,LogW1999,LogCPar,LogCEx], axis=1,keys=['all','our','over','internet', 'order', 'mail', 'report','your','font','money','W_1999','C(','C!', 'people','telnet','CAP_avg','receive','direct','labs'])
df2=pd.concat([LogAll,LogOur,LogOver,LogInternet,LogOrder,LogMail,LogReport,LogYour,LogFont,LogMoney], axis=1,keys=['all','our','over','internet', 'order', 'mail', 'report','your','font','money'])
#df2=pd.concat([LogAll,LogOur,LogOver,LogInternet,LogOrder,LogMail,LogReport,LogYour,LogFont], axis=1,keys=['all','our','over','internet','order', 'mail','report','your','font'])
df2=pd.concat([LogOver,LogCEx,LogYour], axis=1,keys=['Lnover','LnC!','Lnyour'])

XMod=XMod.join(df2)


lrMod = LogisticRegression(C=1e64, solver='newton-cg', max_iter=1000)
lrMod.fit(XMod, y)
print("\nLogistic Regression")
nx = XMod.shape[1]
ny = 2
colMod = list(XMod.columns)
logreg.display_coef(lrMod, nx, ny, colMod)
logreg.display_binary_metrics(lrMod, XMod, y)

print("\nStats Model Fit:\n")
Xc = sm.add_constant(XMod)
logit_model = smd.Logit(y, Xc)
results   = logit_model.fit()
print(results.summary())
result


all
our
over
internet
order
mail
report
your
font
money
W_1999
C(
C!


