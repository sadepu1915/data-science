#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 15 20:02:58 2019

@author: sadepu
"""

import pandas as pd
import numpy  as np
from sklearn.linear_model import LogisticRegression
# Statsmodel
import statsmodels.api as sm
import statsmodels.discrete.discrete_model as smd
#  classes provided for the course
from AdvancedAnalytics import ReplaceImputeEncode, logreg
import os

print("***** Read Data for Exercise 4, Chapter 8 - Heart Disease Data ***")
datafile = os.path.join(os.getcwd(), "SheatherData/HeartDisease.xlsx")
#df   = pd.read_csv(datafile)
df = pd.read_excel(datafile)

y   = np.asarray(df['HeartDisease'])
X   = np.asarray(df.drop('HeartDisease', axis=1))
col = ['x1', 'x2', 'x3', 'x4', 'x5']

lr = LogisticRegression(solver='lbfgs')
lr.fit(X, y)
print("\nLogistic Regression")
nx = X.shape[1]
ny = 2
logreg.display_coef(lr, nx, ny, col)
logreg.display_binary_metrics(lr, X, y)

print("\nStats Model Fit:\n")
Xc = sm.add_constant(X)
logit_model = smd.Logit(y, Xc)
results   = logit_model.fit()
print(results.summary())

LogX2 = np.log(X[:,0])
LogX2 = np.reshape(LogX2, (X.shape[0], 1))
LogX4 = np.log(X[:,3])
LogX4 = np.reshape(LogX4, (X.shape[0], 1))
XMod = np.append(X, LogX2, axis=1)
XMod = np.append(XMod, LogX4, axis=1)
lrMod = LogisticRegression(C=1e64, solver='newton-cg', max_iter=1000)
lrMod.fit(XMod, y)
print("\nLogistic Regression")
nx = XMod.shape[1]
ny = 2
colMod = ['x1', 'x2', 'x3', 'x4', 'x5', 'f1x1', 'f2x4']
logreg.display_coef(lrMod, nx, ny, colMod)
logreg.display_binary_metrics(lrMod, XMod, y)

print("\nStats Model Fit:\n")
Xc = sm.add_constant(XMod)
logit_model = smd.Logit(y, Xc)
results   = logit_model.fit()
print(results.summary())

#Ex5
print("***** Read Data for Exercise 5, Chapter 8 - Direct Marketing ***")
df = pd.read_excel("../Excel/FundRaising.xlsx")
droplist = ['Sample', 'TARGET_B', 'TARGET_D', 'MAXRAMNT', 'zipconvert_2', \
            'zipconvert_3', 'zipconvert_4', 'zipconvert_5']
y   = np.asarray(df['TARGET_B'])
X   = np.asarray(df.drop(droplist, axis=1))
col = list(df.drop(droplist, axis=1))

lr = LogisticRegression(C=1e64, solver='newton-cg', max_iter=1000)
lr.fit(X, y)
print("\nLogistic Regression")
nx = X.shape[1]
ny = 2
logreg.display_coef(lr, nx, ny, col)
logreg.display_binary_metrics(lr, X, y)

print("\nStats Model Fit:\n")
Xc = sm.add_constant(X)
logit_model = smd.Logit(y, Xc)
results   = logit_model.fit()
print(results.summary())

droplist = ['Sample', 'TARGET_B', 'TARGET_D', 'MAXRAMNT', 'zipconvert_5']
y   = np.asarray(df['TARGET_B'])
X   = np.asarray(df.drop(droplist, axis=1))
col = list(df.drop(droplist, axis=1))

lr = LogisticRegression(C=1e64, solver='newton-cg', random_state=12345, \
                        tol=1e-12, max_iter=10000)
lr.fit(X, y)
print("\nLogistic Regression")
nx = X.shape[1]
ny = 2
logreg.display_coef(lr, nx, ny, col)
logreg.display_binary_metrics(lr, X, y)

print("\nStats Model Fit:\n")
Xc = sm.add_constant(X)
logit_model = smd.Logit(y, Xc)
results   = logit_model.fit(maxiter=10000)
print(results.summary())

LnNumprom  = np.log(X[:,9])
LnRamntall = np.log(X[:,10])
LnLastGift = np.log(X[:,12]+1)
LnTimeLag  = np.log(X[:,13]+1)
LnAvgGift  = np.log(X[:,14])
LnNumprom  = np.reshape(LnNumprom , (X.shape[0], 1))
LnRamntall = np.reshape(LnRamntall , (X.shape[0], 1))
LnLastGift = np.reshape(LnLastGift , (X.shape[0], 1))
LnTimeLag  = np.reshape(LnTimeLag , (X.shape[0], 1))
LnAvgGift  = np.reshape(LnAvgGift , (X.shape[0], 1))
XMod = np.append(X, LnNumprom, axis=1)
XMod = np.append(XMod, LnRamntall, axis=1)
XMod = np.append(XMod, LnLastGift, axis=1)
XMod = np.append(XMod, LnTimeLag, axis=1)
XMod = np.append(XMod, LnAvgGift, axis=1)
XMod = np.delete(XMod, (14, 13, 12, 10, 9), axis=1)
lrMod = LogisticRegression(C=1e64, tol=1e-12, solver='newton-cg', max_iter=1000)
lrMod.fit(XMod, y)
print("\nLogistic Regression")
nx = XMod.shape[1]
ny = 2
col = ['homeowner', 'gender', 'NUMCHLD', 'INCOME', 'WEALTH', \
       'HV', 'Icmed', 'Icavg', 'IC15', \
       'totalmonths', 'zipconvert_2', 'zipconvert_3', \
       'zipconvert_4',  'LnNumprom', \
       'LnRamntall', 'LnLastGift', 'LnTimeLag', 'LnAvgGift']
logreg.display_coef(lrMod, nx, ny, col)
logreg.display_binary_metrics(lrMod, XMod, y)

print("\nStats Model Fit:\n")
Xc = sm.add_constant(XMod)
logit_model = smd.Logit(y, Xc)
results   = logit_model.fit()
print(results.summary())



