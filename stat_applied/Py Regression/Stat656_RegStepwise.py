#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Feb  4 14:37:07 2018

@author: eJones
@title: Stat 656 Week 3 Homework Soultion
"""

import pandas as pd
import numpy  as np
from sklearn.linear_model import LinearRegression, Ridge, Lasso
from sklearn.model_selection import train_test_split
# Statsmodel
import statsmodels.api as sm
#  classes provided for the course
from AdvancedAnalytics import ReplaceImputeEncode
from AdvancedAnalytics import linreg
    
print("***** Linear Regression Variable Selection ***")
df = pd.read_csv("CSV/PGATourDrivingDistanceTraining.csv")
target = 'DrivingDistance'
wtg    = 'nDD'

# First Integer Designates Data Type
# 0=Interval, 1=Binary, 2=Nominal, 4=Other (No Changes, do not include)
attribute_map = {
    'ClubHeadSpeed':['I', (100, 150)],
    'BallSpeed':['I',(100, 200)],
    'LaunchAngle':['I',(7,15)],
    'SpinRate':['I',(2000, 4000)],
    'DistanceToApex':['I',(150, 300)],
    'ApexHeight':['I',(60, 150)],
    'HangTime':['I',(5,9)], 
    'CarryDistance':['I',(250, 450)],
    'nDD':['I', (50, 300)],
    'DrivingDistance':['I', (250, 450)]}


rie = ReplaceImputeEncode(data_map=attribute_map, display=True)
encoded_df = rie.fit_transform(df)

lr = LinearRegression()

col = rie.col
col.remove(target)
col.remove(wtg)
varlist= [target, wtg]
X   = np.asarray(encoded_df.drop(varlist, axis=1))
y   = np.asarray(encoded_df[target]) # The target is not scaled or imputed
w   = np.asarray(encoded_df[wtg]) 

lr.fit(X, y, sample_weight=w)
print("\nLinear Regression")
linreg.display_coef(lr, X, y, col)
linreg.display_metrics(lr, X, y)

print("\nStats Model Fit:\n")
Xc = sm.add_constant(X)
wls_model = sm.WLS(y, Xc, weights=w)
results   = wls_model.fit()
print(results.summary())

a = 1000.0
rr = Ridge(alpha=a)

rr.fit(X, y, sample_weight=w)
print("\nRidge Regression with alpha=", a)
linreg.display_coef(rr, X, y, col)
linreg.display_metrics(rr, X, y)

a = 0.4
la = Lasso(alpha=a)

la.fit(X, y) #Lasso does not allow sample weights
print("\nLasso Regression with alpha=", a)
linreg.display_coef(la, X, y, col)
linreg.display_metrics(la, X, y)
