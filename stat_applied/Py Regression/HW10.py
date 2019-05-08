#!/usr/bin/env python3
import pandas as pd
import numpy  as np
# sklearn packages
from sklearn.linear_model import LinearRegression
# Statsmodel
import statsmodels.api as sm
#  classes provided for the course
from AdvancedAnalytics import ReplaceImputeEncode
from AdvancedAnalytics import linreg
import os

print("***** Linear Regression Variable Selection ***")
datafile = os.path.join(os.getcwd(), "SheatherData/Haldcement.txt")
df   = pd.read_csv(datafile, sep="\t")
df.drop('Y', axis=1).corr()


X4  = np.asarray(df.drop(['Y', 'X1', 'X2', 'X3'], axis=1))
X12 = np.asarray(df.drop(['Y', 'X3', 'X4'], axis=1))
X124 = np.asarray(df.drop(['Y', 'X3'], axis=1))
X   = np.asarray(df.drop('Y', axis=1))
y   = np.asarray(df['Y'])
col = ['X1', 'X2', 'X3', 'X4']

lr = LinearRegression()
lr.fit(X4, y)
print("\nLinear Regression")
linreg.display_coef(lr, X4, y, [col[3]])
linreg.display_metrics(lr, X4, y)

print("\nStats Model Fit:\n")
S4 = sm.add_constant(X4)
ols_model = sm.OLS(y, S4)
results   = ols_model.fit()
print(results.summary())
print("X4 Stats Model", results.rsquared_adj, results.aic, results.bic)

lr.fit(X12, y)
print("\nLinear Regression for ", col[0:2])
linreg.display_coef(lr, X12, y, col[0:2])
linreg.display_metrics(lr, X12, y)

print("\nStats Model Fit:\n")
S12 = sm.add_constant(X12)
ols_model = sm.OLS(y, S12)
results   = ols_model.fit()
print(results.summary())
print("X12 Stats Model", results.rsquared_adj, results.aic, results.bic)


lr.fit(X124, y)
print("\nLinear Regression for ", ['X1', 'X2', 'X4'])
linreg.display_coef(lr, X124, y, ['X1', 'X2', 'X4'])
linreg.display_metrics(lr, X124, y)

print("\nStats Model Fit:\n")
S124 = sm.add_constant(X124)
ols_model = sm.OLS(y, S124)
results   = ols_model.fit()
print(results.summary())
print("X124 Stats Model", results.rsquared_adj, results.aic, results.bic)


lr.fit(X, y)
print("\nLinear Regression for ", col)
linreg.display_coef(lr, X, y, col)
linreg.display_metrics(lr, X, y)

print("\nStats Model Fit:\n")
SX = sm.add_constant(X)
ols_model = sm.OLS(y, SX)
results   = ols_model.fit()
print(results.summary())
print("X1234 Stats Model", results.rsquared_adj, results.aic, results.bic)



############# Prob3##############
datafile = os.path.join(os.getcwd(), "SheatherData/pgatour2006.csv")
df   = pd.read_csv(datafile, sep=",")
ndf = pd.DataFrame([df.PrizeMoney,df.DrivingAccuracy , df.GIR, df.PuttingAverage,df.BirdieConversion, df.SandSaves, df.Scrambling, df.PuttsPerRound]).transpose()

XDrPaSrPr = np.asarray(ndf.drop(['PrizeMoney', 'GIR', 'BirdieConversion', 'SandSaves'], axis=1))
XGiBrSa = np.asarray(ndf.drop(['PrizeMoney','DrivingAccuracy','PuttingAverage', 'Scrambling','PuttsPerRound'],  axis=1))
XDrBr = np.asarray(ndf.drop(['PrizeMoney', 'GIR', 'PuttingAverage', 'SandSaves', 'Scrambling','PuttsPerRound'], axis=1))
X = np.asarray(ndf.drop('PrizeMoney', axis=1))
Y = np.asarray(ndf['PrizeMoney'].apply(np.log))

col = ['DrivingAccuracy', 'GIR', 'PuttingAverage','BirdieConversion', 'SandSaves', 'Scrambling', 'PuttsPerRound']

lr = LinearRegression()
lr.fit(X4, y)
print("\nLinear Regression")
linreg.display_coef(lr, X4, y, [col[3]])
linreg.display_metrics(lr, X4, y)

print("\nStats Model Fit:\n")
S4 = sm.add_constant(X4)
ols_model = sm.OLS(y, S4)
results   = ols_model.fit()
print(results.summary())
print("X4 Stats Model", results.rsquared_adj, results.aic, results.bic)




