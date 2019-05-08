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
datafile = os.path.join(os.getcwd(), "SheatherData/Mantel.txt")
df   = pd.read_csv(datafile, sep="\t")
df.drop('Y', axis=1).corr()

"""
X1  = (df.drop(['Y', 'X2', 'X3'], axis=1))
    X1
0    1
1  200
2  -50
3  909
4  506
"""

X   = np.asarray(df.drop('Y', axis=1))
X1  = np.asarray(df.drop(['Y', 'X2', 'X3'], axis=1))

"""
X1
Out[29]: 
array([[  1],
       [200],
       [-50],
       [909],
       [506]])
"""
 
X2  = np.asarray(df.drop(['Y', 'X1', 'X3'], axis=1))
X3  = np.asarray(df.drop(['Y', 'X1', 'X2'], axis=1))
X12 = np.asarray(df.drop(['Y', 'X3'], axis=1))
X13 = np.asarray(df.drop(['Y', 'X2'], axis=1))
X23 = np.asarray(df.drop(['Y', 'X1'], axis=1))
y   = np.asarray(df['Y'])
print(df)
col = ['X1', 'X2', 'X3']

#model with X1,X2,X3
lr = LinearRegression()
lr.fit(X, y)
print("\nLinear Regression")
linreg.display_coef(lr, X, y, col)
linreg.display_metrics(lr, X, y)

print("\nStats Model Fit:\n")
Xc = sm.add_constant(X)
ols_model = sm.OLS(y, Xc)
results   = ols_model.fit()
print(results.summary())

#The Models with X1, X2, and X3, All 1 Factor Models
lr.fit(X1, y)
print("\nLinear Regression for ", col[0])
linreg.display_coef(lr, X1, y, [col[0]])
linreg.display_metrics(lr, X1, y)

print("\nStats Model Fit:\n")
Xc = sm.add_constant(X1)
ols_model = sm.OLS(y, Xc)
results   = ols_model.fit()
print(results.summary())

lr.fit(X2, y)
print("\nLinear Regression for ", col[1])
linreg.display_coef(lr, X2, y, [col[1]])
linreg.display_metrics(lr, X2, y)

print("\nStats Model Fit:\n")
Xc = sm.add_constant(X2)
ols_model = sm.OLS(y, Xc)
results   = ols_model.fit()
print(results.summary())
      
print("\nLinear Regression for ", col[2])
lr.fit(X3, y)
linreg.display_coef(lr, X3, y, [col[2]])
linreg.display_metrics(lr, X3, y) 
print("\nStats Model Fit:\n")
Xc = sm.add_constant(X3)
ols_model = sm.OLS(y, Xc)
results   = ols_model.fit()
print(results.summary())




#********** My Version *******#
#model = sm.ols('Y ~ X1 + X2 + X3', data=df)
"""
model = sm.ols('Y ~ X1+X2', data=df)
full_results = model.fit()
print(full_results.summary(), "\n")
av.stats.anova_lm(full_results)


ndf = pd.DataFrame([df.Y, df.X1, df.X2, df.X3]).transpose()
#pd.scatter_matrix(ndf[:], figsize=(12,10), diagonal='kde')
X = ndf.drop('Y', axis=1)
Y = ndf['Y']

lm = LinearRegression()
lm.fit(X,Y)
lm.score(X,Y)

yhat = lm.predict(X)
"""
