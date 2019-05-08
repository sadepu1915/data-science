#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 13 13:14:27 2019

@author: EJones
"""

import pandas as pd
import numpy  as np
from sklearn.linear_model import LinearRegression
import statsmodels.api as sm
import statsmodels.formula.api as smf
from statsmodels.sandbox.regression.predstd import wls_prediction_std
import matplotlib.pyplot as plt

path = "SheatherData/production.txt"
df = pd.read_csv(path, sep='\t')
print(df)

y = np.row_stack(df['RunTime'])
x = np.row_stack(df['RunSize'])
lr = LinearRegression()
lr.fit(x, y)
print("\nSimple Linear Regression:  RunTime = B0 + B1*RunSize")
print("Intercept:", lr.intercept_)
print("Slope:    ", lr.coef_[0])

    
# Solution for Pandas Dataframe
# This approach uses R notation for specifying the model
results = smf.ols('RunTime ~ RunSize', data=df).fit()

print(results.summary())

# Solution for Numpy Arrays/Matrices
# This approach creates solutions using the Normal Equations
y = np.array(df['RunTime'])
x1 = np.array(df['RunSize'])
n = df.shape[0]
x = np.column_stack((np.ones(n), x1))
x = np.matrix([[1,175], [1,189], [1,344], [1,88], [1,114], [1,338], [1,271], \
               [1,173], [1,284], [1,277], [1,337], [1,58], [1,146], [1,277], \
               [1,123], [1, 227], [1, 63], [1, 337], [1, 146], [1, 68] ] )
results = sm.OLS(y, x, missing='drop').fit()

print(results.summary())
y_pred = results.fittedvalues
prstd, iv_l, iv_u = wls_prediction_std(results)

fig, ax = plt.subplots(figsize=(8,6))
x1 = np.array(df['RunSize'])
ax.plot(x1, y, 'o', label="Data")
ax.plot(x1, y_pred, 'k--', label='Predicted')
ax.plot(x1, iv_u, 'r--')
ax.plot(x1, iv_l, 'g--', label="Lower CI")
legend = ax.legend(loc='best')