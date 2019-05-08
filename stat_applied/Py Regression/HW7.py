#!/usr/bin/env python3

import math
import pandas as pd
import numpy  as np
import statsmodels.formula.api as sm
import statsmodels.api as av
import matplotlib.pyplot as plt
import os
from sklearn.linear_model import LinearRegression
from statsmodels.stats.outliers_influence import variance_inflation_factor


datafile = os.path.join(os.getcwd(), "SheatherData/pgatour2006.csv")
df   = pd.read_csv(datafile, sep=",")

ndf = pd.DataFrame([df.PrizeMoney,df.DrivingAccuracy , df.GIR, df.PuttingAverage,df.BirdieConversion, df.SandSaves, df.Scrambling, df.PuttsPerRound]).transpose()
pd.scatter_matrix(ndf[:], figsize=(12,10), diagonal='kde')
X = ndf.drop('PrizeMoney', axis=1)
Y = ndf['PrizeMoney'].apply(np.log)

lm = LinearRegression()
lm.fit(X,Y)
lm.score(X,Y)

yhat = lm.predict(X)

#residual plots
plt.scatter(ndf['DrivingAccuracy'], Y-yhat)
plt.title("std residual vs Driving accuracy")
plt.show()

plt.scatter(ndf['GIR'], Y-yhat)
plt.title("std residual vs GIR")
plt.show()

plt.scatter(ndf['PuttingAverage'], Y-yhat)
plt.title("std residual vs PuttingAverage")
plt.show()

plt.scatter(ndf['BirdieConversion'], Y-yhat)
plt.title("std residual vs BirdieConversion")
plt.show()

plt.scatter(ndf['SandSaves'], Y-yhat)
plt.title("std residual vs SandSaves")
plt.show()

plt.scatter(ndf['Scrambling'], Y-yhat)
plt.title("std residual vs Scrambling")
plt.show()

plt.scatter(ndf['PuttsPerRound'], Y-yhat)
plt.title("std residual vs PuttsPerRound")
plt.show()

vif = pd.DataFrame()
vif["VIF Factor"] = [variance_inflation_factor(X.values, i) for i in range(X.shape[1])]
vif["features"] = X.columns
