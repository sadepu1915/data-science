#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import math
import pandas as pd
import numpy  as np
import statsmodels.formula.api as sm
from statsmodels.sandbox.regression.predstd \
        import wls_prediction_std
import matplotlib.pyplot as plt
import os

datafile = os.path.join(os.getcwd(), "SheatherData/ProfessorSalaries.txt")
df   = pd.read_csv(datafile, sep="\t")
df=df.loc[df['Experience'] >= 6]

# Fit Simple Linear Regression Model
model = sm.wls('ThirdQuartile ~ Experience', data=df, sample_weight=df['SampleSize'])
results = model.fit()
print(results.summary(), "\n")

pred  = results.fittedvalues
resid = results.resid

# Calculate Hii
results.HC2_se
het = results.het_scale        # het = r^2 / (1-Hii)
h   = 1.0 - (1.0/het)*resid**2 # h = Hii

# Calculate Standardized Residuals
std_resid = np.sqrt(het)/math.sqrt(results.mse_resid)
std_resid = pd.Series(std_resid) # Move into Pandas Series


# Calculate Cook's D
D = ((std_resid**2)*h)/(2*(1.0-h))

df2 = pd.concat([pred, resid, std_resid, D, h], axis=1, \
                keys=["Predicted", "Residual", "Std. Residual", \
                      "Cooks D", "H-Hat"])
df = df.join(df2)
print(df, "\n")
print("Rule of Thumb for Cook's D: ", 4/df.shape[0])

# Plot Predicted vs Observed
fig, ax = plt.subplots(figsize=(8,6))
ax.set_title("Predicted vs Observed", fontweight="bold", fontsize="14")
ax.plot(df['Experience'], df['ThirdQuartile'], 'o', label="Data")
ax.plot(df['Experience'], df['Predicted'], 'k-', label="Predicted")
legend = ax.legend(loc='best')


# Plot Predicted vs Std. Residuals
fig, ax = plt.subplots(figsize=(8,6))
ax.set_title("Predicted vs Standardized Residuals", \
             fontweight="bold", fontsize="14")
ax.plot(df['Predicted'], df['Std. Residual'], 'o', label="Data")
ax.axhline(y=0, linewidth=2, color='b', linestyle='--')
ax.axhline(y=2, linewidth=2, color='r', linestyle='-')
ax.axhline(y=-2, linewidth=2, color='r', linestyle='-')

# Plot Predicted vs Cook's D
fig, ax = plt.subplots(figsize=(8,6))
ax.set_title("Predicted vs Cook's D", \
             fontweight="bold", fontsize="14")
ax.plot(df['Predicted'], df['Cooks D'], 'o', label="Data")
ax.axhline(y=4/df.shape[0], linewidth=2, color='r', linestyle='-')

# Plot Cook's D vs Std. Residuals
fig, ax = plt.subplots(figsize=(8,6))
ax.set_title("Cook's D vs Std. Residuals", \
             fontweight="bold", fontsize="14")
ax.plot(df['Cooks D'], df['Std. Residual'], 'o', label="Data")
ax.axhline(y=0, linewidth=2, color='b', linestyle='--')
ax.axhline(y=2, linewidth=2, color='r', linestyle='-')
ax.axhline(y=-2, linewidth=2, color='r', linestyle='-')
ax.axvline(x=4/df.shape[0], linewidth=2, color='g', linestyle='-')
