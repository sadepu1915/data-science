rm(list=ls())
graphics.off()
library(astsa)
#data <- read.csv("/Volumes/Macintosh HD/users/michaelmcphail/documents/tamu/stat626/project/finaldataset.csv",header=TRUE)

PlotFile="C:\\Users\\Madhavi\\Documents\\MS\\626\\Proj\\Plot.pdf"
pdf(file=PlotFile)

data <- read.csv("C:\\Users\\Madhavi\\Documents\\MS\\626\\Proj\\finaldataset.csv",header=TRUE)


#Plot the raw lumber data and independent variables
dates <- seq(as.Date("12/01/1999", format = "%m/%d/%Y"),
             by = "months", length = nrow(data))

plot(dates, data$Lumber, xaxt="n", type="l", xlab="Month", ylab = "Lumber PPI", main = "Plot of Lumber PPI")
axis.Date(side = 1, dates, format = "%m/%d/%Y")

plot(dates, data$S.P_Price, xaxt = "n", type="l", xlab = "Month", ylab = "S&P Index Price", main="Plot of S&P Index Price")
axis.Date(side = 1, dates, format = "%m/%d/%Y")

plot(dates, data$NewHousingStarts, xaxt = "n", type="l", xlab = "Month", ylab = "Number of New Housing Starts", main="Plot of New Housing Starts")
axis.Date(side = 1, dates, format = "%m/%d/%Y")

#Take the first difference to create a stationary dataset
Lumber_diff <- diff(data$Lumber)
NHS_diff = diff(data$NewHousingStarts)
SP_diff = diff(data$S.P_Price)

#Plot the differenced lumber data and independent variables
dates_diff <- seq(as.Date("01/01/2000", format = "%m/%d/%Y"),
                  by = "months", length = nrow(data)-1)

plot(dates_diff, Lumber_diff, xaxt="n", type="l", xlab="Month", ylab = "First Difference of Lumber PPI", main = "Plot of First Difference of Lumber PPI")
axis.Date(side = 1, dates_diff, format = "%m/%d/%Y")

plot(dates_diff, NHS_diff, xaxt="n", type="l", xlab="Month", ylab = "First Difference of Number of New Housing Starts", main = "Plot of First Difference of Number of New Housing Starts")
axis.Date(side = 1, dates_diff, format = "%m/%d/%Y")

plot(dates_diff, SP_diff, xaxt="n", type="l", xlab="Month", ylab = "First Difference of S&P Index Price", main = "Plot of First Difference of S&P Index Price")
axis.Date(side = 1, dates_diff, format = "%m/%d/%Y")


#ACF and PACF of differenced lumber data beginning 12 points after beginning of data (due to NHS_lag11) and ending
#12 points before end of data (so only model on training dataset)
acf(dataLag$Lumber_diff[1:(nrow(dataLag)-12)], main="ACF of Lumber_Diff Training Data") #suggests MA(1)
pacf(dataLag$Lumber_diff[1:(nrow(dataLag)-12)], main="PACF of Lumber_Diff Training Data") #suggests AR(1)

#Plot CCF between differenced lumber data and independent variables
ccf(NHS_diff[1:(nrow(dataLag)-12)], Lumber_diff[1:(nrow(dataLag)-12)], main = "CCF Between Lumber_Diff and NHS_Diff", ylab="CCF") #Lag 11 looks predictive
ccf(SP_diff[1:(nrow(dataLag)-12)], Lumber_diff[1:(nrow(dataLag)-12)], main = "CCF Between Lumber_Diff and SP_Diff", ylab="CCF") #Lag 1 looks predictive

#Create Lagged Dataset
dataLag <- data.frame(Lumber_diff, NHS_diff, SP_diff)
NHS_diff_lag11 <- rep(0, nrow(dataLag))
SP_diff_lag1 <- rep(0, nrow(dataLag))
dataLag <- data.frame(Lumber_diff, NHS_diff, SP_diff, NHS_diff_lag11, SP_diff_lag1)

for (i in 12:(nrow(dataLag))) {
  dataLag$NHS_diff_lag11[i] = dataLag$NHS_diff[(i-11)]  
}

for (i in 2:(nrow(dataLag))) {
  dataLag$SP_diff_lag1[i] = dataLag$SP_diff[(i-1)]  
}


#Fit ARIMA(1,0,1) to differenced data
fit <- arima(dataLag$Lumber_diff[1:(nrow(dataLag)-12)], order=c(1,0,1))
fit #Fit shows that both AR terms and MA terms are NOT significant; AIC = 1023.04

#Fit ARIMA(0,0,1) to differenced data
fit2 <- arima(dataLag$Lumber_diff[1:(nrow(dataLag)-12)], order = c(0,0,1))
fit2 #MA term is significant; AIC = 1021.05
shapiro.test(fit2$residuals) #Suggests non-normality in residuals

acf(fit2$residuals, main="ACF of MA(1) Model") #Looks good
pacf(fit2$residuals, main="PACF of MA(1) Model") #Looks good

#Fit ARIMA(1,0,0) to differenced data
fit3 <- arima(dataLag$Lumber_diff[1:(nrow(dataLag)-12)], order = c(1,0,0))
fit3 #AR term is significant; AIC = 1023.29
shapiro.test(fit3$residuals) #Suggests non-normality in residuals

acf(fit3$residuals, main="ACF of AR(1) Model") #Looks good
pacf(fit3$residuals, main="PACF of AR(1) Model") #Looks good

#Based on the AIC, MA model looks better than AR model.  Let's look at the Sarima
#diagnostics

sarima(dataLag$Lumber_diff[1:(nrow(dataLag)-12)], 1, 0,0)

#Now, let's consider additing predictors.  Fit MA(1) with NHS Lag 11 and SP Lag 1 as predictors
fit.reg <- arima(dataLag[12:(nrow(dataLag)-12), "Lumber_diff"]
           ,xreg = cbind(dataLag[12:(nrow(dataLag)-12), "NHS_diff_lag11"],
                         dataLag[12:(nrow(dataLag)-12), "SP_diff_lag1"]) 
                         ,order=c(0,0,1))

fit.reg #All terms are significant except for intercept; AIC = 1013.84
shapiro.test(fit.reg$residuals) #Suggests data is normal

acf(fit.reg$residuals, main="ACF of MA(1) + Predictors Model") #Looks good
pacf(fit.reg$residuals, main="PACF of MA(1) + Predictors Model") #Looks good

#Create forecasted values for next year and plot them along with raw data and original fitted values
fore = predict(fit.reg, n.ahead=12, newxreg = cbind(dataLag[(nrow(dataLag)-11):nrow(dataLag), "NHS_diff_lag11"], 
               dataLag[(nrow(dataLag)-11):nrow(dataLag), "SP_diff_lag1"]))

#par(mfrow=c(1,1))
plot(dataLag$Lumber_diff[12:(nrow(dataLag))], type="l", xlab = "", xlim=c(0, 207), ylab = "Lumber PPI",main="Plot of Actual vs. Fitted/Forecast", xaxt="n")
lines(dataLag$Lumber_diff[12:(nrow(dataLag)-12)] - fit.reg$residuals, type="l", col="red")
lines(fore$pred, type="l", col="blue")
lines(fore$pred + 1.96*fore$se, lty="dashed", col="green")
lines(fore$pred - 1.96*fore$se, lty="dashed", col="green")
legend(0,-10, legend=c("Actual", "Fitted", "Forecast", "PI Bounds"),lev	
       col=c("black", "red", "blue", "green"), lty = c(1,1,1,2),cex=0.5)

dev.off()


