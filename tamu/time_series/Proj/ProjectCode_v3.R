rm(list=ls())
graphics.off()
library(astsa)
library(zoo)
library(Hmisc)
library(vars)
library(fGarch)
library(forecast)
library(tseries)
library(fracdiff)
library(stats)

data <- read.csv("/Volumes/Macintosh HD/users/michaelmcphail/documents/tamu/stat626/project/finaldataset.csv",header=TRUE)

#Plot the raw lumber data and independent variables
dates <- seq(as.Date("12/01/1999", format = "%m/%d/%Y"),
             by = "months", length = nrow(data))

plot(dates, data$Lumber, xaxt="n", type="l", xlab="Month", ylab = "Lumber PPI", main = "Plot of Lumber PPI")
axis.Date(side = 1, dates, format = "%m/%d/%Y")

plot(dates, data$S.P_Price, xaxt = "n", type="l", xlab = "Month", ylab = "S&P Index Price", main="Plot of S&P Index Price")
axis.Date(side = 1, dates, format = "%m/%d/%Y")

plot(dates, data$NewHousingStarts, xaxt = "n", type="l", xlab = "Month", ylab = "Number of New Housing Starts", main="Plot of New Housing Starts")
axis.Date(side = 1, dates, format = "%m/%d/%Y")

#Create holdout data
LUM<-ts(data$Lumber,start = 1, end = 209, class = "ts")
NEWLUM<-LUM[1:(length(LUM)-12)]
LUMTEST<-LUM[(length(LUM)-11):length(LUM)]
NHS<-ts(data$NewHousingStarts, start = 1, end = 209, class = "ts")
NEWNHS<-NHS[1:(length(NHS)-12)]
NHSTEST<-NHS[(length(NHS)-11):length(NHS)]
SPP<-ts(data$S.P_Price,start = 1, end = 209, class = "ts")
NEWSPP<-SPP[1:(length(SPP)-12)]
SPPTEST<-SPP[(length(SPP)-11):length(SPP)]

#Create lag plots for regression portion
lag2.plot( NEWNHS,NEWLUM, 13)
lag2.plot( NEWSPP,NEWLUM, 13)

#Create lagged dataset based on lag plots
mylist<-list(TIME=c(1:197),NHS0=NEWNHS,LUM=NEWLUM,NHS1=Lag(x = NEWNHS,shift = -1),
             SPP1=Lag(x = NEWSPP,shift = -1))
mydata = ts.intersect(mylist, dframe=TRUE)
summary(fit<-(lm(LUM ~ TIME+NHS1+SPP1,data=mydata)))

#Plot residuals and determine if a white noise is present
dates2 <- seq(as.Date("01/01/2000", format = "%m/%d/%Y"),
             by = "months", length = 196)
plot(dates2, fit$residuals,xaxt="n", type="l", xlab="Month", ylab = "Standardized Residuals", main = "Plot of Standardized Residuals")
axis.Date(side = 1, dates2, format = "%m/%d/%Y")
acf((fit$residuals),20, main="ACF of Standardized Residuals")
pacf((fit$residuals), main="PACF of Standardized Residuals")

#Create time series model for residuals
fit2 <- arima((fit$residuals), order = c(1,0,0))
fit2
summary(fit2)
sarima(fit$residuals, 1, 0,0)
shapiro.test(fit2$residuals)

#GARCH Model for Squared Residuals
acf((fit2$residuals)^2, main="ACF of Squared Residuals")
pacf((fit2$residuals)^2, main="PACF of Squared Residuals")
summary(gar1<-garchFit(~arma(1,0)+garch(3,0),data = (fit$residuals)))

#Holdout Sample
gar1.pr<-predict(gar1,n.ahead=12)
mytestlist<-list(TIME=c(198:209),NHS1=Lag(x = NHSTEST,shift = -1),SPP1=Lag(x = SPPTEST,shift = -1))
TESTDATA = ts.intersect(mytestlist, dframe=TRUE)
reg<-predict(fit,newdata=TESTDATA)
pred12<-reg[1:12]+gar1.pr$meanForecast
plot(c(LUM[1:197],pred12),col=2,type="l")
lines(LUM,type="l",col=4)

#VAR Model
Y=cbind(NEWLUM,NEWNHS,NEWSPP)
VARselect(y = Y,lag.max = 10,type = "both")
fit1.1<-VAR(y = Y,p = 1,type = "both")
summary(fit1.1)
fit.pr<-predict(fit1.1, n.ahead = 12,ci = 0.95)
fit.pr.fcst <- c(201.473, 201.298, 201.165, 201.068, 201.002, 200.9626, 200.946,
                 200.948, 200.966, 200.999, 201.045, 201.101)

plot(LUM,type="l",col='black', ylab="Lumber PPI", xlab="Time", main="Actual vs. Predicted Lumber PPI")
lines(c(LUM[1:197],pred12),col='black',lty="dashed")
lines(c(LUM[1:197],fit.pr.fcst),col='black',lty="dotted")
lines(LUM,type="l",col='black', lwd=2)
legend("bottomleft",legend=c("Actual", "Fitted - Regression", "Fitted - VARMA "),col=c("black", "black", "black"), lty = c("solid","dashed","dotted"),cex=0.75)
fanchart(fit.pr)


