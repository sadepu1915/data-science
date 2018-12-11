#30 4-week periods
#Koteswara Rao Kadiyala (1970) Testing for the independence of regression disturbances. Econometrica, 38, 97-117

icecream <- read.csv("/Users/home/Documents/TAMU/stat608/Data/icecream.csv", header=TRUE)
attach(icecream)
n<-dim(icecream)[1]
library(nlme)
library(car)


par(mfrow=c(2,2), cex.axis=2, cex.lab=2, mar=c(4, 5, 2, 1))
plot(date, IC, col=(Year+1), pch=(Year + 16), ylab="Consumption")
plot(income, IC, col=(Year+1), pch=(Year + 16), ylab="Consumption")
plot(price, IC, col=(Year+1), pch=(Year + 16), ylab="Consumption")
plot(temp, IC, col=(Year+1), pch=(Year + 16), ylab="Consumption")

par(mfrow=c(1,2), cex.axis=2, cex.lab=2,  mar=c(4, 5, 4, 1), lwd=2, cex.main=2)
acf(IC, main="Consumption")
acf(temp, main="Temperature")

par(mfrow=c(2,3), cex.axis=2, cex.lab=2,  mar=c(4, 5, 2, 1))
plot(date, temp, col=(Year + 1), pch=(Year + 16))
plot(date, income, col=(Year + 1), pch=(Year + 16))
plot(date, price, col=(Year + 1), pch=(Year + 16))
plot(temp, price, col=(Year + 1), pch=(Year + 16))
plot(temp, income, col=(Year + 1), pch=(Year + 16))
plot(income, price, col=(Year + 1), pch=(Year + 16))

#If we don't use autocorrelation of errors:
par(mfrow=c(1,1), cex.axis=2, cex.lab=2,  mar=c(4, 5, 4, 1), cex.main=1.5, lwd=2)
m1<-lm(IC ~ temp + price + income + date)  #What does it mean to include date as a predictor?
res1<-rstandard(m1)
acf(res1, main="Model 1")


#Run the transformation
m1g <- gls(IC ~ temp + price + income + date, correlation=corAR1(form = ~date), method="ML")


#Running by hand:
rho <- 0.6525471 
x <- model.matrix(m1)
iden <- diag(n)
Sigma <- rho^abs(row(iden)-col(iden))
sm <- chol(Sigma)
smi <- solve(t(sm))
xstar <- smi %*% x
ystar <- smi %*% IC
m1tls <- lm(ystar ~ xstar - 1)
summary(m1tls)

#Diagnostic plots for the model:
StanRes1<- rstandard(m1tls)
par(mfrow=c(2,2), cex.axis=2, cex.lab=2, pch=19, mar=c(4, 5, 2, 1))
plot( date, StanRes1, col=(Year + 1), pch=(Year + 16))
plot(income, StanRes1, col=(Year + 1), pch=(Year + 16))
plot(price, StanRes1, col=(Year + 1), pch=(Year + 16))
plot(temp, StanRes1, col=(Year + 1), pch=(Year + 16))

par(mfrow=c(2,2), cex.axis=2, cex.lab=2, pch=19, mar=c(4, 5, 2, 1))
plot(m1tls)




#Try this model instead:
hist(income)
m2 <- lm(IC ~  date + price +  temp  + log(income) + I(temp^2))
m2g <- gls(IC ~  date + price + temp + log(income) + I(temp^2), correlation=corAR1(form = ~date), method="ML")
rho <- 0.7288417  
x <- model.matrix(m2)
iden <- diag(n)
Sigma <- rho^abs(row(iden)-col(iden))
sm <- chol(Sigma)
smi <- solve(t(sm))
xstar <- smi %*% x
ystar <- smi %*% IC
m2tls <- lm(ystar ~ xstar - 1)
summary(m2tls)

par(mfrow=c(1,1), cex.axis=2, cex.lab=2, pch=19, mar=c(4, 5, 3, 1))
res2t<-rstandard(m2tls)
acf(res2t, main="Temperature not lagged")

par(mfrow=c(2,2), cex.axis=2, cex.lab=2, pch=19, mar=c(4, 5, 2, 1))
plot(m2tls)

StanRes2 <- rstandard(m2tls)
par(mfrow=c(2,2), cex.axis=2, cex.lab=2, pch=19, mar=c(4, 5, 2, 1))
plot(date, StanRes2, col=(Year + 1), pch=(Year + 16))
plot(temp, StanRes2, col=(Year + 1), pch=(Year + 16))
plot(Year, StanRes2, col=(Year + 1), pch=(Year + 16))
plot(price, StanRes2, col=(Year + 1), pch=(Year + 16))


