bridge<-read.csv("C:/Users/Madhavi/Documents/MS/608/ecampus-R/pgatour2006.csv", header=TRUE)
attach(bridge)

#Step 1:  Expl+ore the data .
plot(bridge)

PrizeMoney 
DrivingAccuracy   
GIR 
PuttingAverage
BirdieConversion 
SandSaves 
Scrambling
PuttsPerRound

lm<-lm(PrizeMoney ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves + Scrambling + PuttsPerRound)
summary(lm)
vif(lm)


par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfrow=c(3,2))
#plot(lm.logs$fitted, lm.logs$residuals)
plot(DrivingAccuracy, lm$residuals)
plot(GIR, lm$residuals)
plot(PuttingAverage, lm$residuals)
plot(BirdieConversion, lm$residuals)
plot(SandSaves, lm$residuals)
plot(Scrambling, lm$residuals)
plot(PuttsPerRound, lm$residuals)


X<-cbind(DrivingAccuracy,GIR,PuttingAverage,BirdieConversion,SandSaves,Scrambling,PuttsPerRound)
library(car)
tranx<-powerTransform(X)
summary(tranx)


lm1<-lm(PrizeMoney ~ log(DrivingAccuracy) + log(GIR)+log(PuttingAverage)+log(BirdieConversion)+log(SandSaves)
                     +log(Scrambling)+log(PuttsPerRound))
trany<-powerTransform(lm1)
summary(trany)


lm1.log<-lm(log(PrizeMoney) ~ log(DrivingAccuracy) + log(GIR)+log(PuttingAverage)+log(BirdieConversion)+log(SandSaves)
        +log(Scrambling)+log(PuttsPerRound))

pairs(log(PrizeMoney) ~ log(DrivingAccuracy) + log(GIR)+log(PuttingAverage)+log(BirdieConversion)+log(SandSaves)
            +log(Scrambling)+log(PuttsPerRound))

summary(lm1.log)


## removing high p val
lm1.log<-lm(log(PrizeMoney) ~ log(DrivingAccuracy) + log(GIR)+log(BirdieConversion)+log(SandSaves)
            +log(Scrambling)+log(PuttsPerRound))
vif(lm1.log)
plot(lm1.log)



###########################################################
#All Subsets:  (DON'T MISSPELL AS "SUBSET")
#Setting up full design matrix for later use:
X.labels<-c("DrivingAccuracy", "GIR", "PuttingAverage", "BirdieConversion", "SandSaves", "Scrambling", "PuttsPerRound")
X<-matrix(cbind(log(DrivingAccuracy), log(GIR), log(PuttingAverage), log(BirdieConversion),log(SandSaves), log(Scrambling), log(PuttsPerRound)), 
          nrow=1372, ncol=7, dimnames=list(c(1:1372),X.labels))
b<-regsubsets(X, log(PrizeMoney))
summary(b)


par(cex.axis=2, cex.lab=2, mar=c(5,5,2,2))
subsets(b, statistic=c("adjr2"))


par(cex.axis=2, cex.lab=2, mar=c(5,5,2,2))
subsets(b, statistic=c("bic"))


all.subs<-leaps(X, log(Time), method=c("adjr2"), nbest=1, names=X.labels)
all.subs$size #Notice this gives the number of parameters, not the number of variables.
all.subs$which
all.subs$adjr2

X.labels<-c("DrivingAccuracy", "GIR", "PuttingAverage", "BirdieConversion", "SandSaves", "Scrambling", "PuttsPerRound")
X<-matrix(cbind(log(DrivingAccuracy), log(GIR), log(PuttingAverage), log(BirdieConversion),log(SandSaves), log(Scrambling), log(PuttsPerRound)), 
          nrow=1372, ncol=7, dimnames=list(c(1:1372),X.labels))

om1<-lm(log(PrizeMoney) ~ log(DrivingAccuracy))
om2<-lm(log(PrizeMoney) ~ log(DrivingAccuracy) + log(GIR))

om1<-lm(log(PrizeMoney) ~ log(DrivingAccuracy))
om1<-lm(log(PrizeMoney) ~ log(DrivingAccuracy))  
om2<-lm(log(Time) ~ log(Dwgs) + log(Spans))
om1<-lm(log(PrizeMoney) ~ log(DrivingAccuracy))
om3<-lm(log(Time) ~ log(Dwgs) + log(Spans) + log(CCost))
om1<-lm(log(PrizeMoney) ~ log(DrivingAccuracy))
om4<-lm(log(Time) ~ log(Dwgs) + log(Spans) + log(CCost) + log(DArea))
om1<-lm(log(PrizeMoney) ~ log(DrivingAccuracy))
om5<-lm(log(Time) ~ log(Dwgs) + log(Spans) + log(CCost) + log(DArea) + log(Length))

#Subset size = 1:  
n <- length(om1$residuals)
K <- length(om1$coefficients) +1  
#The number of estimated parameters in the model K = the number of predictors plus one intercept, plus the error variance.

#Calculate AIC
extractAIC(om1,k=2)
#Calculate AICc
extractAIC(om1,k=2)+2*K*(K+1)/(n-K-1)
#Calculate BIC
extractAIC(om1,k=log(n))

#Subset size=2
K <- length(om2$coefficients) +1
#Calculate AIC
extractAIC(om2,k=2)
#Calculate AICc
extractAIC(om2,k=2)+2*K*(K+1)/(n-K-1)
#Calculate BIC
extractAIC(om2,k=log(n))

#Subset size=3
K <- length(om3$coefficients) +1
#Calculate AIC
extractAIC(om3,k=2)
#Calculate AICc
extractAIC(om3,k=2)+2*K*(K+1)/(n-K-1)
#Calculate BIC
extractAIC(om3,k=log(n))

#Subset size=4
K <- length(om4$coefficients) +1
#Calculate AIC
extractAIC(om4,k=2)
#Calculate AICc
extractAIC(om4,k=2)+2*K*(K+1)/(n-K-1)
#Calculate BIC
extractAIC(om4,k=log(n))

#Subset size=5
K <- length(om5$coefficients) +1
#Calculate AIC
extractAIC(om5,k=2)
#Calculate AICc
extractAIC(om5,k=2)+2*K*(K+1)/(n-K-1)
#Calculate BIC
extractAIC(om5,k=log(n))







#Lasso:
#Need to standardize or normalize variables beforehand!
#Standardize: calculate z-scores:
zlDArea <- (log(DArea) - mean(log(DArea)))/sd(log(DArea))
zlCCost <- (log(CCost) - mean(log(CCost)))/sd(log(CCost))
zlDwgs <-  (log(Dwgs)  - mean(log(Dwgs)))/sd(log(Dwgs))
zlLength <- (log(Length) - mean(log(Length)))/sd(log(Length))
zlSpans <- (log(Spans) - mean(log(Spans))) / sd(log(Spans))

zX.labels<-c("zLDArea", "zLLength", "zLDwgs", "zLSpans", "zLCCost")
zX<-matrix(cbind(zlDArea, zlCCost, zlDwgs, zlLength, zlSpans), 
           nrow=45, ncol=5, dimnames=list(c(1:45),X.labels))
mlasso<-lars(zX, log(Time), type="lasso", normalize = FALSE, trace=TRUE)


#Automatic normalization (Put on [0, 1] scale):
mlasso<-lars(X, log(Time), type="lasso", normalize = TRUE, trace=TRUE)
summary.lars(mlasso)
coef(mlasso)
print(mlasso)

par(mfrow=c(1,1), mar=c(5,5,4,4), cex.axis=0.8)
plot.lars(mlasso, xvar="step")


#Without standardizing:
mlasso<-lars(X, log(Time), type="lasso", normalize = FALSE, trace=TRUE)
summary.lars(mlasso)
coef(mlasso)
print(mlasso)

par(mfrow=c(1,1), mar=c(5,5,4,4), cex.axis=0.8)
plot.lars(mlasso, xvar="step")



### ********************####



#Step 2:  Transform it all.  First the X's:
X<-cbind(DArea,CCost,Dwgs,Length,Spans)
library(car)
tranx<-powerTransform(X)
summary(tranx)

hist(Length)
hist(log(Length))

#Then the response variable:

lm1<-lm(Time ~ log(DArea) + log(CCost)+log(Dwgs)+log(Length)+log(Spans))
trany<-powerTransform(lm1)
summary(trany)

#Step 3:  Fit our transformed model, and see whether our model is valid:
lm.logs<-lm(log(Time) ~ log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))
pairs(log(Time) ~ log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))

par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfrow=c(2,2))
plot(lm.logs)
add_conservative_cooks_line <- function(model){
  dc <- 4/model$df.residual
  h <- seq(0.01, max(hatvalues(model))*1.1, by = 0.01)
  r <- sqrt(length(model$coefficients) * dc * (1-h)/h)
  lines(h, r, col ='purple', lty='dotdash')
  lines(h, -r, col ='purple', lty='dotdash')
  #mtext(side = 4,las=1,at = c(max(hatvalues(model)), max(r))+1, cex=0.7, text = bquote(.(dc)))  
  #mtext(side = 4,las=1,at = c(max(hatvalues(model)), max(r))-1, cex=0.7, text = bquote(.(dc)))
}
add_conservative_cooks_line(lm.logs)


par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfrow=c(3,2))
plot(lm.logs$fitted, lm.logs$residuals)
plot(log(DArea), lm.logs$residuals)
plot(log(CCost), lm.logs$residuals)
plot(log(Dwgs), lm.logs$residuals)
plot(log(Length), lm.logs$residuals)
plot(log(Spans), lm.logs$residuals)

par(cex.axis=2,cex.lab=1.5, mar=c(5.2,5.2,2,2),lwd=2, pch=19, mfcol=c(3,2))
library(alr3)
mmps(lm.logs)

#Looks good; let's check out the regression output:
summary(lm.logs)
#Notice log(DArea) and log(Length) have the wrong sign!

#Look at the correlation between the predictor variables:
X<-cbind(log(DArea), log(CCost), log(Dwgs), log(Length), log(Spans))
cor(X)
#Remember this correlation matrix won't pick up on everything, e.g. x3 + x2 = x1 + x5.

#Added variable plots:
library(car)
par(cex.axis=1.5,cex.lab=1.5, mar=c(6,7,4,4),lwd=2, pch=19, mfcol=c(3,2))
avPlots(lm.logs)

#Variance Inflation Factors:
vif(lm.logs)
#Several larger than 5.  Use methods in Chapter 7.











