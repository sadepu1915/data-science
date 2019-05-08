bridge<-read.csv("/Users/home/Documents/TAMU/stat608/Data/bridge.csv", header=TRUE)
library(MASS)
library(car)
library(leaps)
library(My.stepwise)
library(lars)
attach(bridge)


#Backward Selection Using P-values by hand:
full<-lm(log(Time) ~ log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))
summary(full)

m1<-update(full, .~. - log(Length))
summary(m1)

m2<-update(m1, .~. - log(DArea))
summary(m2)

m3<-update(m2, .~. - log(CCost))
summary(m3)

#Final model: log(Time) ~ log(Dwgs) + log(Spans) 


##########################################################
#Forward selection by hand:
nothing<-lm(log(Time) ~ 1)
addterm(nothing, scope=full, test="F")

m1<-lm(log(Time)~log(Dwgs))
addterm(m1, scope=full, test="F")

m2<-lm(log(Time)~log(Dwgs) + log(Spans))
addterm(m2, scope=full, test="F")

#Final model: log(Time) ~ log(Dwgs) + log(Spans) 


###########################################################
#Stepwise:  Use alpha to enter = 0.20, alpha to remove = 0.20
nothing<-lm(log(Time)~1)
addterm(nothing, scope=full, test="F")  #Forward Step

m1<-lm(log(Time)~log(Dwgs))
addterm(m1, scope=full, test="F")  #Forward Step

m2<-lm(log(Time)~log(Dwgs) + log(Spans))  #Backward Step?
summary(m2)

addterm(m2, scope=full, test="F")  #Forward Step

#Final model would be log(Dwgs) and log(Spans) if we used alpha = 0.05, but if we use 0.2:
m3 <- lm(log(Time)~log(Dwgs) + log(Spans) + log(CCost))
summary(m3)  #Try to take a backward step

addterm(m3, scope=full, test="F")  #Try to add another variable
#That's where we stop.


#Using a package instead of doing stepwise by hand: (Set sls=1 to simply do forward selection.)
lTime <- log(Time)
lDArea <- log(DArea)
lCCost <- log(CCost)
lDwgs <- log(Dwgs)
lLength <- log(Length)
lSpans <- log(Spans)
My.stepwise.lm(Y = "lTime", variable.list= c("lDArea", "lCCost", "lDwgs", "lLength", "lSpans"), data=bridge, sle=0.05, sls=0.05)



#Remember: selection based on p-values, AIC, BIC, and AICC choose the same 3-variable model; 
#the stopping criterion is what changes.
#For forward selection based on AIC, we would have added log(CCost):

m4 <- lm(log(Time)~log(Dwgs) + log(Spans) + log(CCost) + log(DArea))

extractAIC(m1, k=2)
extractAIC(m2, k=2)
extractAIC(m3,k=2)
extractAIC(m4,k=2)











##############################################################
#All Subsets:  (DON'T MISSPELL AS "SUBSET")
#Setting up full design matrix for later use:
X.labels<-c("LDArea", "LLength", "LDwgs", "LSpans", "LCCost")
X<-matrix(cbind(log(DArea), log(Length), log(Dwgs),log(Spans), log(CCost)), 
            nrow=45, ncol=5, dimnames=list(c(1:45),X.labels))
b<-regsubsets(X, log(Time))
summary(b)


par(cex.axis=2, cex.lab=2, mar=c(5,5,2,2))
subsets(b, statistic=c("adjr2"))


par(cex.axis=2, cex.lab=2, mar=c(5,5,2,2))
subsets(b, statistic=c("bic"))


all.subs<-leaps(X, log(Time), method=c("adjr2"), nbest=1, names=X.labels)
all.subs$size #Notice this gives the number of parameters, not the number of variables.
all.subs$which
all.subs$adjr2

om1<-lm(log(Time) ~ log(Dwgs))
om2<-lm(log(Time) ~ log(Dwgs) + log(Spans))
om3<-lm(log(Time) ~ log(Dwgs) + log(Spans) + log(CCost))
om4<-lm(log(Time) ~ log(Dwgs) + log(Spans) + log(CCost) + log(DArea))
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



