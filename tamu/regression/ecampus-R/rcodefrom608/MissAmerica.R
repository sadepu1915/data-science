MA<-read.csv("/Users/home/Documents/TAMU/stat608/Data/MissAmerica08.csv", header=TRUE)

attach(MA)
library(car)
library(alr3)

#Red is the smallest value, 0; orange is next, then along the rainbow to purple being the highest.
par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, mfrow=c(1,1))
plot(-MA$Longitude, MA$Latitude, col=rainbow(9)[MA$Top10+1], pch=19)

plot(MA$LogPopulation, MA$LogContestants, col=rainbow(9)[MA$Top10+1], pch=19)


#Preliminary plots:
par(mfrow=c(2,3), cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2)
plot(LogPopulation,Top10, col=rainbow(9)[MA$Top10+1], pch=19)
plot(LogContestants, Top10, col=rainbow(9)[MA$Top10+1], pch=19)
plot(LogTotalArea, Top10, col=rainbow(9)[MA$Top10+1], pch=19)
plot(Longitude, Top10, col=rainbow(9)[MA$Top10+1], pch=19)
plot(Latitude, Top10, col=rainbow(9)[MA$Top10+1], pch=19)


n<-dim(MA)[1]  #Sets sample size n to the number of rows in the data set MA.
mi<-rep(9, n)
fails<-mi - MA$Top10

m1<-glm(cbind(Top10, fails) ~ LogPopulation + LogContestants + LogTotalArea + Latitude + Longitude, family = binomial(), data = MA)
summary(m1)


#Create new data set of one row per state/year combo.
fulln <- sum(Top10) + sum(fails)
myx <- rep(NA, 6*fulln) #seq(1:(5*fulln))
fullMA <- matrix(myx, ncol=6, nrow=fulln, byrow=T)
k <- 1
for(i in 1:n){
  if(Top10[i] > 0){
    for(j in 1:Top10[i]){
      fullMA[k,1] <- 1
      fullMA[k,2] <- LogPopulation[i]
      fullMA[k,3] <- LogContestants[i]
      fullMA[k,4] <- LogTotalArea[i]
      fullMA[k,5] <- Latitude[i]
      fullMA[k,6] <- Longitude[i]
      k <- k+1
    }
  }
  if(fails[i]>0){
    for(m in 1:fails[i]){
      fullMA[k,1] <- 0
      fullMA[k,2] <- LogPopulation[i]
      fullMA[k,3] <- LogContestants[i]
      fullMA[k,4] <- LogTotalArea[i]
      fullMA[k,5] <- Latitude[i]
      fullMA[k,6] <- Longitude[i]
      k <- k+1
    }
  }
}

fullMA<- as.data.frame(fullMA, row.names=seq(1:fulln))
names(fullMA) <- c("y", "LogPop", "LogCon", "LogA", "Lat", "Long")
attach(fullMA)

mmps(m1)
####################################################
#Marginal Model Plots BY HAND. 
par(mfrow=c(3,2), pch=19)
#LogPopulation plot:
loessfit1 <- loess(y ~ LogPop,degree=1,span=2/3)
loessfit2 <- loess(m1$fitted.values ~ LogPopulation, degree=1, span=2/3)
xx <- seq(9.5,14.2,0.05)  #Figure this out by max and min of LogPopulation.
plot(fullMA$LogPop, fullMA$y, xlab="LogPopulation", ylab="Y, In Top10? (0=No, 1=Yes)")
lines(xx, predict(loessfit1, data.frame(LogPop = xx)))
lines(xx, predict(loessfit2, data.frame(LogPopulation = xx)), col=2, lty=2)

#Log Contestants:
loessfit1 <- loess(y ~ LogCon,degree=1,span=2/3)
loessfit2 <- loess(m1$fitted.values ~ LogContestants, degree=1, span=2/3)
xx <- seq(1.9,4.2,0.05)  #Figure this out by max and min of LogContestants
plot(fullMA$LogCon, fullMA$y, xlab="LogContestants", ylab="Y, In Top10? (0=No, 1=Yes)")
lines(xx, predict(loessfit1, data.frame(LogCon = xx)))
lines(xx, predict(loessfit2, data.frame(LogContestants = xx)), col=2, lty=2)

#Log Total Area:
loessfit1 <- loess(y ~ LogA,degree=1,span=2/3)
loessfit2 <- loess(m1$fitted.values ~ LogTotalArea, degree=1, span=2/3)
xx <- seq(4.2,13.5,0.05)  #Figure this out by max and min of LogTotalArea
plot(fullMA$LogA, fullMA$y, xlab="Log Total Area", ylab="Y, In Top10? (0=No, 1=Yes)")
lines(xx, predict(loessfit1, data.frame(LogA = xx)))
lines(xx, predict(loessfit2, data.frame(LogTotalArea = xx)), col=2, lty=2)

#Latitude:
loessfit1 <- loess(y ~ Lat,degree=1,span=2/3)
loessfit2 <- loess(m1$fitted.values ~ Latitude, degree=1, span=2/3)
xx <- seq(21.1,58.4,0.05)  #Figure this out by max and min of Latitude
plot(fullMA$Lat, fullMA$y, xlab="Latitude", ylab="Y, In Top10? (0=No, 1=Yes)")
lines(xx, predict(loessfit1, data.frame(Lat = xx)))
lines(xx, predict(loessfit2, data.frame(Latitude = xx)), col=2, lty=2)

#Longitude
loessfit1 <- loess(y ~ Long,degree=1,span=2/3)
loessfit2 <- loess(m1$fitted.values ~ Longitude, degree=1, span=2/3)
xx <- seq(69.5,158.1,0.05)  #Figure this out by max and min of Longitude
plot(fullMA$Long, fullMA$y, xlab="Longitude", ylab="Y, In Top10? (0=No, 1=Yes)")
lines(xx, predict(loessfit1, data.frame(Long = xx)))
lines(xx, predict(loessfit2, data.frame(Longitude = xx)), col=2, lty=2)
##################################################################


detach(MA)


mtest<-glm(cbind(Top10,fails) ~ LogPopulation + LogContestants, family=binomial(), data=MA)



MAYr<- read.csv("/Users/home/Documents/TAMU/stat608/Data/MissAmerica08Yrs.csv", header=TRUE)
attach(MAYr)
y<-Top10

#Do we need an interaction term for latitude / longitude?    
par(mar=c(5.1,5.1,2,2),mfrow=c(1,1), cex.axis=2,cex.lab=2)
plot(Longitude, Latitude, pch=y+1,col=y+1,xlab="Longitude",ylab="Latitude")
abline(lsfit(Longitude[y==0],Latitude[y==0]),lty=1,col=1)
abline(lsfit(Longitude[y==1],Latitude[y==1]),lty=2,col=2)
legend(14, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Top 10?")

#Do we need an interaction term for latitude / contestants?  
par(mar=c(5.1,5.1,2,2),mfrow=c(1,1), cex.axis=2,cex.lab=2)
plot(LogContestants, Latitude, pch=y+1,col=y+1,xlab="Log Contestants",ylab="Latitude")
abline(lsfit(LogContestants[y==0],Latitude[y==0]),lty=1,col=1)
abline(lsfit(LogContestants[y==1],Latitude[y==1]),lty=2,col=2)
legend(14, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Top 10?")

#Do we need an interaction term for latitude / population?  
par(mar=c(5.1,5.1,2,2),mfrow=c(1,1), cex.axis=2,cex.lab=2)
plot(LogPopulation, Latitude, pch=y+1,col=y+1,xlab="Log Population",ylab="Latitude")
abline(lsfit(LogPopulation[y==0],Latitude[y==0]),lty=1,col=1)
abline(lsfit(LogPopulation[y==1],Latitude[y==1]),lty=2,col=2)
legend(13, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Top 10?")

#Do we need an interaction term for latitude / totalarea?  
par(mar=c(5.1,5.1,2,2),mfrow=c(1,1), cex.axis=2,cex.lab=2)
plot(LogTotalArea, Latitude, pch=y+1,col=y+1,xlab="Log Total Area",ylab="Latitude")
abline(lsfit(LogTotalArea[y==0],Latitude[y==0]),lty=1,col=1)
abline(lsfit(LogTotalArea[y==1],Latitude[y==1]),lty=2,col=2)
legend(10, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Top 10?")


#Do we need an interaction term for longitude / contestants?  
par(mar=c(5.1,5.1,2,2),mfrow=c(1,1), cex.axis=2,cex.lab=2)
plot(LogContestants, Longitude, pch=y+1,col=y+1,xlab="Log Contestants",ylab="Longitude")
abline(lsfit(LogContestants[y==0],Longitude[y==0]),lty=1,col=1)
abline(lsfit(LogContestants[y==1],Longitude[y==1]),lty=2,col=2)
legend(14, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Top 10?")

#Do we need an interaction term for longitude / population? 
par(mar=c(5.1,5.1,2,2),mfrow=c(1,1), cex.axis=2,cex.lab=2)
plot(LogPopulation, Longitude, pch=y+1,col=y+1,xlab="Log Population",ylab="Longitude")
abline(lsfit(LogPopulation[y==0],Longitude[y==0]),lty=1,col=1)
abline(lsfit(LogPopulation[y==1],Longitude[y==1]),lty=2,col=2)
legend(14, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Top 10?")

#Do we need an interaction term for longitude / total area? 
par(mar=c(5.1,5.1,2,2),mfrow=c(1,1), cex.axis=2,cex.lab=2)
plot(LogTotalArea, Longitude, pch=y+1,col=y+1,xlab="Log Total Area",ylab="Longitude")
abline(lsfit(LogTotalArea[y==0],Longitude[y==0]),lty=1,col=1)
abline(lsfit(LogTotalArea[y==1],Longitude[y==1]),lty=2,col=2)
legend(14, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Top 10?")

#Do we need an interaction term for population/contestants? 
par(mar=c(5.1,5.1,2,2),mfrow=c(1,1), cex.axis=2,cex.lab=2)
plot(LogPopulation, LogContestants, pch=y+3,col=y+1,xlab="Log Population",ylab="Log Contestants")
abline(lsfit(LogPopulation[y==0],LogContestants[y==0]),lty=1,col=1)
abline(lsfit(LogPopulation[y==1],LogContestants[y==1]),lty=2,col=2)
legend(13, 2.5,legend=c("No","Yes"),pch=3:4,col=1:2,lty=1:2,title="In Top 10?")


#Do we need an interaction term for population/totalarea?  
par(mar=c(5.1,5.1,2,2),mfrow=c(1,1), cex.axis=2,cex.lab=2)
plot(LogPopulation, LogTotalArea, pch=y+1,col=y+1,xlab="Log Population",ylab="Log TotalArea")
abline(lsfit(LogPopulation[y==0],LogTotalArea[y==0]),lty=1,col=1)
abline(lsfit(LogPopulation[y==1],LogTotalArea[y==1]),lty=2,col=2)
legend(14, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Top 10?")

#Do we need an interaction term for contestants/totalarea?  
par(mar=c(5.1,5.1,2,2),mfrow=c(1,1), cex.axis=2,cex.lab=2)
plot(LogContestants, LogTotalArea, pch=y+1,col=y+1,xlab="Log Contestants",ylab="Log TotalArea")
abline(lsfit(LogContestants[y==0],LogTotalArea[y==0]),lty=1,col=1)
abline(lsfit(LogContestants[y==1],LogTotalArea[y==1]),lty=2,col=2)
legend(14, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Top 10?")

detach(MAYr)


attach(MA)
n<-dim(MA)[1] 
#loglat<-log(Latitude)
mi<-rep(9, n)
fails<-mi - MA$Top10

m3<-glm(cbind(Top10, fails) ~ LogPopulation + LogContestants + LogTotalArea + Latitude + Longitude 
   + Latitude:Longitude + Latitude:LogPopulation + Latitude:LogTotalArea + Longitude:LogContestants
   + LogPopulation:LogTotalArea + LogContestants:LogTotalArea, family = binomial(), data = MA)


#Run through marginal model plots again.


#Remember: residual plots are problematic (two C-shapes) when the data are binary!
par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19,mfrow=c(3,2))
stanresDeviance <- residuals(m3)/sqrt(1-hvalues)
plot(LogPopulation,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Log Population", pch=19)
plot(LogContestants,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Log Contestants", pch=19)
identify(LogContestants,stanresDeviance, MA$abbreviation)
plot(LogTotalArea,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Log Total Area", pch=19)
identify(LogTotalArea,stanresDeviance, MA$abbreviation)
plot(Latitude,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Latitude", pch=19)
identify(Latitude,stanresDeviance, MA$abbreviation)
plot(Longitude,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Longitude", pch=19)
identify(Longitude,stanresDeviance, MA$abbreviation)

#Leverage vs. Deviance Resid plot
#When y is binary, this plot is V-shaped!
par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2)
hvalues <- influence(m3)$hat
plot(hvalues,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Leverage Values", pch=19)
abline(v=6/n,lty=2)
identify(hvalues,stanresDeviance, MA$abbreviation)



