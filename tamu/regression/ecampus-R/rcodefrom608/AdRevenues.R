#I saved this file as tab delimited, since some of the names of 
#companies and magazines had commas in them.
library(alr3)

ads<- read.delim("/Users/Home/Documents/TAMU/stat608/Data/AdRevenue.txt", sep = "\t", header=TRUE)
attach(ads)

#Step 1:  Explore
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,2,1)) 
hist(Circulation, xlab="Circulation")
hist(AdRevenue, xlab="Ad Revenue")


par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,3,3))
sj <- bw.SJ(Circulation,lower = 0.05, upper = 100)
plot(density(Circulation,bw=sj,kern="gaussian"),type="l",
main="Gaussian kernel density estimate",xlab="Circulation")

sj <- bw.SJ(AdRevenue,lower = 1, upper = 1000)
plot(density(AdRevenue,bw=sj,kern="gaussian"),type="l",
main="Gaussian kernel density estimate",xlab="Ad Revenue")



my.lm<-lm(AdRevenue ~ Circulation)

plot(Circulation, AdRevenue, pch=19, xlab="Circulation", ylab="Ad Revenue")
abline(my.lm)


par(cex.main=2, cex.axis = 2, cex.lab=2, pch=19, mfrow=c(2,2))
plot(my.lm)


#Step 2:  Transformations
par(cex.main=2, cex.axis = 2, cex.lab=2, pch=19, mfrow=c(1,1))
x1<-log(Circulation)
plot(x1, AdRevenue, pch=19, xlab="log(Circulation)", ylab="Ad Revenue")

y1<-log(AdRevenue)
m1<-lm(y1 ~ x1)
plot(x1, y1, pch=19, xlab="log(Circulation)", ylab="log(Ad Revenue)")
abline(m1)

par(cex.main=2, cex.axis = 2, cex.lab=2, pch=19, mfrow=c(2,2))
plot(m1)


#Leverage plot for this model:
cd<-cooks.distance(m1)
par(mar=c(5,5,3,3), cex.axis=1.5,cex.lab=1.5,cex.main=2,pch=19, mfrow=c(1,1))
plot(x1, cd,  xlab="log(Circulation)", ylab="Cook's Distance")
abline(h=(4/68), lty=2)



#Let's pick the transformations.  First, try everything without transforming x:
#Inverse Response Plot Approach:  Look for good transformations for Y:
inverse.response.plot(my.lm, lam=c(-1, 0, 0.33, 0.5, 1, 1.5, 2))
?inverse.response.plot
#Okay, fine.  I won't use that function.
par(mar=c(5,5,3,3), cex.axis=1.5,cex.lab=1.5,cex.main=2,pch=19, mfrow=c(1,1))
inverseResponsePlot(my.lm, lam=c(-1, 0.33, 0.5, 1, 1.5, 2))

#The CAR library stands for "Companion to Applied Regression"
library(car)
invResPlot(my.lm, lam=c(-0.5, 0, 0.5, 1, 1.5, 2), xlab="Ad Revenue")


#Compare results to Box-Cox:
boxCox(my.lm)
#Rather different results for lambda from the two different methods.
bc <- powerTransform(my.lm)  #This gives numeric Box-Cox output
summary(bc)
testTransform(bc, 1.43)




#We should really transform X, too.  Let's just assume we know the log transformation for x:
m2 <- lm(AdRevenue ~ x1)  #Remember x1 is log circulation
inverseResponsePlot(m2, lam=c(-1, 0, 0.33, 0.5, 1, 1.5, 2))
#Now we choose log!


#Box-Cox Approach 1 in the textbook:  
#Transform X first, then transform Y.

my.bc <- powerTransform(Circulation)  
summary(my.bc)
#If the variable being transformed has zeroes, using family="yjPower" will add one before transforming.

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,1,3)) 
x2<-Circulation^(-1/3)
hist(x2)
sj <- bw.SJ(x2,lower = 0.005, upper = 2)
plot(density(x2,bw=sj,kern="gaussian"),type="l",
main="Gaussian kernel density estimate",xlab=expression(Circulation^-0.33))

m3<-lm(AdRevenue ~ x2)
boxCox(m3, lambda=seq(-2,2,0.1))
bc <- powerTransform(m3)
summary(bc)

y2<-(AdRevenue)^(-0.5)

m4<-lm(y2~x2)

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,1,3)) 
plot(x2, y2, pch=19, xlab="Circulation^-0.33", ylab="Ad Revenue^-0.5")
abline(m4)

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,1,3), mfrow = c(2,2), pch=19) 
plot(m4)
#Compare to original choice of log transformations:
plot(m1)

#Leverage plot for this model:
cd<-cooks.distance(m4)
par(mar=c(5,5,1,1), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow = c(1,1)) 
plot(x2, cd,  xlab=expression(Circulation^-0.33), ylab="Cook's Distance")
abline(h=(4/68), lty=2)
identify(x2,cd,ads$Magazine)


cd<-cooks.distance(m1)
par(mar=c(5,5,1,1), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow = c(1,1)) 
plot(x1, cd,  xlab=expression(Circulation^-0.33), ylab="Cook's Distance")
abline(h=(4/68), lty=2)
identify(x1,cd,ads$Magazine)


#Compare to another model:
cub.x<-ads$Circulation
cub.x2<-ads$Circulation^2
cub.x3<-ads$Circulation^3
lm.cubic<-lm(ads$AdRevenue ~ cub.x+cub.x2+cub.x3 )

par(mar=c(5,5,2,2), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow=c(2,2))
plot(lm.cubic)  #Doesn't run for models with more variables.

cd<-cooks.distance(lm.cubic)
par(mar=c(10,5,3,3), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow=c(1,1))
plot(x2, cd,  xlab=expression(Circulation^-0.33), ylab="Cook's Distance")
abline(h=(4/68), lty=2)



#Be careful comparing R-squared with different transformations!
summary(m4)
summary(lm.cubic)







