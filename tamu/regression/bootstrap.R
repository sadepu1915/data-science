
students<-read.csv("/Users/Home/Documents/TAMU/stat608/Data/studentmath.csv",header=TRUE)

x1 <- students$goout
x2 <- students$health
y <- students$G3    #Final Exam scores
n <- length(y)

students.lm <- lm(y ~ x1 + x2)

par(cex=1.5, pch=19, mfrow=c(1,1), mar=c(5,5,1,1))
plot(x1, y, xlab="Going out with friends", ylab="Final Exam Score")
abline(students.lm)

par(cex=1.3, pch=19, mfrow=c(2,2))
plot(students.lm)

#Original method for finding CIs:
lb <- students.lm$coefficients - qt(0.975, (n-3)) * coef(summary(students.lm))[,"Std. Error"]
ub <- students.lm$coefficients + qt(0.975, (n-3)) * coef(summary(students.lm))[,"Std. Error"]
oldcis <- cbind(lb, ub)

#Now bootstrap the CIs:
resids <- students.lm$residuals
yhats <- students.lm$fitted.values
origbetas <- students.lm$coefficients
nreps <- 1000
bootbetas <- matrix(rep(NA, nreps*3), nrow=nreps)
for(i in 1:nreps){
  newy <- yhats + sample(resids, replace=TRUE)
  newmodel <- lm(newy ~ x1 + x2)
  bootbetas[i,] <- newmodel$coefficients
}
par(cex=1.3, mfrow=c(2,1))
hist(bootbetas[,2])  #slope for going out
hist(bootbetas[,3])

sort.intercepts <- sort(bootbetas[,1])
sort.goout <- sort(bootbetas[,2])
sort.health <- sort(bootbetas[,3])
newcis <- matrix(rep(NA,6),nrow=3)
newcis[1,1] <- sort.intercepts[25]
newcis[1,2] <- sort.intercepts[975]
newcis[2,1] <- sort.goout[25]
newcis[2,2] <- sort.goout[975]
newcis[3,1] <- sort.health[25]
newcis[3,2] <- sort.health[975]


#Hypothesis test for slopes and intercept:
nreps <- 1000
bootbetas <- matrix(rep(NA, nreps*3), nrow=nreps)
for(i in 1:nreps){
  newy <- sample(resids, replace=TRUE)
  newmodel <- lm(newy ~ x1 + x2)
  bootbetas[i,] <- newmodel$coefficients
}

par(cex=1.3, pch=19, mfrow=c(2,2))
hist(bootbetas[,1], xlab="Intercepts")
hist(bootbetas[,2], xlab="Going out slopes")
hist(bootbetas[,3], xlab="Health slopes")

#Two-sided p-values without assuming symmetry: 
p.val.int <- (length(which(bootbetas[,1] >= 12.8551)) + length(which(bootbetas[,1] <= -12.8551))) / nreps
p.val.goout <- (length(which(bootbetas[,2] >= 0.5489)) + length(which(bootbetas[,2] <= -0.5489))) / nreps
p.val.health <- (length(which(bootbetas[,3] >= 0.2063)) + length(which(bootbetas[,3] <= -0.2063))) / nreps




#############################################
#Prediction and confidence intervals for y at x*:

#Original method for PI:
hist(resids)  #Left tail looks terrible from the zeroes.
xnew <- data.frame(x1=3, x2=3)  #Say you want to predict at go.out = 3 and health=3 (both on 1-5 scales).
predict(students.lm, xnew, interval="prediction")   #Wrong, since residuals are not normal at all.  Better bootstrap!


#Bootstrap:
resids <- students.lm$residuals
nreps <- 1000
bootystars <- rep(NA, nreps*3)
for(i in 1:nreps){
  newy <- yhats + sample(resids, replace=TRUE)
  newmodel <- lm(newy ~ x1 + x2)
  bootystars[i] <- predict(newmodel, data.frame(x1=3,x2=3)) + sample(resids, size=1)
}

par(cex=1.3, mfrow=c(2,1))
hist(bootystars)
hist(resids)

sorted <- sort(bootystars)
sorted[25]
sorted[975]


