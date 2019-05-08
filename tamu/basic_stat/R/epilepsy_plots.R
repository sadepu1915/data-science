par(ask=TRUE)

x = read.csv("u:\\meth1\\Rfiles\\epilpsy.csv",header=TRUE)
attach(x)

postscript("u:/meth1/psfiles/epsBoxBase.ps",horizontal=TRUE)

boxplot(split(Base,Trt),ylab="Base Seizure Count",xlab="Treatment",
main="Boxplots for Epilepsy Study")

postscript("u:/meth1/psfiles/epsBoxAge.ps",horizontal=TRUE)

boxplot(split(Age,Trt),ylab="Age of Patient",xlab="Treatment",
main="Boxplots for Epilepsy Study")

Y <- cbind(Y1,Y2,Y3,Y4,Base,Age)
c = cor(Y)
c = round(c,3)

postscript("u:/meth1/psfiles/epsMatrix1.ps",horizontal=FALSE)

pairs(~Y1+Y2+Y3+Y4+Trt+Base+Age)

postscript("u:/meth1/psfiles/epsMatrix2.ps",horizontal=FALSE)

pairs(~Y1+Y2+Y3+Y4+Base)

postscript("u:/meth1/psfiles/RegY1Base.ps",horizontal=FALSE)

#Regression Plot of Y1 vs Base
m1 = lm(Y1~Base+I(Base^2)+I(Base^3))
summary(m1)
plot(Base,Y1,main="Seizures in Epilepsy Study (RSqAdj = .828)",xlab="Base Seizure Counts",
ylab="Y1 = Seizure Counts -  First 8 weeks Period",pch=as.numeric(Trt))
legend(20,98,c("Placebo","Treated"),pch=0:1)
av=seq(0,152,.1)
bv = predict(m1,list(Base=av))
lines(av,bv)

postscript("u:/meth1/psfiles/RegY1Age.ps",horizontal=FALSE)

#Regression Plot of Y1 vs Age
m2 = lm(Y1~Age+I(Age^2)+I(Age^3))
summary(m2)
plot(Age,Y1,main="Seizures in Epilepsy Study  (RSqAdj = 0)",xlab="Age of Patient",
ylab="Y1 = Seizure Counts - First 8 weeks Period",pch=as.numeric(Trt))
legend(36,98,c("Placebo","Treated"),pch=0:1)
av=seq(0,42,.1)
bv = predict(m2,list(Age=av))
lines(av,bv)

postscript("u:/meth1/psfiles/RegY1BaseWO.ps",horizontal=FALSE)

#Regression Plot of Y1 vs Base with outlier removed
m3 = update(m1,subset=(1:59)[-c(49)])
summary(m3)
plot(Base[-c(49)],Y1[-c(49)],main="Seizures in Epilepsy Study Without Outlier  
(RSqAdj = .449)",xlab="Base Seizure Counts",
ylab="Y1 = Seizure Counts - First 8 weeks Period",pch=as.numeric(Trt))
legend(13,38,c("Placebo","Treated"),pch=0:1)
av=seq(0,112,.1)
bv = predict(m1,list(Base=av))
lines(av,bv)

postscript("u:/meth1/psfiles/RegY1AgeWO.ps",horizontal=FALSE)

#Regression Plot of Y1 vs Age with outlier removed
m4 = update(m2,subset=(1:59)[-c(49)])
summary(m4)
plot(Age[-c(49)],Y1[-c(49)],main="Seizures in Epilepsy Study Without Outlier  
(RSqAdj = .026)",xlab="Age of Patient",
ylab="Seizure Counts - First 8 wks",pch=as.numeric(Trt))
legend(18,38,c("Placebo","Treated"),pch=0:1)
av=seq(0,42,.1)
bv = predict(m4,list(Age=av))
lines(av,bv)

postscript("u:/meth1/psfiles/RegY4Base.ps",horizontal=FALSE)

#Regression Plot of Y4 vs Base
m5 = lm(Y4~Base+I(Base^2)+I(Base^3))
summary(m5)
plot(Base,Y4,main="Seizures in Epilepsy Study  (RSqAdj = .812)",xlab="Base Seizure Counts",
ylab="Y4 = Seizure Counts - Fourth 8 weeks Period",pch=as.numeric(Trt))
legend(20,60,c("Placebo","Treated"),pch=0:1)
av=seq(0,151,.1)
bv = predict(m5,list(Base=av))
lines(av,bv)

postscript("u:/meth1/psfiles/RegY4Age.ps",horizontal=FALSE)

#Regression Plot of Y4 vs Age
m6 = lm(Y4~Age+I(Age^2)+I(Age^3))
summary(m6)
plot(Age,Y4,main="Seizures in Epilepsy Study  (RSqAdj = 0)",xlab="Age of Patient",
ylab="Y4 = Seizure Counts - Fourth 8 weeks Period",pch=as.numeric(Trt))
legend(36,98,c("Placebo","Treated"),pch=0:1)
av=seq(0,42,.1)
bv = predict(m6,list(Age=av))
lines(av,bv)

postscript("u:/meth1/psfiles/RegY4BaseWO.ps",horizontal=FALSE)

#Regression Plot of Y4 vs Base with outlier removed
m7 = update(m5,subset=(1:59)[-c(49)])
summary(m7)
plot(Base[-c(49)],Y4[-c(49)],main="Seizures in Epilepsy Study Without Outlier 
(RSqAdj = .551)",xlab="Base Seizure Counts",
ylab="Y4 = Seizure Counts - Fourth 8 wks",pch=as.numeric(Trt))
legend(13,28,c("Placebo","Treated"),pch=0:1)
av=seq(0,112,.1)
bv = predict(m7,list(Base=av))
lines(av,bv)

postscript("u:/meth1/psfiles/RegY4AgeWO.ps",horizontal=FALSE)

#Regression Plot of Y4 vs Age with outlier removed
m8 = update(m6,subset=(1:59)[-c(49)])
summary(m8)
plot(Age[-c(49)],Y4[-c(49)],main="Seizures in Epilepsy Study Without Outlier 
(RSqAdj = 0)",xlab="Age of Patient",
ylab="Y4 = Seizure Counts - Fourth 8 wks",pch=as.numeric(Trt))
legend(20,38,c("Placebo","Treated"),pch=0:1)
av=seq(0,42,.1)
bv = predict(m8,list(Age=av))
lines(av,bv)

graphics.off()
