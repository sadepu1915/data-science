
esp<-read.csv("C:/Users/Madhavi/Documents/MS/608/ecampus-R/overdue.csv", header=TRUE)
attach(esp)
plot(esp)







# reduced
m3<-lm(EX ~ MET)
plot(m3)
anova(m3)


#Full
m1<-lm(EX ~ ECAB+GROW+YOUNG+OLD+WEST+MET^2)
anova(m1)
summary(m1)


m2<-lm(EX ~ ECAB+GROW+WEST+MET^2)
anova(m2)
summary(m2)



anova(m3, m1)  #anova(reduced, full)
#By hand, just to illustrate:
F.num <- (120.101 - 81.731)/(48 - 46)
F.denom <- 81.731 / 46
F <- F.num / F.denom
p.value <- 1 - pf(F, 2, 46)
F
p.value


#Using Matrix Notation:
library(car)
A.vec <-c(0, 0, 1, 0,
          0, 0, 0, 1)
A<-matrix(A.vec, nrow = 2, byrow=T)
linearHypothesis(model=m1, hypothesis.matrix=A, test="F")







#Chapter 6:  Marginal Model Plots
library(alr3)
m1<-lm(Food ~ Service + Cost + Decor)
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,2,2), pch=19)
mmp(m1,Service)
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,2,3), pch=19)
mmp(m1, Decor)
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,2,3), pch=19)
mmp(m1,Cost)



