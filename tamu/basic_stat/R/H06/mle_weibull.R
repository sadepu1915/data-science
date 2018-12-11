t = c(15.321,9.008,20.104,7.729,45.154,8.404,5.332,0.577,4.305,4.517,12.594,
     6.829,3.291,37.175,0.841,1.317,7.613,20.582,2.030,10.001,4.666,12.933,
     0.591,39.454,8.875)
tcensored = rep(1,25)
t = sort(t)
lt = log(t)
n = length(t)
i = seq(1,25,1)
u = (i-.5)/n
x =  log(-log(1-u))

#postscript("u:/meth1/psfiles/WeibullRefplot.ps",height=8,horizontal=F)

plot(x,lt, xlab="Log(-Log(1-u))",ylab="Log of Time to Failure",lab=c(13,11,7),
main="Weibull Reference Distribution Plot")
abline(lm(lt~x))
text(-3,3,"Equation of line:")
text(-1.7,3,"y=2.414+.952*x")

#graphics.off()

#postscript("u:/meth1/Rfiles/mle_exp.ps",height=8,horizontal=F)

b = seq(3,25,.01)
LK = (b^(-25))*exp(-289.243/b)*(10)^38
out =  cbind(b,LK)
LKmax  =  max(LK)
bmax  =  which(LK==max(LK))
MLE  =  b[bmax]
plot(b,LK, type="l",xlab="",ylab="",axes=F)
axis(2,at=seq(0,4,.5))
axis(1,at=seq(0,25,1))
plot(b,LK, type="l",xlab="Beta",ylab="Likelihood Function (10^(-38))",lab=c(30,16,7))
title("Likelihood Function for Exponential Model",xlab="Beta",ylab="Likelihood Function (10**(-38))")
#graphics.off()

library(MASS)
mle_exp=fitdistr(t,"exponential")
mle_weibull=fitdistr(t,"weibull")

library(survival)
mle_survreg = survreg(Surv(t,tcensored) ~ 1, dist = 'weibull')


