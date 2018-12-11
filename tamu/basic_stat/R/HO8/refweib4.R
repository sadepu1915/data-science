#The following Splus program generates data from various specified distributions
#and the plots the generated data various various reference distributions.
# This program is ~longneck/meth1/refdist.S
#-------------------------------------------------------------------------------

#generates 5000 observations from weibull with scale=.5 and shape=2,
#weibull with scale=.5 and shape=2,weibull with scale=2 and shape=10,

wei1 <- rweibull(5000,2,.5)
wei2 <- rweibull(5000,10,2)
wei3 <- rweibull(5000,10,.5)
wei4 <- rweibull(5000,2,10)


#postscript("u:/meth1/Rfiles/refwei.ps",height=8,horizontal=F)

#par(mfrow=c(2,2))

qqplot(wei1,wei2,
       main="Weibull(2,.5) vs Weibull(10,2)",cex=.75,
       xlab="Weibull(2,.5)",
       ylab="Weibull(10,2)")
qqplot(wei1,wei3,
       main="Weibull(2,.5) vs Weibull(10,.5)",cex=.75,
       xlab="Weibull(2,.5)",
       ylab="Weibull(10,.5)")
qqplot(wei1,wei4,
       main="Weibull(2,.5) vs Weibull(2,10)",cex=.75,
       xlab="Weibull(2,.5)",
       ylab="Weibull(2,10)")
qqplot(wei2,wei3,
       main="Weibull(10,2) vs Weibull(10,.5)",cex=.75,
       xlab="Weibull(10,2)",
       ylab="Weibull(10,.5)")


#graphics.off()

