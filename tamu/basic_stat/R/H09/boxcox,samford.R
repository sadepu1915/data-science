x <- scan("/home/stat/longneck/meth1/sfiles/ozone1.DAT")
n <- 136
y <- abs(x)
ly <- log(y)
s <- sum(ly)

yt0 <- log(x)
varyt0 <- var(yt0)
Lt0 <- -1*s - .5*n*(log(6.28*varyt0)+1)
th <- 0
Lt <- 0
t <- -2.01
i <- 0
while(t < 2)
{t <- t+.001
i <- i+1
th[i] <- t
yt <- (x^t -1)/t
varyt <- var(yt)
Lt[i] <- (t-1)*s - .5*n*(log(6.28*varyt)+1)
if(abs(th[i])<1.0e-10)Lt[i]<-Lt0
if(abs(th[i])<1.0e-10)th[i]<-0
}
# The following outputs the values of the likelihood and theta and yields
# the value of theta where likelihood is a maximum
out <- cbind(th,Lt)
Ltmax<- max(Lt)
imax<- which(Lt==max(Lt))
thmax<- th[imax]

postscript("boxcox,plot.ps",height=8,horizontal=F)

plot(th,Lt,lab=c(30,50,7),main="Box-Cox Transformations",
           xlab="Theta",
           ylab="Objective Function, Lt(Theta)")

#the following plots a 95\% c.i. for theta

cic <- Ltmax-.5*qchisq(.95,1)  

del<- .01
iLtci <- which(abs(Li-cic)<=del)
iLtciL<- min(iLtci)
iLtciU<- max(iLtci)
thLci<- th[iLtciL]
thUci<- th[iLtciU]
abline(h=cic)
abline(v=thLci)
abline(v=thUci)

par(mfrow=c(1,1))

postscript("boxcox_ozone.ps",height=8,horizontal=F)

qqnorm(x,main="Normal Prob Plots of Samford Ozone Data",
                 xlab="normal quantiles",ylab="ozone concentration",cex=.65)
qqline(x)
text(-2,200,"SW=.9288")
text(-2,190,"p-value=0")

y1<- log(x)
y2<- x^.23
y3<- x^.5
s <- shapiro.test(x)
s1 <- shapiro.test(y1)
s2 <- shapiro.test(y2)
s3 <- shapiro.test(y3)
qqnorm(y2,main="Normal Prob Plots of Samford Ozone Data with (Ozone)^.23",
                 xlab="normal quantiles",ylab="(Ozone)^.23",cex=.65)
          qqline(y2)
text(-2,10,"SW=.9872")
text(-2,9.7,"p-value=.2382")


qqnorm(y1,main="Normal Prob Plots of Samford Ozone Data with Log(Ozone)",
                 xlab="normal quantiles",ylab="Ln(Ozone)",cex=.65)
          qqline(y1)
text(-2,5.0,"SW=.9806")
text(-2,4.85,"p-value=.0501")



qqnorm(y3,main="Normal Prob Plots of Samford Ozone Data with SQRT(Ozone)",
                 xlab="normal quantiles",ylab="(Ozone)^.5",cex=.65)
          qqline(y3)
text(-2,25,"SW=.9789")
text(-2,24,"p-value=.0501")

graphics.off()
