temp=rep(0,10000)
n=100

for (i in 1:10000)temp[i]=mean(rexp(n,rate=2))
hist(temp,main=paste("Simulation of 10000 Means with Rate=2, n=",n),nclass=40,freq=FALSE)
x=seq(min(temp),max(temp),length=10001)
lines(x,dnorm(x,mean=0.5,sd=0.5/sqrt(n)),col=2)
sum(temp<0.5+1.96*0.5/sqrt(n)&temp>0.5-1.96*0.5/sqrt(n))/10000

for (i in 1:10000)temp[i]=1/mean(rexp(n,rate=2))
hist(temp,main=paste("Simulation of 10000 Estimates with Rate=2, n=",n),nclass=40,freq=FALSE)
x=seq(min(temp),max(temp),length=10001)
lines(x,dnorm(x,mean=2,sd=2/sqrt(n)),col=2)

sum(temp<2+1.96*2/sqrt(n) &temp>2-1.96*2/sqrt(n))/10000
mean(temp)
var(temp)
mean((temp-2)^2)
