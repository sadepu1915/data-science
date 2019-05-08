temp=runif(10000)
hist(-log(temp),nclass=40,freq=FALSE,main="Histogram of -log of Uniform RVS")
x=seq(0,8,length=10001)
lines(x,exp(-x))

temp=rnorm(10000)
hist(temp^2,nclass=80,freq=FALSE,main="Histogram of Squares of N(0,1) RVs")
lines(x,dchisq(x,1))

