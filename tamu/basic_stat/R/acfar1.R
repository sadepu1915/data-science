#The following R code produces acf plots from an AR(1).
#The file is contained in ~longneck/meth1/sfiles/simulateacf.s
#------------------------------------------------------------------------

postscript("u:/meth1/psfiles/plotacf_r6.ps",height=7,horizontal=F)          


#Plot acf for an AR(1):

k<- seq(1:20)

acf1 <- (.9)^k
acf2 <- (.6)^k
acf3 <- (.3)^k
acf4 <- (.1)^k

plot(k,acf2,type="p",ylab="Rho_k",xlab="Lag k",
                   main="ACF for AR(1) with rho=.6",lab=c(20,10,4))

postscript("u:/meth1/psfiles/plotacf_r9.ps",height=7,horizontal=F)          



plot(k,acf1,type="p",ylab="Rho_k",xlab="Lag k",
                   main="ACF for AR(1) with rho=.9",lab=c(20,10,4))

#create ar(1) rho=.6
 P <- 2000
 mu <- 12
 sigma<- 2
 rho<- .6
 e <- rep(0,P)
 Y <- rep(0,P)
 X <- rep(0,P-1700)
 e <- rnorm(P,0,1)
 Y[1] <- mu+sigma*e[1]
 for(i in 2:P)

 Y[i] <- mu+rho*Y[i-1]+e[i]
 for(j in 1:300)
 X[j] <- Y[j+1700]

postscript("u:/meth1/psfiles/plotts_r6.ps",height=7,horizontal=F)          


 plot.ts(X,ylab="Y_t",xlab="Time t",
                   main="Time Series for AR(1) with rho=.6")
 abline(h=30)

# acf(X,ylab="Rho_k",xlab="Lag k",
#                   main="ACF for AR(1) with rho=.6")

#create ar(1) rho=.9
 P <- 2000
 mu <- 12
 sigma<- 2
 rho<- .9
 e <- rep(0,P)
 Y <- rep(0,P)
 X <- rep(0,P-1700)
 e <- rnorm(P,0,1)
 Y[1] <- mu+sigma*e[1]
 for(i in 2:P)
 Y[i] <- mu+rho*Y[i-1]+e[i]
 for(j in 1:300)
 X[j] <- Y[j+1700]

postscript("u:/meth1/psfiles/plotts_r9.ps",height=7,horizontal=F)          


 plot.ts(X,ylab="Y_t",xlab="Time t",
                   main="Time Series for AR(1) with rho=.9")
abline(h=120)
# acf(X,ylab="Rho_k",xlab="Lag k",
#                   main="ACF for AR(1) with rho=.9")
 



postscript("u:/meth1/psfiles/plotacf_r3.ps",height=7,horizontal=F)          


plot(k,acf3,type="p",ylab="Rho_k",xlab="Lag k",
                   main="ACF for AR(1) with rho=.3",lab=c(20,10,4))

postscript("u:/meth1/psfiles/plotacf_r1.ps",height=7,horizontal=F)          


plot(k,acf4,type="p",ylab="Rho_k",xlab="Lag k",
                   main="ACF for AR(1) with rho=.1",lab=c(20,10,4))
abline(h=13.3)


#create ar(1) rho=.3
 P <- 2000
 mu <- 12
 sigma<- 2
 rho<- .3
 e <- rep(0,P)
 Y <- rep(0,P)
 X <- rep(0,P-1700)
 e <- rnorm(P,0,1)
 Y[1] <- mu+sigma*e[1]
 for(i in 2:P)
 Y[i] <- mu+rho*Y[i-1]+e[i]
 for(j in 1:300)
 X[j] <- Y[j+1700]

postscript("u:/meth1/psfiles/plotts_r3.ps",height=7,horizontal=F)          


 plot.ts(X,ylab="Y_t",xlab="Time t",
                   main="Time Series for AR(1) with rho=.3")

abline(h=17.1)
# acf(X,ylab="Rho_k",xlab="Lag k",
#                   main="ACF for AR(1) with rho=.3")
 
#create ar(1) rho=.1
 P <- 2000
 mu <- 12
 sigma<- 2
 rho<- .1
 e <- rep(0,P)
 Y <- rep(0,P)
 X <- rep(0,P-1700)
 e <- rnorm(P,0,1)
 Y[1] <- mu+sigma*e[1]
 for(i in 2:P)
 Y[i] <- mu+rho*Y[i-1]+e[i]
 for(j in 1:300)
 X[j] <- Y[j+1700]

postscript("u:/meth1/psfiles/plotts_r1.ps",height=7,horizontal=F)          

 plot.ts(X,ylab="Y_t",xlab="Time t",
                   main="Time Series for AR(1) with rho=.1")
abline(h=13.3)
# acf(X,ylab="Rho_k",xlab="Lag k",
#                   main="ACF for AR(1) with rho=.1")
 
graphics.off()

