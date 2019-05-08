#The following R code produces data from an AR(1) with a linear trend

#------------------------------------------------------------------------


#create ar(1) rho=.6 with trend 2 + .03*t

 P <- 2000
 mu <- 12
 sigma<- 2
 rho<- .6
 e <- rep(0,P)
 Y <- rep(0,P)
 X <- rep(0,P-1700)
 X2 <- rep(0,P-1700)
 M <- rep(0,P-1700)
 e <- rnorm(P,0,1)
 Y[1] <- mu+sigma*e[1]
 for(i in 2:P)
 Y[i] <- mu+rho*Y[i-1]+e[i]

 for(j in 1:300){
 M[j] = 2+.03*j
 X[j] <- Y[j+1700]
 X2[j] <-  X[j] + M[j]
}

 plot.ts(X2,ylab="Y_t",xlab="Time t",
                   main="Time Series for AR(1) with rho=.6 and linear trend")
 abline(32,.03)


