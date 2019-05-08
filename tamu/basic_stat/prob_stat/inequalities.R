par(lwd=2)
a=seq(1,5,length=1001)
plot(c(1,5),c(0,1),xlab="a",ylab="prob",type="n",main="Markov Inequality vs. Exponential Prob.")
lines(a,1/a)
lines(a,exp(-a),col=2)

a=seq(1,5,length=1001)
plot(c(1,5),c(0,1),xlab="a",ylab="prob",type="n",main="Chebyshev Inequality vs. Normal Tail Prob.")
lines(a,1/a^2)
lines(a,2*pnorm(-a),col=2)
 lines(a,2*pt(-a,3)/3,col=3)
lines(a,2*pt(-a,4)/2,col=4)
 lines(a,2*pt(-a,5)/(5/3),col=5)


