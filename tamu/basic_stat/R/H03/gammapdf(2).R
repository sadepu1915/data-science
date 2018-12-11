y = seq(0,15,.001)
D1 = dgamma(y,2,2)
D2 = dgamma(y,2,1)
D3 = dgamma(y,2,1/2)
G1 = cbind(D1,D2,D3)
D4 = dgamma(y,.6,1)
D5 = dgamma(y,2,1)
D6 = dgamma(y,5,1)
G2 = cbind(D4,D5,D6)
#postscript("u:/meth1/psfiles/gammapdf.ps",height=7,horizontal=FALSE)          

matplot(y,G1,type="l",ylab="f(y)", xlab="y",lab=c(15,10,7),col="black",lwd=1.8,lty="solid",
ylim=c(0,.8))
title("Gamma Density Functions-Different Scale")
par(font=5)

text(2,.6,"a=2, b=1/2",adj=0)
arrows(3,.57,1.2,.52)

text(3,.3,"a=2, b=1",adj=0)
arrows(4,.28,2.5,.23)

text(7,.15,"a=2, b=2",adj=0)
arrows(8,.13,6.2,.08)
#postscript("u:/meth1/psfiles/gamma2pdf.ps",height=7,horizontal=FALSE)          


matplot(y,G2,type="l",ylab="f(y)", xlab="y",lab=c(15,10,7),col="black",lwd=1.8,lty="solid",
ylim=c(0,1.0))
title("Gamma Density Functions-Different Shape")
par(font=5)

text(1,.8,"a=0.6, b=1",adj=0)
arrows(2,.77,0.6,.70)

text(3,.35,"a=2, b=1",adj=0)
arrows(4,.32,2.5,.23)

text(8,.20,"a=5, b=1",adj=0)
arrows(9,.17,6.9,.12)


#graphics.off()





