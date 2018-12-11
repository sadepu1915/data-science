D = seq(0,1.0,.025)
L1 = (D^2)*12/0.11585
P1 = 1-pf(qf(.95,3,8),3,8,L1)
L2 = (D^2)*3/(2*0.11585)
P2 = 1-pf(qf(.95,3,8),3,8,L2)
postscript("u:\meth2\handouts\powerf.ps",height=8,horizontal=F)
P<-cbind(P1,P2)
matplot(D,P,type="l",main="POWER OF F-TEST \n MIN EFFECT SIZE FOR ALL/AT LEAST ONE",
           xlab="MINIMUM EFFECT SIZE (D)",
           ylab="POWER OF F-TEST",ylim=c(0,1),lab=c(20,20,7),col="black",
 cex=.5)
legend(.05,.95,lty=c(1,2),legend=c("MIN EFFECT FOR ALL","MIN EFFECT AT LEAST ONE"),cex=.4)
graphics.off()


