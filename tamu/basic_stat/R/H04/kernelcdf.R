         par(ask=TRUE)
   #     postscript("u:/meth1/Rfiles/ozonecdfkernSamford.ps",height=7,horizontal=F)

         y1 = scan("u:/meth1/Rfiles/ozone1.DAT")
         deny1 = density(y1,kernel='g',bw="nrd",n=5000,from=0)
         xy1 = deny1$x
         pdfy1 = deny1$y 
         cdfy1 = c(rep(0,5000))
         areay1 = c(rep(0,5000))
         for (i in 1:5000)
         {
         areay1[i] = abs((pdfy1[i+1]+pdfy1[i])/2)*(xy1[i+1]-xy1[i])
         cdfy1[i] = sum(areay1)
         }
         plot(xy1,cdfy1,type="l",xlim=c(0,280),
         xlab="Ozone Concentration",ylab="F(x)",
         main="Kernel Smoothed Sample cdf for Samford Ozone Data",cex=.95)
 
        postscript("u:/meth1/Rfiles/ozonecdfkernYonkers.ps",height=7,horizontal=F)


         y2 = scan("u:/meth1/Rfiles/ozone2.DAT")
         deny2 = density(y2,kernel='g',bw="nrd",n=5000,from=0)
         xy2 = deny2$x
         pdfy2 = deny2$y 
         cdfy2 = c(rep(0,5000))
         areay2 = c(rep(0,5000))
         for (i in 1:5000)
         {
         areay2[i] = abs((pdfy2[i+1]+pdfy2[i])/2)*(xy2[i+1]-xy2[i])
         cdfy2[i] = sum(areay2)
         }
         plot(xy2,cdfy2,type="l",xlim=c(0,280),
         xlab="Ozone Concentration",ylab="F(x)",
         main="Kernel Smoothed Sample cdf for Yonders Ozone Data",cex=.95)
        graphics.off()


