#The following R code produces plots which take into account the
#time element in the ozone data. The file is contained in ozone_ts.R
#------------------------------------------------------------------------
#input data:

          y1 = scan("u:/meth1/Rfiles/ozone1.DAT")
          y2 = scan("u:/meth1/Rfiles/ozone2.DAT")

          y1na = scan("u:/meth1/Rfiles/ozone1,na.DAT")
          y2na = scan("u:/meth1/Rfiles/ozone2,na.DAT")
          postscript("u:/meth1/psfiles/ozonetime_Stamford.ps",height=8,horizontal=F)  
        
         plot.ts(y1na,type="b",ylab="Ozone Conc-Stamford (ppb)",xlab="DAY",
          main="Time Series Plot of Stamford Data",cex=.9)
         abline(h=90)

     postscript("u:/meth1/psfiles/ozonetime_Yonkers.ps",height=8,horizontal=F)  

         plot.ts(y2na,type="b",ylab="Ozone Conc-Yonkers (ppb)",xlab="DAY",
          main="Time Series Plot of Yonkers Data",cex=.9)
          abline(h=55)
#autocorrelations

postscript("u:/meth1/psfiles/ozoneacf_Stamford.ps",height=8,horizontal=F)  


         acf_S=acf(y1,main="ACF for Stamford Ozone Concentration")
postscript("u:/meth1/psfiles/ozoneacf_Yonkers.ps",height=8,horizontal=F)  

         acf_Y=acf(y2,main="ACF for Yonkers Ozone Concentration")

graphics.off()

         sink("u:/meth1/psfiles/autocorr")

 
         acf_S = acf(y1,plot=F)

         acf_S

         acf_Y = acf(y2,plot=F)
         
         acf_Y

        sink()
        

