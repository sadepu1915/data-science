pots<-read.csv("/Users/home/Documents/TAMU/stat608/Data/cookingpot.csv",header=TRUE)

attach(pots)

m1 <- lm(iron ~ pot + dish)


#Should we fit interactions?  Plot it:
par(cex=1.2, mar=c(5,4,1,1))
 interaction.plot(pot,dish,iron, leg.bty="n", col=c("black","red","blue"),lwd=2)


#Fit an interaction:
m2 <- lm(iron ~ pot + dish + pot*dish, x=TRUE)
#Remember to look at the overall F-statistic first, before testing individual variables.
summary(m2)
#Test whether the variables are significant:
anova(m2)


#Examine the design matrix:
m2$x


