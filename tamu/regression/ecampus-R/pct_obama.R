library(gdata)     #This library contains the trim function.  If you don't install and load this package, you won't be able to drop D.C. using the code below.

#Read in the data from the .csv file:  (header=TRUE is the default for read.csv)
election<-read.csv("/Users/Home/Documents/TAMU/stat608/Data/pct_obama.csv",header=TRUE)

#Now I'd like to plot the data on a scatterplot.  To be able to add the regression line, first I create the regression:
obama.lm<-lm(election$Obama_12 ~ election$Obama_08)  #The response variable comes first; explanatory variables are separated by a plus sign.
#If you want to see the results, type summary(my.lm) or anova(my.lm).  

#mar vector is of form c(bottom, left, top, right).  Default is (5.1, 4.1, 4.1, 2.1).
par(cex.axis = 2, cex.main=2, cex.lab=2, mar=c(9,5,1,3), pch=19)  #These options change the plotting parameters; I like to always run something similar because the default selections result in type face that's too small for the average person.  Pch is the plot character; 19 is the solid dot.
plot(election$Obama_08, election$Obama_12, xlab="% Obama 2008", ylab="% Obama 2012")  #Here the x-variable is listed first.
abline(obama.lm)  #This adds the regression line to the existing plot; it won't work if you've closed the plot window already.


#Notice that D.C. is a huge outlier!  90% voted for Obama!  Drop D.C. out of the data set for now:
#states50<-subset(election,State!='D.C. ')  #D.C. had a space after it.  Eliminating the pound sign from the beginning of this line of code will just give you the subset of data you need.  Or learn the trim function:
election<-trim(election)
states50<-subset(election, State!='D.C.')

#Now we can run the plot again, this time without D.C.
obama.lm2<-lm(states50$Obama_12~states50$Obama_08) 
par(cex.axis = 2, cex.main=2, cex.lab=2, mar=c(9,5,1,3))  
plot(states50$Obama_08,states50$Obama_12,xlab="% Obama 2008", ylab="% Obama 2012")
abline(obama.lm2)

#Let's check this model using some residual plots; the lm command has residuals stored in obama.lm2$residuals.
#Depending on what platform you work on, you don't have to keep using this par command.  I always forget and close the plot window on my Mac, so I keep writing it out.  
par(cex.axis = 2, cex.main=2, cex.lab=2, mar=c(9,5,1,3))
plot(states50$Obama_08, my.lm2$residuals, xlab="% Obama 2008", ylab="Residuals")
#Or plot residuals against predicted values:
par(cex.axis = 2, cex.main=2, cex.lab=2, mar=c(9,5,1,3))
plot(my.lm2$fitted.values,my.lm2$residuals,xlab="Predicted values", ylab="Residuals")

#And check normality of the residuals:
par(cex.axis = 2, cex.main=2, cex.lab=2, mar=c(9,5,2.5,3))  #Here I am changing the top margin.
qqnorm(my.lm2$residuals,pch=16) #The pch command changes the plotted character to a solid circle - better for slides.
qqline(my.lm2$residuals)



#Next I'd like to get confidence and prediction intervals for 2008 = 40% and 2008 = 60%.  
#First, I could get intervals for each of the states like this:
predict(my.lm2, interval="confidence")
predict(my.lm2, interval="prediction")

#Output for prediction interval:
#...
#43 41.12113 36.55799 45.68428
#...

#Texas is state #43 (they're in alphabetical order), so Texas was predicted to have between 36.6% and 45.7% of the state voting for Obama in 2012.

#Now suppose I'd like to make a prediction for a point that doesn't already exist in our data set.  Then the predict function gets finicky.  Here's what works for me:  first I store my x and y variables as x and y.  Then I create the linear model:

y<-election$Obama_12
x<-election$Obama_08
 
my.lm3<-lm(y~x)
 
#Next I need to create a data set with values that need to be predicted; let's predict at x = 40 and at x = 50:
new<-data.frame(x=c(40,50))
#Moral: the names of the predictor variables in the new data set must match the names of the variables in the original data set.

#Then I'll make the intervals for only those two points:
predict(my.lm3, new, interval="prediction")
predict(my.lm3, new, interval="confidence")

