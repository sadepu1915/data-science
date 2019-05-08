#Sample mean bootstrap example: Create a confidence interval.
x <- c(4,5,6)
n <- length(x)

nreps <- 1000 #Should be at least 10,000 for most real inference.  
#Setting to 1000 here only to prevent computational overload.

bmeans <- rep(NA, nreps)  #Creating the bootstrapped means vector, full of missing values.
for(i in 1:nreps){
  bsample <- sample(x, replace=TRUE)
  bmeans[i] <- mean(bsample)
}

#Notice that bmeans contains the sampling distribution of the sample means:
hist(bmeans)



#Create a confidence interval:  For a 95% interval, I need the 2.5th and 97.5th percentiles:
sorted <- sort(bmeans)
sorted[25]
sorted[975]
#To find out whether nreps was large enough, run this again and check that the interval stays the same.






#Create a p-value for testing the alternative that mu > 4.5: First, assume the null that mu = 5.
xnull <- x - mean(x) + 4.5
n <- length(xnull)

nreps <- 1000

bmeans <- rep(NA, nreps)  #Creating the bootstrapped means vector, full of missing values.
for(i in 1:nreps){
  bsample <- sample(xnull, replace=TRUE)
  bmeans[i] <- mean(bsample)
}

hist(bmeans)  #Double check that we have the mean of the sampling distribution equal to the hypothesized mean.
p.val <- length(which(bmeans >=5)) / length(bmeans) #P-value is proportion of x-bars >= 5.


