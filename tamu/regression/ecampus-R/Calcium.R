calcium <-read.csv("/Users/home/Documents/TAMU/stat608/Data/CalciumBP.csv")


m1 <- lm(Decrease ~ x, data=calcium)  #Telling it the name of the data set prevents having to use dsn$varname notation.
summary(m1)

vcov(m1)
sqrt(4.961288)  #Useful for CI of placebo mean
sqrt(10.418704)  #Useful for CI of diff between means


anova(m1)

m <- 10
n <- 21
mse <- 54.574   #Remember MSE is the estimate of sigma^2.
sqrt(mse / (n -m))  #s.e. of intercept: use top left entry of matrix.
sqrt(mse * n / (m * (n-m)))  #s.e. of slope: use bottom right entry of matrix.


