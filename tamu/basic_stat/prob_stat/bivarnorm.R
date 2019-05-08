rbivariate_normal <- function(mean.x = 10, sd.x=3, mean.y=10, sd.y=3, r=.50, iter=100) {
   z1 <- rnorm(iter)
   z2 <- rnorm(iter)
   x <- sqrt(1-r^2)*sd.x*z1 + r*sd.x*z2 + mean.x
   y <- sd.y*z2 + mean.y
   return(list(x,y))
}
