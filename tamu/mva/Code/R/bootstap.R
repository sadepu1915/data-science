n <- 100
mu <- 11
sd <- 1

x<-rnorm(n,mu,sd)

x_bar <- mean(x)
s <- sd(x)

# t-based ci for mu

x_bar + c(-1,1) * 1.96 * (s / sqrt(n))

# boot strap

B<-10000
x_b_vec <- numeric(B)

for(b in 1:B)
{
  x_b <- sample(x, replace = TRUE)
  x_b_vec[b] <- mean(x_b)
}

hist(x_b_vec)
quantile(x_b_vec, c(0.025, .975))



y<-numeric(10)
for (i in 1:10) 
{ 
  x<-sample(1:6, 2,replace=TRUE)
  y[i]<-(sum(x)==6)
}

