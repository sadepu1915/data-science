####
#### Bivariate normal.
####

setwd("C:/Users/Madhavi/Documents/MS/636/Code")

## Example densities.
library(mvtnorm)

x <- y <- seq(-3, 3, length = 50)
f <- function(x, y, rho) {
  dmvnorm(cbind(x, y), mean = c(0, 0), sigma = matrix(c(1, rho, rho, 1), nrow = 2))
}

persp(x, y, outer(x, y, f, rho = 0), zlab = "density", theta = 30, phi = 30)
persp(x, y, outer(x, y, f, rho = 0.9), zlab = "density", theta = 30, phi = 30)
persp(x, y, outer(x, y, f, rho = -0.9), zlab = "density", theta = 30, phi = 30)

##
## Ellipses of constant density under various scenarios.
##

library(plotrix)

mu = c(1, 1)

ellipse_f <- function(mu, Sigma, alpha) {
  p <- length(mu)
  c2 <- qchisq(1 - alpha, p)
  clr <- c("red", "blue")
  
  ee <- eigen(Sigma)
  lambda <- ee$values
  theta <- acos(ee$vec[1, 1]) * 360 / (2 * pi) * sign(ee$vec[2, 1])

  plot(c(-1.5, 3.5), c(-1.5, 3.5), xlab = expression(x[1]), ylab = expression(x[2]), 
    asp = 1, type = "n")
  abline(0, 1, lty = 2)
  for(i in 1:length(c2))
    draw.ellipse(mu[1], mu[2], sqrt(c2[i] * lambda[1]), sqrt(c2[i] * lambda[2]), 
      angle = theta, border = clr[i], lwd = 2)
}

# topic 4 - 17pg back
Sigma_f <- function(sg, rho) {
  matrix(c(sg[1] ^ 2, rho * sg[1] * sg[2], rho * sg[1] * sg[2], sg[2] ^ 2), nrow = 2)
}

## When sigma_{11} = sigma_{22}.
pdf("figures/bv_eq_pos.pdf")
ellipse_f(mu, Sigma_f(c(1, 1), 0.8), alpha = c(0.1, 0.5))  #90 (red capture more data), 50%
title(main = expression(paste(sigma[1] == sigma[2], ", ", rho > 0, sep = "")))
dev.off()

pdf("figures/bv_eq_zero.pdf")
ellipse_f(mu, Sigma_f(c(1, 1), 0), alpha = c(0.1, 0.5))
title(main = expression(paste(sigma[1] == sigma[2], ", ", rho == 0, sep = "")))
dev.off()

pdf("figures/bv_eq_neg.pdf")
ellipse_f(mu, Sigma_f(c(1, 1), -0.8), alpha = c(0.1, 0.5))
title(main = expression(paste(sigma[1] == sigma[2], ", ", rho < 0, sep = "")))
dev.off()

## When sigma_{11} != sigma_{22}.
pdf("figures/bv_neq_gt_pos.pdf")
ellipse_f(mu, Sigma_f(c(2, 0.5), 0.8), alpha = 0.4)
title(main = expression(paste(sigma[1] > sigma[2], ", ", rho > 0, sep = "")))
dev.off()

pdf("figures/bv_neq_gt_zero.pdf")
ellipse_f(mu, Sigma_f(c(2, 0.5), 0), alpha = 0.4)
title(main = expression(paste(sigma[1] > sigma[2], ", ", rho = 0, sep = "")))
dev.off()

pdf("figures/bv_neq_lt_pos.pdf")
ellipse_f(mu, Sigma_f(c(0.5, 2), -0.8), alpha = 0.4)
title(main = expression(paste(sigma[1] < sigma[2], ", ", rho < 0, sep = "")))
dev.off()

####
#### Assessing normality.
####

library(mvtnorm)

##
## Q-Q plots.
##

n <- 10

set.seed(101)

## Here is an example Q-Q plot for data from a normal distribution. The linear trend 
## supports the assumption of normality.
x <- rnorm(n, mean = 3, sd = 1.5)
q_s <- sort(x)
q_z <- qnorm((1:n - 0.5) / n)

par(mfrow = c(1, 2))
plot(q_z, q_s)
qqnorm(x)

pdf("qq_1.pdf")
par(mfrow = c(1, 1))
qqnorm(x, main = "")
qqline(x)
dev.off()

## Here is why we expect a linear trend: The quantiles of the N(mu, sigma) distribution 
## are equal to sigma x q_z + mu, where the q_z are the quantiles from the 
## standard normal distribution.
par(mfrow = c(1, 1))

q_1 <- qnorm((1:n - 0.5) / n, mean = 1, sd = 0.5)
plot(q_z, q_1)
abline(1, 0.5)
abline(0, 1, lty = 2)

## When our data do not come from a normal distribution, their Q-Q plots can display a 
## variety of non-linear patterns. Here are some typical ones, based on the uniform 
## (light-tailed), t (heavy-tailed), beta (left-skewed, here), and gamma (right-skewed).
n <- 100
q_z <- qnorm((1:n - 0.5) / n)

pdf("figures/qq_2.pdf")
par(mfrow = c(2, 2))
plot(q_z, qunif((1:n - 0.5) / n), xlab = "Expected", ylab = "Actual", 
  main = "Light-Tailed")
plot(q_z, qt((1:n - 0.5) / n, 2), xlab = "Expected", ylab = "Actual", 
  main = "Heavy-Tailed")
plot(q_z, qbeta((1:n - 0.5) / n, 5, 1), xlab = "Expected", ylab = "Actual", 
  main = "Left-Skewed")
plot(q_z, qgamma((1:n - 0.5) / n, 2, 2), xlab = "Expected", ylab = "Actual", 
  main = "Right-Skewed")
dev.off()
  
## With small sample sizes, it can be difficult to see deviations from normality.
n <- 10

pdf("figures/qq_3.pdf")
par(mfrow = c(2, 2))
qqnorm(x_lt <- runif(n), main = "Light-Tailed"); qqline(x_lt)
qqnorm(x_ht <- rt(n, 2), main = "Heavy-Tailed"); qqline(x_ht)
qqnorm(x_ls <- rbeta(n, 5, 1), main = "Left-Skewed"); qqline(x_ls)
qqnorm(x_rs <- rgamma(n, 2, 2), main = "Right-Skewed"); qqline(x_rs)
dev.off()

p_lt <- round(shapiro.test(x_lt)$p.value, 2)
p_ht <- round(shapiro.test(x_ht)$p.value, 2)
p_ls <- round(shapiro.test(x_ls)$p.value, 2)
p_rs <- round(shapiro.test(x_rs)$p.value, 2)

pdf("figures/qq_3_p.pdf")
par(mfrow = c(2, 2))
qqnorm(x_lt, main = "Light-Tailed"); qqline(x_lt)
text(0.5, 0.3, paste("p = ", p_lt, sep = ""), cex = 1.5)
qqnorm(x_ht, main = "Heavy-Tailed"); qqline(x_ht)
text(0.5, -0.75, paste("p = ", p_ht, sep = ""), cex = 1.5)
qqnorm(x_ls, main = "Left-Skewed"); qqline(x_ls)
text(0.5, 0.45, paste("p = ", p_ls, sep = ""), cex = 1.5)
qqnorm(x_rs, main = "Right-Skewed"); qqline(x_rs)
text(-0.5, 2, paste("p = ", p_rs, sep = ""), cex = 1.5)
dev.off()

####
#### NB10 weight example. These are data on the weights in micrograms of 100 samples of 
#### a 10-gram chrome steel standard known as NB10. Each of the weights is actually of 
#### the form 9,999,xxx, so our numbers are just the last three from the actual microgram 
#### weights. 
####

x <- c(591, 602, 592, 597, 595, 596, 596, 588, 592, 599, 600, 597, 601, 600, 591, 594, 
  595, 594, 594, 593, 594, 593, 601, 590, 601, 593, 608, 591, 599, 588, 601, 598, 598, 
  599, 598, 595, 593, 600, 588, 625, 598, 599, 601, 593, 593, 589, 594, 592, 607, 591, 
  594, 601, 603, 577, 594, 590, 596, 596, 563, 594, 599, 600, 593, 594, 587, 590, 597, 
  599, 582, 602, 597, 599, 599, 594, 591, 590, 592, 596, 585, 594, 599, 595, 601, 598, 
  596, 599, 596, 592, 596, 597, 597, 598, 599, 595, 598, 598, 593, 594, 599, 596)
  
## If these were normally distributed, it would be very unusual for any of their 
## standardized weights to exceed 3 in absolute value. We have two weights that are 
## extreme according to this criterion. Should they be removed from the analysis? It 
## turns out in this example that they should not. The point of collecting these data was 
## to learn about the weighing process. The outliers indicate that there can be upsets in 
## this process. In other words, the weights are not homogenous.
x_st <- (x - mean(x)) / sqrt(var(x))
table(abs(x_st) > 3)

pdf("figures/nb10.pdf")
dotchart(x_st, main = "Dot Plot of Standardized Weights")
points(x_st[abs(x_st) > 3], (1:100)[abs(x_st) > 3], pch = 20, col = "red")
dev.off()

pdf("prob2.pdf")
attach(carsData)
carsData=read.csv("./R/used_cars.csv")
qqnorm(carsData$Age, main="Normal Plots for Age", xlab="normal quantiles", ylab="Age")
qqline(carsData$Age)
shapiro.test(carsData$Age)

trans.25<-carsData$Age^.25
qqnorm(trans.25, main="Normal Plots for Age with transform^.25", xlab="normal quantiles", ylab="Age");qqline(trans.25)

trans.50<-carsData$Age^.50
qqnorm(trans.50, main="Normal Plots for Age with transform^.50", xlab="normal quantiles", ylab="Age");qqline(trans.50)

shapiro.test(trans.50)

qqnorm(carsData$Price, main="Normal Plots for price", xlab="normal quantiles", ylab="price")
qqline(carsData$Price)
shapiro.test(carsData$Price)

ptrans.25<-carsData$Price^.25
qqnorm(ptrans.25, main="Normal Plots for price with transform^.25", xlab="normal quantiles", ylab="price");qqline(ptrans.25)

ptrans.50<-carsData$Price^.50
qqnorm(ptrans.50, main="Normal Plots for price with transform^.50", xlab="normal quantiles", ylab="price");qqline(ptrans.50)

shapiro.test(ptrans.50)

boxCox(lm(Age~Price), data=carsData, lambda =  seq(from = -2, to = 2, length = 100))
ptranscombined<-cbind(Age, Price)^.80
qqnorm(ptranscombined, main="Normal Plots for Age&Price with transform^.80", xlab="normal quantiles", ylab="carsData")
qqline(ptranscombined)

dev.off()

#3A

pdf("prob3.pdf")
advData=read.csv("./R/Advertising.csv")
attach(advData)

qqnorm(X, main="Normal Plots for X", xlab="normal quantiles", ylab="X")
qqline(X)

qqnorm(TV, main="Normal Plots for TV", xlab="normal quantiles", ylab="TV")
qqline(TV)

qqnorm(radio, main="Normal Plots for radio", xlab="normal quantiles", ylab="radio")
qqline(radio)

qqnorm(newspaper, main="Normal Plots for newspaper", xlab="normal quantiles", ylab="newspapaer")
qqline(newspaper)

qqnorm(sales, main="Normal Plots for sales", xlab="normal quantiles", ylab="sales")
qqline(sales)

pairs(advData)

dev.off()

