
rm(list=ls())
Sys.time()

setwd("C:/Users/Madhavi/Documents/MS/636/Code/data")
dta = read.csv("box_office.csv")

####
#### Univariate exploration of variables.
####

revenue = dta[, 4]
num_th = dta[, 5]
incep = dta[, 7]

par(mfrow = c(2, 2))
hist(revenue, main = "Revenue")
hist(num_th, main = "No. of theaters")
hist(incep, main = "Inception time")

## Look at cumulative versions of Views, Users, Rigor, and Edits variables.
views_c = rowSums(dta[, c(9, 13, 17, 21, 25, 29)]) + 1
users_c = rowSums(dta[, c(9, 13, 17, 21, 25, 29) + 1]) + 1
rigor_c = rowSums(dta[, c(9, 13, 17, 21, 25, 29) + 2]) + 1
edits_c = rowSums(dta[, c(9, 13, 17, 21, 25, 29) + 3]) + 1

par(mfrow = c(2, 2))
hist(views_c, main = "Views")
hist(users_c, main = "Users")
hist(rigor_c, main = "Rigor")
hist(edits_c, main = "Edits")

## Distributions of all variables, except inception, are right-skewed. Log transform.
revenue = log(revenue + 1)
num_th = log(num_th + 1)
views_c = log(views_c)
users_c = log(users_c)
rigor_c = log(rigor_c)
edits_c = log(edits_c)

y = cbind(revenue, num_th, views_c, users_c, rigor_c, edits_c)

par(mfrow = c(2, 4))
hist(revenue, main = "Revenue")
hist(num_th, main = "No. of theaters")
hist(incep, main = "Inception time")
hist(views_c, main = "Views")
hist(users_c, main = "Users")
hist(rigor_c, main = "Rigor")
hist(edits_c, main = "Edits")

####
#### Multivariate normality? Doesn't look like it.
####

## Marginal normality? It's the zeros that cause normality to be suspect.
par(mfrow = c(2, 2))
hist(views_c, main = "Views")
hist(users_c, main = "Users")
hist(rigor_c, main = "Rigor")
hist(edits_c, main = "Edits")

## Bivariate normality? Same story.
pairs(cbind(views_c, users_c, rigor_c, edits_c))

views_30 = dta$Views30 + 1
users_30 = dta$Users30 + 1
rigor_30 = dta$Rigor30 + 1
edits_30 = dta$Edits30 + 1

## Chi-square plot. Not multivariate normal.
n = nrow(y)
p = ncol(y)

y_bar = colMeans(y)
S = (t(y) - y_bar) %*% t(t(y) - y_bar) / (n - 1)

d2 = rep(NA, n)
for(i in 1:n)
  d2[i] = (t(y[i, ]) - y_bar) %*% solve(S) %*% t(t(y[i, ]) - y_bar)

table(d2 <= qchisq(0.5, p))
qq = qchisq((1:n - 0.5) / n, p)

par(mfrow = c(1, 1))
plot(qq, sort(d2))
abline(0, 1, lty = 2)

####
#### T^2 test.
####

##
## T^2 test, for illustration purposes only. Consider H_0: mu' = [14, 4, 12, 4, 4, 5]. 
## Testing at alpha = 0.05, we would reject the null hypothesis. Again, multivariate 
## normality does not appear to hold here, so the inference is suspect.
##

## We reject H_0: mu' = [14, 4, 12, 4, 4, 5] at alpha = 0.05.
mu_0 = c(14, 4, 12, 4, 4, 5)
T2 = n * t(y_bar - mu_0) %*% solve(S) %*% (y_bar - mu_0)
crit_val = (n - 1) * p / (n - p) * qf(0.95, p, n - p)

####
#### Principal component analysis.
####

par(mfrow = c(1, 1))

## Let's just look at revenue, number of theaters, and cumulative Wiki variables.
y = cbind(revenue, num_th, views_c, users_c, rigor_c, edits_c)

## Principal component analysis. First 2 PC's account for about 96% of variability. The 
## first looks like an overall "index," and the second is a difference between (revenue 
## and number of theaters) and (the four cumulative Wikipedia variables).
pc = princomp(y, cor = TRUE)
summary(pc)
pc$loadings

y_std = scale(y, center = TRUE, scale = TRUE)
pc_1 = y_std %*% pc$loadings[, 1]
pc_2 = y_std %*% pc$loadings[, 2]
var(pc_1)
var(pc_2)

eigen(cov(y_std))

## Plot the first 2 PC's. There's an obvious cluster of points, possibly three of them. 
plot(pc_1, pc_2)

ii = (1:312)[pc_1 < -0.5 & pc_2 > 0]
points(pc_1[ii], pc_2[ii], pch = 20, col = "pink")
points(pc_1[-ii], pc_2[-ii], pch = 20, col = "lightblue")

jj = (1:312)[pc_1 > -2 & pc_2 < 0]
points(pc_1[jj], pc_2[jj], pch = 20, col = "lightblue")

kk = (1:312)[pc_1 > 2 & pc_2 > 0]
points(pc_1[kk], pc_2[kk], pch = 20, col = "lightgreen")

## Exploring the cluster in previous plot. The first 2 PC's apparently distinguish the 
## "blockbusters" from the rest.
cbind(dta[ii, c(2, 4:5)], views_c[ii], users_c[ii], rigor_c[ii], edits_c[ii])
cbind(dta[jj, c(2, 4:5)], views_c[jj], users_c[jj], rigor_c[jj], edits_c[jj])
cbind(dta[kk, c(2, 4:5)], views_c[kk], users_c[kk], rigor_c[kk], edits_c[kk])

par(mfrow = c(1, 2))
hist(y[, 1])
hist(y[ii, 1], add = TRUE, col = "pink", xlab = "revenue")

plot(y[, 3], y[, 1], xlab = "views_c", ylab = "revenue")
points(y[ii, 3], y[ii, 1], pch = 20, col = "pink")
points(y[jj, 3], y[jj, 1], pch = 20, col = "lightblue")
points(y[kk, 3], y[kk, 1], pch = 20, col = "lightgreen")


































## Try standardizing the variables first. Weighted average of the variables is by far 
## the linear combination that explains the variation in the data. 
y = scale(y, center = TRUE, scale = TRUE)

pc = princomp(y)
summary(pc)
pc$loadings

## Mean-center the individuals first (to prevent everything else from being drowned out 
## by the first principal component above). Now, the first PC (68.5%) reflects variation 
## in the number of theaters. The second PC (14%) reflects the 'views' variables. And 
## the third PC (7.8%) reflects variation in the revenue variable.
y = t(scale(t(y), center = TRUE))

pc = princomp(y)
summary(pc)
pc$loadings





























####
#### Wikipedia variables at 30 days. And T^2 test.
####

views_30 = dta$Views30 + 1
users_30 = dta$Users30 + 1
rigor_30 = dta$Rigor30 + 1
edits_30 = dta$Edits30 + 1

y_30 = cbind(views_30, users_30, rigor_30, edits_30)

n = nrow(y_30)
p = ncol(y_30)

##
## Multivariate normality? Doesn't look like it.
##

## Marginal normality? Not really.
par(mfrow = c(2, 2))
hist(log(y_30[, 1]), main = "Views")
hist(log(y_30[, 2]), main = "Users")
hist(log(y_30[, 3]), main = "Rigor")
hist(log(y_30[, 4]), main = "Edits")

## Bivariate scatters. Not bivariate normal.
pairs(log(y_30))

## Go ahead and log transform.
y_30 = log(y_30)

## Chi-square plot. Not multivariate normal.
y_bar = colMeans(y_30)
S = (t(y_30) - y_bar) %*% t(t(y_30) - y_bar) / (n - 1)

d2 = rep(NA, n)
for(i in 1:n)
  d2[i] = (t(y_30[i, ]) - y_bar) %*% solve(S) %*% t(t(y_30[i, ]) - y_bar)

table(d2 <= qchisq(0.5, p))

qq = qchisq((1:n - 0.5) / n, p)
plot(qq, sort(d2))
abline(0, 1, lty = 2)

##
## T^2 test, for illustration purposes only. Consider H_0: mu' = [14, 4, 4, 5]. Testing 
## at alpha = 0.05, we would reject the null hypothesis. Again, multivariate normality 
## does not appear to hold here, so the inference is suspect.
##

## We reject H_0: mu' = [14, 4, 4, 5] at alpha = 0.05.
mu_0 = c(14, 4, 4, 5)
T2 = n * t(y_bar - mu_0) %*% solve(S) %*% (y_bar - mu_0)
crit_val = (n - 1) * p / (n - p) * qf(0.95, p, n - p)

####
#### Only movies shown in at least 100 theaters. 
####

ii = (1:nrow(dta))[dta[, 5] >= 100]
dta = dta[ii, ]

plot(dta[, 5], log(dta[, 4])) ## number of theaters
plot(dta[, 7], log(dta[, 4])) ## article "inception"

par(mfrow = c(2, 2))
plot(log(dta[, 9]), log(dta[, 4])) ## Views60
plot(log(dta[, 10]), log(dta[, 4])) ## Users60
plot(log(dta[, 11]), log(dta[, 4])) ## Rigor60
plot(log(dta[, 12]), log(dta[, 4])) ## Edits60

plot(log(dta[, 9 + 4]), log(dta[, 4])) ## Views50
plot(log(dta[, 10 + 4]), log(dta[, 4])) ## Users50
plot(log(dta[, 11 + 4]), log(dta[, 4])) ## Rigor50
plot(log(dta[, 12 + 4]), log(dta[, 4])) ## Edits50

plot(log(dta[, 9 + 8]), log(dta[, 4])) ## Views40
plot(log(dta[, 10 + 8]), log(dta[, 4])) ## Users40
plot(log(dta[, 11 + 8]), log(dta[, 4])) ## Rigor40
plot(log(dta[, 12 + 8]), log(dta[, 4])) ## Edits40

plot(log(dta[, 9 + 12]), log(dta[, 4])) ## Views30
plot(log(dta[, 10 + 12]), log(dta[, 4])) ## Users30
plot(log(dta[, 11 + 12]), log(dta[, 4])) ## Rigor30
plot(log(dta[, 12 + 12]), log(dta[, 4])) ## Edits30

plot(log(dta[, 9 + 16]), log(dta[, 4])) ## Views20
plot(log(dta[, 10 + 16]), log(dta[, 4])) ## Users20
plot(log(dta[, 11 + 16]), log(dta[, 4])) ## Rigor20
plot(log(dta[, 12 + 16]), log(dta[, 4])) ## Edits20

plot(log(dta[, 9 + 20]), log(dta[, 4])) ## Views10
plot(log(dta[, 10 + 20]), log(dta[, 4])) ## Users10
plot(log(dta[, 11 + 20]), log(dta[, 4])) ## Rigor10
plot(log(dta[, 12 + 20]), log(dta[, 4])) ## Edits10

## Plot of log revenue vs. log number of views 60 days before release, categorized by 
## the number of theaters.
par(mfrow = c(1, 2))
plot(log(dta$Views60[dta[, 5] <= 100]), log(dta[dta[, 5] <= 100, 4]), 
  xlab = "Log of number of views 60 days before release", 
  ylab = "Log of first weekend revenue", main = "Number of Theaters <= 100",
  xaxt = "n", xlim = c(0, 15.5))
axis(1, at = c(0, 5, 10, 15))
lines(lowess(log(dta$Views60[dta[, 5] <= 100] + 1), log(dta[dta[, 5] <= 100, 4] + 1)), 
  lwd = 2, col = "blue")
plot(log(dta$Views60[dta[, 5] > 100] + 1), log(dta[dta[, 5] > 100, 4] + 1), 
  xlab = "Log of number of views 60 days before release", 
  ylab = "Log of first weekend revenue", main = "Number of Theaters > 100", 
  xaxt = "n", xlim = c(0, 15.5))
axis(1, at = c(0, 5, 10, 15))
lines(lowess(log(dta$Views60[dta[, 5] > 100] + 1), log(dta[dta[, 5] > 100, 4] + 1)), 
  lwd = 2, col = "blue")
  
pdf("figures/numviews60_rev.pdf", height = 4, width = 6)
par(mfrow = c(1, 1))
plot(log(dta$Views60), log(dta[, 4]), 
  xlab = "Log of number of views 60 days before release", 
  ylab = "Log of first weekend revenue")
points(log(dta$Views60[dta[, 5] <= 100]), log(dta[, 4][dta[, 5] <= 100]), 
  col = "lightblue", pch = 20)
points(log(dta$Views60[dta[, 5] > 100]), log(dta[, 4][dta[, 5] > 100]), 
  col = "pink", pch = 20)
legend(-0.5, 19, legend = c("100 theaters or less", "more than 100 theaters"), lwd = c(2, 2), 
  col = c("lightblue", "pink"), bty = "n")
dev.off()

## Release date. Season. Day of week. 

## Predict the bimodality feature? It might only be apparent if you include all the 
## movies (you already filtered out a bunch with limited releases). I bet number of 
## theaters tells you everything about this.
hist(dta[, 4])










