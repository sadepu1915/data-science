####
#### For each of n_1 = 271 private, n_2 = 138 nonprofit, and n_3 = 107 government nursing 
#### homes, we have observations on p = 4 variables: (1) cost of nursing labor, (2) cost 
#### of dietary labor, (3) cost of plant operation and maintenance labor, and (4) cost of 
#### housekeeping and laundry labor. 
####

n_1 <- 271
n_2 <- 138
n_3 <- 107
n <- n_1 + n_2 + n_3
p <- 4
g <- 3

## Summary statistics.
x_bar_1 <- c(2.066, 0.480, 0.082, 0.360)
x_bar_2 <- c(2.167, 0.596, 0.124, 0.418)
x_bar_3 <- c(2.273, 0.521, 0.125, 0.383)
x_bar <- (n_1 * x_bar_1 + n_2 * x_bar_2 + n_3 * x_bar_3) / n
S_1 <- matrix(c(0.291, -0.001, 0.002, 0.010, -0.001, 0.011, 0.000, 0.003, 0.002, 0.000, 
  0.001, 0.000, 0.010, 0.003, 0.000, 0.010), nrow = p)
S_2 <- matrix(c(0.561, 0.011, 0.001, 0.037, 0.011, 0.025, 0.004, 0.007, 0.001, 0.004, 
  0.005, 0.002, 0.037, 0.007, 0.002, 0.019), nrow = p)
S_3 <- matrix(c(0.261, 0.030, 0.003, 0.018, 0.030, 0.017, 0.000, 0.006, 0.003, 0.000, 
  0.004, 0.001, 0.018, 0.006, 0.001, 0.013), nrow = p)

## MANOVA. Reject H_0: tau_1 = tau_2 = tau_3 = 0 (no differences in the mean cost vectors 
## between the nursing home types) at alpha = 0.05. Also see 'manova' function.
W <- (n_1 - 1) * S_1 + (n_2 - 1) * S_2 + (n_3 - 1) * S_3
B <- n_1 * (x_bar_1 - x_bar) %*% t(x_bar_1 - x_bar) + 
  n_2 * (x_bar_2 - x_bar) %*% t(x_bar_2 - x_bar) + 
  n_3 * (x_bar_3 - x_bar) %*% t(x_bar_3 - x_bar)
Lambda <- det(W) / det(B + W)
1 - pchisq(-(n - 1 - (p + g) / 2) * log(Lambda), p * (g - 1))
1 - pf(((n - p - 2) / p) * ((1 - sqrt(Lambda)) / sqrt(Lambda)), 2 * p, 2 * (n - p - 2))

## Bonferroni 95% intervals. There are 4(3)(2)/2 = 12 possible pairwise comparisons. Here 
## are intervals for comparing mean costs between private and government-owned nursing 
## homes. The government-owned homes tend to spend more on both nursing labor and plant 
## operation and maintenance than privately-owned homes.
x_bar_1[1] - x_bar_3[1] + c(-1, 1) * qt(1 - 0.05 / (12 * 2), n - g) * 
  sqrt((W[1, 1] / (n - g)) * (1 / n_1 + 1 / n_3))
x_bar_1[2] - x_bar_3[2] + c(-1, 1) * qt(1 - 0.05 / (12 * 2), n - g) * 
  sqrt((W[2, 2] / (n - g)) * (1 / n_1 + 1 / n_3))
x_bar_1[3] - x_bar_3[3] + c(-1, 1) * qt(1 - 0.05 / (12 * 2), n - g) * 
  sqrt((W[3, 3] / (n - g)) * (1 / n_1 + 1 / n_3))
x_bar_1[4] - x_bar_3[4] + c(-1, 1) * qt(1 - 0.05 / (12 * 2), n - g) * 
  sqrt((W[4, 4] / (n - g)) * (1 / n_1 + 1 / n_3))
