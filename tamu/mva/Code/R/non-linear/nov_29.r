####
#### y = f(x) + e
####

n <- 100
x <- runif(n, 0, 1)
sg <- 0.25

## Parametrize the model to simulate from.
beta <- c(0, 0.5, 5, -5)
X_df <- data.frame("x" = x, "x2" = x^2, "x3" = x^3)
X <- model.matrix(~ 1 + x + x2 + x3, data = X_df)

## Use model to simulate a dataset.
y <- X %*% beta + rnorm(n, 0, sg)

## Plot simulated data and overlay simulation model.
plot(x, y)
plot_function <- function(x) {
  X_df <- data.frame("x" = x, "x2" = x^2, "x3" = x^3)
  X <- model.matrix(~ 1 + x + x2 + x3, data = X_df)

  return(X %*% beta)
}
curve(plot_function, add = TRUE, col = "green", lwd = 2)

## Linear regression model.
fit_linear <- lm(y ~ x)
beta_hat <- fit_linear$coefficients
abline(beta_hat[1], beta_hat[2], col = "red", lwd = 2)

pred_linear <- predict(fit_linear)
MSE_linear <- mean((y - pred_linear)^2)

##
## A non-linear model, using "natural cubic spline".
##

library(splines)

fit_spline <- lm(y ~ ns(x, df = 5))
summary(fit_linear)
summary(fit_spline)

## Overlay estimated model.
plot_function_sp <- function(x) {
  X <- cbind(1, ns(x, df = 5))

  return(X %*% fit_spline$coefficients)
}
curve(plot_function_sp, add = TRUE, col = "blue", lwd = 2)

## Make predictions.
pred_spline <- predict(fit_spline)
MSE_spline <- mean((y - pred_spline)^2)
