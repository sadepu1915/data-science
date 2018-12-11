####
#### Career statistics for Major League Baseball players, as well as indicators for 
#### whether the players are in the Hall of Fame (HOF).
####

## Load data.
DTA <- read.csv("hof_data.csv")

## Extract a few offensive statistics (numerical variables).
num_vars <- c("H", "HR", "RBI", "AVG", "SLG", "OBP")
X <- as.matrix(DTA[, num_vars])
X_st <- scale(X, center = TRUE, scale = TRUE)
DTA_st <- data.frame(DTA$HOF, X_st)
colnames(DTA_st) <- c("HOF", num_vars)

p <- ncol(X)

## Summary statistics.
x_bar <- colMeans(X)
S <- var(X)
R <- cor(X)

eigen(S)

##
## Principal component analysis.
##

pca <- prcomp(X, center = TRUE, scale = TRUE)
pca
summary(pca)

## Eigenvalues and eigenvectors of R.
egn_R <- eigen(R)
egn_R

## Compute PCs.
PC <- scale(X, center = TRUE, scale = TRUE) %*% egn_R$vectors
apply(PC, 2, var)
egn_R$values

##
## Linear discriminant analysis.
##

library(MASS)

lda_out <- lda(HOF ~ H + HR + RBI + AVG + SLG + OBP, data = DTA)
lda_pred <- predict(lda_out, newdata = DTA)$class
table(lda_pred, DTA$HOF)

LDs <- lda_pred$x
our_linear_combo <- X_st %*% rep(1 / 6, 6)

par(mfrow = c(1, 2))
boxplot(LDs[DTA$HOF == "Y", 1], LDs[DTA$HOF == "N", 1], ylim = c(-3, 8))
boxplot(our_linear_combo[DTA$HOF == "Y"], our_linear_combo[DTA$HOF == "N"], 
  ylim = c(-3, 8))

par(mfrow = c(1, 1))
hist(LDs[DTA$HOF == "N", 1], prob = TRUE, col = "red", xlim = c(-2.5, 8), 
  xlab = "LD", main = "")
hist(LDs[DTA$HOF == "Y", 1], prob = TRUE, col = "green", add = TRUE)

cor(LDs[, 1], DTA_st$H)
cor(LDs[, 1], DTA_st$HR)
cor(LDs[, 1], DTA_st$RBI)
cor(LDs[, 1], DTA_st$AVG)
cor(LDs[, 1], DTA_st$SLG)
cor(LDs[, 1], DTA_st$OBP)

## Partial F statistics.
n_k <- table(DTA$HOF)
X_bar_Y <- colMeans(X_st[DTA$HOF == "Y", ])
X_bar_N <- colMeans(X_st[DTA$HOF == "N", ])
S_Y <- var(X_st[DTA$HOF == "Y", ])
S_N <- var(X_st[DTA$HOF == "N", ])
S_po <- ((n_k[2] - 1) * S_Y + (n_k[1] - 1) * S_N) / (n_k[1] + n_k[2] - 2)
T2_full <- t(X_bar_Y - X_bar_N) %*% solve(S_po) %*% (X_bar_Y - X_bar_N) / 
  (1 / n_k[1] + 1 / n_k[2])
F_stat <- numeric(p)
for(i in 1:p) {
  X_bar_Y_red <- X_bar_Y[-i]
  X_bar_N_red <- X_bar_N[-i]
  S_Y_red <- var(X_st[DTA$HOF == "Y", -i])
  S_N_red <- var(X_st[DTA$HOF == "N", -i])
  S_po_red <- ((n_k[2] - 1) * S_Y_red + (n_k[1] - 1) * S_N_red) / (n_k[1] + n_k[2] - 2)
  T2_red <- t(X_bar_Y_red - X_bar_N_red) %*% solve(S_po_red) %*% 
    (X_bar_Y_red - X_bar_N_red) / (1 / n_k[1] + 1 / n_k[2])

  F_stat[i] <- (n_k[1] + n_k[2] - 2 - 6 + 1) * ((T2_full - T2_red) / 
    (n_k[1] + n_k[2] - 2 + T2_red))
}

## Picture of first LD.
with(DTA, boxplot(LDs[HOF == "Y"], LDs[HOF == "N"]))

## Investigate players with high values of LD who are not in HOF.
ii <- (1:nrow(DTA))[DTA$HOF == "N" & LDs > 2]
DTA_st[ii, ]

##
## Logistic regression + lasso.
## 

library(glmnet)

cv_lasso <- cv.glmnet(model.matrix(~., data = DTA_st, DTA_st$HOF, alpha = 1, 
  family = "binomial", type.measure = "auc")
lambda_grid <- cv_lasso$lambda
lambda_min <- cv_lasso$lambda.min
fit_lasso <- glmnet(model.matrix(~., data = DTA_st, STA_st$HOF, alpha = 1, 
  lambda = lambda_grid, family = "binomial")
pred_lasso <- predict(fit_lasso, newx = DTA_st, s = lambda_min, type = "response")

##
## Random forest.
##

library(randomForest)

fit_rf <- randomForest(HOF ~ ., data = DTA_st, mtry = sqrt(6))
pred_rf <- predict(fit_rf, newdata = DTA_st)
table(pred_rf, DTA_st$HOF)

##
## SVM, considering each of linear, polynomial, and radial kernels.
##

library(e1071)

tune_svm_linear <- tune(svm, HOF ~ ., data = DTA_st, kernel = "linear", 
  ranges = list(cost = seq(0.001, 100, length = 100)))
fit_svm_linear <- tune_svm_linear$best.model
pred_svm_linear <- predict(fit_svm_linear, newdata = DTA_st)
table(pred_svm_linear, DTA_st$HOF)

tune_svm_poly <- tune(svm, HOF ~ ., data = DTA_st, kernel = "polynomial", 
  ranges = list(cost = seq(0.001, 100, length = 100)))
fit_svm_poly <- tune_svm_poly$best.model
pred_svm_poly <- predict(fit_svm_poly, newdata = DTA_st)
table(pred_svm_poly, DTA_st$HOF)

tune_svm_radial <- tune(svm, HOF ~ ., data = DTA_r_5_tr, kernel = "radial", 
  ranges = list(cost = seq(0.001, 100, length = 100)))
fit_svm_radial <- tune_svm_radial$best.model
pred_svm_radial <- predict(fit_svm_radial, newdata = DTA_st)
table(pred_svm_radial, DTA_st$HOF)

