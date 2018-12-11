####
#### Career statistics for Major League Baseball players, as well as indicators for 
#### whether the players are in the Hall of Fame (HOF).
####

## Load data.
rm(list=ls())
Sys.time()

setwd("C:/Users/Madhavi/Documents/MS/636/Code/data")
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
R <- cor(X) # standize of capital sigma matrix

##
## Principal component analysis.
##

pca <- prcomp(X, center = TRUE, scale = TRUE)

pca <- princomp(X) # no sscale b y def --  row matrix for stand

pca
summary(pca) # can give proptions

names(pca)
pca$loadings # coeff

eigen(S)

## Eigenvalues and eigenvectors of R.
egn_R <- eigen(R)
egn_R

## Compute PCs.

#es<-eigen(S)
#es$vectors
#pc_s<-X %*% es$vectors n*6 6*6 
#X %*% es$vectors[,1] n*6 6*1

#var(X %*% es$vectors[,1])
#[,1]
#[1,] 548068
#var(pc_s[,1]) = es$values[1]

#u take any other linear combo ...none can beat like above max var explained
#any_other_linear<-X %*% rep(1/6, 6)
#> var(any_other_linear[,1])
#[1] 36677.47

#corr  coeff 0 
#t(es$vectors[,1]) %*% es$vectors[,2]
#[,1]
#[1,] -1.685392e-16

#sum(diag(S))
#[1] 574126.2
#> sum(es$values)
#[1] 574126.2
#porpotion totlal variation 
#fst pc .95 % 
#2nd one uncorelated ....followed

#> es$values / sum(es$values)
#[1] 0.9546124767 0.0413661116 0.0022956759 0.0010409162 0.0005586671
#[6] 0.0001261524
 


#same as above 

x_std <- scale(X, center = TRUE, scale = TRUE)
apply(x_std, 2, mean) #0
apply(x_std, 2, sd)  #1

PC <- scale(X, center = TRUE, scale = TRUE) %*% egn_R$vectors
apply(PC, 2, var)=egn_R$values
PC[,1] %*% PC[,2] #= 0

plot(1:6, egn_R$values / sum(egn_R$values), type='l')

cumsum(egn_R$values / sum(egn_R$values))

plot(PC[,1], PC[,2])

# sd(PC[,1])
#[1] 2.066722
#> egn_R$values
#[1] 4.27134056 0.83733564 0.53336164 0.29786011 0.04624833 0.01385372
#> egn_R$values[1]
#[1] 4.271341
#> sqrt(egn_R$values[1])
#[1] 2.066722

DTA
head(DTA)
names(PC)
dim(PC)
order1<-order(PC[,1], decreasing = TRUE) #indicies diff then sort
order2<-order(PC[,2], decreasing = TRUE)
DTA1<-DTA[order1, ]
DTA2<-DTA[order2, ]
head(DTA1)
tail(DTA1)
DTA1[1:5,]
DTA1[1:5,num_vars]
#H HR RBI AVG SLG OBP
#946  90  6  44 161 222 203
#833  48  1  21 190 234 244
#731 168 14  82 188 279 267
#799 370 28 161 193 280 252
#480 230  0  65 213 253 266


#cluster
#hierarchial cluster analysis

#D<-dist(X) # on indv

D<-dist(t(X)) #on varaible
#sqrt(sum((X[,1] - X[,2]) ^2 )) #ecludence
hc_out<-hclust(D) #complete linkage by default
plot(hc_out)

hc_out$merge
[,1] [,2]
[1,]   -4   -6
[2,]   -5    1
[3,]   -2    2
[4,]   -3    3
[5,]   -1    4
> hc_out$height
[1]  2227.453  4456.998  9109.351 16055.416 38258.526
> plot(hc_out$height, type='l')
> 

##
## Linear discriminant analysis.
##


library(MASS)
nr<-nrow(X)
Y<- DTA$HOF
kappa<-seq(0,0.5,by=.01)

for(i in 1:nr)
{
  Y_te<-Y[i]
  # to treat as matrix if not drop added it will be like vector
  # cannot access with index
  # R needed as matrx
  # see its impact
  X_te<-data.frame(X_st[i,,drop=FALSE])
  Y_tr<-Y[-i]
  X_tr<-data.frame(X_st[-i,])
  
  fit_lda <- lda(Y_tr ~ H + HR + RBI + AVG + SLG + OBP, data = X_tr)
  pre_lda <- predict(fit_lda, newdata=X_te)
  
}
  

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

#cv_lasso <- cv.glmnet(as.matrix(DTA_st, DTA_st$HOF, alpha = 1, family = "binomial", type.measure = "auc"))


#cv_lasso <- cv.glmnet(as.matrix(Y_df[, -1]), Y_df$GRP, alpha = 1, family = "multinomial", 
 #                     type.measure = "class")


lambda_grid <- cv_lasso$lambda
lambda_min <- cv_lasso$lambda.min

fit_lasso <- glmnet(model.matrix(~., data = DTA_st, STA_st$HOF, alpha = 1, lambda = lambda_grid, family = "binomial"))
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

