#credit <- read.csv("http://www.stat.tamu.edu/~irinag/Data/Credit.csv")

rm(list=ls())
setwd("~/MS/636/Code")
credit <- read.csv("./data/Credit.csv")

head(credit)
# Want to predict balance using other variables

# Special case of best subset selection - fit all possible models of size 2 (2 predictors)
p = ncol(credit) - 2

R2 <- matrix(0, p, p)
colnames(R2) <- colnames(credit)[2:(p+1)]
rownames(R2) <- colnames(credit)[2:(p+1)]

# Subset selection with 2 predictors at a time
for (i in 2:(ncol(credit)-2)){
  for (j in (i+1):(ncol(credit)-1)){
    out = lm(credit$Balance ~ credit[,i] + credit[,j])
    R2[i-1,j-1] = summary(out)$r.squared
    
  }
}

boxplot(R2[R2!=0])
plot(R2[R2!=0])

# Same but using leaps library, and complete subset selection
library(leaps)
regfit.full = regsubsets(Balance~., credit[,-1])
reg.summary = summary(regfit.full)
reg.summary

reg.summary$rsq
plot(reg.summary$rsq, type='l')


# Using forward selection
regfit.fwd = regsubsets(Balance~., credit[,-1], method = "forward")
reg.fwd.summary = summary(regfit.fwd)
reg.fwd.summary
# Note how best model with 4 predictors differs from the one above using full subset selection

# Using backward selection
regfit.bwd = regsubsets(Balance~., credit[,-1], method = "backward")
reg.bwd.summary = summary(regfit.bwd)
reg.bwd.summary
# Note how the smallest models differ from the ones above

##############################################
# Split the data into the test and train - validation approach
#############################################
set.seed(93465)
n = nrow(credit)
train = sample(1:n, round(2/3*n))
# try full subset selection on train data
regfit.full = regsubsets(Balance~., credit[train,-1])

# Calculate model matrix for the test data
test.mat = model.matrix(Balance~., credit[-train, -1])

# Calculate errors from each model (up to size 8) on the validation data
val.errors = rep(NA, 8)
for (i in 1:8){
  coefi = coef(regfit.full, id = i)
  pred = test.mat[, names(coefi)]%*%coefi
  val.errors[i] = mean((credit$Balance[-train] - pred)^2)
}
plot(val.errors, type = 'o')

##############################################
# Split the data into k folds of roughly equal size - k-fold cross-validation approach
#############################################
set.seed(2384)
k = 10
fold_id <- sample(rep(seq_len(k), length.out = n))
cv_errors <- matrix(NA, k, 8)
for (i in 1:k){ # over folds
  train = c(1:n)[fold_id != i]
  regfit.full = regsubsets(Balance~., credit[train,-1])
  test.mat = model.matrix(Balance~., credit[-train, -1])
  for (m in 1:8){
    coefi = coef(regfit.full, id = m)
    pred = test.mat[, names(coefi)]%*%coefi
    cv_errors[i, m] = mean((credit$Balance[-train] - pred)^2)
  }
}
mean_cv_errors = colMeans(cv_errors)
plot(mean_cv_errors, type = 'o')