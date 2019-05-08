######################
#########HW2########

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

#colnames(data3) <- c("Cy", "X1", "X2", "X3", "F2")

###############################################################
#############HW03#################################
##### 1#####
sink("../../HW/HW03/636Hw03.pdf", split = TRUE, type="message")
data3<-read.csv("../../HW/HW03/Auto_hw.csv")
attach(data3)
summary(manova(as.matrix(data3[, 2:4]) ~ cylinders + origin + cylinders * origin),test="Wilks")


###################
##2nd#############
library(MASS) 
n <- 30
p <- 5
mu <- rep(0, p) 
rho <- 0.6
Rho  <- matrix(rho, nrow  =  p, ncol =  p); 
diag(Rho) <- 1   
sg <- 1
Sigma <- sg * Rho

N <- 1000
CI_t <- CI_b <- CI_T2 <- matrix(NA, nrow =  p,  ncol  =  2) 
cover_t <- cover_b <- cover_t2 <- matrix(NA, nrow = N, ncol = p) 
F_crit <- (n - 1) * p * qf(0.95, p, n - p) / (n - p)

for(k in 1:N) {
  ## Simulate sample  and  compute  summary statistics. 
  X <- mvrnorm(n, mu, Sigma)
  X_bar <- colMeans(X) 
  S <- var(X)
  
  ## Usual t-based 95 CIs for mean components. And record whether the intervals ’cover’ 
  ## (include) the true population mean value.
  for(i in 1:p) {
    CI_t[i, ] <- X_bar[i] + c(-1, 1) * qt(0.975, n - 1) * sqrt(S[i, i] / n) 
    cover_t[k, i] <- mu[i] >= CI_t[i, 1] & mu[i] <= CI_t[i, 2]
  }
  
  ## Now do Bonferroni simultaneous 95 CIs.
  for(i in 1:p) {
    CI_b[i, ] <- X_bar[i] + c(-1, 1) * qt(1 - 0.025 / p, n - 1) * sqrt(S[i, i] / n) 
    cover_b[k, i] <- mu[i] >= CI_b[i, 1] & mu[i] <= CI_b[i, 2]
  }
  
  #ci_T2 <- x_bar[k] + c(-1, 1) * sqrt(F_crit * S[k, k] / n)
  
  for(i in 1:p) {
    CI_T2[i, ] <- X_bar[i] + c(-1, 1) * sqrt(F_crit * S[i, i] / n) 
    cover_t2[k, i] <- mu[i] >= CI_T2[i, 1] & mu[i] <= CI_T2[i, 2]
  }
}

total_coverage<-matrix(c(colMeans(cover_t),colMeans(cover_b),colMeans(cover_t2)), nrow=3, byrow = TRUE)
rownames(total_coverage)<-c("one-at-a-atime", "Bonferroni", "T2 simul")
total_coverage
sink()



######################################################
####################### HW4 ###########################33


rm(list=ls())
Sys.time()

setwd("C:/Users/Madhavi/Documents/MS/636/HW")
data <- read.csv("./HW04/winequality_red.csv")

dim(data)
summary(data)

S <- var(data)
R <- cor(data)

PCA<-prcomp(data, center = TRUE, scale=TRUE)

summary(PCA)
names(PCA)

#1A) 


PCA$sdev^2/sum(PCA$sdev^2)

#1B

cumsum(PCA$sdev^2/sum(PCA$sdev^2))
plot(1:12, PCA$sdev^2/sum(PCA$sdev^2), type='l')


#1C)
   

std_data<-scale(data, center = TRUE, scale = TRUE)

pc<-std_data %*% PCA$rotation
var(pc[,1])
var(pc[,2])

eigen(var(std_data))$values[1:2]


#1D)

plot(pc[,1], pc[,2])

plot(pc[,1], pc[,3])

#1E)

 order1<-order(pc[,1], decreasing = TRUE)
 data1<-data[order1,]
 maxWine<-data1[1,]
 minWine<-data[1599,]

   
 
#2B)

D <- as.dist(matrix(c(0, 3,2,5,0,0,4,1,0,0,0,7,0,0,0,0), nrow = 4))
par(mfrow = c(2, 2))

hc_single <- hclust(D, method = "single")
hc_single
plot(hc_single)

hc_complete <- hclust(D, method = "complete")
plot(hc_complete)

hc_average <- hclust(D, method = "average")
plot(hc_average)

#4)

ncdata <- read.csv("./HW04/NCI60(1).csv")
dim(ncdata)
head(ncdata)
summary(ncdata)
numcol<-ncol(ncdata)


#A

std_ncdata<-scale(ncdata[,2:numcol])
D<-dist(std_ncdata)

hc_single <- hclust(D, method = "single")
plot(hc_single, main="Single Cluster Dendrogram")
hc_single$merge
hc_single$height
plot(hc_single$height, type='l')

hc_complete <- hclust(D, method = "complete")
plot(hc_complete, main="Complete Cluster Dendrogram")
hc_complete$merge
hc_complete$height
plot(hc_complete$height, type='l')


hc_avg <- hclust(D, method = "average")
plot(hc_avg, main="average Cluster Dendrogram")
hc_avg$mere
hc_avg$height
plot(hc_avg$height, type='l')

#4B

hccls_cut<-cutree(hc_complete,4)
table(hccls_cut, ncdata$labs)
plot(hc_complete, labels=ncdata$labs)
abline(h=139, col="red")

#4C

set.seed(2)
km=kmeans(std_ncdata, 4, nstart = 20)
names(km)
kmcl=km$cluster
table(kmcl,hccls_cut)
pc<-prcomp(std_ncdata, scale. = TRUE, center = TRUE)
hcout=hclust(dist(pc$x[,1:2]))
plot(hcout, labels=ncdata$labs, main="Hier. Clust. on First two PC")
table(cutree(hcout,4), ncdata$labs)


 
#3

library(cluster)
X<-matrix(c(5,1,-1,3,4,-2,1,1), nrow = 4)

class_0<-factor(c(1,1,2,2))
centrd_0<-by(X, class_0, colMeans)

######################################################
####################### HW5 ###########################

#sink("out.pdf", split = TRUE)
#pdf("out.pdf")

#Prob1
rm(list=ls())
Sys.time()

setwd("~/MS/636/HW")
raw_data <- read.csv("./HW05/Life_Expectancy.csv")

vars <- c("Life.expectancy", "Alcohol", "percentage.expenditure", "Total.expenditure", "GDP", "Income.composition.of.resources", "Schooling")
no_na_data<-na.omit(as.matrix(raw_data[, vars]))
data_st <- data.frame(no_na_data[,1], no_na_data[,-1])
colnames(data_st) <- c("Life.expectancy", vars[-1])

head(data_st)
dim(data_st)
p<-ncol(data_st)
n<-nrow(data_st)

#Ai
#linear regression , numer response
MSE<-numeric(n)
for (i in 1:n)
{
  dt_te<-data_st[i, ]
  dt_tr<-data_st[-i,]
  
  fit_dt<-lm(Life.expectancy ~ Alcohol + percentage.expenditure + Total.expenditure + GDP + 
               Income.composition.of.resources + Schooling, data = dt_tr)
  
  #if on training data overfit and biased
  #on test data is unbaised 
  pred_dt <- predict(fit_dt, newdata = dt_te)
  
  #true resp - pred data
  MSE[i] <-  (dt_te$Life.expectancy - pred_dt)^2
}

mean(MSE)

#Aii
set.seed(2)
bt<-1000
MSE_boot<-numeric(bt)
for(j in 1:bt)
{
  dt_b<-data_st[sample(1:n, replace=TRUE),]
  fit_bdt<-lm(Life.expectancy ~ Alcohol + percentage.expenditure + Total.expenditure + GDP + 
               Income.composition.of.resources + Schooling, data = dt_b)
  
  pred_bdt <- predict(fit_bdt, newdata = dt_b)
  
  MSE_boot[j] <- mean((dt_b$Life.expectancy - pred_bdt)^2)
}

sd(MSE_boot)

#Bi

pdf("HW05_1B.pdf")
# data doesnt fit with normal line
qqnorm(fit_dt$residuals); qqline(fit_dt$residuals)

#Bii
#for covariates percentage.expenditure and gdp points not random on the horizontal axis.
#may be linear regression not appropriate for this data

par(mfrow=c(1,1))
plot(data_st$Alcohol, fit_dt$residuals)
plot(data_st$percentage.expenditure, fit_dt$residuals)
plot(data_st$Total.expenditure, fit_dt$residuals)
plot(data_st$GDP, fit_dt$residuals)
plot(data_st$Income.composition.of.resources, fit_dt$residuals)
plot(data_st$Schooling, fit_dt$residuals)
dev.off()
 
#Ci

#shrink beta coff to zero if not contr to model
#fit 5k pv no use 
# reg approach impose some penalties when too many coeff big in abs value...
#it will shrink them to zero if covariate coff no impact


library(glmnet)
set.seed(101)

grid<-10^seq(10,-2,length=100)  
cv_lasso <- cv.glmnet(as.matrix(data_st[, -1]), data_st$Life.expectancy, alpha = 1, family = "gaussian", 
                      type.measure = "mse", lambda=grid)
lambda_min <- cv_lasso$lambda.min
lambda_min


#Cii
lasso_mean<-numeric(n)
for (te in 1:n)
{
fit_lasso <- glmnet(as.matrix(data_st[-te, -1]), data_st[-te,]$Life.expectancy, alpha = 1, family = "gaussian", lambda = grid)
pred_lasso <- predict(fit_lasso, s = lambda_min, newx = as.matrix(data_st[te,-1]))
lasso_mean[i]<-((data_st[te,]$Life.expectancy-pred_lasso)^2)
}

mean(lasso_mean)

#Ciii

set.seed(2)
lasso_bt<-1000
lasso_MSE_boot<-numeric(bt)
for(k in 1:lasso_bt)
{
  tr<-sample(1:n, replace=TRUE)
   
  fit_lasso_bt <- glmnet(as.matrix(data_st[tr, -1]), data_st$Life.expectancy[tr], alpha = 1, family = "gaussian", lambda = grid)
  pred_lasso_bt <- predict(fit_lasso_bt, s = lambda_min, newx = as.matrix(data_st[tr,-1]))
  lasso_MSE_boot[k]<-mean((data_st$Life.expectancy[tr] - pred_lasso_bt)^2)
}

sd(lasso_MSE_boot)

#Civ

#Alchol, percentage.expenditure, Total.expenditure shrinkage to zero
pred_lasso_coeff <- predict(fit_lasso, type="coefficients", s = lambda_min)
round(pred_lasso_coeff, 2)

#Prob2

rm(list=ls())
Sys.time()

setwd("~/MS/636/HW")
DTA <- read.csv("./HW05/hof_data.csv")
num_vars <- c("H", "HR", "AVG")
no_na_data<-na.omit(as.matrix(DTA[, num_vars]))

X_st <- scale(no_na_data, center = TRUE, scale = TRUE)
DTA_st <- data.frame(DTA$HOF, X_st)
colnames(DTA_st) <- c("HOF", num_vars)
p <- ncol(DTA_st)
n <-nrow(DTA_st)

hof_yes <- DTA_st[which(DTA_st$HOF == 'Y'),]
hof_no <- DTA_st[which(DTA_st$HOF == 'N'),]
set.seed(2)

#train data
hof_y_sample <- sample(1:nrow(hof_yes), (2/3)*nrow(hof_yes))
hof_n_sample <- sample(1:nrow(hof_no), (2/3)*nrow(hof_no))
tr_hof_y <- DTA_st[hof_y_sample, ]
tr_hof_n <- DTA_st[hof_n_sample, ]
tr_data <- rbind(tr_hof_y, tr_hof_n)

#test data
hof_y_sample_te <- sample(1:nrow(hof_yes), (1/3)*nrow(hof_yes))
hof_n_sample_te <- sample(1:nrow(hof_no), (1/3)*nrow(hof_no))
te_hof_y <- DTA_st[hof_y_sample_te, ]
te_hof_n <- DTA_st[hof_n_sample_te, ]
te_data <- rbind(te_hof_y, te_hof_n)

#Ai

fit_glmodel <- glm (HOF ~ H + HR + AVG, data = tr_data, family = binomial)
pred_glm <- predict(fit_glmodel,type = 'response')

tr_truth_table<-table(tr_data$HOF, pred_glm > 0.5)[2:1,2:1]

tr_msclass_rate<-(1-sum(diag(tr_truth_table))/sum(tr_truth_table))
tr_msclass_rate

tr_sen<-tr_truth_table[1]/(tr_truth_table[1] + tr_truth_table[3])
tr_sen

tr_spec<-tr_truth_table[4]/(tr_truth_table[4] + tr_truth_table[2])
tr_spec

#Aii

fit_glmodel_te <- glm (HOF ~ H + HR + AVG, data = te_data, family = binomial)
pred_glm_te <- predict(fit_glmodel_te,type = 'response')

te_truth_table<-table(te_data$HOF, pred_glm_te > 0.5)[2:1,2:1]

te_msclass_rate<-(1-sum(diag(te_truth_table))/sum(te_truth_table))
te_msclass_rate

te_sen<-te_truth_table[1]/(te_truth_table[1] + te_truth_table[3])
te_sen

te_spec<-te_truth_table[4]/(te_truth_table[4] + te_truth_table[2])
te_spec


#Bii

fit_glmodel_cv <- glm (HOF ~ H + HR + AVG, data = tr_data, family = binomial)
pred_glm_cv <- predict(fit_glmodel_te,newdata=te_data,type = 'response')

cv_truth_table<-table(te_data$HOF, pred_glm_cv > 0.5)[2:1,2:1]
cv_truth_table

cv_msclass_rate<-(1-sum(diag(cv_truth_table))/sum(cv_truth_table))
cv_msclass_rate

cv_sen<-cv_truth_table[1]/(cv_truth_table[1] + cv_truth_table[3])
cv_sen

cv_spec<-cv_truth_table[4]/(cv_truth_table[4] + cv_truth_table[2])
cv_spec

#Bi
library(ROCR)
ROCRpred <- prediction(pred_glm_cv, te_data$HOF)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf)

#Ci

set.seed(900)

grid<-10^seq(10,-2,length=100)  
hof_cv_lasso <- cv.glmnet(as.matrix(tr_data[, -1]), as.numeric(tr_data$HOF), alpha = 1, family = "gaussian", 
                      type.measure = "mse", lambda=grid)
hof_lambda_min <- hof_cv_lasso$lambda.min
hof_lambda_min


#Cii

hof_fit_lasso <- glmnet(as.matrix(tr_data[, -1]), as.numeric(tr_data$HOF), alpha = 1, family = "gaussian", lambda = grid)
hof_pred_lasso <- predict(hof_fit_lasso, s = hof_lambda_min, newx = as.matrix(te_data[, -1]))
hof_lasso_coeff <- predict(hof_fit_lasso, type="coefficients", s = hof_lambda_min)
round(hof_lasso_coeff, 2)


#Ciii

hof_lasso_truth_table<-table(te_data$HOF, hof_pred_lasso > 0.5)[2:1,2:1]
hof_lasso_truth_table

hof_lasso_msclass_rate<-(1-sum(diag(hof_lasso_truth_table))/sum(hof_lasso_truth_table))
hof_lasso_msclass_rate

hof_las_sen<-hof_lasso_truth_table[1]/(hof_lasso_truth_table[1] + hof_lasso_truth_table[3])
hof_las_sen

hof_las_spec<-hof_lasso_truth_table[4]/(hof_lasso_truth_table[4] + hof_lasso_truth_table[2])
hof_las_spec



