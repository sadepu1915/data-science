
install.packages("lsmeans")
install.packages("ggplot2")
install.packages("lme4")
library(lsmeans)
library(ggplot2)
library(lme4)

stim = as.factor(c(rep("S1",15),rep("S2",16),rep("S3",14),rep("S4",12)))
plotS1 = as.factor(c(rep("1",3),rep("2",3),rep("3",3),rep("4",3),rep("5",3)))
plotS2 = as.factor(c(rep("1",3),rep("2",3),rep("3",3),rep("4",2),rep("5",3),rep("6",2)))
plotS3 = as.factor(c(rep("1",3),rep("2",3),rep("3",2),rep("4",2),rep("5",2),rep("6",2)))
plotS4 = as.factor(c(rep("1",3),rep("2",3),rep("3",3),rep("4",3)))
plot = as.factor(c(plotS1,plotS2,plotS3,plotS4))
rootwt = 
c(3.3,3.4,3.5,3.1,3.5,3.0,3.2,3.1,3.4,3.3,2.9,3.0,3.3,3.3,3.1,  
  3.8,3.7,4.0,3.5,3.8,3.9,3.6,3.4,3.8,3.4,3.7,3.6,3.7,3.6,3.5,3.9, 
  3.8,3.9,4.0,3.6,3.7,3.8,3.3,3.4,3.6,3.7,3.5,3.9,3.4,3.7,
  4.3,4.3,4.4,4.1,3.9,3.8,4.2,4.1,3.9,3.7,3.9,4.0)

ranmod = lmer(rootwt ~ 1 + stim + (1|plot:stim))

summary(ranmod)
anova(ranmod)
lsmeans(ranmod, "stim")


