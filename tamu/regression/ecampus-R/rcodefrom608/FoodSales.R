food<-read.csv("/Users/home/Documents/TAMU/stat608/Data/food.csv",header=TRUE)
#Week Sales Price

m1<-lm(food$Sales ~ food$Price)
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,1,1)) 
plot(food$Price, food$Sales, pch=19, xlab="Price", ylab="Sales")
abline(m1)

#Now transform both x and y:
x<-log(food$Price)
y<-log(food$Sales)


m2<-lm(y~x)
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,1,1), mfrow=c(1,1)) 
plot(x, y, pch=19, xlab="log(Price)", ylab="log(Sales)")
abline(m2)


par(cex.main=2, cex.axis = 1.5, cex.lab=1.5, mar=c(5,5,2,1.5), mfrow=c(2,2), pch=19) 
plot(m2)


