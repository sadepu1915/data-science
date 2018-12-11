x =  c(15.321, 9.008, 20.104, 7.729, 45.154, 8.404, 5.332, 0.577, 4.305, 4.517,
  12.594, 6.829, 3.291, 37.175, 0.841, 1.317, 7.613, 20.582, 2.030, 10.001,
  4.666, 12.933, 0.591, 39.454,  8.875)
n =  length(x)
x  =  sort(x)
weib  =  log(x)
y = -log(x)
y = sort(y)
n  =  length(weib)
i =  1:n
ui =  (i-.5)/n
QW =  log(-log(1-ui))

postscript("u:/meth1/psfiles/weibgofmle.ps",height=6,horizontal=F)

plot(QW,weib,abline(lm(weib~QW)),
       main="Weibull Reference Plot",cex=.75,lab=c(7,11,7),
       xlab="Q(u) = log(-log(1-u))",
       ylab="log(W(i))" )
legend(-3.5,5.0,"y = 4.388+.4207 Q(u)")
legend(-3.5,4.7,"ADM = .3413, pvalue > .25")

library(MASS)
fitdistr(x,"weibull")
#  OUTPUT from R:
#
#       shape        scale
#    0.9839245   11.4852981
#  ( 0.1512936) ( 2.4660607)
#gamma = 0.9839245
#alpha = 11.4852981
gamma = fitdistr(x,"weibull")$estimate[1]
alpha = fitdistr(x,"weibull")$estimate[2]
a = -log(alpha) 
b = 1/gamma
z   = exp(-exp(-(y-a)/b))  
A1i = (2*i-1)*log(z)
A2i = (2*n+1-2*i)*log(1-z)
s1  = sum(A1i)
s2  = sum(A2i)

AD  = -n-(1/n)*(s1+s2)
ADM  = AD*(1+.2/sqrt(n))
ADM
#ADM = 0.303 with p-value = p-value > .25






B  =  10000
W  =  matrix(0,B,n)
A =  numeric(B)
A =  rep(0,B)
G =  numeric(B)
G =  rep(0,B)
S = numeric(B)
S = rep(0,B)
{
for (i in 1:B) 
W[i,]  =  rweibull(n,gamma,alpha)
}

{
for (i in 1:B) 
G[i] = fitdistr(W[i,],"weibull")$estimate[1]
}
{
for (i in 1:B) 
A[i] = fitdistr(W[i,],"weibull")$estimate[2]
}
{
for (i in 1:B) 
S[i] = exp(-(20/A[i])^G[i])
}
summary(S)
sd(S)

postscript("u:/meth1/psfiles/boxplotparaboot.ps",height=6,horizontal=F)


boxplot(S,ylab="S(20)",main="Boxplot of 1000 values of S(20)")
out=c(mean(G),sd(G),mean(A),sd(A))
out
graphics.off()
