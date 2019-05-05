################################
# example of Bayesian estimation for a linear model
# Coded by Nam Lethanh (lethanh@ibi.baug.ethz.ch)
################################
library(LearnBayes) #requires R package to perform Bayesian estimation
data=read.csv("accidentdata.csv", header=TRUE) #Data
attach(data) 
plot(noaccident,CS,xlab="Numbers of accident")
plot(noaccident,dtv,xlab="Numbers of accident")
plot(noaccident,slope,xlab="Numbers of accident")
plot(noaccident,speed,xlab="Numbers of accident")
##### Least-squares fit
fit=lm(noaccident~CS+dtv+slope+speed,data=data,x=TRUE,y=TRUE)
summary(fit)
cat("RESULTS OF LEAST SQUARED FIT METHOD \n")
print(summary(fit))
##### Sampling from posterior
N=10000 #sampling numbers
theta.sample=blinreg(fit$y,fit$x,N) #Sampling from Posterior function
hist(theta.sample$beta[,1],main="Intercept",
     xlab=expression(beta[0]))
hist(theta.sample$beta[,2],main="CS",
     xlab=expression(beta[1]))
hist(theta.sample$beta[,3],main="dtv",
     xlab=expression(beta[2]))
hist(theta.sample$beta[,4],main="slope",
     xlab=expression(beta[3]))
hist(theta.sample$beta[,5],main="speed",
     xlab=expression(beta[4]))
hist(theta.sample$sigma,main="error",
     xlab=expression(epsilon))
cat("RESULTS OF BAYESIAN ESTIMATION \n")
cat("Confident interval \n")
quantile1<-apply(theta.sample$beta,2,quantile,c(.05,.5,.95))
quantile2<-quantile(theta.sample$sigma,c(.05,.5,.95))
print(quantile1)
print(quantile2)
cat("THE END")