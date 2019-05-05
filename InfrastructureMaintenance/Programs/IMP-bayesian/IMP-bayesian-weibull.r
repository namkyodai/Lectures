#This program was coded by Nam Lethanh for use in IMP class
data=read.csv("pipelinedata.csv", header=TRUE) #Data
attach(data) 
library(LearnBayes) #R package for Bayesian statistics
library(splines)
library(survival) #R package for regression analysis with MLE approach
#BEGIN Bayesian estimation approach on data
#-----function to define posterior likelihood
weibullregpost=function (theta, data)
{
  logf = function(t, c, x, sigma, mu, beta) {
    z = (log(t) - mu - x %*% beta)/sigma
    f = 1/sigma * exp(z - exp(z))
    S = exp(-exp(z))
    c * log(f) + (1 - c) * log(S) }
  k = dim(data)[2]
  p = k - 2
  t = data[, 1]
  c = data[, 2]
  X = data[, 3:k]
  sigma = exp(theta[1])
  mu = theta[2]
  beta = array(theta[3:k], c(p, 1))
  return(sum(logf(t, c, X, sigma, mu, beta)))
}
#-----------------------------
start=c(-.5,9,.5,-.05)
d=cbind(time,status,waterlost,depth)
fit=laplace(weibullregpost,start,d)
proposal=list(var=fit$var,scale=1.5)
bayesfit=rwmetrop(weibullregpost,proposal,fit$mode,10000,d) #This function rwmetrop is a standard function in LearnBayes R package describing the Metro-Polish Hasting Algorithm used to generate data based on MCMC.
par(mfrow=c(2,2))
sigma=exp(bayesfit$par[,1])
mu=bayesfit$par[,2]
beta1=bayesfit$par[,3]
beta2=bayesfit$par[,4]
hist(mu,xlab="Intercept", main="")
hist(sigma,xlab="sigma",main="")
#hist(mu,xlab="mu",main="")
hist(beta1,xlab="Water lost",main="")
hist(beta2,xlab="depth",main="")
#END of Bayesian estimation approach for the data
cat("RESULTS OF BAYESIAN ESTIMATION \n")
cat("Confident interval \n")
beta<-c(mu,beta1,beta2)
cat("THE END")
mean(beta1)
