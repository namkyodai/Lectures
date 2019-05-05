#This program is coded by Nam Lethanh for use in the class IMP
#Purpose: To calculate reliability from Cox Model
data=read.csv("IMP-A-fan.csv",header=TRUE)
attach(data)
library(survival)
likelihood=survreg(Surv(time,status)~1,data=data,dist="weibull")
print(likelihood)

density<-function(t,theta,m){theta*m*(theta*t)^(m-1)*exp(-(theta*t)^m)}
rate<-function(t,theta,m){theta*m*(theta*t)^(m-1)}
reliability<-function(t,theta,m){exp(-(theta*t)^m)}
TIME=10
T=seq(0,TIME, by =0.1)
den<-matrix(double(1),nrow=T,ncol=1)
hazard<-matrix(double(1),nrow=T,ncol=1)
reli<-matrix(double(1),nrow=T,ncol=1)
theta=1/exp(2.915419)
m=1/0.170694
for (t in (1:length(T))){
  den[t]<-density(T[t],theta,m)
  hazard[t]<-rate(T[t],theta,m)
  reli[t]<-reliability(T[t],theta,m)
}
results=cbind(den,hazard,reli)
print(results)

theta=1/exp(2.915419)
m=1/0.170694

a=integrate(reliability, lower = 4, upper = Inf,theta=theta,m=m)$value
print(a)


