#This program is coded by Nam Lethanh for use in the class IMP-HS2014
#Purpose: To calculate reliability from Cox Model
data=read.csv("example2.csv",header=TRUE)
attach(data)
library(survival)
likelihood=survreg(Surv(time,status)~1,data=data,dist="weibull")
print(likelihood)

density<-function(t,theta,m){theta*m*(theta*t)^(m-1)*exp(-(theta*t)^m)}
rate<-function(t,theta,m){theta*m*(theta*t)^(m-1)}
reliability<-function(t,theta,m){exp(-(theta*t)^m)}
TIME=100
T=seq(0,TIME, by =1)
den<-matrix(double(1),nrow=T,ncol=1)
hazard<-matrix(double(1),nrow=T,ncol=1)
reli<-matrix(double(1),nrow=T,ncol=1)
#theta=0.912
#m=3.624

theta=1/exp(3.624)
m=1/0.912

for (t in (1:length(T))){
  den[t]<-density(T[t],theta,m)
  hazard[t]<-rate(T[t],theta,m)
  reli[t]<-reliability(T[t],theta,m)
}
results=cbind(den,hazard,reli)
print(results)

plot.new()
par(mar=c(5,4,4,6)+0.3)
limy=c(0,1)
limx=c(0,100)
plot(T,reli,lwd=2,col="red",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=4)
axis(2,ylim=limy,col="black",las=1)
axis(1,c(seq(0,100,by=10)),c(seq(0,100,by=10)))
mtext(expression(paste('Reliability')),side=2,col="black",line=3)
mtext(expression(paste('Units of time')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
par(new=TRUE)
limy=c(0,0.05)
limx=c(0,100)
plot(T,hazard,lwd=2,col="blue",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=2)
axis(4,ylim=limy,col="black",las=1)
mtext(expression(paste('Failure rate')),side=4,col="black",line=3)
colors=c("red","blue")
legend("topright", inset=0.09, col=colors,lty=2,lwd=2,legend=c("Reliability","Failure rate"),pch=c(4,2),bg="azure2",cex=1.3)

cat("THE END")


