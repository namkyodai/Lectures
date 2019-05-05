#This program is coded by Nam Lethanh for use in the class IMP-HS2014
#Purpose: To calculate availability
data <- read.csv("IMP-5-A-availability-network.csv",header=TRUE,sep=",") #
attach(data)
T=20
N=10
cat("How data looks like?\n")
print(data)


#define function
#reliability function for exponential distribution
failureexp<-function(t,lambda){lambda*exp(-lambda*t)}
survivalexp<-function(t,lambda){exp(-lambda*t)}
#reliability function for weibull distribution

failureweib<-function(t,theta,m){theta*m*(theta*t)^(m-1)*exp(-(theta*t)^m)}
rateweib<-function(t,theta,m){theta*m*(theta*t)^(m-1)}
survivalweib<-function(t,theta,m){exp(-(theta*t)^m)}

#define structure function
decompose<-function(x,y,z){
  x*y+(1-x)*z
}
#calculating the structure function when link 1-3 is perfect and link 3-5 is perfect
expression1<-function(xA1,xA4,x34,x45,xB5,xB2,x12,x23){
  (1-(1-xA1)*(1-xA4*(1-(1-x34)*(1-x45))))*(1-(1-xB5)*(1-xB2*(1-(1-x12)*(1-x23))))
}

#calculating the structure function when link 1-3 is perfect and link 3-5 is not perfect

expression2<-function(xA1,xA4,x34,x45,xB5,xB2,x12,x23){
  xA1*x34*xB2*(1-(1-x12)*(1-x23))+xA1*x34*x45*xB5+xA4*x34*xB2*(1-(1-x12)*(1-x23))+xA4*x34*x45*xB5+xA1*xA4*x45*xB5*xB2*(1-(1-x12)*(1-x23))-xA1*xA4*x34*xB2*(1-(1-x12)*(1-x23))-xA1*x34*xB2*(1-(1-x12)*(1-x23))*x45*xB5-xA1*xA4*x34*x45*xB5-xA4*x34*xB2*(1-(1-x12)*(1-x23))*x45*xB5
}

#calculating the structure function when link 1-3 is not perfect and link 3-5 is perfect
expression3<-function(xA1,xA4,x34,x45,xB5,xB2,x12,x23){
  xA1*x12*x23*xB2+xA1*x12*x23*xB5+xA4*(1-(1-x34)*(1-x45))*x23*xB2+xA4*(1-(1-x34)*(1-x45))*x23*xB5+xA1*x12*xA4*(1-(1-x34)*(1-x45))*xB5*xB2-xA1*x12*xA4*(1-(1-x34)*(1-x45))*x23*xB2-xA1*x12*x23*xB2*xB5-xA1*x12*xA4*(1-(1-x34)*(1-x45))*x23*xB5-xA4*(1-(1-x34)*(1-x45))*x23*xB5*xB2
}

#calculating the structure function when link 1-3 is not perfect and link 3-5 is not perfect
expression4<-function(xA1,xA4,x34,x45,xB5,xB2,x12,x23){
  xA1*x12*x23*x34*xB2+xA1*x12*x23*x34*x45*xB5+xA4*x23*x34*xB2+xA4*x23*x34*x45*xB5+xA1*x12*xA4*xB2*x45*xB5-xA1*x12*xA4*x23*x34*xB2-xA1*x12*x23*x34*xB2*x45*xB5-xA1*x12*xA4*x23*x34*x45*xB5-xA4*x23*x34*xB2*x45*xB5
}

xA1<-matrix(double(1),nrow=T,ncol=1)
xA4<-matrix(double(1),nrow=T,ncol=1)
x12<-matrix(double(1),nrow=T,ncol=1)
x13<-matrix(double(1),nrow=T,ncol=1)
x23<-matrix(double(1),nrow=T,ncol=1)
x34<-matrix(double(1),nrow=T,ncol=1)
x35<-matrix(double(1),nrow=T,ncol=1)
x45<-matrix(double(1),nrow=T,ncol=1)
xB2<-matrix(double(1),nrow=T,ncol=1)
xB5<-matrix(double(1),nrow=T,ncol=1)
R131<-matrix(double(1),nrow=T,ncol=1)
R130<-matrix(double(1),nrow=T,ncol=1)
R<-matrix(double(1),nrow=T,ncol=1)
b<-matrix(double(1),nrow=T,ncol=1)


for (t in 0:T){
  
  xA1[t]<-survivalexp(t,data$alpha[1])
  x23[t]<-survivalexp(t,data$alpha[2])
  x34[t]<-survivalexp(t,data$alpha[3])
  x35[t]<-survivalexp(t,data$alpha[4])
  x45[t]<-survivalexp(t,data$alpha[5])
  x12[t]<-survivalweib(t,data$alpha[6],data$m[6])
  x13[t]<-survivalweib(t,data$alpha[7],data$m[7])
  xA4[t]<-survivalweib(t,data$alpha[8],data$m[8])
  xB2[t]<-survivalexp(t,data$alpha[9])
  xB5[t]<-survivalexp(t,data$alpha[10])
  
  
  R131[t]<-decompose(x35[t],expression1(xA1[t],xA4[t],x34[t],x45[t],xB5[t],xB2[t],x12[t],x23[t]),expression2(xA1[t],xA4[t],x34[t],x45[t],xB5[t],xB2[t],x12[t],x23[t]))
  
  #R130[t]<-decompose(x35,expression3(xA1[T[t]],xA4[T[t]],x34[T[t]],x45[T[t]],xB5[T[t]],xB2[T[t]],x12[T[t]],x23[T[t]]),expression4(xA1[T[t]],xA4[T[t]],x34[T[t]],x45[T[t]],xB5[T[t]],xB2[T[t]],x12[T[t]],x23[T[t]]))
  
  
  #R[t]<-decompose(x13[t],R131[t],R130[t])
  
}





print(cbind(R131,R130,R))

