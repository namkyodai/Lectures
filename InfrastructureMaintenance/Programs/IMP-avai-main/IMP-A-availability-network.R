#This program is coded by Nam Lethanh for use in the class IMP-HS2014
#Purpose: To calculate availability
data <- read.csv("IMP-10-HS2014-A-availability-network.csv",header=TRUE,sep=",") #
attach(data)
TIME=20
T=seq(0,TIME, by =1) #investigate time
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

#reliability of the link
rA1<-matrix(double(1),nrow=length(T)-1,ncol=1) 
rA4<-matrix(double(1),nrow=length(T)-1,ncol=1)
r12<-matrix(double(1),nrow=length(T)-1,ncol=1)
r13<-matrix(double(1),nrow=length(T)-1,ncol=1)
r23<-matrix(double(1),nrow=length(T)-1,ncol=1)
r34<-matrix(double(1),nrow=length(T)-1,ncol=1)
r35<-matrix(double(1),nrow=length(T)-1,ncol=1)
r45<-matrix(double(1),nrow=length(T)-1,ncol=1)
rB2<-matrix(double(1),nrow=length(T)-1,ncol=1)
rB5<-matrix(double(1),nrow=length(T)-1,ncol=1)
#reliability of the decompose function when link 1-3 is perfect
R131<-matrix(double(1),nrow=length(T)-1,ncol=1)
#reliability of the decompose function when link 1-3 is not perfect
R130<-matrix(double(1),nrow=length(T)-1,ncol=1)
#reliability of the network
R<-matrix(double(1),nrow=length(T)-1,ncol=1) 


#failure of each link
fA1<-matrix(double(1),nrow=length(T)-1,ncol=1) 
fA4<-matrix(double(1),nrow=length(T)-1,ncol=1)
f12<-matrix(double(1),nrow=length(T)-1,ncol=1)
f13<-matrix(double(1),nrow=length(T)-1,ncol=1)
f23<-matrix(double(1),nrow=length(T)-1,ncol=1)
f34<-matrix(double(1),nrow=length(T)-1,ncol=1)
f35<-matrix(double(1),nrow=length(T)-1,ncol=1)
f45<-matrix(double(1),nrow=length(T)-1,ncol=1)
fB2<-matrix(double(1),nrow=length(T)-1,ncol=1)
fB5<-matrix(double(1),nrow=length(T)-1,ncol=1)
#availability of each link
aA1<-matrix(double(1),nrow=length(T)-1,ncol=1) 
aA4<-matrix(double(1),nrow=length(T)-1,ncol=1)
a12<-matrix(double(1),nrow=length(T)-1,ncol=1)
a13<-matrix(double(1),nrow=length(T)-1,ncol=1)
a23<-matrix(double(1),nrow=length(T)-1,ncol=1)
a34<-matrix(double(1),nrow=length(T)-1,ncol=1)
a35<-matrix(double(1),nrow=length(T)-1,ncol=1)
a45<-matrix(double(1),nrow=length(T)-1,ncol=1)
aB2<-matrix(double(1),nrow=length(T)-1,ncol=1)
aB5<-matrix(double(1),nrow=length(T)-1,ncol=1)

#reliability of the decompose function when link 1-3 is perfect
A131<-matrix(double(1),nrow=length(T)-1,ncol=1)
#reliability of the decompose function when link 1-3 is not perfect
A130<-matrix(double(1),nrow=length(T)-1,ncol=1)
#reliability of the network
A<-matrix(double(1),nrow=length(T)-1,ncol=1) 


for (t in 1:(length(T))){
  #reliability
  rA1[t]<-survivalexp(T[t],data$alpha[1])
  r23[t]<-survivalexp(T[t],data$alpha[2])
  r34[t]<-survivalexp(T[t],data$alpha[3])
  r35[t]<-survivalexp(T[t],data$alpha[4])
  r45[t]<-survivalexp(T[t],data$alpha[5])
  r12[t]<-survivalweib(T[t],data$alpha[6],data$m[6])
  r13[t]<-survivalweib(T[t],data$alpha[7],data$m[7])
  rA4[t]<-survivalweib(T[t],data$alpha[8],data$m[8])
  rB2[t]<-survivalexp(T[t],data$alpha[9])
  rB5[t]<-survivalexp(T[t],data$alpha[10])
  #availability
  aA1[t]<-integrate(survivalexp,0,t,data$alpha[1])$value/(integrate(survivalexp,0,t,data$alpha[1])$value+(data$mit[1]/365)*(1-rA1[t]))
  a23[t]<-integrate(survivalexp,0,t,data$alpha[2])$value/(integrate(survivalexp,0,t,data$alpha[2])$value+(data$mit[2]/365)*(1-rA1[t]))
  a34[t]<-integrate(survivalexp,0,t,data$alpha[3])$value/(integrate(survivalexp,0,t,data$alpha[3])$value+(data$mit[3]/365)*(1-rA1[t]))
  a35[t]<-integrate(survivalexp,0,t,data$alpha[4])$value/(integrate(survivalexp,0,t,data$alpha[4])$value+(data$mit[4]/365)*(1-rA1[t]))
  a45[t]<-integrate(survivalexp,0,t,data$alpha[5])$value/(integrate(survivalexp,0,t,data$alpha[5])$value+(data$mit[5]/365)*(1-rA1[t]))
  a12[t]<-integrate(survivalweib,0,t,data$alpha[6],data$m[6])$value/(integrate(survivalweib,0,t,data$alpha[6],data$m[6])$value+(data$mit[6]/365)*(1-rA1[t]))
  a13[t]<-integrate(survivalweib,0,t,data$alpha[7],data$m[7])$value/(integrate(survivalweib,0,t,data$alpha[7],data$m[7])$value+(data$mit[7]/365)*(1-rA1[t]))
  aA4[t]<-integrate(survivalweib,0,t,data$alpha[8],data$m[8])$value/(integrate(survivalweib,0,t,data$alpha[8],data$m[8])$value+(data$mit[8]/365)*(1-rA1[t]))
  aB2[t]<-integrate(survivalexp,0,t,data$alpha[9])$value/(integrate(survivalexp,0,t,data$alpha[9])$value+(data$mit[9]/365)*(1-rA1[t]))
  aB5[t]<-integrate(survivalexp,0,t,data$alpha[10])$value/(integrate(survivalexp,0,t,data$alpha[10])$value+(data$mit[10]/365)*(1-rA1[t]))
  
  #reliability of the decompose link
 R131[T[t]]<-decompose(r35[T[t]],expression1(rA1[T[t]],rA4[T[t]],r34[T[t]],r45[T[t]],rB5[T[t]],rB2[T[t]],r12[T[t]],r23[T[t]]),expression2(rA1[T[t]],rA4[T[t]],r34[T[t]],r45[T[t]],rB5[T[t]],rB2[T[t]],r12[T[t]],r23[T[t]]))
  
 R130[T[t]]<-decompose(r35[T[t]],expression3(rA1[T[t]],rA4[T[t]],r34[T[t]],r45[T[t]],rB5[T[t]],rB2[T[t]],r12[T[t]],r23[T[t]]),expression4(rA1[T[t]],rA4[T[t]],r34[T[t]],r45[T[t]],rB5[T[t]],rB2[T[t]],r12[T[t]],r23[T[t]]))
 #reliability of the network  
  R[T[t]]<-decompose(r13[T[t]],R131[T[t]],R130[T[t]])
 
 #availability of the decompose link
 A131[T[t]]<-decompose(a35[T[t]],expression1(aA1[T[t]],aA4[T[t]],a34[T[t]],a45[T[t]],aB5[T[t]],aB2[T[t]],a12[T[t]],a23[T[t]]),expression2(aA1[T[t]],aA4[T[t]],a34[T[t]],a45[T[t]],aB5[T[t]],aB2[T[t]],a12[T[t]],a23[T[t]]))
 
 A130[T[t]]<-decompose(a35[T[t]],expression3(aA1[T[t]],aA4[T[t]],a34[T[t]],a45[T[t]],aB5[T[t]],aB2[T[t]],a12[T[t]],a23[T[t]]),expression4(aA1[T[t]],aA4[T[t]],a34[T[t]],a45[T[t]],aB5[T[t]],aB2[T[t]],a12[T[t]],a23[T[t]]))
 #availability of the network  
 A[T[t]]<-decompose(a13[T[t]],A131[T[t]],A130[T[t]])
 
}

cat("value of reliability for each link and the network")
R_link<-data.frame(rA1,r23,r34,r35,r45,r12,r13,rA4,rB2,rB5)
print(R_link)
print(R)
cat("value of availability for each link and the network")

A_link<-data.frame(aA1,a23,a34,a35,a45,a12,a13,aA4,aB2,aB5)
print(A_link)
print(A)


#plotting
#Plotting

plot.new()
par(mar=c(5,4,4,6)+0.3)
limy=c(0,1)
limx=c(0,TIME)
plot(R,lwd=4,col="red",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=4,cex=0.8)
axis(2,ylim=limy,col="black",las=1)
axis(1,c(seq(0,TIME,by=1)),c(seq(0,TIME,by=1)))
mtext(expression(paste('Probability')),side=2,col="black",line=3)
mtext(expression(paste('Units of time')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()

par(new=TRUE)
limx=c(1,TIME)
plot(A,lwd=4,col="blue",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=14,type="b",pch=22,cex=0.8)

par(new=TRUE)
limy=c(0,1)
limx=c(1,TIME)
plot(rA1,lwd=1,col="cyan",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=3,cex=0.6)

par(new=TRUE)
limy=c(0,1)
limx=c(1,TIME)
plot(r23,lwd=1,col="chocolate4",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=4,cex=0.6)

par(new=TRUE)
limy=c(0,1)
limx=c(1,TIME)
plot(r34,lwd=1,col="chartreuse4",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=5,cex=0.6)

par(new=TRUE)
limy=c(0,1)
limx=c(1,TIME)
plot(r35,lwd=1,col="darkgoldenrod",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=6,cex=0.6)

par(new=TRUE)
limy=c(0,1)
limx=c(1,TIME)
plot(r45,lwd=1,col="blueviolet",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=7,cex=0.6)


par(new=TRUE)
limy=c(0,1)
limx=c(1,TIME)
plot(r12,lwd=1,col="dodgerblue1",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=8,cex=0.6)

par(new=TRUE)
limy=c(0,1)
limx=c(1,TIME)
plot(r13,lwd=1,col="darkslategray4",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=9,cex=0.6)


par(new=TRUE)
limy=c(0,1)
limx=c(1,TIME)
plot(rA4,lwd=1,col="deeppink2",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=10,cex=0.6)

par(new=TRUE)
limy=c(0,1)
limx=c(1,TIME)
plot(rB2,lwd=1,col="gray57",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=11,cex=0.6)

par(new=TRUE)
limy=c(0,1)
limx=c(1,TIME)
plot(rB5,lwd=1,col="hotpink1",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=12,cex=0.6)


colors=c("red","blue","cyan","chocolate4","chartreuse4","darkgoldenrod","blueviolet","dodgerblue1","darkslategray4","deeppink2","gray57","hotpink1")

items<-c("R-network","A-network","R-A1","R-23","R-34","R-35","R-45","R-12","R-13","R-A4","R-B2","R-B5")

linestyles<-c(1,1,1,1,1,1,1,1,1,1,1,1)
lineweights<-c(4,4,1,1,1,1,1,1,1,1,1,1)
pchstyles<-c(4,22,3,4,5,6,7,8,9,10,11,12)


legend("bottomleft", inset=0.009, col=colors,lty=linestyles,lwd=lineweights,legend=items,pch=pchstyles,bg="azure2",cex=0.8)

#mean time between failure
mA1<-integrate(survivalexp,0,Inf,data$alpha[1])$value
m23<-integrate(survivalexp,0,Inf,data$alpha[2])$value
m34<-integrate(survivalexp,0,Inf,data$alpha[3])$value
m35<-integrate(survivalexp,0,Inf,data$alpha[4])$value
m45<-integrate(survivalexp,0,Inf,data$alpha[5])$value
m12<-integrate(survivalweib,0,Inf,data$alpha[6],data$m[6])$value
m13<-integrate(survivalweib,0,Inf,data$alpha[7],data$m[7])$value
mA4<-integrate(survivalweib,0,Inf,data$alpha[8],data$m[8])$value
mB2<-integrate(survivalexp,0,Inf,data$alpha[9])$value
mB5<-integrate(survivalexp,0,Inf,data$alpha[10])$value

#cost of intervention for each link
cost<-matrix(double(1),nrow=4,ncol=10)
cost[1,1]<-cA1_owner<-20*(data$owner[1])*data$mit[1]*(1-R_link[21,1])/(mA1+data$mit[1]/365)
cost[2,1]<-cA1_user<-20*(data$user[1])*data$mit[1]*(1-R_link[21,1])/(mA1+data$mit[1]/365)
cost[3,1]<-cA1_dap<-20*(data$dap[1])*data$mit[1]*(1-R_link[21,1])/(mA1+data$mit[1]/365)
cost[4,1]<-cA1_iap<-20*(data$iap[1])*data$mit[1]*(1-R_link[21,1])/(mA1+data$mit[1]/365)
cA1<-cA1_owner+cA1_user+cA1_dap+cA1_iap

cost[1,2]<-c23_owner<-20*(data$owner[2])*data$mit[2]*(1-R_link[21,2])/(mA1+data$mit[2]/365)
cost[2,2]<-c23_user<-20*(data$user[2])*data$mit[2]*(1-R_link[21,2])/(mA1+data$mit[2]/365)
cost[3,2]<-c23_dap<-20*(data$dap[2])*data$mit[2]*(1-R_link[21,2])/(mA1+data$mit[2]/365)
cost[4,2]<-c23_iap<-20*(data$iap[2])*data$mit[2]*(1-R_link[21,2])/(mA1+data$mit[2]/365)
c23<-c23_owner+c23_user+c23_dap+c23_iap

cost[1,3]<-c34_owner<-20*(data$owner[3])*data$mit[3]*(1-R_link[21,3])/(mA1+data$mit[3]/365)
cost[2,3]<-c34_user<-20*(data$user[3])*data$mit[3]*(1-R_link[21,3])/(mA1+data$mit[3]/365)
cost[3,3]<-c34_dap<-20*(data$dap[3])*data$mit[3]*(1-R_link[21,3])/(mA1+data$mit[3]/365)
cost[4,3]<-c34_iap<-20*(data$iap[3])*data$mit[3]*(1-R_link[21,3])/(mA1+data$mit[3]/365)
c34<-c34_owner+c34_user+c34_dap+c34_iap

cost[1,4]<-c35_owner<-20*(data$owner[4])*data$mit[4]*(1-R_link[21,4])/(mA1+data$mit[4]/365)
cost[2,4]<-c35_user<-20*(data$user[4])*data$mit[4]*(1-R_link[21,4])/(mA1+data$mit[4]/365)
cost[3,4]<-c35_dap<-20*(data$dap[4])*data$mit[4]*(1-R_link[21,4])/(mA1+data$mit[4]/365)
cost[4,4]<-c35_iap<-20*(data$iap[4])*data$mit[4]*(1-R_link[21,4])/(mA1+data$mit[4]/365)
c35<-c35_owner+c35_user+c35_dap+c35_iap

cost[1,5]<-c45_owner<-20*(data$owner[5])*data$mit[5]*(1-R_link[21,5])/(mA1+data$mit[5]/365)
cost[2,5]<-c45_user<-20*(data$user[5])*data$mit[5]*(1-R_link[21,5])/(mA1+data$mit[5]/365)
cost[3,5]<-c45_dap<-20*(data$dap[5])*data$mit[5]*(1-R_link[21,5])/(mA1+data$mit[5]/365)
cost[4,5]<-c45_iap<-20*(data$iap[5])*data$mit[5]*(1-R_link[21,5])/(mA1+data$mit[5]/365)
c45<-c45_owner+c45_user+c45_dap+c45_iap

cost[1,6]<-c12_owner<-20*(data$owner[6])*data$mit[6]*(1-R_link[21,6])/(mA1+data$mit[6]/365)
cost[2,6]<-c12_user<-20*(data$user[6])*data$mit[6]*(1-R_link[21,6])/(mA1+data$mit[6]/365)
cost[3,6]<-c12_dap<-20*(data$dap[6])*data$mit[6]*(1-R_link[21,6])/(mA1+data$mit[6]/365)
cost[4,6]<-c12_iap<-20*(data$iap[6])*data$mit[6]*(1-R_link[21,6])/(mA1+data$mit[6]/365)
c12<-c12_owner+c12_user+c12_dap+c12_iap

cost[1,7]<-c13_owner<-20*(data$owner[7])*data$mit[7]*(1-R_link[21,7])/(mA1+data$mit[7]/365)
cost[2,7]<-c13_user<-20*(data$user[7])*data$mit[7]*(1-R_link[21,7])/(mA1+data$mit[7]/365)
cost[3,7]<-c13_dap<-20*(data$dap[7])*data$mit[7]*(1-R_link[21,7])/(mA1+data$mit[7]/365)
cost[4,7]<-c13_iap<-20*(data$iap[7])*data$mit[7]*(1-R_link[21,7])/(mA1+data$mit[7]/365)
c13<-c13_owner+c13_user+c13_dap+c13_iap

cost[1,8]<-cA4_owner<-20*(data$owner[8])*data$mit[8]*(1-R_link[21,8])/(mA1+data$mit[8]/365)
cost[2,8]<-cA4_user<-20*(data$user[8])*data$mit[8]*(1-R_link[21,8])/(mA1+data$mit[8]/365)
cost[3,8]<-cA4_dap<-20*(data$dap[8])*data$mit[8]*(1-R_link[21,8])/(mA1+data$mit[8]/365)
cost[4,8]<-cA4_iap<-20*(data$iap[8])*data$mit[8]*(1-R_link[21,8])/(mA1+data$mit[8]/365)
cA4<-cA4_owner+cA4_user+cA4_dap+cA4_iap


cost[1,9]<-cB2_owner<-20*(data$owner[9])*data$mit[9]*(1-R_link[21,9])/(mA1+data$mit[9]/365)
cost[2,9]<-cB2_user<-20*(data$user[9])*data$mit[9]*(1-R_link[21,9])/(mA1+data$mit[9]/365)
cost[3,9]<-cB2_dap<-20*(data$dap[9])*data$mit[9]*(1-R_link[21,9])/(mA1+data$mit[9]/365)
cost[4,9]<-cB2_iap<-20*(data$iap[9])*data$mit[9]*(1-R_link[21,9])/(mA1+data$mit[9]/365)
cB2<-cB2_owner+cB2_user+cB2_dap+cB2_iap

cost[1,10]<-cB5_owner<-20*(data$owner[10])*data$mit[10]*(1-R_link[21,10])/(mA1+data$mit[10]/365)
cost[2,10]<-cB5_user<-20*(data$user[10])*data$mit[10]*(1-R_link[21,10])/(mA1+data$mit[10]/365)
cost[3,10]<-cB5_dap<-20*(data$dap[10])*data$mit[10]*(1-R_link[21,10])/(mA1+data$mit[10]/365)
cost[4,10]<-cB5_iap<-20*(data$iap[10])*data$mit[10]*(1-R_link[21,10])/(mA1+data$mit[10]/365)
cB5<-cB5_owner+cB5_user+cB5_dap+cB5_iap

totalcost<-cA1+c23+c34+c35+c45+c12+c13+cA4+cB2+cB5
costinfomration<-data.frame(cA1,c23,c34,c35,c45,c12,c13,cA4,cB2,cB5,totalcost)
cat("cost of intervention on each link and on network for 20 year \n")
print(costinfomration)


file.remove("data.csv")
file.create("data.csv")
write.table(cost, file="data.csv", sep = ",", append = TRUE,col.names = FALSE) 
#cost calculation