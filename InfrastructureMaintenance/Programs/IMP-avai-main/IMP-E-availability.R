#

#This program is coded by Nam Lethanh for use in the class IMP
#Purpose: To calculate reliability
TIME=30
T=seq(0,TIME, by =1) #investigate time
N=6 #Number of item
lambda<-matrix(double(1),nrow=1,ncol=N)
lambda<-1/20 # value of hazard rate

delta<-matrix(double(1),nrow=length(T),ncol=N-1)

delta[1]<-1*7/365
delta[2]<-2*7/365
delta[3]<-5*7/365
delta[4]<-4*7/365
delta[5]<-3*7/365


# define dimension of matrix
fail<-matrix(double(1),nrow=length(T),ncol=N)
survive<-matrix(double(1),nrow=length(T),ncol=N)
availability<-matrix(double(1),nrow=length(T),ncol=N)


#reliability function for exponential distribution
failure<-function(t,lambda){lambda*exp(-lambda*t)}
survival<-function(t,lambda){exp(-lambda*t)}
#

for (t in 1:length(T)){
  for (n in 1:N){
    if (n < N){
      survive[t,n]<-survival(T[t],lambda)
      availability[t,n]<-integrate(survival,0,t,lambda)$value/(integrate(survival,0,t,lambda)$value+1*delta[n]*(1-survive[t,n]))
   #  availability[t,n]<-integrate(survival,0,t,lambda)$value/(integrate(survival,0,t,lambda)$value+delta[n]*integrate(failure,0,t,lambda)$value)
    } else {
      survive[t,n]=
        survive[t,1]*survive[t,3]*survive[t,4]+
        survive[t,1]*survive[t,3]*survive[t,5]+
        survive[t,2]*survive[t,3]*survive[t,4]+
        survive[t,2]*survive[t,3]*survive[t,5]+
        survive[t,1]*survive[t,2]*survive[t,4]*survive[t,5]-
        survive[t,1]*survive[t,2]*survive[t,3]*survive[t,4]-
        survive[t,1]*survive[t,3]*survive[t,4]*survive[t,5]-
        survive[t,1]*survive[t,2]*survive[t,3]*survive[t,5]-
        survive[t,2]*survive[t,3]*survive[t,4]*survive[t,5]
      
      availability[t,n]=
        availability[t,1]*availability[t,3]*availability[t,4]+
        availability[t,1]*availability[t,3]*availability[t,5]+
        availability[t,2]*availability[t,3]*availability[t,4]+
        availability[t,2]*availability[t,3]*availability[t,5]+
        availability[t,1]*availability[t,2]*availability[t,4]*availability[t,5]-
        availability[t,1]*availability[t,2]*availability[t,3]*availability[t,4]-
        availability[t,1]*availability[t,3]*availability[t,4]*availability[t,5]-
        availability[t,1]*availability[t,2]*availability[t,3]*availability[t,5]-
        availability[t,2]*availability[t,3]*availability[t,4]*availability[t,5]
    }
  }
}
cat("Reliability of each item and the network \n")
print(survive)
cat("Availability of each item and the network \n")
print(availability)


#Plotting

plot.new()
par(mar=c(5,4,4,6)+0.3)
limy=c(0,1)
limx=c(0,TIME)
plot(survive[,1],lwd=2,col="red",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=4,cex=0.8)
axis(2,ylim=limy,col="black",las=1)
axis(1,c(seq(0,TIME,by=1)),c(seq(0,TIME,by=1)))
mtext(expression(paste('Probability')),side=2,col="black",line=3)
mtext(expression(paste('Units of time')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
par(new=TRUE)
limy=c(0,1)
limx=c(0,TIME)
plot(survive[,6],lwd=2,col="blue",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=2,cex=0.8)

par(new=TRUE)
limy=c(0,1)
limx=c(0,TIME)
plot(availability[,6],lwd=1,col="darkblue",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=8,cex=0.7)

par(new=TRUE)
limy=c(0,1)
limx=c(0,TIME)
plot(availability[,1],lwd=1,col="coral",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=9,cex=0.2)

par(new=TRUE)
limy=c(0,1)
limx=c(0,TIME)
plot(availability[,2],lwd=1,col="coral4",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=10,cex=0.2)

par(new=TRUE)
limy=c(0,1)
limx=c(0,TIME)
plot(availability[,3],lwd=1,col="brown3",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=11,cex=0.2)

par(new=TRUE)
limy=c(0,1)
limx=c(0,TIME)
plot(availability[,4],lwd=1,col="darkgrey",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=12,cex=0.2)

par(new=TRUE)
limy=c(0,1)
limx=c(0,TIME)
plot(availability[,5],lwd=1,col="darkorchid1",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=22,cex=0.2)

colors=c("red","blue","darkblue","coral","coral4","brown3","darkgrey")
legend("bottomleft", inset=0.009, col=colors,lty=2,lwd=2,legend=c("R-subitem","R-network","A-network","A-1","A-2","A-3","A-4","A-5"),pch=c(4,2,8,9,10,11,12,22),bg="azure2",cex=0.8)

file.remove("data.csv")
file.create("data.csv")
write.table(availability, file="data.csv", sep = ",", append = TRUE,col.names = FALSE) 

