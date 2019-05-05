#This program is coded by Nam Lethanh for use in the class IMP
#Purpose: To calculate reliability
TIME=4
T=seq(0,TIME, by =0.5) #investigate time
N=3 #Number of item
lambda<-matrix(double(1),nrow=1,ncol=N)
lambda<-c(0.5,1,1.5) # value of hazard rate
# define dimension of matrix
fail<-matrix(double(1),nrow=length(T),ncol=N)
survive<-matrix(double(1),nrow=length(T),ncol=N)
#reliability function for exponential distribution
failure<-function(t,lambda){lambda*exp(-lambda*t)}
survival<-function(t,lambda){exp(-lambda*t)}
for (i in 1:N){
for (t in (1:length(T))){
  fail[t,i]<-failure(T[t],lambda[i])
  survive[t,i]<-survival(T[t],lambda[i])
}
}
cat('PROBABILITY DENSITY \n')
print(cbind(T,fail))
cat('RELIABILITY \n')
print(cbind(T,survive))
cat('EXPECTED LIFE TIME \n')
theta=lambda
print(1/theta)
#Plotting for reliability
plot.new()
par(mar=c(5,4,4,6)+0.3)
limy=c(0,1)
limx=c(0,4)
plot(T,survive[,1],lwd=2,col="red",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=4)
axis(2,ylim=limy,col="black",las=1)
axis(1,c(seq(0,max(limx),by=0.5)),c(seq(0,max(limx),by=0.5)))
mtext(expression(paste('Reliability')),side=2,col="black",line=3)
mtext(expression(paste('Units of time')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
par(new=TRUE)
plot(T,survive[,2],lwd=2,col="blue",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=2)
par(new=TRUE)
plot(T,survive[,3],lwd=2,col="cyan4",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=1)
colors=c("red","blue","cyan4")
legend("topright", inset=0.09, title="Items",col=colors,lty=2,lwd=2,legend=c(1:N),pch=c(4,2,1),bg="azure2",cex=0.8)
#Plotting for probability density function
plot.new()
par(mar=c(5,4,4,6)+0.3)
limy=c(0,1.5)
limx=c(0,4)
plot(T,fail[,1],lwd=2,col="red",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=4)
axis(2,ylim=limy,col="black",las=1)
axis(1,c(seq(0,max(limx),by=0.5)),c(seq(0,max(limx),by=0.5)))
mtext(expression(paste('Probability density')),side=2,col="black",line=3)
mtext(expression(paste('Units of time')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
par(new=TRUE)
plot(T,fail[,2],lwd=2,col="blue",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=2)
par(new=TRUE)
plot(T,fail[,3],lwd=2,col="cyan4",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=1)
colors=c("red","blue","cyan4")
legend("topright", inset=0.09, title="Items",col=colors,lty=2,lwd=2,legend=c(1:N),pch=c(4,2,1),bg="azure2",cex=0.8)
#Plotting for reliability
plot.new()
par(mar=c(5,4,4,6)+0.3)
limy=c(0,1)
limx=c(0,4)
plot(T,survive[,1],lwd=2,col="red",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=4)
axis(2,ylim=limy,col="black",las=1)
axis(1,c(seq(0,max(limx),by=0.5)),c(seq(0,max(limx),by=0.5)))
mtext(expression(paste('Reliability')),side=2,col="black",line=3)
mtext(expression(paste('Units of time')),side=1,col="black",line=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
hazardrate=theta[1]
points(1/hazardrate,survival(1/hazardrate,hazardrate),pch=23,bg="black",lwd=5)
segments(0,survival(1/hazardrate,hazardrate),1/hazardrate,survival(1/hazardrate,hazardrate), col= 'red',lty=1,lwd=1)
segments(1/hazardrate,survival(1/hazardrate,hazardrate),1/hazardrate,0, col= 'red',lty=1,lwd=1)
par(new=TRUE)
plot(T,survive[,2],lwd=2,col="blue",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=2)
hazardrate=theta[2]
points(1/hazardrate,survival(1/hazardrate,hazardrate),pch=23,bg="black",lwd=5)
segments(0,survival(1/hazardrate,hazardrate),1/hazardrate,survival(1/hazardrate,hazardrate), col= 'red',lty=1,lwd=1)
segments(1/hazardrate,survival(1/hazardrate,hazardrate),1/hazardrate,0, col= 'blue',lty=1,lwd=1)
par(new=TRUE)
plot(T,survive[,3],lwd=2,col="cyan4",ylab="",xlab="",xlim=limx,ylim=limy,axes=FALSE,lty=1,type="b",pch=1)
hazardrate=theta[3]
points(1/hazardrate,survival(1/hazardrate,hazardrate),pch=23,bg="black",lwd=5)
segments(0,survival(1/hazardrate,hazardrate),1/hazardrate,survival(1/hazardrate,hazardrate), col= 'red',lty=1,lwd=1)
segments(1/hazardrate,survival(1/hazardrate,hazardrate),1/hazardrate,0, col= 'cyan4',lty=1,lwd=1)
colors=c("red","blue","cyan4")
legend("topright", inset=0.09, title="Items",col=colors,lty=2,lwd=2,legend=c(1:N),pch=c(4,2,1),bg="azure2",cex=0.8)
cat("THE END")