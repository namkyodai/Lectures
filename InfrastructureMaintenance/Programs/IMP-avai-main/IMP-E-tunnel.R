#This program is coded by Nam Lethanh for use in the class IMP
#Purpose: To calculate reliability, availability, and maintainability of tunnel

data <- read.csv("IMP-E-tunnel.csv",header=TRUE,sep=",") #
attach(data)
TIME=20
T=seq(0,TIME, by =1) #investigate time
N=5 #Number of item
M<-5 #number of configuration

cat("how data look like? \n")
print(data)
theta<-matrix(double(1),nrow=M,ncol=N)

MTBF<-matrix(double(1),nrow=M,ncol=1) #mean time between failure for each configuration
Reli<-matrix(double(1),nrow=M,ncol=1) #Reliability for each configuration
Avai<-matrix(double(1),nrow=M,ncol=1) #Availability for each configuration
Cost<-matrix(double(1),nrow=M,ncol=1) #Cost for each configuration in 20 years
Benefit_no_upfront<-matrix(double(1),nrow=M,ncol=1) #
Benefit_with_upfront<-matrix(double(1),nrow=M,ncol=1) #

for (i in 1:N){
  for (j in 1:M){
    theta[i,j]<-1/data[i,j+1]  
  }
}
cat("Value of failure rate for each item i (column) in each configuration j (row) \n")
print(theta)

for (i in 1:N){
  MTBF[i]<-1/sum(theta[i,])
  Reli[i]<-exp(-1/MTBF[i]*1)
  Avai[i]<-MTBF[i]/(MTBF[i]+data$MID[i]/12)
  Cost[i]<-20*data$costcr[i]*data$MID[i]*(1-Reli[i])/(MTBF[i]+data$MID[i]/12)
  
}
cat("Value of mean time between failure (MTBF) of each configuration \n")
print(MTBF)

cat("Reliability of each configuration (Reli) \n")
print(Reli)

cat("Availability of each configuration \n")
print(Avai)
cat("Cost of each configuration in 20 years \n")
print(Cost)

for (i in 1:M){
  if (i==1){
    Benefit_no_upfront[i]<-Cost[i]-Cost[i]
    Benefit_with_upfront[i]<-Benefit_no_upfront[i]-data$costint[i]
  } else {
    Benefit_no_upfront[i]<-Cost[1]-Cost[i]
    Benefit_with_upfront[i]<-Benefit_no_upfront[i]-data$costint[i]
  }
}
cat("Net benefit without upfront cost \n")
print(Benefit_no_upfront)

cat("Net benefit with upfront cost \n")
print(Benefit_with_upfront)
