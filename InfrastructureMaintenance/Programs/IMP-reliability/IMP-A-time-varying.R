#This program is coded by Nam Lethanh for use in the class IMP-HS2014
#Purpose: To calculate reliability from Cox Model

density<-function(t,theta,m){theta*m*(theta*t)^(m-1)*exp(-(theta*t)^m)}
rate<-function(t,theta,m){theta*m*(theta*t)^(m-1)}
reliability<-function(t,theta,m){exp(-(theta*t)^m)}

theta<-c(0.005,0.004,0.003,0.004,0.004)
m<-c(1.3,1.4,1.5,1.5,1.5)
elapsedtime20<-c(22,17,17,17,17)
elapsedtime14<-c(16,11,11,11,11)

No =5 
R2020<-matrix(double(1),nrow=No,ncol=1)
for (i in 1:No){
  R2020[i]<-reliability(elapsedtime20[i],theta[i],m[i])
}
cat("Reliability of each pipe in Zone A in year 2020")
print(R2020)

mean(1-R2020)


R2014<-matrix(double(1),nrow=No,ncol=1)
for (i in 1:No){
  R2014[i]<-reliability(elapsedtime14[i],theta[i],m[i])
}
cat("Reliability of each pipe in Zone A in year 2014")
print(R2014)




cat("THE END")


