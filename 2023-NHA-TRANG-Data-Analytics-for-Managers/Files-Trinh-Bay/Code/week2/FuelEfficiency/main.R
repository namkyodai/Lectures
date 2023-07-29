## first we read in the data
#FuelEff <- read.csv("https://www.biz.uiowa.edu/faculty/jledolter/DataMining/FuelEfficiency.csv")
df <- read.csv("FuelEfficiency.csv")

df <-data.frame(df)

library(dplyr)

glimpse(df)

#GPM--> Gallon per 100 miles
#MPG--> miles per Gallons
#WT --> weiggt of the car
#DIS --> Cubic displacement
#NC --> No. of cylinders
#HP --> Hourse power
#ACC --> Acceleration
# ET --> engine type


#----------------------------
plot.new()
#par(mfrow=c(1,1))
par(mar=c(4,4,1,1)+0.1,mfrow=c(3,3),bg="white",cex = 1, cex.main = 1)

plot(GPM~MPG,data=df)

plot(GPM~WT,data=df)

plot(GPM~DIS,data=df)

plot(GPM~NC,data=df)

plot(GPM~HP,data=df)


plot(GPM~ACC,data=df)
plot(GPM~ET,data=df)

dev.copy(png,'fueleff_xyplot.png',width = 800, height = 800)
dev.off()

df=df[-1] #ignore the MPG column
df

## regression on all data
m1=lm(GPM~.,data=df)
summary(m1)

cor(df)

## best subset regres0sion in R
library(leaps)
X=df[,2:7]
y=df[,1]

#use the regsubsets function from package leaps to compute regression of the subsets
#https://www.rdocumentation.org/packages/leaps/versions/2.1-1/topics/regsubsets


out=summary(regsubsets(X,y,nbest=2,nvmax=ncol(X)))
tab=cbind(out$which,out$rsq,out$adjr2,out$cp)
tab



m2=lm(GPM~WT,data=df)
summary(m2)

m3=lm(GPM~WT+HP,data=df)
m3

summary(m3)


## cross-validation (leave one out) for the model on all six regressors
n=length(df$GPM)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  ## the R expression "train1[train1!=k]" picks from train1 those
  ## elements that are different from k and stores those elements in the
  ## object train.
  ## For k=1, train consists of elements that are different from 1; that
  ## is 2, 3, …, n.
  m1=lm(GPM~.,data=df[train,])
  pred=predict(m1,newdat=df[-train,]) #adding the new data, which is ignored earlier
  obs=df$GPM[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error

## cross-validation (leave one out) for the model on weight only
n=length(df$GPM)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m2=lm(GPM~WT,data=df[train,])
  pred=predict(m2,newdat=df[-train,])
  obs=FuelEff$GPM[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error
