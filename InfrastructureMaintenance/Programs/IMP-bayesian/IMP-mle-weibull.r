#This program is coded by Nam Lethanh @IBI, ETH Z?rich.
# ESTIMATE THE MODEL'S PARAMETER and Draw the graph
library(MASS)
library(survival)
data <- read.csv("pipelinedata.csv",header=TRUE,sep=",") 
attach(data)
sr.fit = survreg(Surv(time,status)~1+waterlost+depth,dist='weibull')
summary(sr.fit)
cat('model parameters \n')
print(summary(sr.fit))
scale=exp(sr.fit$icoef[1])
shape=1/sr.fit$scale[1]
YearMax=2000 # this is the time frame of investigation
plot.new()
#par(mar=c(5, 4, 4, 1) + 0.1)
x=seq(0,YearMax,length=200)
curve(pweibull(x, shape=shape,scale=scale,  lower.tail=FALSE), from=0, to=YearMax, col='red', lwd=2, ylab='', xlab='',bty='n',ylim=c(0,1),axes=FALSE)
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext(expression(paste("Reliability")),side=2,col="black",line=2.2) 
axis(1,c(seq(0,YearMax,200)))
mtext(expression(paste("Time (tus)")),side=1,col="black",line=2.2) 
#draw the grid on the chart
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()