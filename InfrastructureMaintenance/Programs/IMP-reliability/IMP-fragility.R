# Evolution
data=read.csv("fragility.csv", header=TRUE)
attach(data)
#object 1
scalefactor=1
trucy=1
colors=c("red", "darkorchid","green", "violet", "darkorange3","brown3")

plot(data$CS1/scalefactor,pch=16,axes=FALSE,ylim=c(0,trucy),ylab="", xlab="",col="red",type="o",lwd=4)
axis(2, ylim=c(0,trucy),col="darkblue",las=1)
mtext(expression(paste("Probability of failure")),side=2,line=2.2, adj = 0.5 )
axis(1,pretty(range(data$rockfall),10))
mtext(expression(paste("Rock fall volume" (m^3))),side=1,col="black",line=2.2)  
box()

par(new=TRUE)
plot(data$CS2/scalefactor,pch=15,axes=FALSE,ylim=c(0,trucy),ylab="", xlab="",col="darkorchid",type="o",lwd=4)
par(new=TRUE)
plot(data$CS3/scalefactor,pch=14,axes=FALSE,ylim=c(0,trucy),ylab="", xlab="",col="green",type="o",lwd=4)
grid(10, 10, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

legend("topleft",inset=.02,legend=c("i=1","i=2","i=3"),text.col=colors,pch=c(16,15,14),col=colors,horiz=F,bty != "n",bg="white")


text(15, 18.5,expression(paste("I3-GCS1 (14.73x",10^6, "mu-year 15)")),pos=1,cex = 0.8, srt = 0)
text(15, data$CS1[15],"A (0.089)",pos=4,cex = 0.8, srt = 0)
text(15, data$CS2[15],"B (0.372)",pos=4,cex = 0.8, srt = 0)
text(15, data$CS3[15],"C (0.716)",pos=4,cex = 0.8, srt = 0)
points(15,data$CS1[15],pch=23,bg="black",lwd=3)
points(15,data$CS2[15],pch=23,bg="black",lwd=3)
points(15,data$CS3[15],pch=23,bg="black",lwd=3)
segments(15, 0, 15, data$CS3[15], col= 'cadetblue',lty=2,lwd=0.5)
segments(0, data$CS1[15], 15,data$CS1[15], col= 'cadetblue',lty=2,lwd=0.5)
segments(0, data$CS2[15], 15, data$CS2[15], col= 'cadetblue',lty=2,lwd=0.5)
segments(0, data$CS3[15], 15, data$CS3[15], col= 'cadetblue',lty=2,lwd=0.5)
