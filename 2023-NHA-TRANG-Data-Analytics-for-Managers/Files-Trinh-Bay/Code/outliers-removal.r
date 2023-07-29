# Removing outliers method 1

data("warpbreaks")
boxplot(warpbreaks)$out

library(ggstatsplot)
ggbetweenstats(warpbreaks,
               wool, breaks, outlier.tagging = TRUE)

Q <- quantile(warpbreaks$breaks, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(warpbreaks$breaks)

up <-  Q[2]+1.5*iqr # Upper Range
low<- Q[1]-1.5*iqr # Lower Range
eliminated<- subset(warpbreaks, warpbreaks$breaks > (Q[1] - 1.5*iqr) & warpbreaks$breaks < (Q[2]+1.5*iqr))

ggbetweenstats(eliminated, wool, breaks, outlier.tagging = TRUE)


# removing outliner method 2

data("warpbreaks")
boxplot(warpbreaks$breaks, plot=FALSE)$out
outliers <- boxplot(warpbreaks$breaks, plot=FALSE)$out
x<-warpbreaks
x<- x[-which(x$breaks %in% outliers),]
