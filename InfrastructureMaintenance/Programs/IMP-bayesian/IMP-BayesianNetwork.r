# This program was coded by  Nam Lethanh (lethanh@ibi.baug.ethz.ch)
#Bayesian network example
library(deal) #this is dependency package
data <- read.csv("BayesianNetwork-dataaccident.csv",header=TRUE)
accident<-network(data)
plot(accident)
accident.nd <- nodes(accident)# the list of nodes
accident.j <- jointprior(accident)
accident.prior <- jointprior(accident)
#accident.prior <- jointprior(accident,12)
accident <- learn(accident,data,accident.prior)$nw
nodes(accident)$Time$condprior
nodes(accident)$Time$condposterior
nodes(accident)$Time$loglik
accident$score
allaccident <- networkfamily(data,accident,accident.prior)
allaccident <- nwfsort(allaccident$nw)
print(allaccident)
plot(allaccident)
accident.s <- autosearch(accident,data,accident.prior)$nw
plot(accident.s)
#THE END
cat("THE END")