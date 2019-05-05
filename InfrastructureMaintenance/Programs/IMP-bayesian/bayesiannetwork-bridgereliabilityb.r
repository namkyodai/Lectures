#Created by Nam Lethanh for the IMP class
library(bnlearn) #pls install the bnlearn package from CRAN

#------------A simplified BN example-----------

#creating an empty a network in abstract level

dag1 <- empty.graph(nodes = c("Earthquake","Avalanche","Rockfall","Bridge"))
#creating arcs for the network
dag1 <- set.arc(dag1, from = "Earthquake", to = "Avalanche")
dag1 <- set.arc(dag1, from = "Earthquake", to = "Rockfall")
dag1 <- set.arc(dag1, from = "Avalanche", to = "Bridge")
dag1 <- set.arc(dag1, from = "Rockfall", to = "Bridge")
dag1 <- model2network("[Earthquake][Avalanche|Earthquake][Rockfall|Earthquake][Bridge|Avalanche:Rockfall]")
plot(dag1) #plot the structure of the network
graphviz.plot(dag1) ##plot the structure of the network but make it more appealling
library(gRain)
#Define the conditional probability table for the network CPT
yn <- c("yes","no")
c <- cptable(~Earthquake, values=c(0.01,0.99), levels=yn)
s.c <- cptable(~Avalanche | Earthquake, values=c(0.7,0.3,0.1,0.9), levels=yn)
r.c <- cptable(~Rockfall | Earthquake, values=c(0.4,0.6,0.05,0.95), levels=yn)
w.src <- cptable(~Bridge | Avalanche:Rockfall,values=c(0.6,0.4,0.3,0.7,0.2,0.8,0.05,0.95),levels=yn)
cpt.list <- compileCPT(list(c,s.c,r.c,w.src))
print(cpt.list)
#Create a graphical representation
bnet <- grain(cpt.list)
plot(bnet)
# Compile network (details follow)
bnet <- compile(bnet)
# Query network to find marginal probabilities of diseases

p<-querygrain(bnet, nodes=c("Avalanche","Rockfall","Bridge"))
print(p)






#-------------Extended Example for more complex structure

#creating an empty a network in detail
dag <- empty.graph(nodes = c("Aval", "Rockfall", "Flood", "Cracks", "Bearing", "Joint", "Earthquake", "Snow", "Rain", "DTV", "Bridge"))

#creating arcs for the network
dag <- set.arc(dag, from = "Earthquake", to = "Aval")
dag <- set.arc(dag, from = "Earthquake", to = "Rockfall")
dag <- set.arc(dag, from = "Snow", to = "Flood")
dag <- set.arc(dag, from = "Snow", to = "Aval")
dag <- set.arc(dag, from = "Rain", to = "Flood")
dag <- set.arc(dag, from = "DTV", to = "Cracks")
dag <- set.arc(dag, from = "DTV", to = "Bearing")
dag <- set.arc(dag, from = "DTV", to = "Joint")
dag <- set.arc(dag, from = "Aval", to = "Bridge")
dag <- set.arc(dag, from = "Rockfall", to = "Bridge")
dag <- set.arc(dag, from = "Flood", to = "Bridge")
dag <- set.arc(dag, from = "Cracks", to = "Bridge")
dag <- set.arc(dag, from = "Bearing", to = "Bridge")
dag <- set.arc(dag, from = "Joint", to = "Bridge")
plot(dag) #plot the structure of the network
graphviz.plot(dag) ##plot the structure of the network but make it more appealling

#define sets of variables for each nodes
Earthquake.state <- c("yes", "no")
Snow.state <- c("low", "high")
Rain.state <- c("low", "medium","high")
DTV.state <- c("low", "medium","high")
Joint.state <- c("good", "bad")
Bearing.state <- c("good", "bad")
Cracks.state <- c("small", "moderate","high")
Aval.state <- c("yes", "no")
Rockfall.state <- c("yes", "no")
Flood.state <- c("yes", "no")
Bridge.state <- c("low", "medium","high")


#Assign conditional probability table for each node

#read the data of conditional probability
Earthquake.prob <- array(c(0.01, 0.99), dim = 2,dimnames = list(Earthquake = Earthquake.state))

#this part creates a random conditional probability 
nrow=1
ncol=2
N=10
Snow.sam<-matrix(double(1),nrow=nrow,ncol=ncol)
for (t in 1:nrow){
  sample=sample(c(1:N),ncol,replace=TRUE)
  sample=sample/sum(sample)
  Snow.sam[t,]=sample
}
Snow.sam=as.vector(t(Snow.sam))
Snow.prob <- array(Snow.sam, dim = 2,dimnames = list(Snow = Snow.state))

#this part creates a random conditional probability 
nrow=1
ncol=3
N=10
Rain.sam<-matrix(double(1),nrow=nrow,ncol=ncol)
for (t in 1:nrow){
  sample=sample(c(1:N),ncol,replace=TRUE)
  sample=sample/sum(sample)
  Rain.sam[t,]=sample
}
Rain.sam=as.vector(t(Rain.sam))

Rain.prob <- array(Rain.sam, dim = 3,dimnames = list(Rain = Rain.state))

#this part creates a random conditional probability 
nrow=1
ncol=3
N=10
DTV.sam<-matrix(double(1),nrow=nrow,ncol=ncol)
for (t in 1:nrow){
  sample=sample(c(1:N),ncol,replace=TRUE)
  sample=sample/sum(sample)
  DTV.sam[t,]=sample
}
DTV.sam=as.vector(t(DTV.sam))

DTV.prob <- array(DTV.sam, dim = 3,dimnames = list(DTV = DTV.state))

#this part creates a random conditional probability 
nrow=2*2*2
ncol=2
N=10
Aval.sam<-matrix(double(1),nrow=nrow,ncol=ncol)
for (t in 1:nrow){
  sample=sample(c(1:N),ncol,replace=TRUE)
  sample=sample/sum(sample)
  Aval.sam[t,]=sample
}
Aval.sam=as.vector(t(Aval.sam))

Aval.prob <- array(Aval.sam, dim = c(2,2,2),dimnames = list(Aval = Aval.state, Snow = Snow.state,Earthquake=Earthquake.state))

#this part creates a random conditional probability 
nrow=2*2
ncol=2
N=10
Rockfall.sam<-matrix(double(1),nrow=nrow,ncol=ncol)
for (t in 1:nrow){
  sample=sample(c(1:N),ncol,replace=TRUE)
  sample=sample/sum(sample)
  Rockfall.sam[t,]=sample
}
Rockfall.sam=as.vector(t(Rockfall.sam))

Rockfall.prob <- array(Rockfall.sam, dim = c(2,2),dimnames = list(Rockfall = Rockfall.state, Earthquake=Earthquake.state))

#this part creates a random conditional probability 
nrow=2*2*3
ncol=2
N=10
Flood.sam<-matrix(double(1),nrow=nrow,ncol=ncol)
for (t in 1:nrow){
  sample=sample(c(1:N),ncol,replace=TRUE)
  sample=sample/sum(sample)
  Flood.sam[t,]=sample
}
Flood.sam=as.vector(t(Flood.sam))
Flood.prob <- array(Flood.sam, dim = c(2,2,3),dimnames = list(Flood = Flood.state, Snow = Snow.state,Rain=Rain.state))

#this part creates a random conditional probability 
nrow=3*3
ncol=3
N=10
Cracks.sam<-matrix(double(1),nrow=nrow,ncol=ncol)
for (t in 1:nrow){
  sample=sample(c(1:N),ncol,replace=TRUE)
  sample=sample/sum(sample)
  Cracks.sam[t,]=sample
}
Cracks.sam=as.vector(t(Cracks.sam))
Cracks.prob <- array(Cracks.sam, dim = c(3,3),dimnames = list(Cracks = Cracks.state, DTV = DTV.state))

#this part creates a random conditional probability 
nrow=2*3
ncol=2
N=10
Bearing.sam<-matrix(double(1),nrow=nrow,ncol=ncol)
for (t in 1:nrow){
  sample=sample(c(1:N),ncol,replace=TRUE)
  sample=sample/sum(sample)
  Bearing.sam[t,]=sample
}
Bearing.sam=as.vector(t(Bearing.sam))
Bearing.prob <- array(Bearing.sam, dim = c(2,3),dimnames = list(Bearing = Bearing.state, DTV = DTV.state))
#this part creates a random conditional probability 
nrow=2*3
ncol=2
N=10
Joint.sam<-matrix(double(1),nrow=nrow,ncol=ncol)
for (t in 1:nrow){
  sample=sample(c(1:N),ncol,replace=TRUE)
  sample=sample/sum(sample)
  Joint.sam[t,]=sample
}
Joint.sam=as.vector(t(Joint.sam))
Joint.prob <- array(Joint.sam, dim = c(2,3),dimnames = list(Joint = Joint.state, DTV = DTV.state))

#this part creates a random conditional probability 
nrow=2*2*2*2*3*2
ncol=3
N=10
Brige.sam<-matrix(double(1),nrow=nrow,ncol=ncol)
for (t in 1:nrow){
  sample=sample(c(1:N),ncol,replace=TRUE)
  sample=sample/sum(sample)
  Brige.sam[t,]=sample
}
Brige.sam=as.vector(t(Brige.sam))
Bridge.prob <- array(Brige.sam, dim = c(3,2,2,2,2,3,2), dimnames = list(Bridge = Bridge.state, Aval = Aval.state, Rockfall=Rockfall.state,Flood=Flood.state,Joint=Joint.state,Cracks=Cracks.state,Bearing=Bearing.state))

#re-arrange the network so it looks nicer
dag1 <- model2network("[Earthquake][Snow][Rain][DTV][Aval|Earthquake:Snow][Rockfall|Earthquake][Flood|Snow:Rain][Cracks|DTV][Bearing|DTV][Joint|DTV][Bridge|Aval:Rockfall:Flood:Cracks:Bearing:Joint]")
plot(dag1) #plot the structure of the network
graphviz.plot(dag1) #plot the structure of the network but make it more appealling
#compiling all conditional probability table into one network table
cpt <- list(Earthquake=Earthquake.prob,Snow=Snow.prob,Rain=Rain.prob,DTV=DTV.prob,Aval=Aval.prob,Rockfall=Rockfall.prob,Flood=Flood.prob,Cracks=Cracks.prob,Bearing=Bearing.prob,Joint=Joint.prob,Bridge=Bridge.prob) #list all individual condition table into a network conditional table
bn1 <- custom.fit(dag1, cpt) #calculate the marginal probability distribution based on the defined network and conditional probability table
#-------------Model from data file -------------------
survey=read.csv("bayesiannetwork-bridgereliabilityb.csv", header=TRUE) #Data
attach(survey) 
#using the Maximum likelihood method
bn.mle <- bn.fit(dag1, data = survey, method = "mle")
print(bn.mle)
#prop.table(table(survey[, c("O", "E")]), margin = 2)
#using Bayesian method
bn.bayes <- bn.fit(dag1, data = survey, method = "bayes",iss = 10)            
print(bn.bayes)
