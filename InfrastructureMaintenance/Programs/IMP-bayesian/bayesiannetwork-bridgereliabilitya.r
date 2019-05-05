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

# Estimating CPT from data
survey=read.csv("bayesiannetwork-bridgereliabilitya.csv", header=TRUE) #Data
attach(survey) 
#-------------Model from data file -------------------
#define sets of variables for each nodes
Earthquake.state <- c("yes", "no")
Aval.state <- c("yes", "no")
Rockfall.state <- c("yes", "no")
Bridge.state <- c("yes", "no")

#using the Maximum likelihood method
bn.mle <- bn.fit(dag1, data = survey, method = "mle")
print(bn.mle)
#prop.table(table(survey[, c("O", "E")]), margin = 2)
#using Bayesian method
bn.bayes <- bn.fit(dag1, data = survey, method = "bayes",iss = 10)            
print(bn.bayes)
