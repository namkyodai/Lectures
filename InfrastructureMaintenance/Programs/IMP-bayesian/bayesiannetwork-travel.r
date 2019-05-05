library(bnlearn)
#creating an empty a network
dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
#creating arcs for the network
dag <- set.arc(dag, from = "A", to = "E")
dag <- set.arc(dag, from = "S", to = "E")
dag <- set.arc(dag, from = "E", to = "O")
dag <- set.arc(dag, from = "E", to = "R")
dag <- set.arc(dag, from = "O", to = "T")
dag <- set.arc(dag, from = "R", to = "T")
plot(dag)
#define sets of variables for each nodes
A.lv <- c("young", "adult", "old")
S.lv <- c("M", "F")
E.lv <- c("high", "uni")
O.lv <- c("emp", "self")
R.lv <- c("small", "big")
T.lv <- c("car", "train", "other")
#Assign conditional probability table for each node
A.prob <- array(c(0.30, 0.50, 0.20), dim = 3,dimnames = list(A = A.lv))
S.prob <- array(c(0.60, 0.40), dim = 2,dimnames = list(S = S.lv))
O.prob <- array(c(0.96, 0.04, 0.92, 0.08), dim = c(2, 2),dimnames = list(O = O.lv, E = E.lv))
R.prob <- array(c(0.25, 0.75, 0.20, 0.80), dim = c(2, 2), dimnames = list(R = R.lv, E = E.lv))
E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64,
                  0.36, 0.70, 0.30, 0.90, 0.10), dim = c(2, 3, 2),
                dimnames = list(E = E.lv, A = A.lv, S = S.lv))
T.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58,
                  0.24, 0.18, 0.70, 0.21, 0.09), dim = c(3, 2, 2),
                dimnames = list(T = T.lv, O = O.lv, R = R.lv))
#re-arrange the network so it looks nicer
dag1 <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")
plot(dag3)
cpt <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob,
             R = R.prob, T = T.prob) #list all individual condition table into a network conditional table
bn1 <- custom.fit(dag, cpt) #calculate the marginal probability distribution based on the defined network and conditional probability table
#-------------Model from data file -------------------
survey=read.csv("bayesiannetwork-travel.csv", header=TRUE) #Data
attach(survey) 
#using the Maximum likelihood method
bn.mle <- bn.fit(dag, data = survey, method = "mle")
print(bn.mle)
prop.table(table(survey[, c("O", "E")]), margin = 2)
#using Bayesian method
bn.bayes <- bn.fit(dag, data = survey, method = "bayes",iss = 10)            
print(bn.bayes)