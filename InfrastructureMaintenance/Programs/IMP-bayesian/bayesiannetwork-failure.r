library(gRain)
#Define the conditional probability tables for the network CPT
yn <- c("yes","no")
a <- cptable(~asia, values=c(1,99), levels=yn)
t.a <- cptable(~tub | asia, values=c(5,95,1,99), levels=yn)
s <- cptable(~smoke, values=c(5,5), levels=yn)
l.s <- cptable(~lung | smoke, values=c(1,9,1,99), levels=yn)
b.s <- cptable(~bronc | smoke, values=c(6,4,3,7), levels=yn)
e.lt <- cptable(~either | lung:tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e <- cptable(~xray | either, values=c(98,2,5,95), levels=yn)
d.be <- cptable(~dysp | bronc:either, values=c(9,1,7,3,8,2,1,9),levels=yn)
cpt.list <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
print(cpt.list)
#Create a graphical representation
bnet <- grain(cpt.list)
# Compile network (details follow)
bnet <- compile(bnet)
# Query network to find marginal probabilities of diseases
querygrain(bnet, nodes=c("tub","lung","bronc"))










