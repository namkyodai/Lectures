library(gRain)
#Define the conditional probability table for the network CPT
trueorfault <- c("fault","true")
c <- cptable(~cloudy, values=c(50,50), levels=trueorfault)
s.c <- cptable(~sprinker | cloudy, values=c(50,50,90,10), levels=trueorfault)
r.c <- cptable(~rain | cloudy, values=c(80,20,20,80), levels=trueorfault)
w.src <- cptable(~wet | sprinker:rain,values=c(100,0,10,90,10,90,1,99),levels=trueorfault)
cpt.list <- compileCPT(list(c,s.c,r.c,w.src))
print(cpt.list)
#Create a graphical representation
bnet <- grain(cpt.list)
plot(bnet)
# Compile network (details follow)
bnet <- compile(bnet)
# Query network to find marginal probabilities of diseases

p<-querygrain(bnet, nodes=c("sprinker","rain","wet"))
print(p)











