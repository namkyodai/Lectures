library(ggVennDiagram)
library(ggplot2)
x <- list(A = 1:5, B = 2:7, C = 5:10)


v<-ggVennDiagram(x,category.names = c("Database Management","Statistics","Modelling"), label = "none")+ 
  theme(legend.position = "none")+
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")

v
