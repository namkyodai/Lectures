library(ggplot2)
N<-10000 # Total number of of students in the class
R <- seq(1:10) # Level of efficiency


BE <-sample(c(1:6),size=N,replace = TRUE)
AF <-sample(c(7:9),size=N,replace = TRUE)

df <- data.frame(BE,AF)

p <- ggplot(df, aes(x=BE)) + 
  geom_bar()
p


