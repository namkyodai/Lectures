library(ggplot2)

# The iris dataset is provided natively by R
#head(iris)

# basic scatterplot

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
   geom_point()


p<-ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) 

p + geom_point()

p + geom_point()+
  annotate("rect", xmin = 5.5, xmax = 7, ymin = 2, ymax = 3.5, alpha = .2) + # tô thêm màu thế nào cho đẹp 
  annotate("segment", x = 5.5, xend = 7, y = 2.5, yend = 3.3)+
  annotate("pointrange", x = 6, y = 3, ymin = 2, ymax = 4, colour = "red", size = 1.5)

# let play with some emoji

#https://cran.r-project.org/web/packages/emojifont/vignettes/emojifont.html
library(emojifont)
p + geom_point()+
  annotate("rect", xmin = 5.5, xmax = 7, ymin = 2, ymax = 3.5, alpha = .2) + # tô thêm màu thế nào cho đẹp 
  annotate("segment", x = 5.5, xend = 7, y = 2.5, yend = 3.3)+
  annotate("pointrange", x = 6, y = 3, ymin = 2, ymax = 4, colour = "red", size = 1.5)+
  geom_emoji('smiley', x=7, y=4, size=20)


p + geom_point()+
   annotate("segment", x = 5.5, xend = 7, y = 2.5, yend = 3.3)+
  annotate("rect", xmin = 5.5, xmax = 7, ymin = 2, ymax = 3.5, alpha = .2) +
  labs(title= "Biểu đồ scatterplot",
       x ="Chiều rộng loài Sepal",
       y = "chiều dài loài Sepal")
  



