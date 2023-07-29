# Values of height
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)

# Values of weight.
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

df <- data.frame(x,y)

relation <- lm(y~x)

summary(relation)

# Tiên đón cân năng của 1 người có chiều cao là a

a <- data.frame(x = 170)

result <-  predict(relation,a)

result

# Tiên đón cân nặng của một nhóm người

b <- data.frame(x= c(144,165,180))

result <-  predict(relation,b)

result


# Vẽ biểu đồ

library(ggplot2)

p <-df %>% 
  ggplot(aes(x,y))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x, se = FALSE)
p




