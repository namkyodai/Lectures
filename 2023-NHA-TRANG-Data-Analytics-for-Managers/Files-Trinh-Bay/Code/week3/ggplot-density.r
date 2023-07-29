library(ggplot2)
library(dplyr)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)
data <- data.frame(data)
#view(data)
# Make the histogram




data %>%
  # filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)


data %>%
  # filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  geom_boxplot()

quantile(data$price,probs=seq(0,1,0.05)) #quantile from 0 to 1 with a step of 5%
quantile(data$price,probs=seq(0.95,1,0.01)) #quantilte from 0.95 to 1 with a step of 1%


data %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)


# thay đổi một chút cho đẹp hơn :)

p<-data %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

p+labs(title = "Biểu đồ mật độ",
       y = "Mật độ",
       x = "Giá (đô la mỹ)"

)
# biểu đồ tần số

q<-data %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price))+
  geom_histogram(fill="red",alpha=0.4) #fill="#69b3a2", color="#e9ecef", alpha=0.8


q
# Giờ ta có thể vẻ thêm một chút cho đẹp

# vẽ density plot vào

#https://www.datacamp.com/community/tutorials/make-histogram-ggplot2?utm_source=adwords_ppc&utm_medium=cpc&utm_campaignid=1655852085&utm_adgroupid=61045433982&utm_device=c&utm_keyword=ggplot%20histogram&utm_matchtype=p&utm_network=g&utm_adpostion=&utm_creative=469789579335&utm_targetid=aud-299261629654:kwd-346130199260&utm_loc_interest_ms=1003265&utm_loc_physical_ms=9061646&gclid=CjwKCAiA0KmPBhBqEiwAJqKK4744PmpKCH45EGbYS96M0DYY0LmiFIjccdmYSTeLttGUy3jr0lod6hoCjZ8QAvD_BwE

# https://r-charts.com/distribution/histogram-density-ggplot2/

p  +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white")


p  +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", alpha = 0.2)

# vẽ theo histogram tần số

q +
  annotate("text", x=250, y=500, label= "Ví dụ 1", cex=5, colour = "blue", angle = 0)
  # vẽ đường thẳng

q +
  annotate("text", x=250, y=750, label= "Ví dụ 1", cex=5, colour = "red", angle = 90)+
  geom_vline(xintercept = 200, colour="blue", lwd = 1)

#https://ggplot2.tidyverse.org/reference/geom_abline.html

library(plotly)

a1<-ggplotly(p)

a2<-ggplotly(q)
