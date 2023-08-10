# Load ggplot2
library(ggplot2)

# The mtcars dataset is natively available
# head(mtcars)

# A really basic boxplot.

# Biểu đồ hộp cho 1 biến

dulieu<-mtcars

ggplot(dulieu, aes(x="", y=mpg)) +
  geom_boxplot()


ggplot(dulieu, aes(x="", y=mpg)) +
  geom_boxplot() +
  xlab("")


ggplot(dulieu, aes(x="", y=mpg)) +
  geom_boxplot(alpha=0.6, colour = "lightgreen", fill = "hotpink2") +
  xlab("")


ggplot(dulieu, aes(x="", y=mpg)) +
  geom_boxplot(fill="midnightblue", alpha=0.3, colour = "mediumseagreen") +
  xlab("")+
  labs(title="Biểu đồ hộp cho biến mpg",
       subtitle="ví dụ về thêm màu",
       y = "Quãng đường trên 1 lít xăng")


#thêm một lớp cho biểu đồ
ggplot(dulieu, aes(x="", y=mpg)) +
  geom_boxplot(fill="slateblue", alpha=0.2) +
  xlab("")+
  ylab("Quãng đường trên 1 lít xăng")

# Cùng một biểu đồ nhưng thêm biến

#http://www.sthda.com/english/wiki/ggplot2-point-shapes
ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) +
  geom_boxplot(fill="red", alpha=0.2)+
  xlab("Dung tích xi lanh")+
  ylab("Quãng đường trên 1 lít xăng")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")



ggplot(mtcars, aes(x=as.factor(cyl), y=mpg, fill = as.factor(cyl))) +
  geom_boxplot(alpha=0.7) +
  xlab("Dung tích xi lanh")+
  ylab("Quãng đường trên 1 lít xăng")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")




ggplot(mtcars, aes(x=as.factor(cyl), y=mpg, fill = as.factor(cyl))) +
  geom_boxplot(alpha=0.7) +
  xlab("Dung tích xi lanh")+
  ylab("Quãng đường trên 1 lít xăng")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")



ggplot(mtcars, aes(x=as.factor(cyl), y=mpg, fill = as.factor(cyl))) +
  geom_boxplot(alpha=0.7) +
  xlab("Dung tích xi lanh")+
  ylab("Quãng đường trên 1 lít xăng")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")


#about palette http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html

# tự định nghĩa palette
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
scale_colour_manual(values=cbPalette)



ggplot(mtcars, aes(x=as.factor(cyl), y=mpg, fill = as.factor(cyl))) +
  geom_boxplot(alpha=0.7) +
  xlab("Dung tích xi lanh")+
  ylab("Quãng đường trên 1 lít xăng")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  theme(legend.position="none") +
  scale_fill_brewer(palette="cbPalette")


# thêm trang trí viền
p<-ggplot(mtcars, aes(x=as.factor(cyl), y=mpg, fill = as.factor(cyl))) +
  geom_boxplot(alpha=0.7, color ="red") +
  xlab("Dung tích xi lanh")+
  ylab("Quãng đường trên 1 lít xăng")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")




# thay đổi background

p+theme_gray(base_size = 14)

p+theme_bw()

p + theme_minimal()

p + theme_classic()

p + theme_void()

p + theme_dark()+ labs(title="Biểu đồ hộp cho biến mpg", subtitle="ví dụ về thêm màu", fill="Kí hiệu")

# vị trí của chú giải


q<-ggplot(mtcars, aes(x=as.factor(cyl), y=mpg, fill = as.factor(cyl))) +
  geom_boxplot(alpha=0.7, color ="red") +
    stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")

q+
  labs(title="Biểu đồ Boxplot",
       subtitle = "Ví dụ xử dụng labs thay vì xlab và ylab",
       x ="Dung tích xi lanh",
       y ="Quãng đường trên lượng xăng tiêu thụ",
       caption = "(Nguồn dữ liệu từ ...)",fill="Kí hiệu")+
      theme(legend.position="bottom")+
      scale_fill_manual(values = c("#d8b365", "#f5f5f5", "#5ab4ac"))

# Thảy đổi Legends (Chú giải)
#http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/


#Hiển thị số trên đồ thị???

means <- aggregate(mpg ~  cyl, mtcars, mean)



q+
  geom_text(data = means, aes(label = mpg, y = mpg + 0.08))

#### trang trí thêm một chút

means <-round(means,2)

means



q+ geom_text(data = means, aes(label = mpg, y = mpg + 0.08))

q+ geom_text(data = means, aes(label = mpg, y = mpg + 1))

# thêm kí tự

#http://www.sthda.com/english/wiki/ggplot2-texts-add-text-annotations-to-a-graph-in-r-software

#https://ggplot2.tidyverse.org/reference/annotate.html
q+ geom_text(data = means, aes(label = mpg, y = mpg + 1))+
  annotate(geom="text", x=3, y=30, label="Đây là biểu đồ hộp",
           color="red")
















# chú ý phải cài các thư viện phụ thuộc vào

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(dplyr)

# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)


# biểu đồ đàn vĩ cầm, dùng biểu đồ này khi dữ liệu của chúng ta khá nhiều và phân tán

data %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

# quay trở lại dữ liệu về oto

#cách 1
mtcars %>%
  ggplot( aes(x=as.factor(cyl), y=mpg, fill=as.factor(cyl))) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

#cách 2

  ggplot(mtcars, aes(x=as.factor(cyl), y=mpg, fill=as.factor(cyl))) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")



# biểu đồ thể hiện tất cả các dữ liệu

# Plot
data %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")

# dữ liệu về mtcars

z<-mtcars %>%
  ggplot(aes(x=as.factor(cyl), y=mpg, fill=as.factor(cyl))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Đây là biểu đồ hợp với jitter") +
  xlab("")


z
