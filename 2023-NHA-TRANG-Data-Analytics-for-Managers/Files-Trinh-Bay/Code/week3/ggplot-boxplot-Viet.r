# Load ggplot2
library(ggplot2)

# The mtcars dataset is natively available
head(mtcars)
glimpse(mtcars)
# A really basic boxplot.

# Biểu đồ hộp cho 1 biến

ggplot(mtcars, aes(x="", y=mpg)) + #ggplot(cơ sở dữ liệu, aes: thiết lập biến cho trục tung và trục hoành: trục x = rỗng, trục y = cột mpg) +
  geom_boxplot() # goem_boxplot() hàm mặc định chạy hiển thị biểu đồ boxplot, tuy nhiên trục x do không gán giá trị nào nên sẽ hiển thị dấu X




ggplot(mtcars, aes(x="", y=mpg)) + #ggplot(cơ sở dữ liệu, aes: thiết lập biến cho trục tung và trục hoành: trục x = rỗng, trục y = cột mpg) +
  geom_boxplot() + # goem_boxplot() hàm mặc định chạy hiển thị biểu đồ boxplot
  xlab("") #đặt tên trục x là rỗng


ggplot(mtcars, aes(x="", y=mpg)) + #ggplot(cơ sở dữ liệu, aes: thiết lập biến cho trục tung và trục hoành: trục x = rỗng, trục y = cột mpg) +
  geom_boxplot(alpha=0.1) + # goem_boxplot() chạy hiển thị biểu đồ boxplot, tuy nhiên làm mờ toàn bộ biểu đồ với giá trị 0.1
  xlab("") #đặt tên trục x là rỗng


ggplot(mtcars, aes(x="", y=mpg)) + #ggplot(cơ sở dữ liệu, aes: thiết lập biến cho trục tung và trục hoành: trục x = rỗng, trục y = cột mpg)
  geom_boxplot(fill="slateblue", alpha=0.2) + # goem_boxplot() chạy hiển thị biểu đồ boxplot, đổ màu xanh và làm mờ toàn bộ biểu đồ với giá trị 0.2
  xlab("")+ #đặt tên trục x là rỗng
  labs(title="Biểu đồ hộp cho biến mpg", subtitle="ví dụ về thêm màu") #đặt tiêu đề cho toàn bộ biểu đồ và tiêu đề phụ

#thêm một lớp cho biểu đồ
ggplot(mtcars, aes(x="", y=mpg)) + #ggplot(cơ sở dữ liệu, aes: thiết lập biến cho trục tung và trục hoành: trục x = rỗng, trục y = cột mpg)
  geom_boxplot(fill="slateblue", alpha=0.2) + # goem_boxplot() chạy hiển thị biểu đồ boxplot, đổ màu xanh và làm mờ toàn bộ biểu đồ với giá trị 0.2
  xlab("")+ #đặt tên trục x là rỗng
  ylab("Quãng đường trên 1 lít xăng") #đặt tiêu đề cho toàn bộ biểu đồ và tiêu đề phụ

# Cùng một biểu đồ nhưng thêm biến

#http://www.sthda.com/english/wiki/ggplot2-point-shapes
ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) + 
  #ggplot(cơ sở dữ liệu mtcars, aes: thiết lập biến cho trục tung và trục hoành: trục x = nhóm giá trị thuộc cột cyl, trục y = cột mpg)
  geom_boxplot(alpha=0.2) + # goem_boxplot() chạy hiển thị biểu đồ boxplot, đổ màu xanh và làm mờ toàn bộ biểu đồ với giá trị 0.2
  xlab("Dung tích xi lanh")+ #đặt tên trục x
  ylab("Quãng đường trên 1 lít xăng")+ #đặt tên trục y
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")# +
  #thiết lập thêm một lớp hiển thị giá trị trung bình fun=mean, hình hiển thị là hình tròn shape = 20, 
  #kích thước size = 4, màu color = red, màu full bên trong fill = red
 # theme(legend.position="none") + #chưa biết là gì --> KHÔNG CẦN THIẾT CHO TRƯỜNG HỢP NÀY, NHƯNG NHIỀU DẠNG ĐỒ THỊ KHÁC VỚI BOXPLOT THÌ GGPLOT TỰ ĐỘNG TẠO RA LEGEND (VÍ DỤ Ở BÊN PHẢI CỦA ĐỒ THÌ, DO ĐÓ, NẾU KHAI BÁO LÀ NONE THÌ LEGENDS ĐÓ SẼ KHÔNG HIỆN RA)
#  scale_fill_brewer("Dark2") #chưa biết là gì, thay đổi chưa thấy tác dụng

glimpse(mtcars)

ggplot(mtcars, aes(x=as.factor(cyl), y=mpg, fill = as.factor(cyl))) + 
  #ggplot(cơ sở dữ liệu mtcars, aes: thiết lập biến cho trục tung và trục hoành: trục x = nhóm giá trị thuộc cột cyl, trục y = cột mpg, 
  #đổ màu theo giá trị nhóm của trục x
  geom_boxplot(alpha=0.7) + 
  # goem_boxplot() chạy hiển thị biểu đồ boxplot, không cần đổ màu nữa vì đã đổ màu ở trên và làm mờ toàn bộ biểu đồ với giá trị 0.2
  xlab("Dung tích xi lanh")+ #đặt tên trục x
  ylab("Quãng đường trên 1 lít xăng")+ #đặt tên trục y
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  #thiết lập thêm một lớp hiển thị giá trị trung bình fun=mean,kiểu biểu đồ là điểm geom = point, hình hiển thị là hình tròn shape = 20, 
  #kích thước size = 4, màu color = red, màu full bên trong fill = red
  theme(legend.position="none") + #chưa biết là gì
  scale_fill_brewer(palette="Set1") #chưa biết là gì, thay đổi chưa thấy tác dụng

ggplot(mtcars, aes(x=as.factor(cyl), y=mpg, fill = as.factor(cyl))) + 
  #ggplot(cơ sở dữ liệu mtcars, aes: thiết lập biến cho trục tung và trục hoành: trục x = nhóm giá trị thuộc cột cyl, trục y = cột mpg, 
  #đổ màu theo giá trị nhóm của trục x
  geom_boxplot(alpha=0.7) + 
  # goem_boxplot() chạy hiển thị biểu đồ boxplot, không cần đổ màu nữa vì đã đổ màu ở trên và làm mờ toàn bộ biểu đồ với giá trị 0.2
  xlab("Dung tích xi lanh")+ #đặt tên trục x
  ylab("Quãng đường trên 1 lít xăng")+ #đặt tên trục y
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  #thiết lập thêm một lớp hiển thị giá trị trung bình fun=mean,kiểu biểu đồ là điểm geom = point, hình hiển thị là hình tròn shape = 20, 
  #kích thước size = 4, màu color = red, màu full bên trong fill = red
  theme(legend.position="none") + #chưa biết là gì
  scale_fill_brewer(palette="Set1") #chưa biết là gì, thay đổi chưa thấy tác dụng



ggplot(mtcars, aes(x=as.factor(cyl), y=mpg, fill = as.factor(cyl))) + 
  #ggplot(cơ sở dữ liệu mtcars, aes: thiết lập biến cho trục tung và trục hoành: trục x = nhóm giá trị thuộc cột cyl, trục y = cột mpg, 
  #đổ màu theo giá trị nhóm của trục x
  geom_boxplot(alpha=0.7) + 
  # goem_boxplot() chạy hiển thị biểu đồ boxplot, không cần đổ màu nữa vì đã đổ màu ở trên và làm mờ toàn bộ biểu đồ với giá trị 0.2
  xlab("Dung tích xi lanh")+ #đặt tên trục x
  ylab("Quãng đường trên 1 lít xăng")+ #đặt tên trục y
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  #thiết lập thêm một lớp hiển thị giá trị trung bình fun=mean,kiểu biểu đồ là điểm geom = point, hình hiển thị là hình tròn shape = 20, 
  #kích thước size = 4, màu color = red, màu full bên trong fill = red
  theme(legend.position="none") + #chưa biết là gì
  scale_fill_brewer(palette="Dark2") #thay đổi palette chung thành Dark2


#about palette http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html

# tự định nghĩa palette
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9") #thiết lập 1 biến mới là 1 vector chứa 3 giá trị màu khác nhau

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
#thiết lập 1 biến mới là 1 vector chứa 8 giá trị màu khác nhau

# To use for fills, add
scale_fill_manual(values=cbPalette) 
#thay đổi palette màu đổ vào trong thành vector đã tạo sẵn, bước này có vẻ như cần làm trước khi muốn sử dụng trong hàm vẽ đồ thị, 
#nhưng chưa chạy được

# To use for line and point colors, add
scale_colour_manual(values=cbPalette) 
#thay đổi palette màu đổ vào trong thành vector đã tạo sẵn, bước này có vẻ như cần làm trước khi muốn sử dụng trong hàm vẽ đồ thị
#nhưng chưa chạy được



ggplot(mtcars, aes(x=as.factor(cyl), y=mpg, fill = as.factor(cyl))) + 
  #ggplot(cơ sở dữ liệu mtcars, aes: thiết lập biến cho trục tung và trục hoành: trục x = nhóm giá trị thuộc cột cyl, trục y = cột mpg, 
  #đổ màu theo giá trị nhóm của trục x
  geom_boxplot(alpha=0.7) + 
  # goem_boxplot() chạy hiển thị biểu đồ boxplot, không cần đổ màu nữa vì đã đổ màu ở trên và làm mờ toàn bộ biểu đồ với giá trị 0.2
  xlab("Dung tích xi lanh")+ #đặt tên trục x
  ylab("Quãng đường trên 1 lít xăng")+ #đặt tên trục y
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  #thiết lập thêm một lớp hiển thị giá trị trung bình fun=mean,kiểu biểu đồ là điểm geom = point, hình hiển thị là hình tròn shape = 20, 
  #kích thước size = 4, màu color = red, màu full bên trong fill = red
  theme(legend.position="none") + #chưa biết là gì
  scale_fill_brewer(palette="cbPalette") 
  #thay đổi palette chung thành cbPalette, là biến màu đã tạo ở trên, nhưng không chạy được do k tìm thấy palette cbPalette


# thêm trang trí viền
#thiết lập 1 object p và gán hàm vẽ biểu đồ vào
p<-ggplot(mtcars, aes(x=as.factor(cyl), y=mpg, fill = as.factor(cyl))) + 
  #ggplot(cơ sở dữ liệu mtcars, aes: thiết lập biến cho trục tung và trục hoành: trục x = nhóm giá trị thuộc cột cyl, trục y = cột mpg, 
  #đổ màu theo giá trị nhóm của trục x
  geom_boxplot(alpha=0.7, color ="red") + 
  # goem_boxplot() chạy hiển thị biểu đồ boxplot, không cần đổ màu nữa vì đã đổ màu ở trên và làm mờ toàn bộ biểu đồ với giá trị 0.2
  xlab("Dung tích xi lanh")+ #đặt tên trục x
  ylab("Quãng đường trên 1 lít xăng")+ #đặt tên trục y
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  #thiết lập thêm một lớp hiển thị giá trị trung bình fun=mean,kiểu biểu đồ là điểm geom = point, hình hiển thị là hình tròn shape = 20, 
  #kích thước size = 4, màu color = red, màu full bên trong fill = red
  theme(legend.position="none") + #chưa biết là gì
  scale_fill_brewer(palette="Dark2") #thay đổi palette chung thành Dark2

p #chạy thử p, tương đương gọi hàm vẽ đồ thị đã thiết lập ở trên


# thay đổi background

p+theme_gray(base_size = 14)
#thiết lập loại giao diện chính là màu xám (gray), 
#trong theme này đã có sẵn chú thích nên mới xuất hiện phần chú thích bên tay phải biểu đồ

p+theme_bw()
#thiết lập loại giao diện chính là trắng đen, 
#trong theme này đã có sẵn chú thích nên mới xuất hiện phần chú thích bên tay phải biểu đồ

p + theme_minimal()
#sử dụng giao diện tối giản (không có khung đen bao ngoài biểu đồ)

p + theme_classic()
#sử dụng giao diện cơ bản (không có khung đen bao ngoài biểu đồ, không có lưới, có 2 trục x y)

p + theme_void()
#sử dụng giao diện trống (chỉ có biểu đồ hộp và chú thích)

p + theme_dark()+ labs(title="Biểu đồ hộp cho biến mpg", subtitle="ví dụ về thêm màu", fill="Kí hiệu")
#sử dụng giao diện tối và thêm tiêu đề, lưu ý fill là hàm đặt tiêu đề cho chú thích
# vị trí của chú giải

#thiết lập 1 object q và gán hàm vẽ biểu đồ vào
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
      theme(legend.position="top")+
      scale_fill_manual(values = c("#d8b365", "#f5f5f5", "#5ab4ac"))

# Thảy đổi Legends (Chú giải)
#http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/


#Hiển thị số trên đồ thị???

means <- aggregate(mpg ~  cyl, mtcars, mean)



q+ 
  geom_text(data = means, aes(label = mpg, y = mpg + 0.08))

#### trang trí thêm một chút

means <-round(means,2)

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



