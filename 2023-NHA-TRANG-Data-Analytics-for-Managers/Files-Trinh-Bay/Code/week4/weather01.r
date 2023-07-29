library(rstudioapi) # thư viện dùng để đặt đường dẫn tới file gốc
setwd(dirname(getActiveDocumentContext()$path))  
# Đọc dữ liệu thời tiết từ file excel

library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)


thoi.tiet <-data.frame(read_excel("weather.xlsx",sheet="01",skip = 0)) # đọc dữ liệu từ file excel

thoi.tiet$temp.d <- cut(thoi.tiet$temp, breaks = c(-Inf,20,26,Inf),
                        labels = c("cool","mild","hot"),include.lowest = F)

thoi.tiet$humi.d <- cut(thoi.tiet$humi, breaks = c(-Inf,80,Inf),
                        labels = c("normal","high"),include.lowest = F)

thoi.tiet$wind.d <- cut(thoi.tiet$wind, breaks = c(-Inf,20,Inf),
                        labels = c("weak","strong"),include.lowest = F)

# Biến các biến rời rạc thành dạng factor
## Cách 1


thoi.tiet$outlook <- factor(thoi.tiet$outlook, levels = c("sunny","overcast","rain"))
thoi.tiet$temp.d <- factor(thoi.tiet$temp.d, levels = c("hot","mild","cool"))
thoi.tiet$humi.d <- factor(thoi.tiet$humi.d, levels = c("high","normal"))
thoi.tiet$wind.d <- factor(thoi.tiet$wind.d, levels = c("strong","weak"))
thoi.tiet$play <- factor(thoi.tiet$play, levels = c("yes","no"))

## Cách 2
#thoi.tiet <- thoi.tiet%>%mutate_if(is.character, as.factor)
#str(thoi.tiet)


3*3*2*2

write.csv(thoi.tiet,"weather01-factor.csv", row.names = FALSE)


names(thoi.tiet) #kiểm tra tên biến

soluong.thoitiet <- length(unique(thoi.tiet$outlook))*length(unique(thoi.tiet$temp.d))*length(unique(thoi.tiet$wind.d))*length(unique(thoi.tiet$play)) # nhân số lượng phân loại của từng biến để tính tổng số dạng thời tiết.
soluong.thoitiet

table(thoi.tiet$outlook)
table(thoi.tiet$temp.d)
table(thoi.tiet$humi.d)
table(thoi.tiet$wind.d)

# Sử dụng thư viện vtree để vẽ thống kê cơ bản.
library(vtree) #https://www.infoworld.com/article/3573577/how-to-count-by-groups-in-r.html

vtree(thoi.tiet, "outlook", palette = 3, sortfill = TRUE)

vtree(thoi.tiet, "temp.d", palette = 3, sortfill = TRUE)

vtree(thoi.tiet, "humi.d", palette = 3, sortfill = TRUE)
vtree(thoi.tiet, "wind.d", palette = 3, sortfill = TRUE)
vtree(thoi.tiet, "play", palette = 3, sortfill = TRUE)
vtree(thoi.tiet, c("outlook", "temp.d"),horiz = FALSE)
vtree(thoi.tiet, c("outlook", "play"),horiz = FALSE)
vtree(thoi.tiet, c("outlook", "temp.d","humi.d"),horiz = FALSE)
vtree(thoi.tiet, c("outlook", "temp.d","humi.d", "play"),horiz = FALSE)

vtree(thoi.tiet, c("temp.d", "outlook","humi.d", "play"),horiz = FALSE)




## bắt đầu tính giá trị hàm số entropy
library(vtree)
table(thoi.tiet$play)
vtree(thoi.tiet, c("play"),horiz = FALSE)
h.s = -5/14*log(5/14) - 9/14*log(9/14)
h.s


#Với biến bầu trời
vtree(thoi.tiet, c("outlook", "play"),horiz = FALSE)

thoi.tiet %>%
  filter(outlook == "sunny")

h.s.sunny = -2/5*log(2/5) - 3/5*log(3/5)
h.s.sunny

thoi.tiet %>%
       filter(outlook == "overcast")
h.s.overcast = -4/4*log(4/4) - 0/4*log(0.000000001/4)
h.s.overcast

thoi.tiet %>%
       filter(outlook == "rain")
h.s.rain = -2/5*log(2/5) - 3/5*log(3/5)
h.s.rain

h.outlook.s <- 5/14*h.s.sunny + 4/14*h.s.overcast + 5/14*h.s.rain
h.outlook.s

## Với biến nhiệt độ

vtree(thoi.tiet, c("temp.d", "play"),horiz = FALSE)

thoi.tiet %>%
  filter(temp.d == "cool")

h.s.cool = -3/4*log(3/4) - 1/4*log(1/4)
h.s.cool

thoi.tiet %>%
  filter(temp.d == "mild")
h.s.mild = -2/6*log(2/6) - 4/6*log(4/6)
h.s.mild

thoi.tiet %>%
  filter(temp.d == "hot")
h.s.hot = -2/4*log(2/4) - 2/4*log(2/4)
h.s.hot

h.temp.s <- 4/14*h.s.cool + 6/14*h.s.mild + 4/14*h.s.hot
h.temp.s


## Với biến gio

vtree(thoi.tiet, c("wind.d", "play"),horiz = FALSE)

thoi.tiet %>%
  filter(wind.d == "strong")

h.s.strong = -3/6*log(3/6) - 3/6*log(3/6)
h.s.strong

thoi.tiet %>%
  filter(wind.d == "weak")

h.s.weak = -2/8*log(2/8) - 6/8*log(6/8)
h.s.weak

h.wind.s <- 6/14*h.s.strong + 8/14*h.s.weak
h.wind.s




## Với biến do am

vtree(thoi.tiet, c("humi.d", "play"),horiz = FALSE)

thoi.tiet %>%
  filter(humi.d == "high")

h.s.high = -4/7*log(4/7) - 3/7*log(3/7)
h.s.high

thoi.tiet %>%
  filter(humi.d == "normal")

h.s.normal = -1/7*log(1/7) - 6/7*log(6/7)
h.s.normal

h.humi.s <- 7/14*h.s.high + 7/14*h.s.normal
h.humi.s

## 
c(h.outlook.s,h.temp.s,h.wind.s,h.humi.s)



###


# Đến đây chúng ta có thể thấy rằng giá trị h.outlook.s là nhỏ nhất, tương ứng với information gain là lớn nhất

# --> Phân chia node đầu sẽ là outlook
# --> overcast thì không cần phân chia nữa vì nó đã tinh khiết
# ---> tương ứng với sunny thì 




glimpse(thoi.tiet)
str(thoi.tiet)


control <- rpart.control(minsplit = 2,
                         minbucket = 1,
                         maxdepth = 4,
                         cp = 0.01)

#regression tree
#classificatin tree


# building the classification tree with rpart
tree <- rpart(play~ outlook + temp.d + humi.d + wind.d,
              data=thoi.tiet,
              parms=list(split='information'),
              control = control,
              method = "class")
tree
rpart.plot(tree, nn=TRUE)

