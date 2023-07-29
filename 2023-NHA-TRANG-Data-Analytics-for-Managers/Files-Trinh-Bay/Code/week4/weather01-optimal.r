# Đọc dữ liệu thời tiết từ file excel
library(rstudioapi) # thư viện dùng để đặt đường dẫn tới file gốc
setwd(dirname(getActiveDocumentContext()$path))       # Đặt thư mục làm việc trùng với thư mục của file
library(readxl) 
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
thoi.tiet <- thoi.tiet%>%mutate_if(is.character, as.factor)

# Sử dụng thư viện vtree để vẽ thống kê cơ bản.
library(vtree) #https://www.infoworld.com/article/3573577/how-to-count-by-groups-in-r.html
vtree(thoi.tiet, c("outlook", "temp.d","humi.d", "play"),horiz = FALSE)


## bắt đầu tính giá trị hàm số entropy
control <- rpart.control(minsplit = 2,
                         minbucket = 1,
                         maxdepth = 4,
                         cp = 0.01)
#https://www.rdocumentation.org/packages/rpart/versions/4.1-15/topics/rpart.control

# building the classification tree with rpart
tree <- rpart(play~ outlook + temp.d + humi.d + wind.d,
              data=thoi.tiet,
              parms=list(split='information'),
              control = control,
              method = "class")
tree
rpart.plot(tree, nn=TRUE)
