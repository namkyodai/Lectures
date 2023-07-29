#--------------------------------------------------------------
#--------------------------------------------------------------
library(rstudioapi) # thư viện dùng để đặt đường dẫn tới file gốc
setwd(dirname(getActiveDocumentContext()$path))
library(tidyverse) #
library(dplyr)
library(psych)
library(lubridate) # sử dụng để chuyển đổi biến thời gian
library(xts) # một thư viện khác để sử lý biến thời gian
library(tseries) # gói này dùng để sử dụng để phân tích dữ liệu theo thời gian
library(forecast) # dự đoán tương lại cho các biến thười gian thông qua các mô hình

#Visualisation  --> Các gói thư viện dùng để vẽ đồ thị và trang trí đồ thị
library(corrplot) # đồ thị vẽ tương quan giữa các biến
library(plotly) # đồ thị động
library(viridis) # dùng để tô màu cho bản đồ
library(ggmap) # sử dụng để lấy bản đồ từ google
library(knitr) # gói này dùng để tạo bảng đẹp --> trong trường hợp cần dùng
library(dygraphs) # sử dụng để vẽ đồ thị theo giời gian --> nhưng cũng k cần thiết vì thư viện ggplot cũng có rồi
library(ggthemes) # tạo theme cho các đồ thị

#### PHẦN 1: Giới thiệu về gói dữ liệu liên quan đến giá nhà tại thành phố Bắc Kinh
#https://www.kaggle.com/eraw0x/house-prices-in-beijing-eda-arima
#--------------------------------------------------------------
#--------------------------------------------------------------
beijin <- data.frame(read.csv("housingprice-beijin.csv",sep=","))
beijin <- beijin%>% mutate(floor = str_trim(str_extract(floor,"( .*)"), side = "both"))

beijin <- beijin %>%
            select(-url, -id, -Cid)

glimpse(beijin)
summary(beijin)

# 1. Xử lý dữ liệu trống

# Tìm hiểu về các biến có dữ liệu trống

library(psych)
library(lubridate)
x1 <- map_df(beijin, function(x){sum(is.na(x))}) # Các bạn để ý là sum(is.na(x)) chúng ta thật ra đã học rồi đấy. Ở đây chỉ gọi thêm hàm map_df là một hàm trong gói thư viện tidyversa để tổng hợp một hàm (function) khác.
x1

summary(beijin)

missing <- x1 %>% gather(key = "Variable") %>% filter(value > 0) %>% mutate(value = value/nrow(beijin)) # như chúng ta thấy ở đây, toàn bộ dữ liệu trống được tổng hợp lại thành 1 object (missing) và được tính % thông qua việc sử dụng hàm mutate trong thư viện dplyr.

missing

ggplot(missing, aes(x = reorder(Variable, -value),y = value)) +
  geom_bar(stat = "identity", fill = "salmon") +
  coord_flip()

#Như chúng ta đã thấy biến DOM (Days on Market - Số ngày trên thị trường) có chứa rất nhiều dữ liệu trống, chiếm tới gần 50% trong tổng dữ liệu.

# Câu hỏi đặt ra là chúng ta có nên xóa tất cả các dòng trống này không? --> xóa đi 50% dữ liệu không phải là lựa chọn tối ưu. Trong trường hợp này, chúng ta sẽ dùng phương pháp quy tụ để xử lý. Bước đầu tiên là phân tích dữ liệu của biến này để xem phân bổ dữ liệu đó ra sao.

summary(beijin$DOM)
# --> ta thấy là dữ liệu này bị lệch sang phải rất lớn, để biết được giá trị độ lệch, ta có thể dùng hàm describe

describe(beijin$DOM)

# như chúng ta thấy, giá trị độ lệch là 4.36, đây là một giá trị lệch lớn so với thang -0.5 đến 0.5

# chúng ta sử dụng giá trị trung bị để xử lý.

a<- ifelse(is.na(beijin$DOM),median(beijin$DOM,na.rm=TRUE),beijin$DOM)

beijin$DOM

b <- data.frame(a,beijin$DOM)

beijin$DOM<- ifelse(is.na(beijin$DOM),median(beijin$DOM,na.rm=TRUE),beijin$DOM)
describe(beijin$DOM)

summary(beijin$DOM)


# với các giá trị rỗng của các biến khác vì số lượng ít, nên chúng ta sẽ loại bỏ nó.

x2 <- map_df(beijin, function(x){sum(is.na(x))})
missing <- x2 %>% gather(key = "Variable") %>% filter(value > 0) %>% mutate(value = value/nrow(beijin))
missing

ggplot(missing, aes(x = reorder(Variable, -value),y = value)) +
  geom_bar(stat = "identity", fill = "salmon") +
  coord_flip()

df <- na.omit(beijin)


dim(df)
summary(df)
glimpse(df)

# chúng ta để ý, là các biến sau cần phải biến đổi

df <- df %>%
  mutate(buildingType = case_when(buildingType == 1 ~ "Tower",
                                  buildingType == 2 ~ "Bungalow",
                                  buildingType == 3 ~ "Plate/Tower",
                                  buildingType == 4 ~ "Plate"))

# chuyển các giá trị, 1, 2,3,4 thành các định nghĩa tương ứng


# df <- df %>%
#   mutate(buildingType = case_when(buildingType == 1 ~ "Tower",
#                                   buildingType == 2 ~ "Bungalow",
#                                   buildingType == 3 ~ "Plate/Tower",
#                                   buildingType == 4 ~ "Plate"))

df <- df %>%
  mutate(renovationCondition = case_when(renovationCondition == 1 ~ "Other",
                                         renovationCondition == 2 ~ "Rough",
                                         renovationCondition == 3 ~ "Simplicity",
                                         renovationCondition == 4 ~ "Hardcover"))


df <- df %>%
  mutate(buildingStructure = case_when(buildingStructure == 1 ~ "Unavailable",
                                       buildingStructure == 2 ~ "Mixed",
                                       buildingStructure == 3 ~ "Brick/Wood",
                                       buildingStructure == 4 ~ "Brick/Concrete",
                                       buildingStructure == 5 ~ "Steel",
                                       buildingStructure == 6 ~ "Steel/Concrete"))

df <- df %>%
  mutate(elevator = case_when(elevator == 1 ~ "Has_Elevator",
                              elevator != 1 ~ "No_elevator"))


df <- df %>%
  mutate(subway = case_when(subway == 1 ~ "Has_Subway",
                            subway != 1 ~ "No_Subway"))


df <- df %>%
  mutate(fiveYearsProperty = case_when(fiveYearsProperty == 1 ~ "Ownership < 5y",
                                       fiveYearsProperty != 1 ~ "Ownership > 5y"))


df <- df %>%
  mutate(district = case_when(district == 1 ~ "DongCheng",
                              district == 2 ~ "FengTai",
                              district == 3 ~ "DaXing",
                              district == 4 ~ "FaXing",
                              district == 5 ~ "FangShang",
                              district == 6 ~ "ChangPing",
                              district == 7 ~ "ChaoYang",
                              district == 8 ~ "HaiDian",
                              district == 9 ~ "ShiJingShan",
                              district == 10 ~ "XiCheng",
                              district == 11 ~ "TongZhou",
                              district == 12 ~ "ShunYi",
                              district == 13 ~ "MenTouGou"))
# 2. chuyển đổi các giá trị biến này thành factor.



group_categorical <- c("buildingType","renovationCondition", "buildingStructure", "elevator", "subway","district")


df <- df %>% mutate_at(group_categorical, as.factor)

summary(df1$buildingType)
str(df1)


df$constructionTime <- as.numeric(df$constructionTime)


x3 <- map_df(df, function(x){sum(is.na(x))})
x3
missing <- x3 %>% gather(key = "Variable") %>% filter(value > 0) %>% mutate(value = value/nrow(df))
missing


ggplot(missing, aes(x = reorder(Variable, -value),y = value)) +
  geom_bar(stat = "identity", fill = "salmon") +
  coord_flip()

# chúng ta có thể thấy là biến constructiontime có rất nhiều dữ liệu trống --> Sao vậy? bởi vì hàm as.numeric để chuyển hệ không đổi được toàn bộ các font chữ, trong trường hợp này dữ liệu gốc dùng hệ TQ chẳng hạn. Đây là một lưu ý rất quan trọng. Như vậy ngay cả khi ta đã thực hiện converstion nhưng vẫn phải kiểm tra lại dữ liệu mới xem nó có thực sự ổn không.
summary(df$constructionTime)

#Trong trường hợp này chúng ta có 2 giải pháp, một là xóa bỏ đi toàn bộ hàng có dữ liệu trông, 2 là dùng giá trị median để thay thế.




#giá trị này chiếm khoảng bao nhiêu %

sum(is.na(df$constructionTime))/length(df$constructionTime)*100

#--> khoảng 5% --> chúng ta có thể xóa nó
df <- na.omit(df)
sum(is.na(df))



df$tradeTime <- as.Date(df$tradeTime)
df$livingRoom <- as.integer(df$livingRoom)
df$drawingRoom <- as.integer(df$drawingRoom)
df$bathRoom <- as.integer(df$bathRoom)

glimpse(df)
sum(is.na(df))

# 3. Dùng bảng và biểu đồ để khám phá dữ liệu EDA

ggplot(df, aes(price))+
  geom_histogram(fill = "red", alpha =0.3)

# chuyển về tiền Việt Nam cho dễ nhớ :)

tygia <- 3600
tyle <- 1000000

df <- df %>% mutate(totalPrice = totalPrice*10000) # giá trị thực tế của ngôi nhà
df <- df %>% mutate(totalPrice = totalPrice*tygia) # chuyển đổi thành tiền Việt Nam



df <- df %>% mutate(price = totalPrice/square) # giá trị ngôi nhà theo diện tích 1 m2
df <- df %>% mutate(price = totalPrice/square/tyle) # giá trị ngôi nhà theo diện tích 1 m2
df <- df %>% mutate(totalPrice = totalPrice/tyle) # giá trị ngôi nhà theo diện tích 1 m2




head(df)

#cách 1
ggplot(df, aes(price))+geom_histogram(fill = "steelblue")
# cách 2
df %>%
  ggplot(aes(price))+
  geom_histogram(fill = "steelblue")

ggplot(df, aes(price))+
  geom_boxplot()

# Loại nhà và giá
ggplot(df , aes(x= buildingType, y=price, color = buildingType))+
  geom_boxplot() + labs(x = "Loại nhà", y =" Giá trên 1m2")

# gọi bản đồ
load(file = "beijing_map.RData",verbose = TRUE)
beijing

beijing + geom_point(data = df, aes(Lng, Lat, color = buildingType),size=1.3,alpha=1) +theme(axis.title= element_blank(), axis.text =element_blank())

# Loại kết cấu và giá
ggplot(df , aes(x= buildingStructure, y=price, color = buildingStructure))+
  geom_boxplot() + labs(x = "Loại kết cấu", y =" Giá trên 1m2")

# Điều kiện cải tạo
ggplot(df , aes(x= renovationCondition, y=price, color = renovationCondition))+
  geom_boxplot() + labs(x = "Điều kiện cải tạo", y =" Giá trên 1m2")

# Thang máy và giá
ggplot(df , aes(x= elevator, y=price, color = elevator))+
  geom_boxplot() + labs(x = "Thang máy", y =" Giá trên 1m2")


# gần ga tàu điện ngầm và giá
ggplot(df , aes(x= subway, y=price, color = subway))+
  geom_boxplot() + labs(x = "Tàu điện ngầm", y =" Giá trên 1m2")



# Sở hữu trên 5 năm và giá
ggplot(df , aes(x= fiveYearsProperty, y=price, color = fiveYearsProperty))+
  geom_boxplot() + labs(x = "Sở hữu 5 năm", y =" Giá trên 1m2")


# Sở hữu trên 5 năm và giá
ggplot(df, aes(reorder(x= district, -price), y=price, color = district))+geom_boxplot() + labs(title = "Giá nhà theo hàm của Quận", y =" Giá trên 1 m2")+coord_flip()


beijing + geom_point(data = df, aes(df$Lng, df$Lat, color = district),size=1.2,alpha=1) +theme(axis.title= element_blank(), axis.text =element_blank())

#--------------------------------------------------------------
#--------------------------------------------------------------
# Dữ liệu về thời gian

glimpse(df)




df$tradeTimeM <- floor_date(df$tradeTime, unit = "month")
df$tradeTimeY <- floor_date(df$tradeTime, unit = "year")
df$constructionTime_date <- ymd(df$constructionTime, truncated = 2L)

df$constructionTime <- as.Date(df$constructionTime)



df_time1 <- df %>%  filter(tradeTimeM >= ymd("2010-01-01") & tradeTimeM < ymd("2018-01-01")) %>%
  group_by(tradeTimeM) %>%
  summarize(mean = mean(price))

df_time2 <- df %>%
  group_by(tradeTimeM) %>%
  summarize(mean = mean(price))


#library(ggplot2)
library(hrbrthemes)


# Plot
ggplot(df_time2, aes(x=tradeTimeM, y=mean)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=3, color="#69b3a2") +
  ggtitle("Evolution of something")

ggplot(df_time2, aes(x=tradeTimeM, y=mean)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=3, color="#69b3a2") +
  theme_ipsum() +
  ggtitle("Giá nhà theo thời gian")


p <- ggplot(df_time2, aes(x=tradeTimeM, y=mean)) +
  geom_area( fill="blue", alpha=0.4) +
  geom_line(color="green", size=2) +
  geom_point(size=3, color="green")+
  labs(
    title = 'Giá nhà theo thời gian',
    x = 'Thời gian',
    y = 'Giá nhà trung bình/m2'
  )
p
# sử dụng thư viện plotly
library(plotly)
ggplotly(p)


q <- ggplot(df_time1, aes(x=tradeTimeM, y=mean)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=3, color="#69b3a2")+
  labs(
    title = 'Giá nhà theo thời gian',
    x = 'Thời gian',
    y = 'Giá nhà trung bình/m2'
  )
q

ggplotly(q)


# Đưa ra một số câu hỏi
# giá nhà và địa điểm, số lượng phòng, tầng, loại nhà, gần ga tàu, thang máy.

# như vậy giá nhà có thể hiểu là biến đầu gia, còn các biến khác là biến đầu vào.


# Chúng ta thử chạy hồi qui xem thế nào?

lr <- lm(price ~ livingRoom + drawingRoom + kitchen + bathRoom, df)
summary(lr)


df1 <- df %>%
    select(price, livingRoom,drawingRoom,kitchen, bathRoom,buildingType)%>%
    filter(buildingType == "Tower")


lr <- lm(price ~ livingRoom + drawingRoom + kitchen + bathRoom, df1)
summary(lr)


# dùng mô hình cây quyết định???

df1 <- df %>%
  select(price, livingRoom, drawingRoom, kitchen, bathRoom)
