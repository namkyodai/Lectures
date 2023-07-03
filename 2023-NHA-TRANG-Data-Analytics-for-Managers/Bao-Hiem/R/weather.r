#--------------------------------------------------------------
#--------------------------------------------------------------
library(rstudioapi) # thư viện dùng để đặt đường dẫn tới file gốc
setwd(dirname(getActiveDocumentContext()$path))  

#### PHẦN 1: Giới thiệu về gói dữ liệu liên quan đến lượng mưa đo được ở một trạm khí tượng.
#--------------------------------------------------------------
#--------------------------------------------------------------
library(readxl)
#weather <-data.frame(read_excel("weather.xlsx",sheet="02", skip = 0)) # đọc dữ liệu từ file excel
weather <- data.frame(read.csv("weather.csv",sep=";",stringsAsFactors=FALSE)) 

#--------------------------------------------------------------
#--------------------------------------------------------------
#### PHẦN 2: Chuẩn bị dữ liệu (Wrangling)
#--------------------------------------------------------------
#--------------------------------------------------------------

dim(weather) # xem tổng quan hàng và cột của gói dữ liệu

names(weather) # xem tên của các biến (Cột)

head(weather) # xem 10 dòng đầu tiên của gói dữ liệu

str(weather) # xem dạng của từng biến


# Kiểm tra xem gói dữ liệu có bao nhiêu mục bị bỏ trống
sum(is.na(weather))

# xem số dòng của gói dữ liệu
nrow(weather)
sum(complete.cases(weather))

nrow(weather) == sum(complete.cases(weather))


#############         TẠO FACTORS
weather <- data.frame(read.csv("weather.csv",sep=";",stringsAsFactors=FALSE)) 

class(weather$season) # kiểm tra dạng của biến mùa
summary(weather$season) # khi chưa sử dụng factor và levels, dữ liệu sẽ ở dạng gốc của nó
#sử dụng factor để đưa dữ liệu về dạng factor và levels để thiết lập các cấp độ chính của những giá trị

weather$season <- factor(weather$season, levels = c("Spring","Summer","Autumn","Winter"))
#các giá trị trong cột season gốc là tập hợp các string spring, summer, autumn, winter
#khi sử dụng factor và levels, cột season sẽ thành dạng nhân tố và có 4 cấp độ khác nhau
class(weather$season) # giờ chúng ta đã có được biến theo dạng nhân tố thay vì dạng đặc điểm.
summary(weather$season)

#check giá trị của cột day trước
weather$day #là cột chứ các giá trị ngày trong tháng, tổng cộng 365 giá trị
class(weather$day) #dạng của các giá trị trong cột day là integer
summary(weather$day) #một số thông số chung về cột day, lưu ý, nếu giá trị của cột là int, summary sẽ khác với str

weather$day <- as.factor(weather$day) #biến đổi cột day về dạng factor
summary(weather$day)
        
weather$month #là cột chứ các giá trị month, tổng cộng 365 giá trị, chạy từ 1 đến 12
class(weather$month) #dạng của các giá trị trong cột month là integer
summary(weather$month) #một số thông số chung về cột month, lưu ý, nếu giá trị của cột là int, summary sẽ khác với str

weather$month <- as.factor(weather$month)
summary(weather$month)

weather$dir.wind #là cột chứ các giá trị dir.wind, tổng cộng 365 giá trị, chạy từ 1 đến 12
class(weather$dir.wind) #dạng của các giá trị trong cột dir.wind là integer
summary(weather$dir.wind) #một số thông số chung về cột dir.wind

weather$dir.wind <- as.factor(weather$dir.wind)
summary(weather$dir.wind)

# Xử lý với các biến liên quan đến gió

#Bắt đầu kiểm tra là thực sự có 16 hướng gió trong biến dir.wind không? 

unique(weather$dir.wind) #thống kê các biến độc nhất trong cột dir.wind, hiển thị dưới dạng vector
length(unique(weather$dir.wind))  #độ dài của vector
#https://www.pinterest.com/pin/606226799819409300/

# đếm tổng số ngày trong gói dữ liệu theo từng nhóm của biến hướng giớ
table(weather$dir.wind) #hàm table dùng các nhân tố được xác thực chéo để tạo ra một bảng đếm các giá trị của mỗi kết hợp
#nghĩa là sau khi đã factor biến dir.wind, ta chỉ cần dùng table để tạo ra 1 bảng đếm các giá trị
#kết quả tương tự với summary nếu đã factor
# 
rel <- round(prop.table(table(weather$dir.wind))*100,1) 
prop.table(table(weather$dir.wind)) #tính tỉ lệ của các biến độc nhất so với toàn bảng, max = 1
#round(giá trị cần tính, số làm tròn sau dấu phẩy)
rel

# tiến hành xắp xếp lại cho bảng theo thứ tự giảm dần
sort(rel,decreasing = TRUE) #hàm sắp xếp, option giảm dần



# Biến đổi hướng giớ từ 16 nhóm sang 8 nhóm
weather$dir.wind.8 <- weather$dir.wind #gán giá trị cột dir.wind vào cột mới tên dir.wind.8, cột này sẽ xuất hiện trong bảng weather
weather$dir.wind.8 <- ifelse(weather$dir.wind %in%  c("NNE","ENE"),"NE",as.character(weather$dir.wind.8))
#hàm ifelse(condition,yes,no) trả về một giá trị có cùng kích thước với giá trị được đưa vào, trong đó các giá trị sẽ được tính toán
#theo dạng nếu phù hợp với điều kiện sẽ sẽ trả lại giá trị trong phần yes, nếu không sẽ trả lại giá trị trong phần no
#giải nghĩa: ifelse(nếu giá trị trong cột dir.wind nằm trong vector c("NNE","ENE","NE"), thì sẽ trả lại giá trị "NE", nếu không sẽ trả lại giá trị)
#weather$dir.wind.8 <- ifelse(weather$dir.wind %in%  c("NNE","ENE"),"NE",weather$dir.wind.8)
#vì sao sử dụng as.character(weather$dir.wind.8) chứ không phải chỉ weather$dir.wind.8? 
#vì khi sử dụng weather$dir.wind.8, hàm vẫn trả lại đúng giá trị tương ứng, nhưng dưới dạng index (vị trí của giá trị đó trong vector)
#dùng as.character() để hàm trả lại giá trị dạng str

weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("NNW","WNW"),"NW",as.character(weather$dir.wind.8)) 

weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("WSW","SSW"),"SW",as.character(weather$dir.wind.8)) 

weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("ESE","SSE"),"SE",as.character(weather$dir.wind.8)) 

#với các bước đã thực hiện ở trên, hiện tại bảng chỉ còn các giá trị NE, NW, SW, SE và N, E, S, W.
weather$dir.wind.8 <- factor(weather$dir.wind.8,levels = c("N","NE","E","SE","S","SW","W","NW"))
#thiết lập lại cột mới thành dạng factor và có 8 levels như trên

# kiểm tra có bao nhiêu nhóm
table(weather$dir.wind.8) #check lại số lượng các biến độc nhất trong cột mới
length(unique(weather$dir.wind.8)) #check lại số lượng các giá trị độc nahats
class(weather$dir.wind.8)

# Bảng 2 chiều (hướng giớ vs mùa)
round(prop.table(table(weather$dir.wind.8,weather$season),margin = 2)*100,1)
table(weather$dir.wind.8, weather$season)
prop.table(table(weather$dir.wind.8, weather$season), margin = NULL) 
#prop.table tính tỉ lệ của từng giá trị với các giá trị còn lại, 
#nếu margin không được gọi, tức là = NULL thì giá trị đó được tính tỉ lệ với tất cả các giá trị trong bảng
#nếu margin = 1, tính với các giá trị trong hàng; nếu margin = 2, tính với các giá trị trong cột

##### SỬA ĐỔI NGÀY THÁNG VÀ GIỜ

first.day <- "2021-01-01"
class(first.day)
first.day <- as.Date(first.day)
class(first.day)

weather$date  <- first.day + weather$day.count - 1 # tạo công thức để xác định ngày và tháng tương ứng với số ngày trong năm
#tạo cột mới tên date, gán giá trị là ngày đầu tiên của năm (đã thiết lập ở trên) + với giá trị đếm thứ tự của cột day - 1
#Nếu chỉ xét về mặt giá trị, weather$day.count là đã đủ, tuy nhiên nếu thế thì sẽ k có class date vì giá trị bên day là int
#vì thế, ta phải gán biến first.day (class date vào trước), giá trị này là giá trị gốc nên mặc định đã là 1, khi + thêm giá trị đếm của hàng tương ứng của cột day
#sẽ làm cho ngày chính xác bị tăng lên 1, vì thế ta phải trừ đi 1 ở giá trị cuối để ra ngày đúng

##### LÀM TRÒN GIỜ

#Làm việc với giờ trong R  thì hơi phức tạp so với làm việc với ngày và tháng hay với nhân tố. 
#Có 2 cách dùng, thứ nhất là dùng POSIXct và thứ 2 là dùng POSIXlt. Khi dùng POSIXct, R sẽ lưu ngày và thời gian theo một dạng số đơn giản tính đến giây, 
#thể hiện theo dạng UNIX (ví dụ như Jan 1, 2020). Nếu dùng POSIXlt, R sẽ lưu ngày và giờ theo dạng danh sách (list) với mỗi thành phần của danh sách là giây, giờ, năm.... 
#Do chúng ta quan tâm đến giờ, chúng ta dùng POSIXlt.

l.temp.time.date <- as.POSIXlt(paste(weather$date,weather$l.temp.time)) 
# khai báo biến mới là một vectors có giá trị tương ứng với tgian theo ngày và giờ của bảng weather, được chuyển dạng sang POSIXlt

#paste(x, sep=" ", collapse = NULL) được dùng để cộng các vectors x bằng cách biến chúng thành các ký tự, và trả lại 1 chuỗi str
#sep= " " dùng để cài đặt ký tự giữa mỗi thành tố trong string kết quả, ví dụ: sep="_" sẽ trả lại chuỗi ký tự là A_B_C_D
#collapse=NULL chỉ dùng khi cộng 2 hoặc nhiều vector, khi đó kết quả cộng các thành tố sẽ được gộp thành 1 string duy nhất và ngăn cách bằng dấu tương ứng, khi không có thì ta sẽ có nhiều string khác nhau
paste(c("K", "D", "L"), 1:9, sep = "|", collapse = NULL) #ví dụ paste có sep và collapse

head(l.temp.time.date) #check lại kết quả thực hiện việc tạo biến tgian mới

l.temp.time.date <- round(l.temp.time.date,"hours")# làm tròn đến giờ gần nhất  
head(l.temp.time.date)

attributes(l.temp.time.date) # kiểm tra các thuộc tính của biến
weather$l.temp.hour <- l.temp.time.date$hour # chỉ lấy ra giờ của biến
head(weather$l.temp.hour) # xem
weather$l.temp.hour <- as.factor(weather$l.temp.hour) # biến đổi số giờ thành factor
head(weather$l.temp.hour)

h.temp.time.date <- as.POSIXlt(paste(weather$date,weather$h.temp.time)) 
head(h.temp.time.date)
h.temp.time.date <- round(h.temp.time.date,"hours")# làm tròn đến giờ gần nhất
head(h.temp.time.date)
attributes(h.temp.time.date) # kiểm tra các thuộc tính của biến
weather$h.temp.hour <- h.temp.time.date$hour # chỉ lấy ra giờ của biến
head(weather$h.temp.hour) # xem
weather$h.temp.hour <- as.factor(weather$h.temp.hour) # biến đổi số giờ thành factor
head(weather$h.temp.hour)


gust.wind.time.date <- as.POSIXlt(paste(weather$date,weather$gust.wind.time)) 
head(gust.wind.time.date)
gust.wind.time.date <- round(gust.wind.time.date,"hours")# làm tròn đến giờ gần nhất
head(gust.wind.time.date)
attributes(gust.wind.time.date) # kiểm tra các thuộc tính của biến
weather$gust.wind.hour <- gust.wind.time.date$hour # chỉ lấy ra giờ của biến
head(weather$gust.wind.hour) # xem
weather$gust.wind.hour <- as.factor(weather$gust.wind.hour) # biến đổi số giờ thành factor
head(weather$gust.wind.hour)

str(weather) # xem lại các dạng của số liệu mới tạo ra

#--------------------------------------------------------------
#--------------------------------------------------------------
#### PHẦN 3: Vẽ Biểu Đồ với ggplot2
#--------------------------------------------------------------
#--------------------------------------------------------------
library(ggplot2)

ggplot(weather,aes(x = date,y = ave.temp)) +
  geom_point(colour = "blue") +
  geom_smooth(colour = "red",size = 1) +
  scale_y_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
  ggtitle ("Nhiệt độ trung bình hàng ngày") +
  xlab("Ngày tháng") +  ylab ("Nhiệt độ trung bình ( ºC )")


ggplot(weather,aes(x = date,y = ave.temp)) + 
  geom_point(aes(colour = ave.temp)) +
  scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = 16) + 
  geom_smooth(color = "red",size = 1) +
  scale_y_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
  ggtitle ("Nhiệt độ trung bình hàng ngày") +
  xlab("Ngày tháng") +  ylab ("Nhiệt độ trung bình ( ºC )")



### Phân tích nhiệt độ theo mùa


ggplot(weather,aes(x = ave.temp, colour = season)) +
  geom_density() +
  scale_x_continuous(limits = c(5,30), breaks = seq(5,30,5))+
  xlab("Nhiệt độ trung bình ( ºC )") +  ylab ("Xác xuất/ Mật độ")+
  labs(title = "Phân bổ nhiệt độ theo mùa") +
  scale_color_manual(name = "Mùa",labels = c("Xuân", "Hạ", "Thu", "Đông"), values = c("red", "blue","green","gray")) 

#### Phân tích nhiệt độ theo tháng - sử dụng biểu đồ đàn vĩ cầm

#weather$month = factor(weather$month,labels = c("Tháng 1","Tháng 2","Tháng 3","Tháng 4", "Tháng 5","Tháng 6","Tháng 7","Tháng 8","Tháng 9","Tháng 10","Tháng 11","Tháng 12"))


ggplot(weather,aes(x = month, y = ave.temp)) +
  geom_violin(fill = "orange") +
  geom_point(aes(size = rain), colour = "blue", position = "jitter") +
  ggtitle ("Phân bổ nhiệt độ theo tháng") +
  xlab("Tháng") +  ylab ("Nhiệt độ trung bình ( ºC )")+
  labs(size="Lượng mưa")


## Phân tích mối tương quan giữa nhiệt độ thấp và cao

ggplot(weather,aes(x = l.temp, y = h.temp)) +
  geom_point(colour = "firebrick", alpha = 0.3) + 
  geom_smooth(aes(colour = season),se= F, size = 1.1) +
   xlab("Nhiệt độ thấp ( ºC )") +  ylab ("Nhiệt độ cao ( ºC )")+
  labs(title = "Tương quan giữa nhiệt độ thấp và cao trong ngày") +
  scale_color_manual(name = "Mùa",labels = c("Xuân", "Hạ", "Thu", "Đông"), values = c("red", "blue","green","gray")) 


## Phân bổ vùng nhiệt độ thấp và cáo theo thời gian trong ngày.


library(reshape2) # sử dụng thư viện này để chuyển đổi bảng từ dạng bản rộng sang bảng dài

temperatures <- weather[c("day.count","h.temp.hour","l.temp.hour")] # Chỉ chọn các biến cần thiết (ngày và nhiệt độ) và đặt tên cho bảng mới này
head(temperatures)
head(temperatures)
tail(temperatures)

#chưa hiểu hàm melt này làm cụ thể cái gì? xóa cột? Dồn về 1 cột có tên mới? 
#hỏi gg: biến 1 object thành dataframe, chưa hiểu lắm 
temperatures_after_melt <- melt(temperatures,id.vars = "day.count", 
                     variable.name = "l.h.temp", value.name = "hour")
#tạo biến tên khác để tiện so sánh
head(temperatures)
head(temperatures_after_melt)
#vẫn chưa hiểu để làm gì? Có vẻ là biến đổi thay đổi biến của dataframe này sang biến tương ứng của dataframe mới
#lấy dữ liệu từ bảng temperatures, sau đó lấy id.vars = cột day.count (tại sao lại vẫn giữ tên cột là cột day.count?
#vì theo bảng mới tạo thì 1 dòng giờ chỉ tính là 1 lần đo nhiệt độ, trong khi bảng cũ 1 dòng là 2 lần đo nhiệt độ),
#đặt tên biến (cột) là l.h.temp, cột này giá trị (có vẻ) sẽ tự động lấy tên cột
#Đặt giá trị thuộc cột có tên hour, cột này sẽ tự động lấy giá trị tương ứng với các cột của bảng cũ
#đến đây thì đã hiểu sơ sơ cách melt() hoạt động, tuy nhiên để làm gì thì vẫn chưa hiểu lắm

#chạy lại dòng melt() cũ của thầy Nam để tiếp tục các đoạn code sau
temperatures <- melt(temperatures,id.vars = "day.count", 
                      variable.name = "l.h.temp", value.name = "hour")

temperatures$hour <- factor(temperatures$hour,levels=0:23) # cần thiết để sắp xếp trật tự của mức độ
summary(temperatures$hour)

ggplot(temperatures) +
  geom_bar(aes(x = hour, fill = l.h.temp)) + #đoạn này khác với bthg do có fill sẽ tự động vẽ dạng biểu đồ cột trồng cột
  #geom_bar(aes(x = hour)) + #thử test không có fill theo giá trị cột
  #có vẻ như bước sử dụng hàm melt() phía trên là để phục vụ mục đích làm biểu đồ này.
  #tức là nếu không tách ra một cột l.h.temp mới, sẽ rất khó thống kê số giờ nhiệt độ cao, và nhiệt độ thấp để trồng cột
  scale_fill_discrete(name= "", labels = c("Cao","Thấp")) +
  scale_y_continuous(limits = c(0,100)) +
  ggtitle ("Nhiệt độ cao thấp theo giờ trong ngày") +
  xlab("Giờ") +  ylab ("Tần suất")

#--------------------------------------------------------------
#--------------------------------------------------------------
#### PHẦN 4: Sử dụng EDA và ggplot2
#--------------------------------------------------------------
#--------------------------------------------------------------
# Và giờ chúng ta bắt đầu phân tích nhé.

# Tìm hiểu về biến phụ thuộc - Lượng mưa
#Sys.setlocale("LC_TIME", "Vietnam")
#Sys.setlocale("LC_TIME", "English")

# độ thị mô tả giữa lượng mưa và thời gian, đường biểu diễn
ggplot(weather, aes(date,rain)) + #gọi hàm vẽ, truy cập bảng weather, dùng biến date và rain cho trục x và y
  geom_point(aes(colour = rain)) + #vẽ biểu đồ điểm, màu mỗi điểm dựa vào lượng mưa tương ứng của ngày đó
  geom_smooth(colour = "blue", size = 1) + #vẽ biểu đồ đường trung bình hoặc xu hướng
  stat_smooth(colour = "green", size = 1) + #gg nói hàm này tương tư geom_smooth nhưng nếu mình muốn vẽ 1 "non-standard geom",
  #vẽ ra thấy như nhau, chưa biết khác gì
  scale_colour_gradient2(low = "green", mid = "orange",high = "red", midpoint = 20) + 
  #cái này có vẻ dành riêng cho point, dùng để phân loại các điểm theo màu từ thấp đến cao, có xác định midpoint
  scale_y_continuous(breaks = seq(-10,100,2)) + #hàm này định nghĩa trục y, thông số break xác định y tick, điểm đầu điểm cuối và giới hạn nhỏ nhất
  #lưu ý, thông số này phụ thuộc vào thông số limit, nghĩa là dù có tăng cao thấp đến đâu, phần hiển thị cũng chỉ trong khoảng đã thiết lập ở limit
  xlab("Ngày") +  ylab("Lưu lượng mưa (mm)") +  ggtitle("Lượng mưa")+
  labs(size = "Lưu lượng")


# Đồ thị tần suất Histogram về lưu lượng mưa

ggplot(weather,aes(rain)) + 
  geom_histogram(binwidth = 1,colour = "blue", fill = "darkgrey") +
  scale_x_continuous(breaks = seq(0,80,5)) +
  scale_y_continuous(breaks = seq(0,225,25)) +
  xlab("Lưu lượng (mm)") +
  ylab ("Tần suất (ngày)") +
  ggtitle("Phân bổ lưu lượng mưa")
#biểu đồ này không có vấn đề gì

summary(weather$rain) # độ lệch sang phải rất cao
quantile(weather$rain) #xem thử tứ phân vị
summary(subset(weather, rain > 0)$rain) # độ lệch vẫn còn cao
#hàm subset() dùng để lọc ra những dòng đáp ứng điều kiện nhất định

#vẻ thử biểu đồ boxplot

ggplot(weather,aes(rain)) + 
  geom_boxplot()

ggplot(weather, aes(x="", y=rain)) + 
  geom_boxplot(fill="red", alpha=0.2)+ 
  xlab("")+
  ylab("Lượng mưa (mm)")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+ 
  #hàm này hình như vẽ biểu đồ tổng hợp thêm (chắc tương tự như geom_smooth), tuy nhiên sẽ dùng 1 số công thức toán nhất định
  #như mean, med, max, min gì đó, chưa biết có dùng công thức được k
  theme(legend.position="none") + #ok
  scale_fill_brewer(palette="Set1") #ok

library(dplyr)

ggplot(weather %>% filter(rain >0), aes(x="", y=rain)) + 
  geom_boxplot(fill="red", alpha=0.2)+ 
  xlab("")+
  ylab("Lượng mưa (mm)")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
#ok

library(e1071) # gói này dùng để tính độ lệch
#Các chức năng phân tích lớp tiềm ẩn, biến đổi Fourier trong thời gian ngắn, phân cụm mờ, hỗ trợ vector machine?, 
#tính toán đường đi ngắn nhất, phân cụm có đường bao, bộ phân loại Bayes Naive, tổng quát hóa k.nearest neighbor ...

#skewness: sự lệch lạc, lệch dương nghĩa là đuôi trải dài về bên phải, lệch âm là đuôi trải dài về bên trái
#https://vietnambiz.vn/do-lech-skewness-la-gi-cong-thuc-tinh-do-lech-201911051611407.htm
skewness(weather$rain) #kết quả ra dương => lệch dương ~ lệch phải
#  công thức skewness trong R = u3/(u2^(3/2)) => u là gì?
#check quantile
quantile((weather$rain))
5.3/(0.3^(3/2)) #không phải
(5.3/0.3)^3/2  #không phải
skewness(subset(weather, rain >0)$rain) #tính độ lệch khi loại dữ liệu = 0 => bớt lệch

# chúng ta tạo ra biến nhị thức (Yes, No), có hoặc không

#hàm nrow(x,y) đếm số hàng trong bảng x, thỏa điều kiện y
nrow(subset(weather, rain == 0)) # số ngày không mưa
#tỉ lệ của ngày có lượng mưa = 0 so với cả năm
nrow(subset(weather, rain ==0))/length(weather$rain)

nrow(subset(weather, rain <1 & rain >0)) # số ngày mưa nhưng rất ít và có thể coi là ngày nắng

weather$rained <- ifelse(weather$rain >= 1, "Rain", "No Rain") 
# biến mới ~ tạo cột mới trong bảng weather có giá trị lượng mưa >= 1 thì gán Rain, không thì gán No rain

table(rained = weather$rained) # thống kê ngày nắng và ngày mưa, tuyệt đối

prop.table(table(rained = weather$rained)) # tạo bảng tương đối

# thành phố này có vẻ mưa rất nhiều trong năm, với 40% số ngày trong năm là mưa.


# Xem xét sự liên kết giữa lượng mưa và mùa trong năm


# Biến liên tục


ggplot(weather, aes(season,rain)) + #bảng weather, trục x là mùa, trục y là lượng mưa
  geom_jitter(aes(colour=rain), position = position_jitter(width = 0.2)) + #vẽ biểu đồ jitter 
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = 30) +
  scale_y_continuous(breaks = seq(0,80,20)) +
  xlab("Mùa") +
  ylab ("Lượng mưa (mm)") +
  ggtitle("Lượng mưa hàng ngày theo mùa") +
  labs(size ="Lưu lượng")
#ok
  
# tính nhanh lượng mưa theo mùa
tapply(weather$rain,weather$season,summary)
#tapply cho phép tạo 1 công thức (có thể là summary) cho nhóm dựa vào các levels của factor. 
#tapply(dữ liệu, nhóm, công thức)
#giải nghĩa: thực thi hàm summary cho biến rain, phân theo nhóm mùa

# Phân tích sự xuất hiện mưa theo mùa dùng với biến nhị thức


# Đồ thị thanh, ngày nắng và ngày mưa theo mùa
ggplot(weather,aes(season)) +
  geom_bar(aes(fill = rained), position = "fill") +
  geom_hline(aes(yintercept = prop.table(table(rained))["No"]), colour = "blue",linetype = "dashed", size = 1) + #chưa thấy khác biệt gì từ dòng này
  annotate("text", x = 1, y = 0.65, label = "yr. w/o = 0.60", colour = "blue") +
  xlab("Mùa") +
  ylab ("Tỷ lệ") +
  ggtitle("Tỷ lệ các ngày nắng và mưa theo mùa")

round(prop.table(table(season = weather$season, rained= weather$rained),1),2) 

#

weather.num <- weather[c("rain","l.temp","h.temp","ave.temp","ave.wind","gust.wind", "l.temp.hour","h.temp.hour","gust.wind.hour")] 
#tạo bảng mới chỉ lấy dữ liệu từ các cột trên của bảng weather

str(weather.num) #check class của các biến trogn bảng

# chuyển đổi các biến thông số sau thành biến số

weather.num$l.temp.hour <- as.numeric(weather.num$l.temp.hour)
weather.num$h.temp.hour <- as.numeric(weather.num$h.temp.hour)
weather.num$gust.wind.hour <- as.numeric(weather.num$gust.wind.hour) 

str(weather.num) #check xem class đã chuyển chưa?
#tại sao phải làm bước này? Bởi vì muốn tính tương quan thì nên đưa về dạng num hoặc int


# dùng hàm cor() cho dữ liệu mới này và tạo ra ma trận tương quan
round(cor(weather.num),2) #đọc, phân tích bảng này như thế nào?
round(cor(weather.num),2)[1,] # trọn ra dòng đầu tiên của ma trận --> lương quan giữa lượng mưa và các biến còn lại

# chia khung dữ liệu thành 4 phần, mỗi phần cho một mùa
weather.num.season <- split(weather.num,weather$season) #chia bảng thành các phần theo factor của mùa

class(weather.num.season) # kiểm ra dạng của dữ liệu này --> là dạng danh sách

length(weather.num.season) # kiểm tra trong danh sách này có bao nhiêu cấu phần

summary(weather.num.season) # kiểm tra xem từng cầu phần của danh sách là thuộc định dạng nào

attributes(weather.num.season) # kiểm tra tên cho từng cấu phần của danh sách

# lưu ý trong R hàm apply là một trong những hàm rất mạnh của ngôn ngữ R, nhưng cũng là một hàm rất khó học.
# hàm sapply(x,z) ứng với phương trình z cho mỗi thành phần của biến x
#hàm này trước tiên đi tới các thành phần của một danh sách và tính toán ma trận tương quan (cho tất cả)
# cho mỗi mùa, chỉ tra lại giá trị tương quan giữa lượng mưa và các biến còn lại
sapply(weather.num.season, function (x) round(cor(x)["rain",],2))

# dưới đây là một cách khác mà có thể là dễ hơn khi ta sử dụng hàm lapply

lapply(weather.num.season, function (x) round(cor(x)["rain",],2))
#lapply vs sapply: hàm apply rất tiện lợi cho việc thực hiện 1 số tính toán chung như sum, cumulative sum, mean, etc các yếu tố trong object thuộc list
#lapply() trình bày kết quả dưới dạng list còn sapply() trình bày kết quả dưới dạng vector
#sapply hiệu quả hơn lapply() xét về mặt kết quả trả về vì nó lưu kết quả trực tiếp dưới dạng vector

# giờ chúng ta tìm hiểu sự tuyến tính liên quan đến tương quan giữa mưa và tốc độ gió lớn nhất ghi được trong ngày, phụ thuộc vào biến mùa trong năm.

ggplot(weather,aes(gust.wind,rain)) + 
  geom_point(colour = "firebrick") +
  geom_smooth(size = 0.75, se = F) +
  facet_wrap(~season, ncol  = 3) + #tạo ra một loạt các panels (theo số lượng biến) và xếp nó theo 2 chiều, điều khiển bằng ncol, nrow
  #facet_grid(~season) + #thử facet_grid như gg dạy, k đc, nghiên cứu sau
  xlab("Tốc độ gió tối đa (km/h)") +
  ylab ("Lượng mưa (mm)") +
  ggtitle("Lượng mưa vs tốc độ gió tối đa, theo mùa")

# sử dụng hàm quantile cho chúng ta thấy được 4 khoảng đều nhau
quantile(weather$h.temp) #??? sang phần mới???

# giờ chúng chỉ cần dùng hàm cut dựa trên quantile 
weather$h.temp.quant <- cut(weather$h.temp, breaks = quantile(weather$h.temp),
                            labels = c("Cold","Cool","Warm","Hot"),include.lowest = T)
#hàm cut() chia 1 bộ dữ liệu thành các phần dựa trên 1 công thức = breaks
weather$h.temp
weather$h.temp.quant.test <- cut(weather$h.temp, breaks = quantile(weather$h.temp),include.lowest = T)
#cách hoạt động của breaks: nhập 1 vector vào breaks, ví dụ: c(1, 2, 3, 4), hàm cut sẽ chạy qua tất cả dữ liệu, cứ dữ liệu nào lớn hơn 1 và nhỏ hơn 2 sẽ thuộc nhóm 1
#giá trị nào lớn hơn 2 nhỏ hơn 3 sẽ thuộc nhóm 2
#bởi vì chia theo quantile nên giá trị trả về của hàm cut sẽ có dạng (giá trị tứ phân vị, giá trị của dòng dữ liệu]
#nhìn vào giá trị level đầu tiên sẽ thấy, nếu include.lowest = T thì ngoặc vuông - có nghĩa là bao gồm, F thì ngoặc tròn là không bao gồm
weather$h.temp.quant.test
# kết quả
table(weather$h.temp.quant)

# Sự xuất hiện của mưa, theo mùa và theo nhiệt độ cao trong ngày
ggplot(weather,aes(rained,gust.wind)) +
  geom_boxplot(aes(colour=rained)) +
  facet_grid(h.temp.quant~season) + #h.temp.quant có 4 nhóm Cold, Cool, Warm, Hot, season có 4 nhóm spring, summer, autumn, winter
  xlab("Sự xuất hiện mưa") +
  ylab ("Tốc độ gió tối đa (km/h)") +
  ggtitle("Mưa theo mùa và theo nhiệt độ")

#--------------------------------------------------------------
#--------------------------------------------------------------
#### PHẦN 5: Lập Mô Hình - Tiên đoán lượng mưa
#--------------------------------------------------------------
#--------------------------------------------------------------

set.seed(123) # dùng để tạo lập khả năng tái tạo mẫu, số 123 không có một ý nghĩa cụ thể nào
#tương tự như bên python, muốn tạo 1 con số random khác thì chỉ cần đổi số

# lấy ngẫu nhiên 70% số lượng quan sát trên 365 mẫu

index <- sample(1:nrow(weather),size = 0.7*nrow(weather)) 

#thiết lập biến index, sử dụng hàm sample() để tạo giá trị từ 1 đến số hàng của bảng weather (365), chỉ lấy ngẫu nhiên 70% số giá trị tạo ra 
# gán vào dữ liệu học
index
train <- weather[index,] #nghĩa là chỉ lấy tất cả cột thuộc các dòng tồn tại trong vector index
nrow(train) #vector index có 255 dòng, tương ứng việc biến train sẽ có 255 giá trị
# gán cho dữ liệu thử nghiệm
test <- weather [-index,] #nghĩa là chỉ lấy tất cả cột thuộc các dòng k tồn tại trong vector index

nrow(train)
nrow(test)

# tạo ra một khung dữ liệu với chỉ số học và thử nghiệm
group <- rep(NA,365) #hàm rep(x,y) - replicate là hàm tạo ra 1 vector lặp đi lặp lại giá trị x với số lần là y
group <- ifelse(seq(1,365) %in% index,"Train","Test") #hàm ifelse(condition, x, y) đi qua từng giá trị của dữ liệu, nếu condition đúng
#thì trả về giá trị x, nếu condition sai trả về giá trị y

df <- data.frame(date=weather$date,rain=weather$rain,group) #thiết lập một dataframe gồm cột date = giá trị cột weather$date
#cột rain bằng giá trị của weather$rain, và vector group

ggplot(df,aes(x = date,y = rain)) + geom_point(aes(color = group)) + 
  #tác dụng aes() khi dùng ở ggplot hay geom như nhau nếu chỉ có 1 dạng biểu đồ
  scale_color_discrete(name="") + theme(legend.position="top")

# Đánh giá số liệu


## Chúng ta sẽ sử dụng MAE (mean absolute error) (sai số trung bình tuyệt đối) như là hệ sai số thứ 2 để đánh giá. 

## https://gmd.copernicus.org/articles/7/1247/2014/gmd-7-1247-2014.pdf

# mô hình gốc -- tiên đoán giá trị trung bình trong dữ liệu học
train$rain
best.guess <- mean(train$rain)
best.guess
# Đánh giá trị số  (root mean squared error) và MAE cho dữ liệu thử nghiệm
MSE = mean((best.guess-test$rain)^2) #tương đương với công thức MSE: mean squared error: giá trị sai số bình phương trung bình hoặc lỗi bình phương trunh bình
#đơn giản đề cập đến giá trị trung bình của chênh lệch bình phương giữa tham số dự đoán và các tham số quan sát được

RMSE.baseline <- sqrt(MSE) 
RMSE.baseline


MAE.baseline <- mean(abs(best.guess-test$rain)) #abs: trả lại giá trị tuyệt đối của 1 hàm 
MAE.baseline


# Hồi qui tuyến đính đa biến
# sử dụng hàm lm để làm hồi qui
lin.reg <- lm(log(rain+1) ~ season +  h.temp + ave.temp + ave.wind + gust.wind +
                dir.wind + as.numeric(gust.wind.hour), data = train)

summary(lin.reg)

# Cái gì là ảnh hưởng theo cấp số nhân của biến gió
exp(lin.reg$coefficients["gust.wind"]) #tính hàm mũ của hàng gust.wind trong bảng coefficients của list lin.reg

# Sử dụng mô hình để kiểm tra với dữ liệu thử nghiệm
# đừng quên là phải dùng hàm exp để cho giá trị log quay trở lại ban đầu
test.pred.lin <- exp(predict(lin.reg,test))-1 

test.pred.lin
# đánh giá tính chính xác

RMSE.lin.reg <- sqrt(mean((test.pred.lin-test$rain)^2))
RMSE.lin.reg

MAE.lin.reg <- mean(abs(test.pred.lin-test$rain))
MAE.lin.reg

#cây quyết định là bài toán unsupervised
#cây quyết định hỗ trợ việc xác định mối tương quan giữa các biến


library(rpart) # thư viện rpart dùng để trồng cây

library(rattle) # thư viện rattle cũng dùng để trồng cây, nhưng đẹp hơn (sủ dụng phương trình fancyRpartPlot)

rattle()

# dùng rpart áp dụng cho một biến số --> cây hồi qui
rt <- rpart(rain ~ month + season + l.temp + h.temp + ave.temp + ave.wind +
              gust.wind + dir.wind + dir.wind.8 + as.numeric(h.temp.hour)+
              as.numeric(l.temp.hour)+ as.numeric(gust.wind.hour), data=train)

fancyRpartPlot(rt)


#plot(rt)
# Cây trồng ra sẽ có 8 nhánh với 6 biến.




# 
test.pred.rtree <- predict(rt,test)

RMSE.rtree <- sqrt(mean((test.pred.rtree-test$rain)^2))

RMSE.rtree

MAE.rtree <- mean(abs(test.pred.rtree-test$rain))
MAE.rtree

# giờ chúng ta đã một cây trưởng thành với đầy đủ cành và lá, ta xem chúng ta có thể tỉa cành không?


# kiểm tra các kết quả đánh giá chéo ( cột x-error )
# Nó liên hệ tới 2 nhành và cp
printcp(rt)

# Lấy giá trị tối ưu của cp một cách hệ thống...

min.xerror <- rt$cptable[which.min(rt$cptable[,"xerror"]),"CP"]
min.xerror

# và sử dụng giá trị đó để tỉa cành
rt.pruned <- prune(rt,cp = min.xerror) 

# sau đó vẽ lại cây đã tỉa cành
fancyRpartPlot(rt.pruned)

# Đánh giá cây mới trên dữ liệu thử nghiệm
test.pred.rtree.p <- predict(rt.pruned,test)
RMSE.rtree.pruned <- sqrt(mean((test.pred.rtree.p-test$rain)^2))
RMSE.rtree.pruned

MAE.rtree.pruned <- mean(abs(test.pred.rtree.p-test$rain))
MAE.rtree.pruned

### Phương pháp trồng rừng

# bây giờ ta bắt tay vào công việc trồng rừng nhé

library(randomForest) 

# chuyển đổi các biến nhân tố thành biến số
train$h.temp.hour <- as.numeric(train$h.temp.hour)
train$l.temp.hour <- as.numeric(train$l.temp.hour)
train$gust.wind.hour <- as.numeric(train$gust.wind.hour)
test$h.temp.hour <- as.numeric(test$h.temp.hour)
test$l.temp.hour <- as.numeric(test$l.temp.hour)
test$gust.wind.hour <- as.numeric(test$gust.wind.hour)

# For reproducibility; 123 has no particular meaning
# Run this immediately before creating the random forest
set.seed(123)

# Tạo một rừng cây với 1000 cây
rf <- randomForest(rain ~ month + season + l.temp + h.temp + ave.temp + ave.wind +
                     gust.wind + dir.wind + dir.wind.8 + h.temp.hour + l.temp.hour +
                     gust.wind.hour, data = train, importance = TRUE, ntree=1000)

# Câu hỏi đặt ra là số lượng cây là bao nhiêu để đạt tới giá trị sai số nhỏ nhất?
# Đây là một vấn đề đơn giản, 
which.min(rf$mse)

# vẽ đồ thị sai số ước tính là hàm của số lượng cây
plot(rf) 

# sử dụng hàm importance() để tính toán sự quan trọng của mỗi biến

imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp

# như thường lệ, ta tiên đoán kết quả khi sử dụng gói dữ liệu thử nghiệm

test.pred.forest <- predict(rf,test)
RMSE.forest <- sqrt(mean((test.pred.forest-test$rain)^2))
RMSE.forest


MAE.forest <- mean(abs(test.pred.forest-test$rain))
MAE.forest

# tạo một khung về sự đo lường sai số cho mỗi phương pháp
accuracy <- data.frame(Method = c("Baseline","Linear Regression","Full tree","Pruned tree","Random forest"),
                       RMSE   = c(RMSE.baseline,RMSE.lin.reg,RMSE.rtree,RMSE.rtree.pruned,RMSE.forest),
                       MAE    = c(MAE.baseline,MAE.lin.reg,MAE.rtree,MAE.rtree.pruned,MAE.forest)) 

# làm tròn các giá trị và in ra bảng
accuracy$RMSE <- round(accuracy$RMSE,2)
accuracy$MAE <- round(accuracy$MAE,2) 
accuracy


all.predictions <- data.frame(actual = test$rain,
                              baseline = best.guess,
                              linear.regression = test.pred.lin,
                              full.tree = test.pred.rtree,
                              pruned.tree = test.pred.rtree.p,
                              random.forest = test.pred.forest)

head(all.predictions)

# cần thiết để chuyển cột thành hàng để sử dụng hàm gather()
# thư viện tidyr là một thư viên thay thế cho thư viện reshape2 để làm việc này
library(tidyr) # 

all.predictions <- gather(all.predictions,key = model,value = predictions,2:6)

head(all.predictions)

tail (all.predictions)

# Thể hiện đồ thị để so sánh
ggplot(data = all.predictions,aes(x = actual, y = predictions)) + 
  geom_point(colour = "blue") + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_vline(xintercept = 23, colour = "green", linetype = "dashed") +
  facet_wrap(~ model,ncol = 2) + 
  coord_cartesian(xlim = c(0,70),ylim = c(0,70)) +
  ggtitle("Predicted vs. Actual, by model")

#--------------------------------------------------------------
#### Sử dụng mô hình Decision Tree cho biến Factor
#--------------------------------------------------------------

#---------------------------------------------------------------
#### BTVN tuần 4 ###
#---------------------------------------------------------------

#Dùng dplyr và mutate để tạo biến mới thay vì cách đã nêu trong ví dụ. 
#Các biến mới này có giá trị được chuyển thể/lấy từ các giá trị của biến cũ. 
#Ví dụ như biến lượng mưa à từ character thành numeric.

#set working directory and import libraries
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
library(readxl)
library(dplyr)

#nhập dữ liệu
weather = read.csv("weather.csv",sep=";", stringsAsFactors = FALSE)

#ôn lại phương pháp cũ trong bài này
#phương pháp cũ dùng cách gán dữ liệu vào 1 biến được viết dưới dạng truy cập vào 1 cột chưa có trong bảng
#R sẽ hiểu là mình muốn tạo 1 cột mới (vì cột mình muốn truy cập k tồn tại)

#phương pháp sử dụng dplyr và mutate
#dplyr là thư viện => gg xem mutate làm được gì: https://dplyr.tidyverse.org/reference/mutate.html
#mutate() thêm những biến mới và bảo toàn những biến cũ, transmute() thêm biến mới và bỏ biến cũ
#mutate(.data, name-value, .keep, .before, .after) ~bảng dữ liệu, công thức gồm tên cột mới và giá trị, 
#keep~ giữ lại những cột nào, có thể có các giá trị "all" ~ giữ lại hết, "used" giữ lại những cột dùng trong công thức
#"unused" chỉ giữ lại các cột không dùng trong công thức; "none" xóa toàn bộ những cột của bảng cũ, chỉ còn những cột mới tạo
#before after xác định vị trí cột

#xem lại tên các cột
names(weather)

#dùng mutate để tạo một cột mới là quarter, tương ứng với cột tháng, nếu tháng 1 2 3 thì = Q1, 4 5 6 = Q2,...
#hàm giữ lại tất cả các cột, cột mới ở ngay sau cột month
weather2 = mutate(weather, quarter = month, .keep="all", .after = "month")
#check giá tri class của quarter
str(weather2$quarter)

#sử dụng ifelse để thay đổi giá trị của quarter
weather2$quarter <- ifelse(weather2$month %in% c(1,2,3),"Q1", weather2$quarter) 
weather2$quarter <- ifelse(weather2$month %in% c(4,5,6),"Q2", weather2$quarter) 
weather2$quarter <- ifelse(weather2$month %in% c(7,8,9),"Q3", weather2$quarter) 
weather2$quarter <- ifelse(weather2$month %in% c(10,11,12),"Q4", weather2$quarter)

str(weather2$quarter)

weather2$quarter = factor(weather2$quarter, levels = c("Q1", "Q2", "Q3", "Q4"))
str(weather2$quarter)
summary(weather$quarter)
table(weather2$quarter)

#------------------------------------------------------------------------------------
#### Dùng rpart để vẽ ra cây quyết định bằng Phương pháp cây quyết định phân loại (Classification Decision Tree) ####
#------------------------------------------------------------------------------------

#set working directory and import libraries
library(ggplot2)
library(rpart)
library(vtree)
library(rpart.plot)

#nhập file excel
weather_xl = data.frame(read_excel("weather.xlsx", sheet="02", skip = 0))

#chia các giá trị trong mỗi cột thành các nhóm khác nhau
##cột ave.temp
summary(weather_xl$ave.temp) #check thông tin về cột ave.temp
weather_xl$ave.temp.c = cut(weather_xl$ave.temp, breaks = c(-Inf,14,21,Inf), labels = c("cold", "mild", "warm"), include.lowest = TRUE)
#tạo 1 cột mới tên ave.temp.c bằng cách cut() dữ liệu cột ave.temp, chia thành các khoảng từ dưới 14, 14~21, và trên 21 thành các giá trị COld, Mild, Warm, có bao gồm giá trị nhỏ nhất

##Cột rain
summary(weather_xl$rain) 
#summary giá trị thuộc cột rain, tuy nhiên các giá trị cột rain lệch phải rất lớn nên hàm này không đưa ra được nhiều thông tin giúp quyết định chia khoảng như thế nào
#check các mức độ của lượng mưa có thể chia được, không nhất thiết phải chính xác 100%
#https://www.quora.com/How-much-annual-rainfall-is-considered-low-moderate-and-high => dưới 25 là low, ở bối cảnh này trên 50 là high
weather_xl$rain.c = cut(weather_xl$rain, breaks = c(-Inf, 25, 50, Inf), labels = c("low", "moderate", "high"), include.lowest = TRUE)

#cột ave.wind 
summary(weather_xl$ave.wind)
weather_xl$ave.wind.c = cut(weather_xl$ave.wind, breaks = c(-Inf, 3.5, 5.2, Inf), labels = c("light", "moderate", "strong"), include.lowest =  TRUE)

#cột gust.wind
summary(weather_xl$gust.wind)
weather_xl$gust.wind.c = cut(weather_xl$gust.wind, breaks = c(-Inf, 22.5, 38.6, Inf), labels = c("light", "medium", "strong"), include.lowest =  TRUE)

#lập 1 bảng mới để phân tích dễ hơn
weather_xl_tree = weather_xl %>%
  select(season, dir.wind, ave.temp.c, rain.c, ave.wind.c, gust.wind.c)
#kiểm tra các biến trong bảng
str(weather_xl_tree)

#có 2 biến chưa phải factor, biến các biến này thành factor, lần này k dùng factor() mà dùng hàm mutate_if()

weather_xl_tree = weather_xl_tree %>%
  mutate_if(is.character, as.factor)
#check lại xem các biến đã thành dạng factor hết chưa bằng hàm str()
str(weather_xl_tree) #ok

#kiểm tra tên biến và tính tổng các tổ hợp
names(weather_xl_tree)

num_weather = length(unique(weather_xl_tree$season)) *  length(unique(weather_xl_tree$dir.wind)) * length(unique(weather_xl_tree$ave.temp.c)) * length(unique(weather_xl_tree$ave.wind.c)) * length(unique(weather_xl_tree$rain.c)) * length(unique(weather_xl_tree$gust.wind.c))
num_weather

#check bảng từng cột
table(weather_xl_tree$season)
table(weather_xl_tree$dir.wind)                                                                                                                                                                                                                                         
table(weather_xl_tree$ave.temp.c)
table(weather_xl_tree$rain.c)
table(weather_xl_tree$ave.wind.c)
table(weather_xl_tree$gust.wind.c)

# Sử dụng thư viện vtree để vẽ thống kê cơ bản.
vtree(weather_xl_tree, "season", palette = 6, sortfill= TRUE)
vtree(weather_xl_tree, "dir.wind", palette = 6, sortfill= TRUE) #lưu ý: có nhánh 1%
vtree(weather_xl_tree, "ave.temp.c", palette = 6, sortfill= TRUE)
vtree(weather_xl_tree, "rain.c", palette = 6, sortfill= TRUE)
vtree(weather_xl_tree, "ave.wind.c", palette = 6, sortfill= TRUE)
vtree(weather_xl_tree, "gust.wind.c", palette = 6, sortfill= TRUE)

total = nrow(weather_xl_tree)
#tính giá trị hàm số entropy
table(weather_xl_tree$rain.c)
vtree(weather_xl_tree, c("rain.c"), horiz = FALSE)
h.s = -342/total*log(342/total) -14/total*log(14/total) -9/total*log(9/total) 
h.s #=0.2 ~ độ tinh khiết khá cao

#tính giá trị hàm số entropy của gust.wind 
table(weather_xl_tree$gust.wind.c)
vtree(weather_xl_tree, c("gust.wind.c"), horiz = FALSE)
h.s = -100/total*log(100/total) -185/total*log(185/total) -80/total*log(80/total) 
h.s #=1.03 ~ độ tinh khiết cao, nhưng sao lại lớn hơn 1?

#tính giá trị hàm số entropy của ave.wind.c 
table(weather_xl_tree$ave.wind.c)
vtree(weather_xl_tree, c("ave.wind.c"), horiz = FALSE)
h.s = -186/total*log(186/total) -88/total*log(88/total) -91/total*log(91/total) 
h.s #=1.03 ~ độ tinh khiết cao, nhưng sao lại lớn hơn 1?

table(weather_xl_tree$dir.wind)                                                                                                                                                                                                                                         

table(weather_xl_tree$ave.temp.c)

table(weather_xl_tree$season)

table(weather_xl_tree$ave.wind.c)

table(weather_xl_tree$gust.wind.c)

control <- rpart.control(minsplit = 2, #số lượng quan sát tối thiểu phải có trong mỗi node để một phân nhánh được thực hiện
                         minbucket = 1, #số lượng quan sát tối thiểu cho mỗi node
                         maxdepth = 6, #chiều sâu rễ
                         cp = 0.01) #thang đo phức tạp, bất kỳ phân nhánh nào mà k làm information gain lên được thêm 1% sẽ tự động bị loại 

tree <- rpart(rain.c~ season + ave.temp.c + ave.wind.c +  gust.wind.c + dir.wind , #công thức ~ muốn tạo cây từ những node gì
              data=weather_xl_tree, #dữ liệu lấy ở đâu
              parms=list(split='information'), 
              #optional, đối với phân nhóm (classification splitting), cần đưa vào 1 list có thể là vector các biến khả thi ưu tiên (prior=)
              #hoặc ma trận tổn thất (loss=), hoặc splitting index(split=)
              control = control, #hàm control điều khiển việc phân nhánh đã được thiết lập ở trên
              method = "class") #dạng phân tích: có thể là "anova", "poisson", "class" hoặc "exp"
tree
rpart.plot(tree, nn=TRUE)

#### Kết quả có vẻ lượng mưa khá thấp, tuy nhiên do chưa hiểu lắm cách đọc dữ liệu bảng từ pp hồi quy tuyến tính nên nhóm chưa so sánh được sự khác biệt giữa 2 phương pháp