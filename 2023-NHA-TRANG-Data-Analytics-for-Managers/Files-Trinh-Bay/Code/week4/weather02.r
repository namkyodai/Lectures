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
str(weather)
library(dplyr)
glimpse(weather)

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
# Các biến có thể dễ dàng chuyển đổi thành dạng Factors (Nhân tốt) sử dụng hàm as.factor(). Hàm này là một dạng của hàm chính factor() trong R. Sử dụng as.factor() thì R sẽ sắp xêp các cấp độ theo bảng chữ cái khi mà biến ban đầu là một chuỗi (ở trong R gọi là lớp đặc điểm), mà với chuỗi thì đôi khi chúng ta không muốn dùng cho các biến. Ví dụ như, mùa trong năm.



class(weather$season) # kiểm tra dạng của biến mùa
summary(weather$season)

weather$season <- factor(weather$season, levels = c("Spring","Summer","Autumn","Winter"))

class(weather$season) # giờ chúng ta đã có được biến theo dạng nhân tố thay vì dạng đặc điểm.
summary(weather$season)

weather$day <- as.factor(weather$day)
weather$month <- as.factor(weather$month)
weather$dir.wind <- as.factor(weather$dir.wind)

str(weather)

# Xử lý với các biến liên quan đến gió

#Bắt đầu kiểm tra là thực sự có 16 hướng gió trong biến dir.wind không? nếu đúng thì tiếp theo là xác định xem có đủ quan sát cho từng nhóm không.

length(unique(weather$dir.wind))
#https://www.pinterest.com/pin/606226799819409300/

# đếm tổng số ngày trong gói dữ liệu theo từng nhóm của biến hướng giớ
table(weather$dir.wind)
#
rel <- round(prop.table(table(weather$dir.wind))*100,1)
rel

# tiến hành xắp xếp lại cho bảng theo thứ tự giảm dần
sort(rel,decreasing = TRUE)



# Biến đổi hướng giớ từ 16 nhóm sang 8 nhóm
weather$dir.wind.8 <- weather$dir.wind

weather$dir.wind.8 <- ifelse(weather$dir.wind %in%  c("NNE","ENE"),"NE",as.character(weather$dir.wind.8))


weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("NNW","WNW"),"NW",as.character(weather$dir.wind.8))

weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("WSW","SSW"),"SW",as.character(weather$dir.wind.8))

weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("ESE","SSE"),"SE",as.character(weather$dir.wind.8))

weather$dir.wind.8 <- factor(weather$dir.wind.8,levels = c("N","NE","E","SE","S","SW","W","NW"))


# kiểm tra có bao nhiêu nhóm
table(weather$dir.wind.8)
length(unique(weather$dir.wind.8))



# Bảng 2 chiều (hướng giớ vs mùa)
round(prop.table(table(weather$dir.wind.8,weather$season),margin = 2)*100,1)

##### SỬA ĐỔI NGÀY THÁNG VÀ GIỜ

first.day <- "2021-01-01"
class(first.day)
first.day <- as.Date(first.day)
class(first.day)

weather$date  <- first.day + weather$day.count - 1 # tạo công thức để xác định ngày và tháng tương ứng với số ngày trong năm

##### LÀM TRÒN GIỜ

#Làm việc với giờ trong R  thì hơi phức tạp so với làm việc với ngày và tháng hay với nhân tố. Có 2 cách dùng, thứ nhất là dùng POSIXct và thứ 2 là dùng POSIXlt. Khi dùng POSIXct, R sẽ lưu ngày và thời gian theo một dạng số đơn giản tính đến giây, thể hiện theo dạng UNIX (ví dụ như Jan 1, 2020). Nếu dùng POSIXlt, R sẽ lưu ngày và giờ theo dạng danh sách (list) với mỗi thành phần của danh sách là giây, giờ, năm.... Do chúng ta quan tâm đến giờ, chúng ta dùng POSIXlt.

l.temp.time.date <- as.POSIXlt(paste(weather$date,weather$l.temp.time)) # khai báo biến mới
head(l.temp.time.date)
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

## Ghi chú

# như chúng ta đã thấy, công việc dọn dẹp và chuyển đổi dữ liệu là luôn cần thiết trước khi chúng ta tiến hành phân tích. Trong thực tế, giai đoạn biểu đồ hóa và làm mô hình thì dễ hơn và thật ra là tốn ít thời gian hơn giai đoạn dọn dẹp dữ liệu.

# lưu ý là R rất mạnh khi dùng để biểu đồ hóa và làm mô hình. Nhưng R không phải là ngôn ngữ gần gũi nhất để làm công việc dọn dẹp dữ liệu. Ngoài R, chúng ta có thể dùng các phương án sau

### Xử dụng Excel với các bản Pivot, hàm Vlookup(), Index(), Match() và một vài hàm khác là chúng ta cũng có thể dọn dẹp được dữ liệu. Nhưng tất nhiên là dữ liệu đó không quá lớn.

### Sử dụng các ngôn ngữ như Python/Perl


### Sử dụng các gói thư viện của R chuyên cho công tác dọn dẹp dữ liệu, ví dụ như gói lubricate


#--------------------------------------------------------------
#--------------------------------------------------------------
#### PHẦN 3: Vẽ Biểu Đồ với ggplot2
#--------------------------------------------------------------


#Thư viện ggplot2, được tạo ra bởi Hadley Wickham dựa vào Ngữ Pháp về Đồ Thị được phát triển bởi Leland Wilkison. Đây là một ngữ pháp vẽ đồ thị rất hệ thống nhằm miêu tả các thành phần của một đồ thị. Kể từ năm 2014, thư viện ggplot2 là một thư viện được tải về nhiều nhất.


# Trong thư viện ggplot2, các biểu đồ được xây dựng từ các lớp. Dữ liệu được thể hiện, hệ tọa độ, kích thước, cạnh, nhãn, và ghi chú là những ví dự về lớp. Điều này có nghĩa là với bất cứ đồ thị nào, dù có phức tạp đến đâu, thì đều có thể được xây dựng một cách rất cơ bản bằng cách thêm các lớp theo thứ tự cho đến khi chúng ta có được một đồ thì theo ý muốn. Mỗi một lớp của đồ thì có một hoặc nhiều thành phần, ví dụ như dữ liệu (data), các yếu tố thẩm mỹ (mapping), số liệu thống kê (stats). Bên cạnh dữ liệu, tính thẩm mỹ và hình học là hai thành phần quan trọng và chúng ta nên hiểu rõ về chúng.

## Các yếu tố về thẩm mỹ là các yếu tố liên quan đến sự trực quan thể hiện dữ liệu; các yêu tố cơ bản nhất của tính thẩm mỹ là giá trị trục x và trục y, màu sắc, kích cỡ, và hình dạng.

## Các yếu tố hình học là các thành phần dùng để sự dụng để in ra dữ liệu, ví dụ như điểm, đường thẳng, thành, bản đồ, v.v

# Ok vậy những điều nói ở trên có ý nghĩa gì trong việc thực hành? à, nếu chúng ta muốn vẽ một đồ thị điểm để thể hiện quan hệ nhiệt độ thấp nhất và cao nhất cho mỗi ngày, kiểm soát màu ứng với từng mùa, và chọn lựa kích thước cho lượng mưa. Chúng ta sẽ phản ánh thuộc tính thẩm mỹ x và y, tô màu và chọn kích cỡ cho từng biến tương ứng, và sau đó là vẽ độ thị thông qua việc sử dụng hình học, trong trường hợp này ta sử dụng geom_point().

# Các cú pháp của thư viện ggplot2

## Một đồ thị cơ bản khi sử dụng ggplot2 bao gồm việc tạo ra một đối tượng thông qua sử dụng hàm ggplot(), và ngay sau đó là chúng ta thêm lớp.

### ggplot() --> luôn được sử dụng đầu tiên để khai báo. Ở đây chúng ta sẽ khai báo các cấu phần chung nhất cho các lớp còn lại. Ví dụ như dữ liệu nào và các thuộc tính thẩm mỹ như trục. Ví dụ dưới đây

#### ggplot(data = dataframe, aes(x = var1, y= var2, colour = var3, size = var4))

### geom_xxx()   ---> lớp này được thêm vào sau hàm ggplot() và dùng để tạo đồ thị mong muốn. Nếu toàn bộ các thuộc tính thẩm mỹ đã được khai báo trong hàm ggplot() từ trước thì khi thêm lớp này vào, chúng ta không phải thêm gì vào trong dấu ngoặc đơn (). Trong trường hợp chúng ta thêm khai báo cho lớp này thì các thông số, nếu đã được khai ở ggplot(), sẽ bị ghi đè. Điều này có nghĩa là các thông số được khai báo ở lớp sau sẽ ghi đè và loại bỏ các thông số ở trước.

#### ggplot(data = weather, aes(x = l.temp, y= h.temp, colour = season) + geom_point()   --> tạo đồ thị và khai báo dạng độ thì

#### ggplot(data = weather, aes(x = l.temp, y= h.temp, colour = season) + geom_point(aes(colour=dir.wind))  --> vẫn như trước nhưng giờ ta thêm khai báo cho thuộc tính thẩm mỹ liên quan đến màu sắc và chọn biến là hướng gió dir.wind

#### ggplot(data = weather, aes(x = h.temp, y= l.temp) + geom_point(colour = "blue")  ---> khai báo màu

#### ggplot(data = weather, aes(x = l.temp, y= h.temp) + geom_point(aes(size=rain), colour = "blue")  --> Khai báo về kích thước thể hiện trên đồ thị.



#--------------------------------------------------------------
library(ggplot2)

ggplot(weather,aes(x = date,y = ave.temp)) +
  geom_point(colour = "green") +
  geom_smooth(colour = "red",size = 1) +
  scale_y_continuous(limits = c(5,30), breaks = seq(5,30,5))

# +
#   ggtitle ("Nhiệt độ trung bình hàng ngày") +
#   xlab("Ngày tháng") +  ylab ("Nhiệt độ trung bình ( ºC )")


ggplot(weather,aes(x = date,y = ave.temp)) +
  geom_point(aes(colour = ave.temp)) +
  scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = 16) +
  geom_smooth(color = "red",size = 1) +
  scale_y_continuous(limits = c(5,30), breaks = seq(5,30,5))

#
# +
#   ggtitle ("Nhiệt độ trung bình hàng ngày") +
#   xlab("Ngày tháng") +  ylab ("Nhiệt độ trung bình ( ºC )")



### Phân tích nhiệt độ theo mùa

ggplot(weather,aes(x = ave.temp, colour = season)) +
  geom_density() +
  scale_x_continuous(limits = c(5,30), breaks = seq(5,30,5))+
  xlab("Nhiệt độ trung bình ( ºC )") +  ylab ("Xác xuất/ Mật độ")+
  labs(title = "Phân bổ nhiệt độ theo mùa") +
  scale_color_manual(name = "Mùa",labels = c("Xuân", "Hạ", "Thu", "Đông"), values = c("red", "blue","green","gray"))

#### Phân tích nhiệt độ theo tháng - sử dụng biểu đồ đàn vĩ cầm

#weather$month = factor(weather$month,labels = c("Tháng 1","Tháng 2","Tháng 3","Tháng 4",                    "Tháng 5","Tháng 6","Tháng 7","Tháng 8","Tháng 9","Tháng 10","Tháng 11","Tháng 12"))


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

temperatures <- melt(temperatures,id.vars = "day.count",
                     variable.name = "l.h.temp", value.name = "hour")

temperatures$hour <- factor(temperatures$hour,levels=0:23) # cần thiết để sắp xếp trật tự của mức độ

ggplot(temperatures) +
  geom_bar(aes(x = hour, fill = l.h.temp)) +
  scale_fill_discrete(name= "", labels = c("Cao","Thấp")) +
  scale_y_continuous(limits = c(0,100)) +
  ggtitle ("Nhiệt độ cao thấp theo giờ trong ngày") +
  xlab("Giờ") +  ylab ("Tần suất")


# nhìn vào biểu đồ này, chúng ta thấy rõ ràng rằng giải nhiệt độ thấp trong ngày thường là vào đầu giờ sáng và nhiệt độ tăng cao vào đầu giờ chiều. Nhưng chúng ta có thấy có những dấu hiệu của số liệu ngoại biên không? có đó, có một vài ngày mà nhiệt độ cao lại suất hiện vào ban đêm, và có một số ngày khác thì nhiệt độ thấp nhất lại vào ban ngày. Vậy nhưng số liệu này có phải có liên hệ nào với mưa không? vì mưa sẽ làm cho nhiệt độ giảm xuống? Đó là một loại câu hỏi mà chúng ta cần phải đặt ra để khám phá. Ở phần sau chúng ta sẽ dùng thủ thuật EDA (trực quan hóa) để nhận dạng được các biến dự báo có liên quan đến việc suất hiện mưa. Và tiếp theo ở các phần khác, chúng ta sẽ dùng các thuật toán trong học máy, để tìm ra biến liên quan đến mưa chính tối ưu nhất. Ngoài ra chúng ta còn xác định được tính đúng đắn của mô hình và xác định được mức độ ảnh hưởng của từng biến tới gói dữ liệu.


#--------------------------------------------------------------
#--------------------------------------------------------------
#### PHẦN 4: Sử dụng EDA và ggplot2
#--------------------------------------------------------------
#--------------------------------------------------------------

#Ở phần này chúng ta tiếp tục công việc trực quan hóa với đồ thị để hiểu hơn về dữ liệu. Nhưng bây giờ chúng ta sẽ đặt ra mục tiêu để giải quyết các câu hỏi sau

#Câu hỏi 1: Trong dữ liệu của chúng ta, chúng ta có các biến đủ tốt cho việc dự đoán là xác xuất/khả năng xảy ra mưa vào một ngày nào đó? các biến số thể hiện lượng mưa được gọi là biến phụ thuộc, hay còn gọi là biến đầu ra, hay biển phản hồi, và toàn bộ các biến còn lại trong bảng dữ liệu được coi là các biến độc lập với nhau, các biến này thường được gọi là biến tiên lượng hay biến giải thích.

#Thủ thuật EDA (như là công việc trực quan hóa) được sử dụng để biết sâu hơn về dữ liệu. Dưới đây là một số bước thường được dùng.

# Bước 1: Phân tích các biến phụ thuộc (dạng biến liên tục) - trước hết là vẽ biểu đồ tần suất của biến này. Và khi có thể, vẽ đồ thị của biến theo thời gian để phát hiện ra xu hướng. Biến liên tục này có thể được đổi thành một dạng nhị thức không (ví dụ như có mưa hay không vào một ngày cụ thể), hay chuyển thành dạng đa phương (ví dụ như không mưa, mưa nhỏ, mưa vừa, mưa to, hay mưa nặng hạt).

# Bước 2: Đi tìm các mối liên hệ giữa biến phụ thuộc và các biến độc lập dạng liên tục - các biến này có mối tương quan mạnh không? mối tương quan là tuyến tính hay là phi tuyến tính.

# Bước 3: Làm lại các bước như trên nhưng cố gắng giám sát/điều khiển các biến khác (Vẽ nhiều độ thị trên cùng một bảng là một cách thức rất hữu ích cho việc này) để đánh giá tính trùng hợp và sự sửa đổi có ảnh hưởng. Mỗi liên hệ giữa hai biến liên tục có được giữ cho từng mức độ khác nhau đối với biến thứ 3 (ví dụ như nếu có tương quan thuận giữa biến lượng mưa và tốc độ gió giật, mối liên hệ/tương quan này có được bền vững không kể đến các mùa trong năm, hay là mối quan hệ này chỉ mạnh vào mùa đông).

# Bước 4: Tìm hiểu các mối liên hệ giữa biến phụ thuộc và các biến phân loại (biến nhân tố) - Các giá trị trung bình và trung bị của biến phụ thuộc có thay đổi gì đối với từng cấp độ của biến phân loại hay không? Thế còn về giá trị biên/ngoại lại thì thế nào? chúng phân bổ đều như nhau trong từng cấp độ của biến phân loại, hay là chúng chỉ thấy ở một vài cấp độ thôi.



# Và giờ chúng ta bắt đầu phân tích nhé.

# Tìm hiểu về biến phụ buộ - Lượng mưa
#Sys.setlocale("LC_TIME", "Vietnam")
#Sys.setlocale("LC_TIME", "English")

# độ thị mô tả giữa lượng mưa và thời gian, đường biểu diễn
ggplot(weather, aes(date,rain)) +
  geom_point(aes(colour = rain)) +
  geom_smooth(colour = "blue", size = 1) +
  scale_colour_gradient2(low = "green", mid = "orange",high = "red", midpoint = 20) +
  scale_y_continuous(breaks = seq(0,80,20)) +
  xlab("Ngày") +
  ylab("Lưu lượng mưa (mm)") +
  ggtitle("Lượng mưa")+
  labs(size = "Lưu lượng")


# Đồ thị tần suất Histogram về lưu lượng mưa

ggplot(weather,aes(rain)) +
  geom_histogram(binwidth = 1,colour = "blue", fill = "darkgrey") +
  scale_x_continuous(breaks = seq(0,80,5)) +
  scale_y_continuous(breaks = seq(0,225,25)) +
  xlab("Lưu lượng (mm)") +
  ylab ("Tần suất (ngày)") +
  ggtitle("Phân bổ lưu lượng mưa")

# ở biểu đồ về thời gian, chúng ta thấy rằng lượng mưa ngày biến đổi rất lớn trong năm. Có rất nhiều ngày nặng đan xen với các ngày mua, mà thường là mưa lớn, đặc biệt là vào mùa thu và mùa đông. Còn ở biểu đồ về tuần suất, biểu đồ này không những khẳng định những nhận định trên, mà còn chỉ ra rằng, phân bổ lưu lượng mưa là có độ lệch lớn sang bên phải. Như bảng số liệu dưới đây mô tả, nhận định này là đúng cho cả trường hợp loại bỏ đi các ngày không mưa.


summary(weather$rain) # độ lệch sang phải rất cao
summary(subset(weather, rain > 0)$rain) # độ lệch vẫn còn cao

#vẻ thử biểu đồ boxplot

ggplot(weather,aes(rain)) +
  geom_boxplot()

ggplot(weather, aes(x="", y=rain)) +
  geom_boxplot(fill="red", alpha=0.2)+
  xlab("")+
  ylab("Lượng mưa (mm)")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")

library(dplyr)

ggplot(weather %>% filter(rain >0), aes(x="", y=rain)) +
  geom_boxplot(fill="red", alpha=0.2)+
  xlab("")+
  ylab("Lượng mưa (mm)")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")


library(e1071) # gói này dùng để tính độ lệch

skewness(weather$rain)
skewness(subset(weather, rain >0)$rain)

# nhìn vào độ lệch này, chúng ta có thể nhận định rằng chúng ta có thể biến đổi biến phụ thuộc thành dạng nhị thức (có mưa hoặc không có mưa). Để ý rằng, chúng ta có thể coi các ngày có mưa khi mà lượng mưa phải đạt mức tối thiểu là 1mm, còn dưới mức đó thì coi như không có mưa.

# chúng ta tạo ra biến nhị thức (Yes, No), có hoặc không





# chúng ta tạo ra biến nhị thức (Yes, No), có hoặc không

nrow(subset(weather, rain == 0)) # số ngày không mưa

nrow(subset(weather, rain <1 & rain >0)) # số ngày mưa nhưng rất ít và có thể coi là ngày nắng

weather$rained <- ifelse(weather$rain >= 1, "Rain", "No Rain") # biến mới

table(rained = weather$rained) # ngày nắng và ngày mưa, tuyệt đối

prop.table(table(rained = weather$rained)) # tạo bảng tương đối

# thành phố này có vẻ mưa rất nhiều trong năm, với 40% số ngày trong năm là mưa.


# Xem xét sự liên kết giữa lượng mưa và mùa trong năm


# Ở biểu đồ thời gian ở trên, chúng ta có thể thấy rằng mùa trong năm có thể là biến giải thích tốt cho sự xuất hiện của mưa. Chúng ta sẽ bắt đầu đi tìm hiểu về mối liên kết này, cho cả 2 trường hợp là dùng biến lượng mưa là biến liên tục và là biến rời rạc.

# Biến liên tục


ggplot(weather, aes(season,rain)) +
  geom_jitter(aes(colour=rain), position = position_jitter(width = 0.2)) +
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = 30) +
  scale_y_continuous(breaks = seq(0,80,20)) +
  xlab("Mùa") +
  ylab ("Lượng mưa (mm)") +
  ggtitle("Lượng mưa hàng ngày theo mùa") +
  labs(size ="Lưu lượng")

# tính nhanh lượng mưa theo mùa
tapply(weather$rain,weather$season,summary)

# nhìn vào đồ thị và bảng này, chúng ta có thể thấy được là các giá trị ngoại biên thường vào mùa đông và mùa thu. Tuy nhiên, chúng ta có thể thấy là ở cả 2 mùa này, có rất nhiều ngày nắng, và vì thế giá trị trung bình quá gần nhau. Để chọn một mô hình phù hợp để tiên đoán lượng mưa thật sự (không phải là xác xuất xuất hiện mưa) dựa vào biến mùa thôi thì sẽ không thể cho ta kết quả tốt nhất được.


# Phân tích sự xuất hiện mưa theo mùa dùng với biến nhị thức


# Đồ thị thanh, ngày nắng và ngày mưa theo mùa
ggplot(weather,aes(season)) +
  geom_bar(aes(fill = rained), position = "fill") +
  geom_hline(aes(yintercept = prop.table(table(weather$rained))["No"]),
             colour = "blue",linetype = "dashed", size = 1) +
  annotate("text", x = 1, y = 0.65, label = "yr. w/o = 0.60", colour = "blue") +
  xlab("Mùa") +
  ylab ("Tỷ lệ") +
  ggtitle("Tỷ lệ các ngày nắng và mưa theo mùa")

round(prop.table(table(season = weather$season, rained= weather$rained),1),2)

#
# chúng ta nhận thấy rằng, khi chúng ta sử dụng xác xuất mưa vào một ngày cụ thể nào đó, thì biến mùa có thể đóng góp nhiều. Đặc biệt là vào mùa đông, mùa mà số ngày mưa (63%) là lớn hơn so với con số trung bình là 40%.


# Nhìn vào mối tương quan giữa biến mưa và các biến số khác
# Bây giờ chúng ta sẽ tính toán mối tương quan tuyến tính (Pearson) giữa biến liên tục đầu ra (lượng mưa) và toàn bộ các biến số còn lại bao gồm cả 2 loại biến, biến số (nhiệt độ và tốc độ gió) và các biến thông số (factor), đôi khi các biến thống số nếu thể hiện ở dạng số vẫn có thể được dùng (ví dụ như giờ trong ngày mà có mưa).

# Chúng ta cũng lưu ý răng, ở bước này chúng ta vẫn chưa lập mô hình gì cả, chúng ta chỉ cố gắng hiểu thêm về dữ liệu mà thôi, và do đó chúng ta không quan tấm đến mỗi tương quan là có ý nghĩa (giá trị p hay khoảng tin cậy) và mỗi quan hệ giữa chúng là tuyến tính.



weather.num <- weather[c("rain","l.temp","h.temp","ave.temp","ave.wind","gust.wind", "l.temp.hour","h.temp.hour","gust.wind.hour")]


# chuyển đổi các biến thông số sau thành biến số

weather.num$l.temp.hour <- as.numeric(weather.num$l.temp.hour)
weather.num$h.temp.hour <- as.numeric(weather.num$h.temp.hour)
weather.num$gust.wind.hour <- as.numeric(weather.num$gust.wind.hour)


str(weather.num)

round(cor(weather.num),2)




# dùng hàm cor() cho dữ liệu mới này và tạo ra ma trận tương quan
round(cor(weather.num),2)[1,] # trọn ra dòng đầu tiên của ma trận --> lương quan giữa lượng mưa và các biến còn lại


# nhìn vào các chỉ số trong ma trận này, chúng ta có thể thấy là khả năng có mối tương quan cao giữa lượng mưa và các biến liên quan đến tốc độ gió (tương quan tương đối với giá trị là 0.61), điều này có nghĩa là nếu tốc độ gió càng cao thì khả năng cao có sự liên hệ với lượng mưa. Khi nhìn vào ma trận này, chúng ta phải lưu tâm rắng, mối tương quan không có nghĩa là sự nhân quả, do đó khi chúng ta thấy là có sự tương quan giữa lượng mưa và gió, điều đó không luôn luôn có nghĩa là bản thân gió tạo ra mưa, mà là có thể là ngược lại hoặc có trường hợp là cả 2 biến này phụ thuộc vào một biến thứ 3 mà chúng ta chưa xem xét tới.


# Trong ma trận này, chúng ta nhận thấy là có lượng mưa và nhiệt độ có mối quan hệ âm (đặc biệt là với biến nhiệt độ cao trong ngày), mặc dù là mối tương quan này không lớn như đối với biến gió, nhưng chúng ta cũng không nên bỏ qua. Có thể thấy rằng lượng mưa càng cao thì khả năng tương qua với giải nhiệt độ cao ở vùng thấp. Chúng ta suy nghĩ điều này trong vài phút xem sao: Chúng ta thấy rằng vào mùa đồng, mưa xuất hiện nhiều nhất. Ở phần trước chúng ta cũng thấy rằng nhiệt độ giảm thấp vào mùa đông. Đây là một ví dụ về tính tương tác với một biến thứ 3. Khả năng là là không phải nhiệt độ thấp thì phần nào gây ra mưa, mà sự tích đọng và giải nhiệt độ thấp là 2 biến có xu hướng xuất hiện trong khoảng thời gian này trong năm.


# Lý do nay có khả năng tác động đến các biến khác, chúng ta hay tìm hiểu thêm một chút nữa bằng cách tính toán toàn bộ mối tương quan theo mua và kiểm tra nếu các giá trị tương quan có tương tự không? Nếu giá trị tương quan giữa mưa và các biến còn lại là không giống nhau giữa các mùa, thì chúng ta có thể có bằng chứng về sự tác động này.


# chia khung dữ liệu thành 4 phần, mỗi phần cho một mùa
weather.num.season <- split(weather.num,weather$season)


class(weather.num.season) # kiểm ra dạng của dữ liệu này --> là dạng danh sách

length(weather.num.season) # kiểm tra trong danh sách này có bao nhiêu cấu phần

summary(weather.num.season) # kiểm tra xem từng cầu phần của danh sách là thuộc định dạng nào


attributes(weather.num.season) # kiểm tra tên cho từng cấu phần của danh sách

# lưu ý trong R hàm apply là một trong những hàm rất mạnh của ngôn ngữ R, nhưng cũng là một hàm rất khó học.
# hàm sapply(x,z) ứng với phương trình z cho mỗi thành phần của biến x
#hàm này trước tiến đi tới các thành phần của một danh sách và tín toàn ma trận tương qua (cho tất cả)
# cho mỗi mùa, chỉ tra lại giá trị tương quan giữa lượng mưa và các biến còn lại
sapply(weather.num.season, function (x) round(cor(x)["rain",],2))

# dưới đây là một cách khác mà có thể là dễ hơn khi ta sử dụng hàm lapply

lapply(weather.num.season, function (x) round(cor(x)["rain",],2))


# chúng ta thấy gì từ bảng kết quả về mối tương quan? có thể thấy là mối tương quan giữa lượng mưa và gió thay đôi theo mùa. Mối tương quan giữa mưa và giải nhiệt độ cao không khẳng định được những gì mà chúng ta đã giả định từ trước. Trên thực tế, mối tương quan này còn mạnh hơn vào mùa xuân hơn là vào mùa đông. Như vậy, chúng ta có thể tìm hiểu sâu hơn nếu chúng ta muốn trong khi lưu ý rằng ở đây chúng ta chỉ phân tích khả năng tương quan liên quan đến mùa, nhưng trong thực tế thì có thể nhiều hơn là biến mùa. Đến đây, chúng ta có thể đủ để kết luận rằng mối tương quan là không ổn định trong năm, và thực tế là có lúc không liên quan gì vào mùa thu. Cuối cùng, mối tương quan giữa lượng mưa và thời điểm xảy ra (cả cho biến nhiệt độ cao và biến tốc độ gió) là khá yếu nhưng khá ổn định. Chúng có thể có một phần tác động tới việc tiên đoán, đặt biệt là khi sử dụng mô hình hồi qui tuyến tính.

# Giờ chúng ta biết rằng tốc độ gió có mối tương quan lớn tới mưa, và điều này đúng cho từng mùa. Giờ chúng ta biểu diễn trên đồ thì các biến này, bởi vì chúng ta muốn tìm hiểu thêm về nhưng thứ mà chúng ta có thể chưa khám phá ra. Ví dụ như --> hình dạng của mối quan hệ? là tuyến tính, hay theo tuyến tính cấp bậc, hay là teo tuyến tính đường cong? hãy cùng vẽ đồ thị nào, và đặc biệt là sử dụng facet (đồ thị gộp, hay tập hợp đồ thị).


# Nhìn sâu hơn về mối tương quan giữa gió và mưa - thủ thuật đồ thị tập hợp

# Ý tưởng của đồ thị tập hợp (hay còn gọi là đồ thị lưới mắt cáo) là đơn giản nhưng khá là quan trọng. Một đồ thị, bất kể đồ thị đó thể hiện ở dạng nào, có thể được phân chia ra thành nhiều bảng phụ thuộ vào giá trị của biến điều kiện. Giá trị này hoặc là một của nhiều cấp bậc của biến phân loại hay là một số của một đoạn của một biến số. Việc thể hiện này giúp chúng ta thấy được các hình dạng/biểu hiện thống nhất xuyên xuất các bảng đồ thị. Trong ggplot2, có 2 hàm để tạo đồ thị tổng hợp - facet_wrap() và facet_grid() - hai hàm này được dùng khi chúng ta có một hay 2 biến điều kiện.

# giờ chúng ta tìm hiểu sự tuyến tính liên quan đến tương quan giữa mưa và tốc độ gió lớn nhất ghi được trong ngày, phụ thuộc vào biến mùa trong năm.

ggplot(weather,aes(gust.wind,rain)) +
  geom_point(colour = "firebrick") +
  geom_smooth(size = 0.75, se = F) +
  facet_wrap(~season) +
  xlab("Tốc độ gió tối đa (km/h)") +
  ylab ("Lượng mưa (mm)") +
  ggtitle("Lượng mưa vs tốc độ gió tối đa, theo mùa")


# một lần nữa chúng ta thấy rằng nếu nhìn vào các đồ thị này, chúng ta sẽ thấy mối tương quan dương giữa 2 biến mưa và tốc độ gió. Nhưng chúng ta cũng để ý thấy rằng mối tương quan này là không tuyến tính. Trên thực tế, nếu chúng ta tổng quát hóa, chúng ta có thể nói rằng nó không có tương quan gì cả khi mà tốc độ tối đa ở dưới ngưỡng 25km/h. Khi mà tốc độ gió lớn hơn ngưỡng này, thì có vẻ là có mối tương quan tuyến tính vào mùa thu và mùa đông, nhưng là không tuyến tính vào mùa xuân và mua hè. Nếu chúng ta muốn mô hình mối quan hệ này, chúng ta có thể sử dụng một mô hình phi tuyến tính (ví dụ như mô hình hồi quy cây thư mục) hoặc chúng ta có thể thử áp dụng mô hình tuyến tính bậc (linear spline), mà ở đó phương trình liên hệ giữa biến đầu ra và biến tiên đoán là khác nhau phụ thuộc vào giá trị của tốc độ gió.


# Sự xuất hiện của mưa - thể hiện biểu đồ tổng hợp phức tạp hơn để trực quan hóa mối liên hệ giữa các biến

# Ở phần này, chúng ta tiếp tục kiểm tra mối tương quan và khả năng tiên đoán mưa, dựa vào tốc độ gió tối da khi chúng ta sử dụng biến nhị thức đã tạo ra từ trước cho biến lượng mưa. Chúng ta sẽ vẽ và điều khiển mùa và cũng thêm vào các biến về nhiệt độ (vì chúng ta thấy là các biến này có ảnh hưởng). Chúng ta sẽ dùng thủ thuật tạo đồ thị tổng hợp cùng một lúc cho 2 biến này. Nhưng vì biến nhiệt độ là biến liên tục, chúng ta cần chuyển đổi nó thành biến phân loại. Một chiến lược phổ biến là chia giá trị của các biến liên tục thành 4 khoảng (nhóm) với số lượng quan sát như nhau. Việc này là khá đơn giản khi sử dụng R, chúng ta chỉ cần sử dụng hàm cut() và hàm quantile().


# sử dụng hàm quantile cho chúng ta thấy được 4 khoảng đều nhau
quantile(weather$h.temp)

# giờ chúng chỉ cần dùng hàm cut giựa trên quantile
weather$h.temp.quant <- cut(weather$h.temp, breaks = quantile(weather$h.temp),
                            labels = c("Cold","Cool","Warm","Hot"),include.lowest = T)

# kết quả
table(weather$h.temp.quant)

# Sự xuất hiện của mưa, theo mùa và theo nhiệt độ cao trong ngày
ggplot(weather,aes(rained,gust.wind)) +
  geom_boxplot(aes(colour=rained)) +
  facet_grid(h.temp.quant~season) +
  xlab("Sự xuất hiện mưa") +
  ylab ("Tốc độ gió tối đa (km/h)") +
  ggtitle("Mưa theo mùa và theo nhiệt độ")

#EDA --> Exploratory Data Analysis


# Đồ thị này chỉ ra rằng: trung vị của tốc độ gió thì luôn lớn hơn khi có mưa, và điều này không bị ảnh hưởng bởi phạm vị/khoảng nhiệt độ, thậm chí ngày cả khi chúng ta điều khiển sự thay đổi về nhiệt độ theo mùa trong năm.

# Giờ chúng ta có thể tự tin một phần là chúng ta đã hiểu tương đối/khá rõ về dự liệu này. Chúng ta biết các biến nào có ảnh hưởng nhất, và biến nào không có tác động nhiều. Khi chúng ta nghĩ về việc tiên lượng mưa cho tương lai/hay  ngày cụ thể hay tính xác xuất xảy ra mưa. Lưu ý rằng, sẽ rất là mất công và dường như là không khả thi nếu chúng ta lặp đi lặp lại các bước trên cho từng biến trong khuôn khổ buổi học này.
#--------------------------------------------------------------
#--------------------------------------------------------------
#### PHẦN 5: Lập Mô Hình - Tiên đoán lượng mưa
#--------------------------------------------------------------


# Trong phần này, chúng ta sẽ xây dựng một vài mô hình tiên lượng và đánh giá tính chính xác của các mô hình.


# Với hai trường hợp là biến liên tục và biến nhị thức, chúng ta cố gắng để xây dựng mô hình phù hợp như sau

## Mô hình mẫu (baseline): thông thường có nghĩa là chúng ta giả đinh là không có biến tiên lượng (i.e. các biến không phụ thuộc). Như vậy chúng ta dự đoán bằng cách chọn lọc (educated guess) mà không phải là ngẫu nhiên dựa trên giá trị của chính biến đó. Mô hình này là khá quan trọng bởi vì nó sẽ cho phép chúng ta tính được thế nào là tốt, là xấu. Ví dụ như, hãy tưởng tượng một mô hình cho ta độ chính xác tới 97% - điều này thực sự là good và có giá trị để tiến hành? không phải vậy, thực sự là nó phụ thuộc vào từng tình huống. Nếu mà mô hình chuẩn có tính chính xác là 60%, thì mô hình kia có thể là một mô hình tốt, nhưng nếu mô hình chuẩn là 96.7%, thì mô hình kia dường như không cho ta biết thêm gì cả và do đó việc chọn lựa mô hình là phụ thuộc vào việc chúng ta định lượng về giá trị để chúng ta đi đánh giá sự khác nhau 0.3% này. Trong thực tế, là có rất nhiều mô hình kể cả đơn giản đến phức tạp, không tốt hơn mô hình chuẩn. Một ví dụ điển hình là chúng ta cố gắng tiên đoán về chỉ số chứng khoán đi lên hay đi xuống vào ngày hôm sau dựa vào giá trị của chỉ số này trong N ngày đã qua, chúng ta có thể thêm vào các biến khác chỉ số biến thiên, loại hàng hóa, v.v.. Thậm chí nếu chúng ta xây dựng một mô hình mạng neural với rất nhiều phân tử neurons, chúng ta chưa chắc là đã thay đổi được gì, đơn giản nghĩ rằng chứng khoán ngày mai sẽ không có thay đổi nhiều so với chỉ số của nó vào ngày hôm nay. Trong một số trường hợp, là giá trị tiên đoán từ các mô hình phức tạp còn tệ hơn và không chính xác --> bài toán chứng khoán là một bài toán rất khó.

# Một mô hình từ thống kê suy luận: - thường là mô hình tuyến tính. Trong trường hợp biến đầu ra là liên tục, chúng ta sẽ xây dựng hồi qui tuyến đính đa biến. Với biến nhị thức, chúng ta sẽ xây dựng mô hình hồi qui nhị thức

# Hai mô hình trong học máy --> Là mô hình cây quyết định (regression tree cho biến liên tục và cây phân loại cho biến nhị thức). Đây là hai mô hình phổ biến được sử dụng và chúng cho ra các kết quả khá tốt và mang tính diễn giải cao. Sau đó chúng ta sẽ xây dựng mô hình rừng ngẫu nhiên (random forests), là một phương pháp rất phổ biến.


### Gói dữ liệu học và gói dữ liệu thử nghiệm

# Một nguyên tắc vàng trong học máy và lập mô hình là: mô hình được xây dựng trên gói dữ liệu học và được đánh giá bằng gói dữ liệu thử nghiệm. Lý do cho việc này là để tránh tính trạng mô hình được lập ra có xu hướng thiện vị hay quá mức (overfitting): Sự chính xác của mô hình có thể được tăng lên một cách có chủ định tới một điểm mà ở đó chúng có thể họ được toàn bộ những chi tiết của dữ liệu được sử dụng để tạo ra mô hình đó. Thật không may là chúng mất đi khả năng để tổng quát hóa. Vì thế chúng ta cần dữ liệu chưa được kiểm chứng (dữ liệu thử nghiệm). Nếu mô hình có thiên hướng thể hiện quá mức cho dữ liệu học, thì khi sử dụng nó cho gói dữ liệu thử nghiệm, kết quả cho được sẽ không tốt. Trong thực tế, các mô hình đơn gian thường đánh bại những mô hình phức tạp, bởi vì chúng có thể tổng quá hóa tốt hơn. Trong phần này, chúng ta sẽ chia ra tỉ lệ 70:30 cho 2 gói dữ liệu trên. Cho mục đích tái lập lại dữ liệu, chúng ta cần chỉ định các hạt trong hàm tạo mẫu ngẫu nhiên (tức là cứ mỗi lần chúng ta chạy mã, chúng ta có được chính xác các giá trị như đã làm ở lần trước).


#--------------------------------------------------------------

set.seed(123) # dùng để tạo lập khả năng tái tạo mẫu, số 123 không có một ý nghĩa cụ thể nào

# lấy ngẫu nhiên 70% số lượng quan sát trên 365 mẫu

index <- sample(1:nrow(weather),size = 0.7*nrow(weather))

# gán vào dữ liệu học
train <- weather[index,]
# gán cho dữ liệu thử nghiệm
test <- weather [-index,]

nrow(train)
nrow(test)

# tạo ra một khung dữ liệu với chỉ số học và thử nghiệm
group <- rep(NA,365)
group <- ifelse(seq(1,365) %in% index,"Train","Test")

df <- data.frame(date=weather$date,rain=weather$rain,group)

ggplot(df,aes(x = date,y = rain, color = group)) +
  geom_point() +
  scale_color_discrete(name="") +
  theme(legend.position="top")



# ghi dữ liệu ra file csv

write.csv(train,"weather02-train.csv", row.names = FALSE)
write.csv(test,"weather02-test.csv", row.names = FALSE)


library(rattle)
rattle()


# Đánh giá số liệu

## Với biến đầu ra là liên tục, hệ sai số chính chúng ta dùng để đánh giá mô hình là RMSE (căn của sai số toàn phương trung bình). Đo lượng sai số này cho biết nhiều trọng lượng được gàn vào nhiều phần dư hơn là chỉ một (phần dư là phần khác biệt giữ số tiên đoán và số quan sát). Điều này có nghĩa là chúng ta nhận thấy rằng sự thiếu vắng tiên đoán cho lượng mưa ở mức 20mm, trong một ngày nào đó, thì chỉ có ảnh hưởng 2 lần so với sự thiếu vắng ứng với lượng mưa 10mm.


## Chúng ta sẽ sử dụng MAE (sai số trung bình tuyệt đối) như là hệ sai số thứ 2 để đánh giá.

## https://gmd.copernicus.org/articles/7/1247/2014/gmd-7-1247-2014.pdf

# Mô hình gốc
#Giả thuyết là không có các biến tiến đoán nào, tất cả chúng ta có thể làm là biến phụ thuộc (lượng mưa). Cái gì chúng ta có thể tiên đoán tốt nhất nếu chúng ta phải tiên đoán lượng mưa, vào một ngày cụ thể, dựa vào dữ liệu thử nghiệm? Chúng ta có thể nói rằng đó là giá trị trung bình của lượng mưa, trong dữ liệu học, là giá trị tốt nhất chúng ta có thể đưa ra.

## Chúng ta sẽ sử dụng MAE (sai số trung bình tuyệt đối) như là hệ sai số thứ 2 để đánh giá.

## https://gmd.copernicus.org/articles/7/1247/2014/gmd-7-1247-2014.pdf

# mô hình gốc -- tiên đoán giá trị trung bình trong dữ liệu học
best.guess <- mean(train$rain)
best.guess
# Đánh giá trị số RMSE và MAE cho dữ liệu thử nghiệm
RMSE.baseline <- sqrt(mean((best.guess-test$rain)^2))

RMSE.baseline


MAE.baseline <- mean(abs(best.guess-test$rain))
MAE.baseline


# Hồi qui tuyến đính đa biến


# Hồi qui tuyến đính đa biến

# Đây là mô hình có lẽ là phổ biến nhất trong thống kê. Biến phụ thuộc được giả định là một hàm tuyến tính của các biến tiên lượng, các biến này độc lập với nhau. Mỗi một biến này sẽ có trọng lượng (weight) mà được coi là có ý nghĩa thống kê trong mô hình cuối cùng. Kết quả tường là có sự tương tác cao.

## Mô hình tuyến tính không yêu cầu các biến phải theo phân phối Gaussian (chỉ sai số/phần dư là theo phân bố chuẩn). Mô hình này yêu cầu mối quan hệ tuyến tính giữa biến đầu ra và biến đầu vào. Như chúng ta thấy ở phần trước, phân bổ lượng mưa là bị lệnh sang phải, và mối liên hệ của nó với các biến khác là không tuyến tính. Với lý do là là tuyến tính, và cũng bởi vì chúng ta sửa vấn đề về phần dư với phương sai không cố định cho toàn bộ các biến (thường gọi là phương sai thay đổi), chúng ta sẽ dùng hàm log cho các biến phụ thuộc. Trong dữ liệu có các ngày không mưa ( có giá trị 0), chúng ta không thể dùng ln(x) mà thay vào đó chúng ta dùng ln(x+1). Tại sao chúng ta phải dùng hàm logarit cho trường hợp này? đơn giản làm bởi vì hệ số hồi qui có thể được diễn giải, mặc dụ là ở khía cạnh khác khi so sánh nó trực tiếp với hồi qui tuyến tính. Mô hình này chúng ta thường gọi là tuyến tính logarit

## Ở phần dưới đây là mô hình cuối cùng. Để có được mô hình này, chúng ta có thể bắt đầu với tất cả các biến là các biến tiềm năng để dự đoán và sau đó lược bớt biến ra khỏi mô hình từng biến một, trước là với các biến mà k có ý nghĩa nhiều về mặt thống kê (p <0.05). Chúng ta làm công việc này bởi vì tính đa cộng tuyến giữa các biến. Một vài biến trong dữ liệu thì có tương quan lớn (ví dụ như nhiệt độ thấp, trung bình và cao ở một vài ngày), điều đó có nghĩa là đôi khi khi chúng ta lược đi một biến k có ý nghĩa nào ra khỏi mô hình, một biến khác mà trước đó chúng ta thấy là không có ý nghĩa thì giờ trở thành có ý nghĩa. Quá trình này dừng lại khi toàn bộ các biến trong mô hình là có ý nghĩa (với trường hợp là các biến nhân tố, ở đây chúng ta xem xét là có ít nhất một cấp độ liên quan là có ý nghĩa).

## biến phụ thuộc của chúng ta có rất nhiều giá trị 0 và có thể chỉ có giá trị dương. Nếu chúng ta là nhà thống kê học, chúng ta có lẽ sẽ thích chọn ra một mô hình nào đó mà có thể phù hợp với dữ liệu theo dạng đếm, chẳng hạn như mô hình nhị thức âm, mô hình zero-inflated hay mô hình hurdle. Có một vài thư viện trong R để làm công việc này. Ở đây, chúng ta chỉ sử dụng mô hình tuyến tình chuẩn để đơn giản hóa bài toán.


# sử dụng hàm lm để làm hồi qui
lin.reg <- lm(log(rain+1) ~ season +  h.temp + ave.temp + ave.wind + gust.wind +
                dir.wind + as.numeric(gust.wind.hour), data = train)

summary(lin.reg)

# Cái gì là ảnh hưởng theo cấp số nhân của biến gió
exp(lin.reg$coefficients["gust.wind"])

# Sử dụng mô hình để kiểm tra với dữ liệu thử nghiệm
# đừng quên là phải dùng hàm exp để cho giá trị log quay trở lại ban đầu
test.pred.lin <- exp(predict(lin.reg,test))-1


# đánh giá tính chính xác

RMSE.lin.reg <- sqrt(mean((test.pred.lin-test$rain)^2))
RMSE.lin.reg

MAE.lin.reg <- mean(abs(test.pred.lin-test$rain))
MAE.lin.reg




## Dưới đây là một số kết luận về mô hình mà chúng ta vừa tạo ra

### Giá trị R-squared là 0.66, điều đó có nghĩa là 66% của giá trị phương sai của biến phụ thuộc có thể được giải thích bởi tập hợp các biến đầu vào; bên cạnh đó, giá trị của R-squared điều chỉnh là không xa với giá trị R-squared, điều này có nghĩa là giá trị gốc R-squared đã không bị tăng lên một cách nhân tạo bởi việc cho thêm các biến vào mô hình. Để ý rằng giá trị R-squared có thể chỉ tăng hoặc giữ nguyên khi tăng thêm biến, trong khi đó giá trị R-squared điều chỉnh có thể giảm nếu các biến được thêm vào và các biến này không thực sự giúp cho mô hình tốt hơn.

## toàn bộ các biến là đều có giá trị thống kê ( với giá trị p <0.05), như là đã kì vọng khi xây dựng mô hình, và biến đầu vào có ý nghĩa thống kê nhiều nhất là biến gió (với giá trị p rất nhỏ = 2.09e-09). Thế mạnh của việc sử dụng logrit là ở chỗ, nếu giá trị của hệ số hồi qui là nhỏ (ví dụ trong khoảng -0.1 đến 0.1), tăng một đơn vị của biến độc lập sẽ làm tăng ấp xỉ giá trị đó nhân với 100% trong biến phụ thuộc. Rõ ràng rằng, hệ số hồi qui của gió là 0.052266, nó có nghĩa là tăng một đơn vị tốc đố gió (tăng lên 1km/h), thì sẽ làm tăng giá trị tiên đoán của lượng mưa lên khoảng 5.23%. Giải thích tương tự cho các biến còn lại.
## Cả 2 giá trị RMSE và MAE đều giảm đáng kể khi so sánh với giá trị đó ở mô hình gốc. Điều đó có nghĩa là mô hình tuyến tính này, loại bỏ đi các yếu tố tuyến tính và thực tế là nó tiên đoán giá trị âm của mưa trong một vài ngày, là tốt hơn so với dự đoán của chúng ta.


# Chúng ta sẽ quay lại vấn đề này, khi chúng ta so sánh các giá trị thực và giá trị do mô hình đưa ra để thấy được là mô hình này có một đặc điểm thú vị. Mô hình tiên đoán khá hợp lý lượng mưa trong khoảng từ 0-25 mm, nhưng khả năng tiên đoán giảm dần với lượng mưa trong khoảng từ 25 đến 75 mm.

## Phương pháp cây quyết định

# Một cây quyết định (decision tree), hay còn được gọi là cây hồi qui cho biến liên tục, là một thuật toán rất phổ biến nhưng rất đơn giản trong lĩnh  vực học máy. Mô hình này có một vài điểm tốt khá thú vị sau: (1) mô hình không giả định về mối quan hệ giữa biến đầu ra và đầu vào (điều đó có nghĩa là nó cho phép các liên hệ cả về tuyến tính và phi tuyến tính); (2) khả năng có thể diễn giải của cây quyết định có thể là không được cao cho lắm - phần cuối của một quá trình, một tập hợp các qui tắc, bằng ngôn ngữ tự nhiên, tạo ra mối tương quan giữa đầu ra với đầu vào, có thể dễ dàng được tách ra từ các nhánh của cây. Hơn nữa, một cây quyết định là điều cở bản của một phương pháp có sức mạnh rằng chúng ta sẽ có thể dùng, đó là phương pháp rừng ngẫu nhiên.


# Vậy chúng ta trồng cây thế nào? đơn giản thôi, chúng ta bắt đầu từ nút ban đầu là gốc, là nút chứa tất cả dữ liệu học, và sau đó chúng ta tách ra thành 2 nhánh với 2 nút mới dựa trên biến quan trọng nhất (biến mà chia ra tốt hơn đầu ra cho 2 nhánh/nhóm). Tương ứng với mỗi nút, chúng ta tiếp tục chia thành 2 nhánh mới (luôn luôn dựa vào biến mà sẽ tách ra tốt hơn), chúng ta làm như vậy cho đến khi bắt gặp tiêu chí dừng lại (ví dụ là trong R, một trong những mặc định là nút đó phải chứa ít nhất 20 quan sát, nếu không thì sự chia nhánh mới sẽ không diễn ra). Điểm yếu của phương pháp này là chúng thường đem đến sự phù hợp quá mức; trước khi dạt đến một tiêu chí dừng, thuật toán tiếp tục chia nhánh liên tục, học toàn bộ các chi tiết của dữ liệu học. Các cây lớn thường được xây dựng khi mà thể hiện tốt với dữ liệu học, nhưng không thể tổng quan hóa tốt và vì thế mà có thể không thể hiện được tốt ở dữ liệu thử nghiệm.

## Vậy, chúng ta phải làm gì khi biết điều trên? đây là điểm mà phương pháp đánh giá chéo trở lên quan trọng (chỉ cần hiểu là sự khác biệt giữa dữ liệu học và dữ liệu thủ nghiệm). Khi chúng ta trồng một cây trong R (hay các ngôn ngữ khác), chương trình sẽ tự động thực hiện 10 vòng đánh giá chéo. Điều đó có nghĩa là có 10 vòng lặp được thực hiện, và mỗi vòng đó 90% số lượng dữ liệu học được sử dụng để xây mô hình, và 10% còn lại, mặc dù nó thuộc về dữ liệu học, được sử dụng chỉ để tiên đoán và đánh giá kết quả. Nói theo cách khác, là 10% đó là dữ liệu thử nghiệm tạm thời. Bây giờ chúng ta đã rõ, sau 10 vòng đó, 10 mảng khách nhau của dữ liệu sẽ được dùng để thử mô hình, điều đó có nghĩa là mỗi 1 quan sát được sử dụng, ở một thời điểm nào đó, được không những chỉ dùng để học mà còn được dùng để thử nghiệm. Trong khi chúng ta thử nghiệm cùng lúc với lúc chúng ta trồng cây, chúng ta thu được một giá trị đo đạc sai số, giá trị này được dùng để tìm số tối ưu cho việc tách nhánh. Sau đó, chúng ta tiến hành tỉa cành ở cây gốc, và tiếp tục tách nhánh với con số tối ưu đó. Tổng hợp lại, khi chúng ta trồng cây, đây là những bước chúng ta làm

## Trồng một cây đầy đủ, thường gọi là các cài đặt mặc định
## Kiểm tra lỗi đánh giá chéo (x-error), và tìm con số tối ưu để tách nhánh. Ở trong R, bước này tương ứng với một giá trị của cp (thông số phức)
## Tỉa cây sử dụng giá trị cp ở trên.

## cài thường xuất hiện hoặc là cây tỉa sẽ tốt hơn trong dữ liệu thử nghiệm hoặc là thể hiện gần với cây gốc. Nếu trường hợp sau xuất hiện, thì việc tỉa cây là có ích bởi vì ít nhánh đồng nghĩa với việc ít qui tắc quyết định và sự giải thích sẽ mang tính cao hơn.



library(rpart) # thư viện rpart dùng để trồng cây
library(rpart.plot)
library(rattle) # thư viện rattle cũng dùng để trồng cây, nhưng đẹp hơn (sủ dụng phương trình fancyRpartPlot)

rattle()

# dùng rpart áp dụng cho một biến số --> cây hồi qui
rt <- rpart(rain ~ month + season + l.temp + h.temp + ave.temp + ave.wind +
              gust.wind + dir.wind + dir.wind.8 + as.numeric(h.temp.hour)+
              as.numeric(l.temp.hour)+ as.numeric(gust.wind.hour), data=train)

rt

rpart.plot(rt)
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


# như chúng ta thấy, chúng ta sđã có thể tỉa cành cho cây, từ 8 nhánh ban đầu, chúng ta giờ chỉ còn 2 nhánh trên biến gió. Để có được sự giản lược này mà không phải tốn quá nhiều công sức (giá trị RMSE và MAE là khá tương đồng ở cả 2 trường hợp). Ở cây cuối cùng, chỉ biến gió được xem xét là thích hợp dùng để tiên đoán lượng mưa tại một ngày nào đó, và tạo ra các qui tắc như sau.

## Nếu tốc độ gió tối đa vượt qua ngưỡng 44 km/h, thì có đến 15% là sẽ có một ngày mưa với lưu lượng 23 mm
## Nếu tốc độ gió tối đa trong khoảng


# độ chính xác của mô hình này có vẻ như không tốt so với các mô hình hồi qui tuyến tính phức tạp khác. Tiếp theo, thay vì trồng chỉ một cây, chúng ta sẽ trồng cả một rừng cây --> đó là một phương pháp rất mạnh và thường cho ra kết quả rất tốt.


### Phương pháp trồng rừng

# thay vì trồng một cây và tỉa cành của nó, giờ chúng ta trồng cả một rừng cây với số lượng từ vài trăm cho đến vài ngàn cây, và chúng ta sẽ giới thiệu qua về các nguồn liên quan đến tính ngẫu nhiên, để đảm bảo là các cây là không giống nhau. Điều gì ta sẽ có được khi trồng rừng, một thuật toán phổ biến mà bất cứ nhà khoa học dữ liệu nào trên thế giới điều biết. Trên thực tế, khi gặp phải các bấn đề mà ở đó trọng tậm không quá thiên về hiểu dữ liệu, mà liên quan đến tiên lượng, phương pháp trồng rừng thường được sử dụng ngay lập tức sau khi chúng ta chuẩn bị xong gói dữ liệu, chúng ta bỏ qua toàn bộ quá trình làm EDA. Dưới đây là tóm tắt các bước cơ bản của thuật trồng rừng.


## Để trồng mỗi một cây, một mẫu ngẫu nhiên của gói dữ liệu học, với kích thước bằng kích thước của chính gói dữ liệu đó, được lấy ra với sự thay thế. Điều đó có nghĩa rằng các quan sát có thể xuất hiện nhiều lần trong cùng một tập hợp dữ liệu, và các quan sát khác bị loại ra ngoài (tỷ lệ các quan sát bị loại ra tiến tới ngưỡng 36.8%, cho các mẫu có số lượng quan sát lớn). Nói một cách khác, mỗi một cay sẽ được trồng với chính phiên bản của nó ở dữ liệu học.

## Các biến mà được sử dụng để trồng mỗi cây là được lấy ngẫu nhiên. Kích thước của mẫu là 1/3 và căn của số lượng các biến độc lập, cho cả các bài toán về hồi qui và bài toán phân loại.

## Mỗi một được trồng đầy đủ cành lá và không tỉa cành sử dụng chính phiên bản của nó và các biến ở dữ liệu học.

## Với các bài toán phân loại, mỗi một cây là đại diện cho một lớp mà nó tiên lượng và kết quả tiên lượng cuối cùng là lớp mà có nhiều đại diện/phiếu bầu nhất. Với bài toán hồi qui, giá trị tiên lượng là giá trị trung bình trọng số của giá trị được tiên đoan bởi mỗi cây

# Một vài đặc trưng thú vị về phương pháp trồng rừng là

## Phương pháp này không xảy ra tình trạng tạo ra mô hình quá mức. Mặc dù mỗi một bộ phận của rừng (e.g. một cây) thì có thể xảy ra tình trạng thể hiện quá mức, bởi vì cây này khác cây kia, các sai lầm mà mỗi cây tạo ra là theo rất nhiều chiều và hướng; khi các sai số này được lấy trung bình, các lỗi này sẽ bị loại trừ lẫn nhau. Mặc dù mỗi một phân loại là yếu (nhớ lại là nhóm dữ liệu học và biến là được lấy ngẫu nhiên), khi để chúng cạnh nhau, chúng trở lên thành một phân loại mạnh (đây gọi là ý tưởng bó đũa).

## Chúng ta không cần dùng việc đánh giá chéo. Lưu ý rằng các quan sát tương ứng với 36-37% của gói dữ liệu bị loại? thật ra là các dữ liệu đó (được gọi là ngoài túi/rổ) được dùng không những chỉ để tính toán sai số, mà còn được dùng để đo lượng tầm quan trọng của mỗi biến. Toàn bộ quá trình này xảy ra một cách đồng thời khi mô hình được chạy.

## Chúng ta có thể trồng nhiều cây theo nhu cầu của chúng ta và không có giới hạn (giới hạn ở đây chính là độ mạnh của máy tính). Điều thường xảy ra, tuy nhiêu, đó là các sai số được ước tính có thể không được cải thiện thêm sau khi một số lượng cây nhất định đã lớn. Số lượng đặc trưng cho sự hội tụ của sai số này thường trong khoảng từ 100 đến 2000 cây, phụ thuộc vào sự phức tạp của vấn đề.

## Mặc dù chúng ta thông thường cải thiện độ chính xác, điều đó đi kèm với chi phí: có thể giải thích được. Một rừng cây có gì đó khác giống với một chiếc hộp đen, mà chúng ta rất khó để hiểu điều gì đã xảy ra ở bên trong cái hộp đó; dù gì đi chăng nữa, chúng ta vẫn phải có một con số ước lượng cho sự quan trọng của các biến, đó là điều mà nhiều hơn cả các mô hình khác có thể đưa ra.



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

library(randomForest)

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


# Chúng ta có thể thấy ngay rằng, tính chính xác đã được cải thiện khi so sánh nó với mô hình cây quyết định, và nó gần với kết quả của mô hình hồi qui tuyến tính. Tốc độ gió, một lần nữa, được coi là biến quan trọng nhất; kết quả cho chúng ta tháy rằng, nếu thiếu biến đó thì sai số sẽ tăng lên là 28%.

# Tập hợp lại tất cả các bước trên.

## chúng ta vừa đã xây dựng và đánh giá độ chính xác của 5 mô hình khác nhau: mô hình gốc, mô hình hồi qui tuyến tính; mô hình cây quyết định với đủ cành lá; mô hình cây quyết định đã được tỉa cành; và mô hình trồng rừng. Giờ chúng ta tạo ra một khung cho các giá trị RMSE và MAE để so sánh dễ hơn.


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


# Đồ thị cho ta thấy rằng, không có mô hình nào có thể tiên đoán chính xác khi lượng mưa lớn hơn 25mm. Trong khoảng lượng mưa từ 0-22mm (xem đường màu xanh nhạt), phương pháp trồng rừng có vẻ là phương pháp tốt nhất, sau đó là phương pháp hồi qui tuyến tính. Mặc dù các mô hình này chưa thực sự chuẩn xác, nhưng dù sao nó cúng đã làm được một việc khá tốt so với việc chỉ sử dụng mô hình gốc.

# Trong rất nhiều trường hợp, chúng ta xây dựng mô hình không phải chỉ để tiên lượng, mà chúng ta còn biết và hiểu sâu sắc hơn về dữ liệu. Như ở phần trước khi chúng ta chỉ sử dụng phương pháp EDA, chúng ta thấy được là tốc độ gió có vẻ như là một biến quan trọng. Giờ chúng ta thấy là các mô hình đã xây dựng khẳng định điều đó.


# ở phần tiếp theo, chúng ta tiếp tục xây dựng mô hình, lần này chúng ta sử dụng biến lưu lượng mưa như là biến nhị thức. Có nghĩa là toàn bộ các mô hình sẽ đưa ra xác xuất xuất hiện mưa, cho mỗi một ngày trong gói dữ liệu thử nghiệm.


#--------------------------------------------------------------
#### Sử dụng mô hình Decision Tree cho biến Factor
#--------------------------------------------------------------

