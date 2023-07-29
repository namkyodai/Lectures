###################
#     BƯỚC 1: Khai báo các gói phụ kiện (package) R cần thiết để phân tích
library(rstudioapi) # thư viện dùng để đặt đường dẫn tới file gốc
setwd(dirname(getActiveDocumentContext()$path))

# -----------------------------------------------------------------
library(dplyr) # gói phụ kiện để tương tác và thay đổi dữ liệu
library(readxl)# gói phụ kiện để đọc dữ liệu từ bảng tính excel
library(ggplot2)
library(plyr)

###################

#     BƯỚC 2: Đọc dữ liệu vào R

df=data.frame(read_excel("dieutra-hogiadinh-hcm.xlsx",sheet="ketquadieutra",skip = 1)) #

head(df)

library(DT)
datatable(head(df))
datatable(tail(df))

str(df)
glimpse(df)
summary(df)

df$Q1a <- as.integer(df$Q1a) #chọn ra một số biến và định nghĩa lại

library(purrr) #

df <- df %>%
  purrr::modify_at(c(2:10, 13:14, 16:21,25:30,44,46,50:51,54:55,58:59,62:63,66:67,70:71,74:75,78,79,82:83,86:87,89:90,92:229), factor)

glimpse(df)

df$Q4<- as.character(df$Q4)
df$Q6b <- as.character(df$Q6b)
#df$Q9 <- as.integer(df$Q9) #chọn ra một số biến và định nghĩa lại
df$Q11<- as.character(df$Q11)
df$Q18a8<- as.numeric(df$Q18a8)
df$Q18b8<- as.numeric(df$Q18b8)
df$Q18a9<- as.numeric(df$Q18a9)
df$Q18b9<- as.numeric(df$Q18b9)
df$Q18a10<- as.numeric(df$Q18a10)
df$Q18b10<- as.numeric(df$Q18b10)
df$Q44b <- as.character(df$Q44b)
str(df)

#df<-df%>%
#  mutate_if(is.numeric,as.factor) #Biến đổi toàn bộ giá trị numeric thành factor

# check if there is any strange
str(df)
glimpse(df)
# Tại sao các cột cần được mã hóa.

# ********************************************************************************
#
#                             TRÌNH TRẠNG NHÀ Ở
#
# ********************************************************************************
# ..........................NHÓM 1

# Câu 1:  Chọn các biến liên quan đến tình trạng nhà ở để phân tích, đặt tên mới cho object đó, và tiến hành Tổng hợp theo nhóm để đếm số lượng người theo tình trạng chủ sở hữu căn nhà.

df01 <- df%>%
  select(Q3,Q4,Q7,Q8a,Q8b,Q8c,Q8d,Q8e,Q9,Q10,Q11)

# Câu 2:  Trong số dữ liệu về tình trạng nhà ở thì có bao nhiêu dòng có dữ liệu trống?
summary(df01)
# Các dữ liệu trống này có cần thiết phải loại bỏ ngay bây giờ không?

# Có bao nhiêu dữ liệu theo dạng chủ sở hữu

#Câu 3: Tính toán tổng số hộ ứng với từng nhóm trong bảng của câu hỏi liên quan đến tình trạng nhà ở (Câu hỏi 8), yêu cầu sử dụng dplyr để cho ra bảng mới và sau đó dùng ggplot để vẽ đồ thị dạng thanh cho từng nhóm.

#Khi vẽ đồ thị, các bạn trang trí cho đồ thị của mình với các lớp sau

#- Lớp về tên các trục và tiêu đề của đồ thị
#- Lớp màu sắc --> thay đổi màu sắc và độ nổi và chìm (transparency) của đồ thị
#- Đặt lại tên các nhóm ở trục hoành thay vì các con số (Code), liệu có cách nào làm nhanh hơn không?

#                     Chủ sở hữu (Q8a)


#-----------
table(df01$Q8a) # Cách tính thứ 1 sử dụng trực tiếp hàm table trong R


df01 %>%
count("Q8a")   # cách tính thứ 2 sử dụng dplyr với count
count(df01, "Q8a") # cách tính thứ 3 trực tiếp sử dụng hàm count trong R

#-----------


# Vẽ biểu đồ mô tả số lượng chủ sở hữu nhà

ggplot(df01 %>%
         count("Q8a"), aes(x=Q8a, y=freq))+
  geom_bar(stat="identity") # Cách vẽ thứ 1 kết hợp cả ggplot và dplyr, dữ liệu sử dụng để vẽ là bảng tổng hợp từ gói dplyr

xticks = c('Chủ', 'Thuê', 'Mượn', 'Khác') # khai báo tên chủ sở hữu tương ứng với code 1, 2, 3, 4 --> xem tài liệu ở Word.

df01 %>%
  count("Q8a")

df01 %>%
  count("Q8a") %>%
  ggplot(aes(x=Q8a, y=freq))+ #khai bao trục hoành x và trục tung
  geom_bar(stat="identity", fill="red", alpha =0.5) + # khai báo biểu đồ dạng barchart, loại biểu đồ frequenty, tô đỏ và làm mờ vùng tô đỏ
  labs(title ="Trình trạng nhà ở", # khai báo tên của biểu đồ
       subtitle = "Chủ sở hữu", # khai báo tên phụ của biểu đồ
       x = "Loại chủ sở hữu", # khai báo tên trục hoành
       y = "Số lượng") # khai báo tên trục tung


df01 %>%
  count("Q8a") %>%
  ggplot(aes(x=Q8a, y=freq))+ # cách vẽ thứ 2 trong đó biến các giá trị 1, 2, 3, 4 thành dạng factor (category) thay vì là số, vì bản chất giá trị này không phải là số.
  geom_bar(stat="identity", fill="red", alpha =0.5) +
  labs(title ="Trình trạng nhà ở",
       subtitle = "Chủ sở hữu",
       x = "Loại chủ sở hữu",
       y = "Số lượng")+
  scale_x_discrete(labels = xticks) + # điền tên các giá trị tương ứng ở trục hoành
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25, color ="blue") # thêm trên đầu mỗi thanh các giá trị tương ứng với nhóm


#                     Diện Tích nhà

df01 %>%
  count("Q8b") %>%
  ggplot(aes(x=Q8b, y=freq))+
  geom_bar(stat="identity", fill="red", alpha =0.5) +
  labs(title ="Trình trạng nhà ở",
       subtitle = "Diện tích nhà",
       x = "Diện tích nhà ở",
       y = "Số lượng")+
  scale_x_discrete(labels = c('<=20m2', '21-50m2', '51-100m2', '>100m2')) +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25, color ="blue")


#                     Kết Cấu nhà

df01 %>%
  count("Q8c") %>%
  ggplot(aes(x=Q8c, y=freq))+
  geom_bar(stat="identity", fill="red", alpha =0.5) +
  labs(title ="Trình trạng nhà ở",
       subtitle = "Kết cấu nhà",
       x = "Kết cấu nhà",
       y = "Số lượng")+
  scale_x_discrete(labels = c('Kiên cố', 'Bán kiên cố', 'Lán trại')) +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25, color ="blue")

#                     Tnh trạng căn nhà



df01 %>%
  count("Q8d") %>%
  ggplot(aes(x=Q8d, y=freq))+
  geom_bar(stat="identity", fill="red", alpha =0.5) +
  labs(title ="Trình trạng nhà ở",
       subtitle = "Trình trạng căn nhà",
       x = "Tình trạng căn nhà",
       y = "Số lượng")+
  scale_x_discrete(labels = c('Tốt', 'Bình thường', 'Xấu')) +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25, color ="blue")

#                     Sử dụng đất

df01 %>%
  count("Q8e") %>%
  ggplot(aes(x=Q8e, y=freq))+
  geom_bar(stat="identity", fill="red", alpha =0.5) +
  labs(title ="Trình trạng nhà ở",
 #      subtitle = "Trình trạng căn nhà",
       x = "Sử dụng đất",
       y = "Số lượng")+
  scale_x_discrete(labels = c('Để ở', 'Để kinh doanh', 'Cả hai việc trên')) +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25, color ="blue")


# như vậy công việc ở trên đã phần nào làm cho việc phân tích nhanh hơn, nhưng có thể nhanh hơn nữa? ví dụ như các giá trị ở cột x thì phải đánh lại rất thủ công?

#Khi thiết kế bảng trong hệ cơ sở dữ liệu, thì chúng ta nên tạo ra các bản khác để lưu giữ các định nghĩa code.


Q8=data.frame(read_excel("dieutra-hogiadinh-hcm.xlsx",sheet="Q8",skip = 0)) #
Q8$Q8e


df01 %>%
  count("Q8e") %>%
  ggplot(aes(x=Q8e, y=freq))+
  geom_bar(stat="identity", fill="red", alpha =0.5) +
  labs(title ="Trình trạng nhà ở",
       #      subtitle = "Trình trạng căn nhà",
       x = "Sử dụng đất",
       y = "Số lượng")+
  scale_x_discrete(labels = paste(Q8$Q8e[-4])) +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25, color ="blue")

# loại bỏ NA cho 1 vectorz

df01 %>%
  count("Q8e") %>%
  ggplot(aes(x=Q8e, y=freq))+
  geom_bar(stat="identity", fill="red", alpha =0.5) +
  labs(title ="Trình trạng nhà ở",
       #      subtitle = "Trình trạng căn nhà",
       x = "Sử dụng đất",
       y = "Số lượng")+
  scale_x_discrete(labels = paste(na.omit(Q8$Q8e))) +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25, color ="blue")


#Câu 4: Các bạn có nhận xét gì sau khi làm xong câu hỏi 3??


# ********************************************************************************
#
#                             GIÁ THUÊ NHÀ (Q9)
#
# ********************************************************************************


# Câu 5: Liên quan đến câu hỏi Q9 của bảng dữ liệu.
#
# - Có bao nhiêu giá trị rỗng? tính phần trăm lượng giá trị đó trên tổng số mẫu.Yêu cầu tính và in ra trực tiếp trên màn hình R.
# - Vẽ đồ thị dạng Boxplot (đồ thị hộp) cho giá trị này? sau khi vẽ thì có nhận xét gì không?
#   - Vẽ đồ thị mật độ (density) và đồ thị tần suất (histogram) cho dữ liệu thuộc câu hỏi này --> sau khi vẽ thì có nhận xét gì không?
#   - yêu cầu loại bỏ các dữ liệu không cần thiết? loại bỏ nó bằng cách nào?
#   - Vẽ lại đồ thì boxplot, density plot, và histogram plot sau khi loại bỏ các giá trị ở bước trên.



sum(!complete.cases(df01$Q9)) # cách tính số hàng có giá trị rỗng
sum(is.na(df01$Q9)) # cách tính số hàng có giá trị rỗng

summary(df01$Q9) #kiểm tra lại xem có đúng hay không?

# Dữ liệu của chúng ta có 16140 người không trả lời, chiếm bao nhiêu %
sum(!complete.cases(df01$Q9))/nrow(df01)*100

# dùng biểu đồ boxplot và histogram để mô tả dữ liệu này

df01 %>%
  ggplot(aes(x="", y=Q9))+
  geom_boxplot()

# bạn có để ý rằng boxplot khá là không đẹp phải không? Lý do tại vì tiền việt nam to quá, ai cũng là triệu phú tỉ phú cả.

tyle <- 1000000

df01 %>%
  ggplot(aes(x="", y=Q9/tyle))+
  geom_boxplot()  # vẻ biểu đồ boxplot

df01 %>%
  ggplot(aes(x=Q9/tyle))+
  geom_density() # vẽ biểu đồ histogram


# Khi vẽ các biểu đồ này, R có tự động loại bỏ các giá trị rỗng không?


df01%>%
  filter(!is.na(Q9))%>%
  select(Q9)%>%
  ggplot(aes(x="", y=Q9/tyle))+
  geom_boxplot()  # như vậy chúng ta có thể khẳng định là khi vẽ biểu đồ này, R đã tựng động bỏ đi các giá trị rỗng


#loại bỏ giá trị 0 vì đó là lỗi của người nhập liệu

# Có bao nhiêu dư liệu là 0

nrow(df01%>%
  filter(!is.na(Q9), Q9 != 0))

df01%>%
  filter(!is.na(Q9), Q9 != 0)%>%
  select(Q9)%>%
  ggplot(aes(x="", y=Q9/tyle))+
  geom_boxplot()  # như vậy chúng ta có thể khẳng định là khi vẽ biểu đồ này, R đã tựng động bỏ đi các giá trị rỗng


df01%>%
  filter(!is.na(Q9), Q9 != 0)%>%
  select(Q9)%>%
  ggplot(aes(x="", y=Q9/tyle))+
  geom_boxplot()



df01%>%
  filter(!is.na(Q9), Q9 != 0, Q9 <10000000)%>%
  select(Q9)%>%
  ggplot(aes(x=Q9/tyle))+
  geom_density()+
  geom_histogram(aes(y = ..density..),colour = 1, fill = "white", alpha=0.2)

quantile(data.frame(df01%>%
                      filter(!is.na(Q9), Q9 != 0)%>%
                      select(Q9))$Q9/tyle,
         probs=seq(0,1,0.05)) #quantile from 0 to 1 with a step of 5%

quantile(data.frame(df01%>%
                      filter(!is.na(Q9), Q9 != 0)%>%
                      select(Q9))$Q9/tyle,probs=seq(0.95,1,0.01)) #quantilte from 0.95 to 1 with a step of 1%


quantile(data.frame(df01%>%
                      filter(!is.na(Q9), Q9 != 0)%>%
                      select(Q9))$Q9/tyle,probs=seq(0,0.1,0.01)) #quantilte from 0.95 to 1 with a step of 1%



# ********************************************************************************
#
#   Câu 6 - Lọc ra số liệu với chủ sở hữu là chính chủ, nhà có kết cấu kiên cố và bán kiên cố, và mục đích sử dụng là để ở và kinh doanh.
#
# ********************************************************************************

df01 %>%
  filter(Q8a == 1, Q8c %in% c(1,2), Q8e %in% c(1,2))
# ********************************************************************************
#
#   Câu 7 - Các dạng biểu đồ khác
#
# ********************************************************************************

# histogram
df01%>%
  filter(!is.na(Q9), Q9 != 0, Q9 <10000000)%>%
  select(Q9)%>%
  ggplot(aes(x=Q9/tyle))+
  geom_histogram()


# eatmap

#cách 1
df01%>%
  filter(!is.na(Q9), Q9 != 0, Q9 <10000000)%>%
  ggplot(aes(Q8d, Q8e, fill= Q9)) +
  geom_tile() #vẽ biểu đồ bản đồ sức nóng (heatmap)

#cách 2
df01%>%
  filter(!is.na(Q9), Q9 != 0, Q9 <10000000)%>%
  ggplot(aes(Q8d, Q8e, fill= Q9)) +
  geom_tile()+ #vẽ biểu đồ bản đồ sức nóng (heatmap)
  scale_fill_gradient(low="white", high="blue")
