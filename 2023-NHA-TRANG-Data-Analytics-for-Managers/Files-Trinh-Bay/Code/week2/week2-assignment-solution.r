#000000000000000000000000000000000000000000000000000000000


# A. Sử dụng thư viện dplyr của R để trả lời các câu hỏi sau liên quan đến gói dữ liệu Births2006

#000000000000000000000000000000000000000000000000000000000


# 1. Có bao nhiêu trẻ sơ sinh là Nam, bao nhiêu là Nữ?
# 2. Có bao nhiêu phụ sản mà chúng ta không biết trình độ học vấn?
# 3. Có bao nhiêu phụ sản sinh ít hơn 3 bé sơ sinh và sử dụng phương pháp đẻ tự nhiên?
# 4. Có bao nhiêu trẻ sinh ra có chỉ số APGAR5 nhỏ hơn 3 và có cân nặng lớn hơn 2.5 kg? Trong số trẻ này thì có bao nhiêu trẻ sinh bằng phương pháp tự nhiên, bao nhiêu sinh ra bằng phương pháp cắt.
# 5. Tháng nào trong năm có số lượng trẻ sơn sinh nhiều nhất?
# 6. Câu hỏi phụ (không nhất thiết phải trả lời), các bạn có thẻ vẽ biểu đồ để thể hiện dữ liệu cho 5 câu hỏi trên không? --> sử dụng biểu đồ barchart, histogram sử dụng phụ kiện ggplot2
# 7. Tìm mối tương quan giữa cân nặng của trẻ sơ sinh với các biến còn lại? Các biến nào các bạn sẽ chọn để dùng phân tích? Các bạn có kết luận gì không sau khi chạy phân tích với hồi qui tuyến tính chuẩn?


library(dplyr)

df <-read.csv("births2006.csv",header = TRUE, sep = ",")
df <- data.frame(df)


glimpse(df)

# 1. Có bao nhiêu trẻ sơ sinh là Nam, bao nhiêu là Nữ?

summary(df %>% select(SEX))

#cách 1
df %>%
  select(SEX) %>%
  count(SEX)


#cách 2

df %>%
  count(SEX)


# 2. Có bao nhiêu phụ sản mà chúng ta không biết trình độ học vấn

# cách 1
df %>%
  filter(DMEDUC == "NULL")%>%
  count(DMEDUC)
# cách 2

df %>%
  filter(!DMEDUC %in% c("F","M"))%>%
  count(DMEDUC)


# 3. Có bao nhiêu phụ sản sinh ít hơn 3 bé sơ sinh và sử dụng phương pháp đẻ tự nhiên?


df %>%
  select(DPLURAL,DMETH_REC)%>%
  filter(DPLURAL %in% c("1 Single", "2 Twin"), DMETH_REC == "Vaginal") 


# Cách 1


glimpse(df %>%
          select(DPLURAL,DMETH_REC)%>%
          filter(DPLURAL %in% c("1 Single", "2 Twin"), DMETH_REC == "Vaginal") )

# Cách 2

summary(df %>%
          select(DPLURAL,DMETH_REC)%>%
          filter(DPLURAL %in% c("1 Single", "2 Twin"), DMETH_REC == "Vaginal") )

# cách 3 ????

count(df %>%
        select(DPLURAL,DMETH_REC)%>%
        filter(DPLURAL %in% c("1 Single", "2 Twin"), DMETH_REC == "Vaginal"))



# 4. Có bao nhiêu trẻ sinh ra có chỉ số APGAR5 nhỏ hơn 3 và có cân nặng lớn hơn 2.5 kg? Trong số trẻ này thì có bao nhiêu trẻ sinh bằng phương pháp tự nhiên, bao nhiêu sinh ra bằng phương pháp cắt.

count(df %>% 
        filter(APGAR5 <3, DBWT > 2500))

df %>% 
  filter(APGAR5 <3, DBWT > 2500) %>%
  filter(DMETH_REC %in% c("C-section", "Vaginal")) %>%
  count(DMETH_REC, sort = FALSE)


# 5. Tháng nào trong năm có số lượng trẻ sơn sinh nhiều nhất?

# cách 1
df1<-df %>%
  select(DOB_MM) %>%
  count(DOB_MM) 

which.max(df1$n)

# cách 2

ggplot(df1, aes(x=DOB_MM, y=n)) + 
  geom_bar(stat = "identity") +
  labs(
    #   title = "Số lượng trẻ sinh ra theo ngày trong tuần",
    y = "Số trẻ sơ sinh",
    x = "Tháng trong năm"
  )

# cách 3

result <- df1 %>% 
  group_by(DOB_MM, n) %>%
  filter(DOB_MM == max(DOB_MM)) %>%
  arrange(desc(n),DOB_MM)

result


# 6. Câu hỏi phụ (không nhất thiết phải trả lời), các bạn có thẻ vẽ biểu đồ để thể hiện dữ liệu cho 5 câu hỏi trên không? --> sử dụng biểu đồ barchart, histogram sử dụng phụ kiện ggplot2

# biểu đồ vẽ trẻ sơ sinh theo giới tính
df.sex <- df %>%
  count(SEX, sort = FALSE)
ggplot(df.sex, aes(x=SEX, y=n)) + 
  geom_bar(stat = "identity",color='red',fill='darkolivegreen1') +
  labs(
    title = "Số lượng trẻ sinh theo giới tính",
    y = "Số trẻ sơ sinh",
    x = "Giới tính"
  )

# học vấn


df.hocvan <- df %>%
  select(DPLURAL,DMETH_REC)%>%
  filter(DPLURAL %in% c("1 Single", "2 Twin"), DMETH_REC == "Vaginal") %>%
  count(DPLURAL, sort = FALSE)

ggplot(df.hocvan, aes(x=DPLURAL, y=n)) + 
  geom_bar(stat = "identity",color='red',fill='darkolivegreen1') +
  labs(
    #   title = "Số lượng trẻ sinh theo giới tính",
    y = "Số trẻ sơ sinh",
    x = "Sinh đôi và sinh 1"
  )


count(df %>%
        select(DPLURAL,DMETH_REC)%>%
        filter(DPLURAL %in% c("1 Single", "2 Twin"), DMETH_REC == "Vaginal"))

# 


df.moi <- df %>% 
  filter(APGAR5 <3, DBWT > 2500) %>%
  filter(DMETH_REC %in% c("C-section", "Vaginal")) %>%
  count(DMETH_REC, sort = FALSE)

ggplot(df.moi, aes(x=DMETH_REC, y=n)) + 
  geom_bar(stat = "identity",color='red',fill='darkolivegreen1') +
  labs(
    #   title = "Số lượng trẻ sinh theo giới tính",
    y = "Số trẻ sơ sinh",
    x = "Phương pháp sinh"
  )

ggplot(df1, aes(x=reorder(DOB_MM,-n), y=n)) + 
  geom_bar(stat = "identity") +
  labs(
    #   title = "Số lượng trẻ sinh ra theo ngày trong tuần",
    y = "Số trẻ sơ sinh",
    x = "Tháng trong năm"
  )


# 7. Tìm mối tương quan giữa cân nặng của trẻ sơ sinh với các biến còn lại? Các biến nào các bạn sẽ chọn để dùng phân tích? Các bạn có kết luận gì không sau khi chạy phân tích với hồi qui tuyến tính chuẩn?

m1=lm(DBWT~.,data=df)
m1 # --> có nhận xét gì không?
summary(m1)


m1=lm(DBWT~DOB_MM+DOB_WK+MAGER+TBO_REC+WTGAIN+APGAR5+UPREVIS+ESTGEST,data=df)

summary(m1)


m1=lm(DBWT~MAGER+TBO_REC+WTGAIN+APGAR5+ESTGEST,data=df)

summary(m1)

df2 <- df %>% 
  select(DBWT,MAGER,TBO_REC,WTGAIN,APGAR5,ESTGEST)

m1=lm(DBWT~.,data=df2)
summary(m1)
cor(df2)

# vấn đề của chúng ta bây giờ là gì?

summary(df2)

# --> có rất nhiều số liệu là không tồn tại -- có ảnh hưởng gì không? tất nhiên là có rồi

# https://www.programmingr.com/examples/remove-na-rows-in-r/


# na.omit() – remove rows with na from a list


df3 <- na.omit(df2)

nrow(df2)
nrow(df3)
nrow(df2) - nrow(df3)


m1=lm(DBWT~.,data=df3)
summary(m1)
cor(df3)

#000000000000000000000000000000000000000000000000000000000



# B. Sử dụng thư viện dplyr của R để trả lời các câu hỏi sau liên quan đến gói dữ liệu hanjinshipyard
# 
# 1. Có bao nhiêu hạng mục cần sửa chữa và nâng cấp trong vòng 5 năm tới?
#   
#   Đọc dữ liệu
library(readxl)
df=read_excel("../../../data/tdd/hanjinshipyard.xlsx",sheet="CAPEX",skip = 2) # Tải dữ liệu vào R, đọc bảng tính tab có tên CAPEX, bỏ qua 2 dòng đầu tiên

df <- data.frame(df) # Chuyển đổi dữ liệu thành dạng khung
nrow(df)

nrow(df %>% 
       filter(InterYear <=5))

# còn có cách nào không?



#   2. Hiện tại biến UnitCost có giá trị tiền tệ là Peso --> thêm cột mới vào bảng sử dụng lệnh mutate() để tính giá đơn vị theo tiền VND, tỉ giá Peso/VND là 444 VND.

df %>%
  mutate(giaVND = UnitCost*444)
# 3. Thêm một cột mới tính tổng chi phí theo VND cho từng hạng mục? Sử dụng công thức
# 

# Net Present Value (npv) = (giá đơn vị x khối lượng) x exp(-rho x t)
# 
# Trong đó exp() là hàm exponential
# 
# rho là lãi xuất ngân hàng, lấy trung bình là 6% cho 1 năm.
# 
# t là năm tiến hành công việc liên quan đến từng hạng mục.


df %>%
  mutate(giaVND = UnitCost*444) %>%
  mutate(TongChiPhi = giaVND*Quantity*exp(-0.06*InterYear)) 



# 4. Sau khi làm xong câu hỏi 3, các bạn cho biết tổng số tiền VND cần bỏ ra để thực hiện công tác duy tuy và bảo dưỡng nhà máy đóng tàu Hanjin?
#   

df %>%
  mutate(giaVND = UnitCost*444) %>%
  mutate(TongChiPhi = giaVND*Quantity*exp(-0.06*InterYear)) %>%
  summarise_all(sum)
# không được vì các cột có chữ


df %>%
  mutate(giaVND = UnitCost*444) %>%
  mutate(TongChiPhi = giaVND*Quantity*exp(-0.06*InterYear)) %>%
  select(TongChiPhi) %>%
  summarise_all(sum)



#   5. Có bao nhiêu hạng mục có sự xuống cấp lớn hơn 2 và có rủi do nhỏ hơn 3? tổng chi phí cho các hạng mục này là bao nhiêu?
#   
df %>%
  mutate(giaVND = UnitCost*444) %>%
  mutate(TongChiPhi = giaVND*Quantity*exp(-0.06*InterYear))%>%
  filter(States >2, Risk <3)   %>%
  select(TongChiPhi) %>%
  summarise_all(sum)



#   6. Các bạn có thể vẽ biểu đồ (sử dụng ggplot2) cho các câu hỏi trên không?--> đây là câu hỏi phụ
# 




# C. Câu hỏi lý thuyết
# 
# 1. Vì sao chúng ta cần phải sử dụng dữ liệu sạch (gọn gàng) để phân tích?
#   2. Trong công việc hàng ngày của các bạn, nếu các bạn dùng excel thì có nên trộn nhiều bảng vào 1 sheet của excel không? nếu không thì giải thích tại sao?
#   3. Các bạn có nên sử dụng merge hàng cột trong excel không? tại sao? Có biện pháp gì để sử lý vấn đề này khi dùng excel
# 4. Các bạn có nên sử dụng subtotal trong bảng tính excel không? Giải thích tại sao.








