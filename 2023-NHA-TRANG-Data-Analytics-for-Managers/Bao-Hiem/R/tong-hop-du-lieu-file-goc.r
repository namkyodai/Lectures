#--------------------------------------------------------------
.library(rstudioapi) 
setwd(dirname(getActiveDocumentContext()$path)) #điều hướng R đọc thư mục chứa file R.

#Tải vào Ram các gói thư viện cần thiết.

library(readxl) #gói thư viện đọc file excel
library(DT)
library(dplyr) #gói thư viện để xử lý bảng.
library(pander)
library(writexl)
epsilon=10000000
library(reshape)
library(ggplot2) #gói thư viện vẽ đồ thị
library(psych)
library(lubridate)
library(scales) #gói thư viện để chỉnh sửa ngày tháng.
library(tidyr)


#Đọc các file dữ liệu gốc
rm001=read_excel("../raw/1-RM-Ha.xlsx",sheet="Sale",skip = 2)
rm002=read_excel("../raw/2-RM-Trung.xlsx",sheet="Sale",skip = 2)
rm003=read_excel("../raw/3-RM-Thinh.xlsx",sheet="Sale",skip = 2)
rm004=read_excel("../raw/4-RM-Phuong.xlsx",sheet="Sale",skip = 2)

#nối các dữ liệu có cùng định dạng với nhau thành 1 dữ liệu tổng hợp, đồng thời biến dữ liệu đó thành dataframe để có thể sử dụng dplyr và ggplot2.
df <-data.frame(rbind(rm001,rm002,rm003,rm004))

#Kiểm tra các trường dữ liệu để xem có phải sửa gì không.
glimpse(df)


#
df1<-df%>%
  select(Khu.Vực,Tình.Trạng.Chốt.Deal,IP.Dự.Kiến)%>%
  group_by(Khu.Vực)
df1<-melt(df1,id=c("Khu.Vực","Tình.Trạng.Chốt.Deal"))

df1<-cast(df1,Khu.Vực~Tình.Trạng.Chốt.Deal,sum,na.rm = TRUE)






