#Load vào RAM máy tính các thư viện cần thiết
library(tidyverse)
library(lubridate)  # chứa các chương trình xử lý về ngày tháng
library(scales)     # bổ trợ cho thư viện  {ggplot2}
library(glue)       # dùng để gắn kết các dữ liệu string với nhau
#
# utilities
library(cansim)     # đây là thư viện chứa gói dữ liệu về giá nhà ở Canada
library(janitor)    # dùng để làm sạch tên cột
library(knitr)      # dùng để xuất bản báo cáo, có chứa các hàm để hiển thị bảng như `kable()`
library(kableExtra) # - dùng để định dạng và trang trí bảng 
library(flextable)  # dùng để định dạng và trang trí bảng


#Tải vào các phương trình định sẵn liên quan đến chuyển đổi ngày tháng và vẽ đồ thị theo 1 định dạng chuẩn.

source("f_monthyear.R")
source("bida_chart_theme.R")
# CHÚNG TA BẮT ĐẦU TỪ ĐÂY

source("import.R")            # BƯỚC 1  - Nhập Dữ Liệu
source("cleaning.R")          # BƯỚC 2  - Làm Sạch 
source("filtering.R")         # BƯỚC 3  - Sàng Lọc
source("table_visualize.R")   # BƯỚC 4  - Hiển THị Bảng
source("table_communicate.R") # BƯỚC 5  - Thông Tin Bảng
source("text.R")              # BƯỚC 6  - Chữ
source("visualize.R")         # BƯỚC 7  - Trực Quan Hóa Bằng Đồ Thị










#text







