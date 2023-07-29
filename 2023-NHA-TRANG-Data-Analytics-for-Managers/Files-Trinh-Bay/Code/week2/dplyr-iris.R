## Người soạn bài giảng: Lê Thanh Nam (namlt@asq.vn), https://namkyodai.github.io

###################
#     BƯỚC 1: Khai báo các gói phụ kiện (package) R cần thiết để phân tích


# -----------------------------------------------------------------
library(dplyr) # gói phụ kiện để tương tác và thay đổi dữ liệu

###################

#     BƯỚC 2: Đọc dữ liệu vào R

df <- iris

# để biết có bao nhiêu gói dữ liệu có sẵn trên R, các bạn có thể dùng lệnh

# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html

df <- data.frame(df) # Chuyển đổi dữ liệu thành dạng khung


###################
#   BƯỚC 3: Thực hành với gói phụ kiện Dplyr

#---Các bạn có thể đọc thêm hướng dẫn từ link **https://dplyr.tidyverse.org/**, để dễ nhớ bằng minh họa, chúng ta có thể dùng bảng CheetSheet 
# https://raw.githubusercontent.com/rstudio/cheatsheets/master/pngs/thumbnails/data-transformation-cheatsheet-thumbs.png


# >>>>>>>>>       glimse() sử dụng hàm glimse để xem tổng hợp vắn tắt về dữ liệu
glimpse(df)

# giải thích dữ liệu
# iris là một loài hoa, tên tiếng Việt là hoa Chi Diên Vĩ và có họ hàng với hoa Lay Ơn
# https://eva.vn/cay-canh-vuon/hoa-dien-vi-y-nghia-nguon-goc-cong-dung-va-cach-trong-c283a474719.html
# -------5 cột, tương ứng với 5 biến
#--------



#>>>>>>>>>>       select()
# chọn các cột CAPEXassets, Facilities, FloorLevel, Area, Tenant, WorkType, Disciplines,Quantity,  Unit, States, Risk, IntePec, UnitCost, LifeSpan, Prob, InterYear, NPVCapexP2Php

#cách 1
df %>%
  select(CAPEXassets, Facilities, FloorLevel, Area, Tenant, WorkType, Disciplines,Quantity,  Unit, States, Risk, InteType,IntePec, UnitCost, LifeSpan, Prob, InterYear, NPVCapexP2Php)
#cách 2
df %>%
  select(c(2,3,4,5,6,7,8,9,11,12,13,14,15,17,18,19,20,21,22,26,27))
#cách 3
df %>%
  select(c(2:9,11:15,17:22,26,27))


# chọn các cột CAPEXassets, Facilities, FloorLevel, Area, Tenant, WorkType, Disciplines,Quantity,  Unit, States, Risk, NPVCapexP2Php

#đặt tên mới là df01
df01 <- df%>%
  select(CAPEXassets, Tenant, Disciplines,Quantity,  Unit, States, Risk, NPVCapexP2Php)



#>>>>>>>>>>       arrange()

# sắp xếp theo thứ tự giá trị Capex từ cao xuống thấp

df01 %>% 
  arrange(desc(NPVCapexP2Php))


# lọc dữ liệu có Capex lớn hơn 1000000 Peso


df01 %>%
  filter(NPVCapexP2Php > 1000000)

# lọc dữ liệu có tenant là Navy

df01 %>%
  filter(Tenant == "Navy")

df01 %>%
  filter(Tenant == "Navy", NPVCapexP2Php > 1000000)


# mutate

# chúng ta tạo ra một dữ liệu mới có tên df02 từ dữ liệu ban đầu
df02 <- df %>%
  select(c(2:9,11:15,17:22,26,27))

# thêm một cột có tên capexUSD bằng, tỷ giá giữ USD/Peso là 50 Peso cho 1 USD
df02 %>%
  mutate(capexUSD = NPVCapexP2Php/50)





