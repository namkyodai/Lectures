###################
#     BƯỚC 1: Khai báo các gói phụ kiện (package) R cần thiết để phân tích

# -----------------------------------------------------------------
library(dplyr) # gói phụ kiện để tương tác và thay đổi dữ liệu

###################
#   BƯỚC 2: Thực hành với gói phụ kiện Dplyr

#---Các bạn có thể đọc thêm hướng dẫn từ link **https://dplyr.tidyverse.org/**, để dễ nhớ bằng minh họa, chúng ta có thể dùng bảng CheetSheet 
# https://raw.githubusercontent.com/rstudio/cheatsheets/master/pngs/thumbnails/data-transformation-cheatsheet-thumbs.png

#https://www.rstudio.com/resources/cheatsheets/

# Data


# >>>>>>>>>       glimse() sử dụng hàm glimse để xem tổng hợp vắn tắt về dữ liệu
iris <- data.frame(iris)

car


glimpse(iris)

#>>>>>>>>>>       select()



#cách 1
df %>%
  select(CAPEXassets, Facilities, FloorLevel, Area, Tenant, WorkType, Disciplines,Quantity,  Unit, States, Risk, InteType,IntePec, UnitCost, LifeSpan, Prob, InterYear, NPVCapexP2Php)
#cách 2
df %>%
  select(c(2,3,4,5,6,7,8,9,11,12,13,14,15,17,18,19,20,21,22,26,27))
#cách 3
df %>%
  select(c(2:9,11:15,17:22,26,27))

# cách 4

df %>%
  select(-c(No,YearBuilt,CodeComply,TenantInterYear,IncludedPhase1,NPVCAPEXP1USD,NPVCapexP2Php,NPVCapexUSD,ReturntoAs.built,ReasonsforIntervention,Scopes,PictureReferenceCode,Remarks))





# chọn các cột CAPEXassets, Facilities, FloorLevel, Area, Tenant, WorkType, Disciplines,Quantity,  Unit, States, Risk, NPVCapexP2Php

#đặt tên mới là df01
df01 <- df%>%
  select(CAPEXassets, Tenant, Disciplines,Quantity,  Unit, States, Risk, NPVCapexP2Php)


# không chọn một hay nhiều biến trong toàn bộ các biến

df01 %>%
  select(-NPVCapexP2Php)

# thay đổi tên của biến (tên của cột)


rename(df01, item)


df01 %>%
  rename(item = CAPEXassets)

# or
rename(df01, item = CAPEXassets)


df01 %>%
  rename(item=CAPEXassets, disc=Disciplines, quant=Quantity, unit=Unit, state=States, risk=Risk,npv=NPVCapexP2Php)


#>>>>>>>>>>       arrange()

# sắp xếp theo thứ tự giá trị Capex từ cao xuống thấp

df01 %>% 
  arrange(desc(NPVCapexP2Php))


# lọc dữ liệu có Capex lớn hơn 1000000 Peso


df01 %>%
  filter(NPVCapexP2Php > 1000000)

# lọc dữ liệu có tenant là Navy, lọc có điều kiện

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

# Lọc và sắp xếp
df02 %>%
  filter(Tenant == "Navy") %>%
  arrange(desc(NPVCapexP2Php))

# kết hợp với select(), mutate(), filter(), và arrange()

df03 <- df%>%
  select(CAPEXassets, Area, Disciplines, Facilities, FloorLevel, Tenant, Quantity,  Unit, States, Risk, InteType,InterYear, UnitCost)

# thay đổi tên cột cho gọn gàng hơn
df03 <- df03 %>%
  rename(item=CAPEXassets, area=Area,disc=Disciplines, facility=Facilities, floor=FloorLevel, tenant = Tenant, quant=Quantity, unit=Unit, state=States, risk=Risk, is=InteType, cost=UnitCost, year=InterYear)

# thêm cột tính giá trị hiện tại npv = C*exp(-rho*t). C=Q*R: cost*quant*exp(-rho*year)
# 
rho <- 5/100
df03 %>%
  mutate(costvnd =cost*444.71) %>%
  select(-cost)%>%
  mutate(npv = costvnd*quant*exp(-rho*year)) %>%
  arrange(desc(npv)) %>%
  filter(year == 0) 


###################
#   Các cách tính tổng hợp

# đếm count() --> đếm nhanh các giá trị riêng biệt của một biến hay nhiều biến : df %>% count(a, b) is roughly equivalent to df %>% group_by(a, b) %>% summarise(n = n())

#bao nhiêu hạng mục có tình trạng khác nhau

df03 %>%
  count(state)

df03 %>%
  count(state, risk)

df03 %>%
  count(disc,state, risk, sort = TRUE)

df03 %>%
  tally()

df03 %>% group_by(risk) %>% tally()


df03 %>% add_count(risk, wt = risk)


df03 %>%
  summarise(
    mean(state),
    mean(risk)
  )

df04 <- df03 %>%
  mutate(costvnd =cost*444.71) %>%
  select(-cost)%>%
  mutate(npv = costvnd*quant*exp(-rho*year)) %>%
  arrange(desc(npv)) %>%
  filter(year == 0) 

df04 %>%
  summarise(
    mean(state),
    mean(risk),
    mean(npv)
  )

































