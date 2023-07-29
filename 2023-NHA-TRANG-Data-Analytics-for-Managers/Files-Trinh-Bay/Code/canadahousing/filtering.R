# Chỉ chọn ra 2 vùng là BC - Bristish Columbia và Canada

startdate <- as.Date("2007-01-01") #vì số liệu từ năm 1981 nên nó quá nhiều, nên chọn số liệu từ ngăm 2007 cho tới nay, lưu ý đây là số liệu được cập nhật theo tháng và được xuất bản định kì trên trang web của chính phủ canada.

# Chọn BC và Canada, đồng thời chỉ chọn riêng 4 cột cần quan tâm.
thedata_BC_Can <- thedata %>%
  filter(ref_date >= startdate) %>%
  filter(geo %in% c("British Columbia", "Canada"), 
         new_housing_price_indexes == "Total (house and land)") %>%
  select(ref_date, geo, new_housing_price_indexes, value)
head(thedata_BC_Can)
