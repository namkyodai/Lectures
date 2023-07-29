#Nhập Dữ Liệu

table_id <- "18-10-0205-01" # --> chọn bảng --> xem trên trang web https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1810020501
thedata <- get_cansim(table_id) #sử dụng hàm get_cansim để chọn bảng
get_cansim_table_overview(table_id)
ls.str(thedata) #xem dữ liệu

#xem tổng hợp dữ liêu
thedata %>%
  group_by(GEO, GeoUID) %>%
  tally()

tail(thedata) #xem đuôi của dữ liệu
