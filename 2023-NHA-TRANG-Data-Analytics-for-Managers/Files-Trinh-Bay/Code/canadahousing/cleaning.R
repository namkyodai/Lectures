# Làm sạch dữ liệu, sử dụng thư viện Janitor để viết lại tên cho các cột. Đồng thời đặt lại ngày và tháng theo định dạng chuẩn thông qua việc sử dụng hàm ymd trong gói lubricate

thedata <- janitor::clean_names(thedata)
thedata <- thedata %>%
  mutate(ref_date = ymd(ref_date, truncated = 2)) 
head(thedata)

