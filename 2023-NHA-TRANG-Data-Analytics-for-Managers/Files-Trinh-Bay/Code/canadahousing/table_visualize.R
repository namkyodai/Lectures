#Tìm hiểu về dữ liệu
## Thiết lập các bảng pivot


#chỉ chọn ra vùng BC sau đó thêm cột năm (year), cột tháng,sau đó chọn ra 3 cột là năm, tháng và giá trị.

#sử dụng pivot_wider để coi theo tháng
NHPI_table <- thedata_BC_Can %>%
  filter(geo == "British Columbia") %>%
  mutate(year = year(ref_date),
         month = month(ref_date, label = TRUE)) %>%
  select(year, month, value)%>%
  pivot_wider(names_from = month, values_from = value) %>%
  arrange(desc(year))

# display the table
print(head(NHPI_table))



# Làm thế nào để thêm vào tổng hợp theo năm
# Julie's genius solution --> giải pháp của Julie, là sử dụng chính bảng pivot lúc trước và tính giá trị trung bình các cột, trừ cột thứ 1 (là cột năm)
NHPI_table2 <- NHPI_table %>%
  mutate(annual_avg = rowMeans(NHPI_table[-1], na.rm = TRUE))
print(head(NHPI_table2))



# Stephanie's genius solution --> Cách của Stephanie
# starts with the raw data table --> Sử dụng luôn dữ liệu ban đầu chứ không phải từ bảng pivot

NHPI_table3 <- thedata_BC_Can %>%
  filter(geo == "British Columbia") %>%
  mutate(year = year(ref_date),
         month = month(ref_date, label = TRUE)) %>%
  select(year, month, value) %>%
  group_by(year) %>%  # đến đây các bạn có thể thấy là Stephanie đã sử dụng hàm group_by
  mutate(annual_avg = mean(value, na.rm = TRUE)) %>%  # sau đó cô ấy thêm 1 cột và tính trung bình của cột value
  pivot_wider(names_from = month, values_from = value) %>%
  arrange(desc(year))

print(head(NHPI_table3))

# hoặc là tính thủ công như thế này
NHPI_table4 <- NHPI_table %>%
  rowwise() %>% 
  mutate(annual_avg = mean(c(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec), na.rm = TRUE))

print(head(NHPI_table4))


#---------------
NHPI_table4 <- NHPI_table %>%
  rowwise() %>% 
  #  use `c_across()` to specify the range of columns
  mutate(annual_avg = mean(c_across(Jan:Dec), na.rm = TRUE))

print(head(NHPI_table4))

#như vậy có thể kết luận là có rất nhiều cách tính khác nhau để cho ra 1 kết quả, vậy cách nào là cách tối ưu nhất?