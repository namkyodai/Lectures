library(ggplot2)
library(plotly)
# read excel file
library(readxl) 
library(reshape)
library(reshape2)
df=read_excel("../data/hocvien.xlsx",sheet="Sheet1",skip = 0)
#df<-melt(df,id="activity")
# Grouped
df<-data.frame(df)

colnames(df) <- c("Stt","hoten","ngaysinh","gioitinh","noisinh","truong","nganh","email","sdt","chucvu","donvi")


df$ngaysinh<-as.Date(parse_date_time(df$ngaysinh,c('dmy')),format = "%d/%m/%y")

format(as.Date("27/5/1987", format="%d/%m/%Y"),"%Y")

year(df$ngaysinh)

df <- df %>%
  mutate(namsinh = year(ngaysinh))




p1<-ggplot(df, aes(x = (2023-namsinh), y = gioitinh, fill = namsinh)) +
  geom_boxplot() +
  theme(legend.position = "top")

p2<-ggplot(df, aes(x = (2023-namsinh))) +
  geom_histogram()+
  theme(legend.position = "top")

library(hrbrthemes)
p3 <- df %>%
  ggplot( aes(x=(2023-namsinh))) +
  geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(
    x ="Độ Tuổi",
    y ="Số Lượng"
  )
  theme_ipsum()


a=df %>%
  group_by(truong) %>%
  tally()

b=df %>%
  group_by(nganh) %>%
  tally()




