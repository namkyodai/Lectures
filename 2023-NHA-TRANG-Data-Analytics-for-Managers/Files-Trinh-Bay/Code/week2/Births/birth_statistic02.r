#Graphs are saved in GITHUB
library(ggplot2)
library(dplyr)
library(tidyr)

df <-read.csv("births2006.csv",header = TRUE, sep = ",")
df <-data.frame(df)

glimpse(df)

df %>%
  select(DOB_WK,DMETH_REC,WTGAIN,DBWT)

df %>%
  filter(DMETH_REC =="Vaginal"|DMETH_REC =="C-section")


df %>%
  filter(DMETH_REC %in% c("Vaginal","C-section"))

df %>%
  arrange(desc(DBWT))

df %>%
  mutate(cotmoi01 = DBWT*DOB_WK,cotmoi02 = "abc" )


df %>%
  select(DOB_WK,DMETH_REC,WTGAIN,DBWT) %>%
  filter(DMETH_REC == "Vaginal") %>%
  filter(DOB_WK %in% c(2,3,4,5,6)) %>%
  filter(DOB_WK >= 3)
  

df %>%
  select(DOB_WK,DMETH_REC,WTGAIN,DBWT) %>%
  filter(DMETH_REC == "Vaginal", DOB_WK %in% c(2,3,4,5,6)) %>%
  arrange(desc(DBWT))




#select()
#filter()
#arrange()
#mutate() 







# So luong tre so sinh theo ngay trong tuan
df.dow <- df %>%
  count(DOB_WK, sort = FALSE)

# ve bieu do barchart
# https://www.r-graph-gallery.com/barplot.html  --> 
# http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations  --> tỷ lệ hiển thị ở các trục
#https://ggplot2.tidyverse.org/reference/labs.html  --> Đặt tên các biến trên đồ thị
ggplot(df.dow, aes(x=DOB_WK, y=n)) + 
  geom_bar(stat = "identity",color='red',fill='darkolivegreen1') +
  labs(
    title = "Số lượng trẻ sinh ra theo ngày0 trong tuần",
    y = "Số trẻ sơ sinh",
    x = "Ngày trong tuần"
  )


ggplot(df %>%
         count(DOB_WK, sort = FALSE), 
       aes(x=DOB_WK, y=n)) + 
  geom_bar(stat = "identity",color='skyblue',fill='steelblue') +
  labs(
    title = "Số lượng trẻ sinh ra theo ngày trong tuần",
    y = "Số trẻ sơ sinh",
    x = "Ngày trong tuần"
  )






ggplot(df.dow, aes(x=reorder(DOB_WK,-n), y=n)) + 
  geom_bar(stat = "identity") +
  labs(
    title = "Số lượng trẻ sinh ra theo ngày trong tuần",
    y = "Số trẻ sơ sinh",
    x = "Ngày trong tuần"
  )


ggplot(df.dow, aes(x=DOB_WK, y=n)) + 
  geom_bar(stat = "identity",color='skyblue',fil07l='steelblue') +
  scale_x_continuous(name="Ngày trong tuần", breaks = seq(1,7,1))+
  scale_y_continuous(name="Số trẻ sơ sinh")+
  coord_flip()

ggplot(df.dow, aes(x=DOB_WK, y=n)) + 
  geom_bar(stat = "identity",color='skyblue',fill='steelblue') +
  scale_x_continuous(name="Ngày trong tuần", breaks = seq(1,7,1))+
  scale_y_continuous(name="Số trẻ sơ sinh")+
  coord_flip()


a<-df %>%
  select(DOB_WK,DMETH_REC)%>%
  count(DOB_WK,DMETH_REC, sort = FALSE)%>%
   group_by(DOB_WK,DMETH_REC)

library(reshape)
library(reshape2)

dcast(data = a, DMETH_REC ~ DOB_WK, 
      value.var = "n")

a1<-dcast(data = a, DOB_WK ~ DMETH_REC, 
         value.var = "n")

melt(a1,id = c("DOB_WK"),variable.name = "DMETH_REC",value.name = "n")

#plot between Csection and Vaginal



b <- a%>%
 # select(DOB_WK,DMETH_REC)%>%
  filter(DMETH_REC == "C-section"|DMETH_REC == "Vaginal")
  

#b <- df%>%
#  select(DOB_WK,DMETH_REC)%>%
#  filter(DMETH_REC %in% c("C-section","Vaginal"))

##ve bieu do voi ggplot2

#ggplot(b, aes(fill=DMETH_REC, y=n, x=DOB_WK)) + 
 # geom_bar(position="dodge", stat="identity")

ggplot(b, aes(fill=DMETH_REC, y=n, x=DOB_WK)) + 
  geom_bar(position="stack", stat="identity")

ggplot(b, aes(fill=DMETH_REC, y=n, x=DOB_WK)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()


ggplot(b, aes(fill=DMETH_REC, y=n, x=reorder(DOB_WK,n))) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()

## pleet plot ggplots










