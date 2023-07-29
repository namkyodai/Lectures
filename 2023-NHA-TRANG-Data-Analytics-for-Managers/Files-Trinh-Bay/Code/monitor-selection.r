# Selecting a Monitor for our Class.
library(purrr)
# reading data file
library(readxl) 
library(dplyr)
df=read_excel("../Data/students.xlsx",sheet="student",skip = 0)

l <- length(df$Stt) #Data Frame (df)

df1 <- replicate(sample(1:l, 1, replace = FALSE), n = 10000)
df1 <- data.frame(df1)
df2 <- data.frame(table(df1))
which.max(df2$Freq)


df4 <- df %>%
  select(c(1,2,3,5,7,13,14)) %>%
  mutate(freq = df2$Freq) 

names(df4) <- c('ID', 'lastname', 'firstname', 'age', 'sex', 'field', 'fullname', 'freq')

df4 <- data.frame(df4)

library(ggChernoff)
library(ggplot2)

ggplot(df4) +
  aes(age, freq, fill = field) +
  geom_chernoff()+
  labs(y = "Số phiếu bầu", x ="Độ Tuổi")+ 
  geom_text(aes(label=paste(lastname,firstname)), position=position_dodge(width=1), vjust=2, cex =2.5)


ggplot(df4) +
  aes(age, freq, fill = sex) +
  geom_chernoff()+
  labs(y = "Số phiếu bầu", x ="Độ Tuổi")+ 
  geom_text(aes(label=paste(lastname,firstname)), position=position_dodge(width=1), vjust=2, cex =2.5)

summary(df4$age)

cat(" Chúc mừng bạn", paste(df$'Họ'[which.max(df2$Freq)]), paste(df$'Tên'[which.max(df2$Freq)]))



p.height <- c(180, 155, "aaa", 167, 181)
p.weight <- c(65, 50, 52, 58, 70)
p.names <- c("Joanna", "Charlotte", "Helen", "Karen", "Amy")

dataf <- data.frame(height = p.height, weight = p.weight, names = p.names)
dataf

