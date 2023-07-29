#000000000000000000000000000000000000000000000000000000000


# A. Sá»­ dá»¥ng thÆ° viá»n dplyr cá»§a R Äá» tráº£ lá»i cÃ¡c cÃ¢u há»i sau liÃªn quan Äáº¿n gÃ³i dá»¯ liá»u Births2006

#000000000000000000000000000000000000000000000000000000000


# 1. CÃ³ bao nhiÃªu tráº» sÆ¡ sinh lÃ  Nam, bao nhiÃªu lÃ  Ná»¯?
# 2. CÃ³ bao nhiÃªu phá»¥ sáº£n mÃ  chÃºng ta khÃ´ng biáº¿t trÃ¬nh Äá» há»c váº¥n?
# 3. CÃ³ bao nhiÃªu phá»¥ sáº£n sinh Ã­t hÆ¡n 3 bÃ© sÆ¡ sinh vÃ  sá»­ dá»¥ng phÆ°Æ¡ng phÃ¡p Äáº» tá»± nhiÃªn?
# 4. CÃ³ bao nhiÃªu tráº» sinh ra cÃ³ chá» sá» APGAR5 nhá» hÆ¡n 3 vÃ  cÃ³ cÃ¢n náº·ng lá»n hÆ¡n 2.5 kg? Trong sá» tráº» nÃ y thÃ¬ cÃ³ bao nhiÃªu tráº» sinh báº±ng phÆ°Æ¡ng phÃ¡p tá»± nhiÃªn, bao nhiÃªu sinh ra báº±ng phÆ°Æ¡ng phÃ¡p cáº¯t.
# 5. ThÃ¡ng nÃ o trong nÄm cÃ³ sá» lÆ°á»£ng tráº» sÆ¡n sinh nhiá»u nháº¥t?
# 6. CÃ¢u há»i phá»¥ (khÃ´ng nháº¥t thiáº¿t pháº£i tráº£ lá»i), cÃ¡c báº¡n cÃ³ tháº» váº½ biá»u Äá» Äá» thá» hiá»n dá»¯ liá»u cho 5 cÃ¢u há»i trÃªn khÃ´ng? --> sá»­ dá»¥ng biá»u Äá» barchart, histogram sá»­ dá»¥ng phá»¥ kiá»n ggplot2
# 7. TÃ¬m má»i tÆ°Æ¡ng quan giá»¯a cÃ¢n náº·ng cá»§a tráº» sÆ¡ sinh vá»i cÃ¡c biáº¿n cÃ²n láº¡i? CÃ¡c biáº¿n nÃ o cÃ¡c báº¡n sáº½ chá»n Äá» dÃ¹ng phÃ¢n tÃ­ch? CÃ¡c báº¡n cÃ³ káº¿t luáº­n gÃ¬ khÃ´ng sau khi cháº¡y phÃ¢n tÃ­ch vá»i há»i qui tuyáº¿n tÃ­nh chuáº©n?


library(dplyr)

df <-read.csv("../../Code/week2/Births/births2006.csv",header = TRUE, sep = ",")
df <- data.frame(df)


glimpse(df)

# 1. CÃ³ bao nhiÃªu tráº» sÆ¡ sinh lÃ  Nam, bao nhiÃªu lÃ  Ná»¯?

summary(df %>% select(SEX))

#cÃ¡ch 1
df %>%
  select(SEX) %>%
  count(SEX)


#cÃ¡ch 2

df %>%
  count(SEX)


# 2. CÃ³ bao nhiÃªu phá»¥ sáº£n mÃ  chÃºng ta khÃ´ng biáº¿t trÃ¬nh Äá» há»c váº¥n

# cÃ¡ch 1
df %>%
  filter(DMEDUC == "NULL")%>%
  count(DMEDUC)
# cÃ¡ch 2

df %>%
  filter(!DMEDUC %in% c("F","M"))%>%
  count(DMEDUC)


# 3. CÃ³ bao nhiÃªu phá»¥ sáº£n sinh Ã­t hÆ¡n 3 bÃ© sÆ¡ sinh vÃ  sá»­ dá»¥ng phÆ°Æ¡ng phÃ¡p Äáº» tá»± nhiÃªn?


df %>%
  select(DPLURAL,DMETH_REC)%>%
  filter(DPLURAL %in% c("1 Single", "2 Twin"), DMETH_REC == "Vaginal") 


# CÃ¡ch 1


glimpse(df %>%
          select(DPLURAL,DMETH_REC)%>%
          filter(DPLURAL %in% c("1 Single", "2 Twin"), DMETH_REC == "Vaginal") )

# CÃ¡ch 2

summary(df %>%
          select(DPLURAL,DMETH_REC)%>%
          filter(DPLURAL %in% c("1 Single", "2 Twin"), DMETH_REC == "Vaginal") )

# cÃ¡ch 3 ????

count(df %>%
        select(DPLURAL,DMETH_REC)%>%
        filter(DPLURAL %in% c("1 Single", "2 Twin"), DMETH_REC == "Vaginal"))



# 4. CÃ³ bao nhiÃªu tráº» sinh ra cÃ³ chá» sá» APGAR5 nhá» hÆ¡n 3 vÃ  cÃ³ cÃ¢n náº·ng lá»n hÆ¡n 2.5 kg? Trong sá» tráº» nÃ y thÃ¬ cÃ³ bao nhiÃªu tráº» sinh báº±ng phÆ°Æ¡ng phÃ¡p tá»± nhiÃªn, bao nhiÃªu sinh ra báº±ng phÆ°Æ¡ng phÃ¡p cáº¯t.

count(df %>% 
        filter(APGAR5 <3, DBWT > 2500))

df %>% 
  filter(APGAR5 <3, DBWT > 2500) %>%
  filter(DMETH_REC %in% c("C-section", "Vaginal")) %>%
  count(DMETH_REC, sort = FALSE)


# 5. ThÃ¡ng nÃ o trong nÄm cÃ³ sá» lÆ°á»£ng tráº» sÆ¡n sinh nhiá»u nháº¥t?

# cÃ¡ch 1
df1<-df %>%
  select(DOB_MM) %>%
  count(DOB_MM) 

which.max(df1$n)
which.min(df1$n)

# cÃ¡ch 2
library(ggplot2)

ggplot(df1, aes(x=DOB_MM, y=n)) + 
  geom_bar(stat = "identity") +
  labs(
    #   title = "Sá» lÆ°á»£ng tráº» sinh ra theo ngÃ y trong tuáº§n",
    y = "Sá» tráº» sÆ¡ sinh",
    x = "ThÃ¡ng trong nÄm"
  )

# cÃ¡ch 3

result <- df1 %>% 
  group_by(DOB_MM, n) %>%
  filter(DOB_MM == max(DOB_MM)) %>%
  arrange(desc(n),DOB_MM)

result


# 6. CÃ¢u há»i phá»¥ (khÃ´ng nháº¥t thiáº¿t pháº£i tráº£ lá»i), cÃ¡c báº¡n cÃ³ tháº» váº½ biá»u Äá» Äá» thá» hiá»n dá»¯ liá»u cho 5 cÃ¢u há»i trÃªn khÃ´ng? --> sá»­ dá»¥ng biá»u Äá» barchart, histogram sá»­ dá»¥ng phá»¥ kiá»n ggplot2
# biá»u Äá» váº½ tráº» sÆ¡ sinh theo giá»i tÃ­nh
df.sex <- df %>%
  count(SEX, sort = FALSE)
ggplot(df.sex, aes(x=SEX, y=n)) + 
  geom_bar(stat = "identity",color='red',fill='darkolivegreen1') +
  labs(
    title = "Sá» lÆ°á»£ng tráº» sinh theo giá»i tÃ­nh",
    y = "Sá» tráº» sÆ¡ sinh",
    x = "Giá»i tÃ­nh"
  )

# há»c váº¥n


df.hocvan <- df %>%
  select(DPLURAL,DMETH_REC)%>%
  filter(DPLURAL %in% c("1 Single", "2 Twin"), DMETH_REC == "Vaginal") %>%
  count(DPLURAL, sort = FALSE)

ggplot(df.hocvan, aes(x=DPLURAL, y=n)) + 
  geom_bar(stat = "identity",color='red',fill='darkolivegreen1') +
  labs(
    #   title = "Sá» lÆ°á»£ng tráº» sinh theo giá»i tÃ­nh",
    y = "Sá» tráº» sÆ¡ sinh",
    x = "Sinh ÄÃ´i vÃ  sinh 1"
  )


count(df %>%
        select(DPLURAL,DMETH_REC)%>%
        filter(DPLURAL %in% c("1 Single", "2 Twin"), DMETH_REC == "Vaginal"))

# 


df.moi <- df %>% 
  filter(APGAR5 <3, DBWT > 2500) %>%
  filter(DMETH_REC %in% c("C-section", "Vaginal")) %>%
  count(DMETH_REC, sort = FALSE)

ggplot(df.moi, aes(x=DMETH_REC, y=n)) + 
  geom_bar(stat = "identity",color='red',fill='darkolivegreen1') +
  labs(
    #   title = "Sá» lÆ°á»£ng tráº» sinh theo giá»i tÃ­nh",
    y = "Sá» tráº» sÆ¡ sinh",
    x = "PhÆ°Æ¡ng phÃ¡p sinh"
  )



ggplot(df1, aes(x=reorder(DOB_MM,-n), y=n)) + 
  geom_bar(stat = "identity") +
  labs(
    #   title = "Sá» lÆ°á»£ng tráº» sinh ra theo ngÃ y trong tuáº§n",
    y = "Sá» tráº» sÆ¡ sinh",
    x = "ThÃ¡ng trong nÄm"
  )





# 7. TÃ¬m má»i tÆ°Æ¡ng quan giá»¯a cÃ¢n náº·ng cá»§a tráº» sÆ¡ sinh vá»i cÃ¡c biáº¿n cÃ²n láº¡i? CÃ¡c biáº¿n nÃ o cÃ¡c báº¡n sáº½ chá»n Äá» dÃ¹ng phÃ¢n tÃ­ch? CÃ¡c báº¡n cÃ³ káº¿t luáº­n gÃ¬ khÃ´ng sau khi cháº¡y phÃ¢n tÃ­ch vá»i há»i qui tuyáº¿n tÃ­nh chuáº©n?

m1=lm(DBWT~.,data=df)
m1 # --> cÃ³ nháº­n xÃ©t gÃ¬ khÃ´ng?
summary(m1)


m1=lm(DBWT~DOB_MM+DOB_WK+MAGER+TBO_REC+WTGAIN+APGAR5+UPREVIS+ESTGEST,data=df)

summary(m1)


m1=lm(DBWT~MAGER+TBO_REC+WTGAIN+APGAR5+ESTGEST,data=df)

summary(m1)

df2 <- df %>% 
  select(DBWT,MAGER,TBO_REC,WTGAIN,APGAR5,ESTGEST)

m1=lm(DBWT~.,data=df2)
summary(m1)
cor(df2)

# váº¥n Äá» cá»§a chÃºng ta bÃ¢y giá» lÃ  gÃ¬?

summary(df2)

# --> cÃ³ ráº¥t nhiá»u sá» liá»u lÃ  khÃ´ng tá»n táº¡i -- cÃ³ áº£nh hÆ°á»ng gÃ¬ khÃ´ng? táº¥t nhiÃªn lÃ  cÃ³ rá»i

# https://www.programmingr.com/examples/remove-na-rows-in-r/


# na.omit() â remove rows with na from a list


df3 <- na.omit(df2)

nrow(df2)
nrow(df3)
nrow(df2) - nrow(df3)


m1=lm(DBWT~.,data=df3)
summary(m1)
cor(df3)

#000000000000000000000000000000000000000000000000000000000



# B. Sá»­ dá»¥ng thÆ° viá»n dplyr cá»§a R Äá» tráº£ lá»i cÃ¡c cÃ¢u há»i sau liÃªn quan Äáº¿n gÃ³i dá»¯ liá»u hanjinshipyard
# 
# 1. CÃ³ bao nhiÃªu háº¡ng má»¥c cáº§n sá»­a chá»¯a vÃ  nÃ¢ng cáº¥p trong vÃ²ng 5 nÄm tá»i?
#   
#   Äá»c dá»¯ liá»u
library(readxl)
df=read_excel("../../data/tdd/hanjinshipyard.xlsx",sheet="CAPEX",skip = 2) # Táº£i dá»¯ liá»u vÃ o R, Äá»c báº£ng tÃ­nh tab cÃ³ tÃªn CAPEX, bá» qua 2 dÃ²ng Äáº§u tiÃªn

df <- data.frame(df) # Chuyá»n Äá»i dá»¯ liá»u thÃ nh dáº¡ng khung
nrow(df)

nrow(df %>% 
       filter(InterYear <=5))

# cÃ²n cÃ³ cÃ¡ch nÃ o khÃ´ng?



#   2. Hiá»n táº¡i biáº¿n UnitCost cÃ³ giÃ¡ trá» tiá»n tá» lÃ  Peso --> thÃªm cá»t má»i vÃ o báº£ng sá»­ dá»¥ng lá»nh mutate() Äá» tÃ­nh giÃ¡ ÄÆ¡n vá» theo tiá»n VND, tá» giÃ¡ Peso/VND lÃ  444 VND.

df %>%
  mutate(giaVND = UnitCost*444)
# 3. ThÃªm má»t cá»t má»i tÃ­nh tá»ng chi phÃ­ theo VND cho tá»«ng háº¡ng má»¥c? Sá»­ dá»¥ng cÃ´ng thá»©c
# 

# Net Present Value (npv) = (giÃ¡ ÄÆ¡n vá» x khá»i lÆ°á»£ng) x exp(-rho x t)
# 
# Trong ÄÃ³ exp() lÃ  hÃ m exponential
# 
# rho lÃ  lÃ£i xuáº¥t ngÃ¢n hÃ ng, láº¥y trung bÃ¬nh lÃ  6% cho 1 nÄm.
# 
# t lÃ  nÄm tiáº¿n hÃ nh cÃ´ng viá»c liÃªn quan Äáº¿n tá»«ng háº¡ng má»¥c.


df %>%
  mutate(giaVND = UnitCost*444) %>%
  mutate(TongChiPhi = giaVND*Quantity*exp(-0.06*InterYear)) 



# 4. Sau khi lÃ m xong cÃ¢u há»i 3, cÃ¡c báº¡n cho biáº¿t tá»ng sá» tiá»n VND cáº§n bá» ra Äá» thá»±c hiá»n cÃ´ng tÃ¡c duy tuy vÃ  báº£o dÆ°á»¡ng nhÃ  mÃ¡y ÄÃ³ng tÃ u Hanjin?
#   

df %>%
  mutate(giaVND = UnitCost*444) %>%
  mutate(TongChiPhi = giaVND*Quantity*exp(-0.06*InterYear)) %>%
  summarise_all(sum)
# khÃ´ng ÄÆ°á»£c vÃ¬ cÃ¡c cá»t cÃ³ chá»¯


df %>%
  mutate(giaVND = UnitCost*444) %>%
  mutate(TongChiPhi = giaVND*Quantity*exp(-0.06*InterYear)) %>%
  select(TongChiPhi) %>%
  summarise_all(sum)



#   5. CÃ³ bao nhiÃªu háº¡ng má»¥c cÃ³ sá»± xuá»ng cáº¥p lá»n hÆ¡n 2 vÃ  cÃ³ rá»§i do nhá» hÆ¡n 3? tá»ng chi phÃ­ cho cÃ¡c háº¡ng má»¥c nÃ y lÃ  bao nhiÃªu?
#   
df %>%
  mutate(giaVND = UnitCost*444) %>%
  mutate(TongChiPhi = giaVND*Quantity*exp(-0.06*InterYear))%>%
  filter(States >2, Risk <3)   %>%
  select(TongChiPhi) %>%
  summarise_all(sum)



#   6. CÃ¡c báº¡n cÃ³ thá» váº½ biá»u Äá» (sá»­ dá»¥ng ggplot2) cho cÃ¡c cÃ¢u há»i trÃªn khÃ´ng?--> ÄÃ¢y lÃ  cÃ¢u há»i phá»¥
# 




# C. CÃ¢u há»i lÃ½ thuyáº¿t
# 
# 1. VÃ¬ sao chÃºng ta cáº§n pháº£i sá»­ dá»¥ng dá»¯ liá»u sáº¡ch (gá»n gÃ ng) Äá» phÃ¢n tÃ­ch?
# 2. Trong cÃ´ng viá»c hÃ ng ngÃ y cá»§a cÃ¡c báº¡n, náº¿u cÃ¡c báº¡n dÃ¹ng excel thÃ¬ cÃ³ nÃªn trá»n nhiá»u báº£ng vÃ o 1 sheet cá»§a excel khÃ´ng? náº¿u khÃ´ng thÃ¬ giáº£i thÃ­ch táº¡i sao?
# 3. CÃ¡c báº¡n cÃ³ nÃªn sá»­ dá»¥ng merge hÃ ng cá»t trong excel khÃ´ng? táº¡i sao? CÃ³ biá»n phÃ¡p gÃ¬ Äá» sá»­ lÃ½ váº¥n Äá» nÃ y khi dÃ¹ng excel
# 4. CÃ¡c báº¡n cÃ³ nÃªn sá»­ dá»¥ng subtotal trong báº£ng tÃ­nh excel khÃ´ng? Giáº£i thÃ­ch táº¡i sao.








