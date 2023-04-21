library(readxl) 
library(dplyr)
library(DT)
library(ggplot2)
library(gridExtra)
library(grid)
library(png)
library(downloader)
library(grDevices)
library(cowplot)
library(reshape)
library(lubridate)
library(tidyverse)
library(janitor)
Capex=read_excel("tdd.xlsx",sheet="Capex")
epsilon=1000
#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------

cs1 = aggregate(list(CS = Capex$States), list(Disciplines = factor(Capex$Disciplines)), mean,na.rm = TRUE) %>%
 drop_na(CS) 

plotcs1=ggplot(cs1, aes(x=reorder(Disciplines,-CS),y = CS)) +
  ylim(0, 5.2)+
  geom_bar(stat = "identity",fill = "#FF6666")+
  labs(title = "", x = "Disciplines",y = "States")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(Disciplines, CS+0.1, label = format(CS,digits=3), fill = NULL), data = cs1,cex=3)


plotcs1
#ggsave("picture/plotcs1.png",width = 8, height = 6)

risk1 = aggregate(list(Risk = Capex$Risk), list(Disciplines = factor(Capex$Disciplines)), mean,na.rm = TRUE) %>%
  drop_na(Risk)
plotrisk1=ggplot(risk1, aes(x=reorder(Disciplines,-Risk),y = Risk)) +
  geom_bar(stat = "identity",fill = "#0000FF")+
  ylim(0, 3.2)+
  labs(title = "", x = "Disciplines",y = "Risk")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(Disciplines, Risk+0.1, label = format(Risk,digits=3), fill = NULL), data = risk1,cex=3)
plotrisk1
#ggsave("picture/plotrisk1.png",width = 8, height = 6) This is optional

#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------


#Estimate the total values of CAPEX for each year with percentage of distribution

#Transform table by year




df<-Capex%>%
  select(InterYear,Disciplines,NPVCapex)%>%
  group_by(Disciplines)
df1<-melt(df,id=c("InterYear","Disciplines"))
df2<-cast(df1,Disciplines~InterYear,sum)
df3<-df2%>%
  mutate(immediate=df2[,2],shortterm=rowSums(df2[,c(3:4)]),mediumterm=rowSums(df2[,c(5:7)]),longterm=rowSums(df2[,c(8:12)]),CAPEX=rowSums(df2[,c(2:12)]))%>%
  mutate(freq = 100*CAPEX / sum(CAPEX))%>%
  arrange(desc(CAPEX))%>%
   adorn_totals()


x01 <- Capex %>%
  group_by(InterYear) %>%
  summarize(total = sum(NPVCapex/epsilon)) %>%
  arrange(desc(total))
x01=mutate(x01,weight_pct=100*total/sum(total))

#plot the graph for yearly CApex distribution per level 4
plot01 <- ggplot(Capex)+
  geom_bar(aes(x = InterYear, y = NPVCapex/epsilon,fill=Disciplines),
           stat='identity')+
  labs(title = "", x = "Year",y = "USD (1000)")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+ 
  geom_text(aes(InterYear, total+3, label = round(total,2), fill = NULL), data = x01,cex=3,angle=90)
plot01
#ggsave("picture/plot01.png",width = 10, height = 6)
#Estimate the total values of CAPEX for each generic with percentage of distribution
x11 <- Capex %>%
  group_by(Zone) %>%
  summarize(total = sum(NPVCapex/epsilon)) %>%
  arrange(desc(total))
x11=mutate(x11,weight_pct=100*total/sum(total))

#plot the graph for yearly CApex distribution per level 1
plot11<- ggplot(Capex)+
  geom_bar(aes(x = reorder(Zone, -NPVCapex/epsilon, sum), y = NPVCapex/epsilon,fill=Disciplines),
           stat='identity')+
  labs(title = "", x = "Year",y = "USD (1000)")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(Zone, total+3, label = round(total,2), fill = NULL), data = x11,cex=3,angle=0)

plot11
#ggsave("picture/plot11.png",width = 10, height = 6)

#Estimate the total values of CAPEX for each generic with percentage of distribution
x21 <- Capex %>%
  group_by(Facilities) %>%
  summarize(total = sum(NPVCapex/epsilon)) %>%
  arrange(desc(total))
x21=mutate(x21,weight_pct=100*total/sum(total))

#plot the graph for yearly CApex distribution per level 1
plot21<- ggplot(Capex)+
  geom_bar(aes(x = reorder(Facilities, -NPVCapex/epsilon, sum), y = NPVCapex/epsilon,fill=Disciplines),
           stat='identity')+
  labs(title = "", x = "Year",y = "USD (1000)")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(Facilities, total+3, label = round(total,2), fill = NULL), data = x21,cex=3,angle=90)

plot21
#ggsave("picture/plot21.png",width = 10, height = 6)

#Estimate the total values of CAPEX for each generic with percentage of distribution
x31 <- Capex %>%
  group_by(Facilities) %>%
  summarize(total = sum(NPVCapex/epsilon)) %>%
  arrange(desc(total))
x31=mutate(x31,weight_pct=100*total/sum(total))

#plot the graph for yearly CApex distribution per level 1
plot31<- ggplot(Capex)+
  geom_bar(aes(x = reorder(Facilities, -NPVCapex/epsilon, sum), y = NPVCapex/epsilon,fill=Tenant),
           stat='identity')+
  labs(title = "", x = "Year",y = "USD (1000)")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(Facilities, total+3, label = round(total,2), fill = NULL), data = x31,cex=3,angle=90)

plot31
#ggsave("picture/plot31.png",width = 10, height = 6)





