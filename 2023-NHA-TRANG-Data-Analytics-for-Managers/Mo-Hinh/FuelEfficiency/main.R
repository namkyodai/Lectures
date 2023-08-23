## Nhập Dữ Liệu từ file csv trên mạng
df <- data.frame(read.csv("https://www.biz.uiowa.edu/faculty/jledolter/DataMining/FuelEfficiency.csv"))
#FuelEff <- read.csv("FuelEfficiency.csv")

#GPM-->Gallon trên 100 dặm
#MPG--> Số Dặm đường/ Gallons
#WT --> Trọng lượng của xe
#DIS --> Lượng Thải Khí
#NC --> Số lượng xi lanh
#HP --> Mã Lực
#ACC --> Gia Tốc
# ET --> Loại Máy 

#----------------------------
plot.new()
#par(mfrow=c(1,1))
par(mar=c(4,4,1,1)+0.1,mfrow=c(3,3),bg="white",cex = 1, cex.main = 1)
plot(GPM~MPG,data=df)
plot(GPM~WT,data=df)
plot(GPM~DIS,data=df)
plot(GPM~NC,data=df)
plot(GPM~HP,data=df)
plot(GPM~ACC,data=df)
plot(GPM~ET,data=df)




dev.copy(png,'fueleff_xyplot.png',width = 800, height = 800)
dev.off()

#Vẽ sử dụng gói ggplot2
library(ggplot2)
library(dplyr)
require(gridExtra)
library(ggpubr)
plot.new()
#par(mfrow=c(1,1))

plot01 <- df%>%
  ggplot(aes(x=MPG, y=GPM))+
  geom_point()

plot02 <- df%>%
  ggplot(aes(x=WT, y=GPM))+
  geom_point()

plot03 <- df%>%
  ggplot(aes(x=DIS, y=GPM))+
  geom_point()

plot04 <- df%>%
  ggplot(aes(x=NC, y=GPM))+
  geom_point()

plot05 <- df%>%
  ggplot(aes(x=HP, y=GPM))+
  geom_point()

plot06 <- df%>%
  ggplot(aes(x=ACC, y=GPM))+
  geom_point()

plot07 <- df%>%
  ggplot(aes(x=ET, y=GPM))+
  geom_point()

#Cách thứ 1 để vẽ toàn bộ biểu đồ trong một layout
grid.arrange(                       # Dòng đầu tiên với 2 biểu đồ biểu diễn ở 2 cột
             arrangeGrob(plot01, plot02, plot03, plot04, plot05, plot06, plot07, ncol = 3), # Dòng thứ 2 với 2 biểu đồ được thể hiện ở 2 cột
             nrow = 1) 


#way 2 to draw all graphs in one pallete
grid.arrange(plot01, plot02, plot03, plot04, plot05, plot06, plot07, ncol = 3, nrow = 3)


df=df[-1] #Bỏ qua cột dữ liệu về MPG
df

## Hồi qui với toàn bộ các biến và dữ liệu
m1=lm(GPM~.,data=df)
summary(m1)

cor(df) #Mối tương quan

## Chọn các dữ liệu con và tiến hành hồi qui trên tập con đó bằng việc sử dụng thư viện leaps
library(leaps)

X=df[,2:7]
y=df[,1]

#https://www.rdocumentation.org/packages/leaps/versions/2.1-1/topics/regsubsets


out=summary(regsubsets(X,y,nbest=2,nvmax=ncol(X)))
tab=cbind(out$which,out$rsq,out$adjr2,out$cp)
tab

# for cp --> Mallow CP -->https://www.statology.org/mallows-cp/#:~:text=Mallows'%20Cp%20is%20a%20metric,model%20among%20several%20different%20models.&text=where%3A,the%20model%20(estimated%20by%20MSE)




m2=lm(GPM~WT,data=df)
summary(m2)




## Thiết lập đánh giá chéo (bỏ một biến ra ngoài) cho mô hình trên 6 biến
n=length(df$GPM) # tính toán có bao nhiêu số lượng quan sát trong gói dữ liệu
diff=dim(n)
percdiff=dim(n)

for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  ## Biểu diễn R "train1[train1!=k]" lấy ra dữ liệu train1 trong 
  ## các thành phần khác với k và được lưu trữ ở train
  ## Với k=1, train bao gồm các thành phần khác nhau tính từ thành phần thứ 1
  ## is 2, 3, …, n.
  m1=lm(GPM~.,data=df[train,])
  pred=predict(m1,newdat=df[-train,]) #thêm vào dữ liệu mới
  obs=df$GPM[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error

## cross-validation (leave one out) for the model on weight only
n=length(df$GPM)
  
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m2=lm(GPM~WT,data=df[train,])
  pred=predict(m2,newdat=df[-train,])
  obs=df$GPM[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error

