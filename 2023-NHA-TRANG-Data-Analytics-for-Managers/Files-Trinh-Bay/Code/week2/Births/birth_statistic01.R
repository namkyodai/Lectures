#Graphs are saved in GITHUB
#https://raw.githubusercontent.com/namkyodai/BusinessAnalytics/master/Births/XXX.png

library(lattice)
# library(nutshell.bbdb)
# library(nutshell.audioscrobbler)
# library(nutshell)
#data(births2006.smpl)
#write.csv(births2006.smpl, file = "births2006.csv")

df <-read.csv("births2006.csv",header = TRUE, sep = ",")

df <- data.frame(df)


#https://rdrr.io/cran/nutshell/man/births2006.smpl.html
#R In a Nutshell: A Desktop Quick Reference (Adler, 2009).

##

# DOB_MM
# Month of date of birth/ Tháng sinh
#
# DOB_WK
# Day of week of birth/Ngày trong tuần sinh
#
# MAGER
# Mother's age / tuổi của mẹ
#
# TBO_REC
# Total birth order / Thứ tự sinh
#
# WTGAIN
# Weight gain by mother / Cân nặng của Mẹ
#
# SEX
# a factor with levels F M, representing the sex of the child / Giới tính của trẻ
#
# APGAR5
# APGAR score / Chỉ số APGAR, đây là chỉ số kiểm tra nhanh trong vòng 1 đến 5 phút khi trẻ chào đời. CHỉ số càng cao thì càng tốts
#https://vi.wikipedia.org/wiki/Ch%E1%BB%89_s%E1%BB%91_Apgar

#
# DMEDUC
# Mother's education level / trình độ học vấn phụ sản

# UPREVIS
# Number of prenatal visits / số buổi phụ sản đi gặp bác sĩ để tư vấn khám trước khi sinh
#
# ESTGEST
# Estimated weeks of gestation / ước tính số tuần thai kỳ
#
# DMETH_REC
# Delivery Method / phương pháp sinh
#
# DPLURAL
# "Plural Births;" levels include 1 Single, 2 Twin, 3 Triplet, 4 Quadruplet, and 5 Quintuplet or higher / số tẻ được sinh ra (đơn, sinh đôi, sinh ba)
#
# DBWT
# Birth weight, in grams / cân nặng trẻ, tính trên đơn vị gram

###




df[1:5,]

library(dplyr)

glimpse(df)

# xem có bao nhiêu dữ liệu

dim(df)


#
births.dow=table(df$DOB_WK)




#print(births.dow)
births.dow

barchart(births.dow,ylab="Ngày trong tuần",xlab="Tuần xuất",col="black")
dev.copy(png,'births_dow.png')
dev.off()



#Muốn xem thêm về phương pháp/biện pháp sinh

dob.dm.tbl=table(WK=df$DOB_WK,MM=df$DMETH_REC)
dob.dm.tbl

# bỏ đi phần unknown
dob.dm.tbl=dob.dm.tbl[,-2]
dob.dm.tbl


#trellis.device()
barchart(dob.dm.tbl,ylab="Day of Week")
dev.copy(png,'births_dow_dmeth_rec.png')
dev.off()

# vẽ biểu diễn phân bổ, theo dạng cột
barchart(dob.dm.tbl,horizontal=F,groups=FALSE, xlab="Ngày trong tuần",ylab="Tuần xuất",col="black")
dev.copy(png,'births_dow_dmeth_rec_fre.png')
dev.off()


#vẽ biểu đồ phân bổ
histogram(~DBWT|DPLURAL,data=df,layout=c(1,5), col="black")
dev.copy(png,'births_dplural_dis.png')
dev.off()




histogram(~DBWT|DMETH_REC,data=births2006.smpl,layout=c(1,3), col="black")
dev.copy(png,'births_dmethrec_dis.png')
dev.off()



densityplot(~DBWT|DPLURAL,data=df,layout=c(1,5), plot.points=FALSE,col="black")
dev.copy(png,'births_dmethrec_density.png')
dev.off()


densityplot(~DBWT,groups=DPLURAL,data=births2006.smpl, plot.points=FALSE)
dev.copy(png,'births_DPLURAL_densityall.png')
dev.off()



dotplot(~DBWT|DPLURAL,data=births2006.smpl,layout=c(1,5), plot.points=FALSE,col="black")
dev.copy(png,'births_DPLURAL_dotplot.png')
dev.off()



xyplot(DBWT~DOB_WK,data=births2006.smpl,col="black")
dev.copy(png,'births_DBWTDOB_xy.png')
dev.off()



xyplot(DBWT~DOB_WK|DPLURAL,data=births2006.smpl,layout=c(1,5), col="black")
dev.copy(png,'births_DBWTDOB_xydplural.png')
dev.off()



xyplot(DBWT~WTGAIN,data=births2006.smpl,col="black")
dev.copy(png,'births_DBWTwtgain_xy.png')
dev.off()



xyplot(DBWT~WTGAIN|DPLURAL,data=births2006.smpl,layout=c(1,5), col="black")
dev.copy(png,'births_DBWTwtgaindplural_xy.png')
dev.off()



smoothScatter(births2006.smpl$WTGAIN,births2006.smpl$DBWT)
dev.copy(png,'births_WTGAIN_scatter.png')
dev.off()



## boxplot is the command for a box plot in the standard graphics
## package
boxplot(DBWT~APGAR5,data=births2006.smpl,ylab="DBWT", xlab="AGPAR5")
dev.copy(png,'births_PGAR5box.png')
dev.off()


boxplot(DBWT~DOB_WK,data=births2006.smpl,ylab="DBWT", xlab="Day of Week")
dev.copy(png,'births_DOB_WKbox.png')
dev.off()


## bwplot is the command for a box plot in the lattice graphics
## package. There you need to declare the conditioning variables
## as factors
bwplot(DBWT~factor(APGAR5)|factor(SEX),data=births2006.smpl,
        xlab="AGPAR5")
dev.copy(png,'births_APGAR5bwplot.png')
dev.off()



bwplot(DBWT~factor(DOB_WK),data=births2006.smpl,
        xlab="Day of Week")
dev.copy(png,'births_DOB_WKbwplot.png')
dev.off()



fac=factor(births2006.smpl$DPLURAL)
res=births2006.smpl$DBWT
t4=tapply(res,fac,mean,na.rm=TRUE)

t4

t5=tapply(births2006.smpl$DBWT,INDEX=list(births2006.smpl$DPLURAL,
                                           births2006.smpl$SEX),FUN=mean,na.rm=TRUE)

barplot(t4,ylab="DBWT")
dev.copy(png,'births_DBWTt4barplot.png')
dev.off()

barplot(t5,beside=TRUE,ylab="DBWT")
dev.copy(png,'births_DBWTt5barplot.png')
dev.off()


t5=table(births2006.smpl$ESTGEST)


new=births2006.smpl[births2006.smpl$ESTGEST != 99,]

t51=table(new$ESTGEST)
t51

t6=tapply(new$DBWT,INDEX=list(cut(new$WTGAIN,breaks=10),
                               cut(new$ESTGEST,breaks=10)),FUN=mean,na.rm=TRUE)
t6
levelplot(t6,scales = list(x = list(rot = 90)))
dev.copy(png,'births_levelplott6.png')
dev.off()


contourplot(t6,scales = list(x = list(rot = 90)))
dev.copy(png,'births_contourplott6.png')
dev.off()
