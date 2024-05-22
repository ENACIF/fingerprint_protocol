#Descriptive analysis 

library(readxl)
dir<-file.choose()

#Observer1_thumb
xlsx<-read_excel(dir,sheet=1,col_names=TRUE)
data1<-as.data.frame(xlsx)
head(data1)

#Observer1_index
xlsx<-read_excel(dir,sheet=2,col_names=TRUE)
data4<-as.data.frame(xlsx)
head(data4)

library(hrbrthemes)
library(ggplot2)
library(gridExtra)
library(grid)

#General data of the first iteration THUMB
P<-subset(data1,TOMA==1)

TIPO<-(table(P$T.FUN)/18)*100
TIPO_SEX<-(table(P$T.FUN,P$SEX)/18)*100

summary(P[,8:127])
BI<-data.frame(BI=rowSums(P[,8:15]))
CO<-data.frame(CO=rowSums(P[,16:23]))
IS<-data.frame(IS=rowSums(P[,24:31]))
EN<-data.frame(EN=rowSums(P[,32:31]))
IN<-data.frame(IN=rowSums(P[,40:47]))
VU<-data.frame(VU=rowSums(P[,48:55]))
TR<-data.frame(TR=rowSums(P[,56:63]))
TI<-data.frame(TI=rowSums(P[,64:71]))
CU<-data.frame(CU=rowSums(P[,72:79]))
SE<-data.frame(SE=rowSums(P[,80:87]))#10
DE<-data.frame(DE=rowSums(P[,88:95]))
EM<-data.frame(EM=rowSums(P[,96:103]))
MS<-data.frame(MS=rowSums(P[,104:111]))
INS<-data.frame(INS=rowSums(P[,112:119]))
IT<-data.frame(IT=rowSums(P[,120:127]))
measure<-c("BI","CO","IS","EN","IN","VU","TR","TI","CU","SE","DE","EM","MS","INS","IT")
dcm<-data.frame(P[,1:7],BI,CO,IS,EN,IN,VU,TR,TI,CU,SE,DE,EM,MS,INS,IT)

T2<-subset(dcm,T.FUN=="2.- Presilla interna")

median_sex<-T2[,8:22]
c<-data.frame(rep(0,15))
das<-t(median_sex)
T_median<-data.frame(das,c)
colnames(T_median)<-c("Male","Female")
Minutiae<-rownames(T_median)
nam<-c("Bifurcation","End","Dock","Overlap","Bridge","Enclosure","Break","Dot_in_ridge","Fragment","Dot_between_ridge","M","Opposite_bifurcation","Trifurcation","Crossbar","Return")
Dats<-data.frame(T_median,Minutiae,nam)

p <- ggplot(Dats) +
  geom_col(aes(x = nam, y = Male), 
           fill = rgb(0.3,0.8,0.8,0.5), width = 0.4) +
  geom_col(aes(x = nam, y = Female), 
           alpha = 0.3, fill = 'gray40', width = 0.8) +
  labs(title="a) Right-slanted Loop",x = "", y = "Median counts") +
  theme_minimal()+ylim(0,30)

p2<-p+coord_flip()+theme_ipsum(axis_title_size = 12)

T3<-subset(dcm,T.FUN=="3.- Presilla externa")
median_sex<-aggregate(T3[,measure], list(T3[,"SEX"]), median,na.rm=TRUE)

T_median<-t(data.frame(median_sex[,2:16]))
colnames(T_median)<-c("Male","Female")
Minutiae<-rownames(T_median)
nam<-c("Bifurcation","End","Dock","Overlap","Bridge","Enclosure","Break","Dot_in_ridge","Fragment","Dot_between_ridge","M","Opposite_bifurcation","Trifurcation","Crossbar","Return")
Dats<-data.frame(T_median,Minutiae,nam)

p <- ggplot(Dats) +
  geom_col(aes(x = nam, y = Male), 
           fill = rgb(0.3,0.8,0.8,0.5), width = 0.4) +
  geom_col(aes(x = nam, y = Female), 
           alpha = 0.3, fill = 'gray40', width = 0.8) +
  labs(title="b) Left-slanted Loop",x = "", y = "Median counts") +
  theme_minimal()+ylim(0,30)#+theme(axis.text.y = element_text(size = 13, hjust = 1))

p3<-p+coord_flip()+theme_ipsum(axis_title_size = 12)


T4<-subset(dcm,T.FUN=="4.- Verticilo")
median_sex<-aggregate(T4[,measure], list(T4[,"SEX"]), median,na.rm=TRUE)

T_median<-t(data.frame(median_sex[,2:16]))
colnames(T_median)<-c("Male","Female")
Minutiae<-rownames(T_median)
nam<-c("Bifurcation","End","Dock","Overlap","Bridge","Enclosure","Break","Dot_in_ridge","Fragment","Dot_between_ridge","M","Opposite_bifurcation","Trifurcation","Crossbar","Return")
Dats<-data.frame(T_median,Minutiae,nam)


p <- ggplot(Dats) +
  geom_col(aes(x = nam, y = Male), 
           fill = rgb(0.3,0.8,0.8,0.5), width = 0.4) +
  geom_col(aes(x = nam, y = Female), 
           alpha = 0.3, fill = 'gray40', width = 0.8) +
  labs(title="c) Whorl",x = "", y = "Median counts") +
  theme_minimal()+ylim(0,30)

p4<-p+coord_flip()+theme_ipsum(axis_title_size = 12)

dev.new()

plot1<-grid.arrange(p2, p3, p4, ncol = 3, nrow = 1,top = grid::textGrob("1) Thumb right",gp=gpar(fontsize=16,font=2), x = 0, hjust = 0))

#General data of the first iteration INDEX

P<-subset(data4,TOMA==1)

TIPO<-(table(P$T.FUN)/18)*100
TIPO_SEX<-(table(P$T.FUN,P$SEX)/18)*100

summary(P[,8:127])
BI<-data.frame(BI=rowSums(P[,8:15]))
CO<-data.frame(CO=rowSums(P[,16:23]))
IS<-data.frame(IS=rowSums(P[,24:31]))
EN<-data.frame(EN=rowSums(P[,32:31]))
IN<-data.frame(IN=rowSums(P[,40:47]))
VU<-data.frame(VU=rowSums(P[,48:55]))
TR<-data.frame(TR=rowSums(P[,56:63]))
TI<-data.frame(TI=rowSums(P[,64:71]))
CU<-data.frame(CU=rowSums(P[,72:79]))
SE<-data.frame(SE=rowSums(P[,80:87]))
DE<-data.frame(DE=rowSums(P[,88:95]))
EM<-data.frame(EM=rowSums(P[,96:103]))
MS<-data.frame(MS=rowSums(P[,104:111]))
INS<-data.frame(INS=rowSums(P[,112:119]))
IT<-data.frame(IT=rowSums(P[,120:127]))
measure<-c("BI","CO","IS","EN","IN","VU","TR","TI","CU","SE","DE","EM","MS","INS","IT")
dcm<-data.frame(P[,1:7],BI,CO,IS,EN,IN,VU,TR,TI,CU,SE,DE,EM,MS,INS,IT)

T1<-subset(dcm,T.FUN=="1.- Arco")
median_sex<-aggregate(T1[,measure], list(T1[,"SEX"]), median,na.rm=TRUE)

T_median<-t(data.frame(median_sex[,2:16]))
colnames(T_median)<-c("Male","Female")
Minutiae<-rownames(T_median)
nam<-c("Bifurcation","End","Dock","Overlap","Bridge","Enclosure","Break","Dot_in_ridge","Fragment","Dot_between_ridge","M","Opposite_bifurcation","Trifurcation","Crossbar","Return")
Dats<-data.frame(T_median,Minutiae,nam)


p <- ggplot(Dats) +
  geom_col(aes(x = nam, y = Male), 
           fill = rgb(0.6,0.4,0.9,0.5), width = 0.4) +
  geom_col(aes(x = nam, y = Female), 
           alpha = 0.3, fill = 'gray40', width = 0.8) +
  labs(title="a) Arch",x = "", y = "Median counts") +
  theme_minimal()+ylim(0,30)

p1<-p+coord_flip()+theme_ipsum(axis_title_size = 12)

T3<-subset(dcm,T.FUN=="3.- Presilla externa")
median_sex<-aggregate(T3[,measure], list(T3[,"SEX"]), median,na.rm=TRUE)

T_median<-t(data.frame(median_sex[,2:16]))
colnames(T_median)<-c("Male","Female")
Minutiae<-rownames(T_median)
nam<-c("Bifurcation","End","Dock","Overlap","Bridge","Enclosure","Break","Dot_in_ridge","Fragment","Dot_between_ridge","M","Opposite_bifurcation","Trifurcation","Crossbar","Return")
Dats<-data.frame(T_median,Minutiae,nam)

p <- ggplot(Dats) +
  geom_col(aes(x = nam, y = Male), 
           fill = rgb(0.6,0.4,0.9,0.5), width = 0.4) +
  geom_col(aes(x = nam, y = Female), 
           alpha = 0.3, fill = 'gray40', width = 0.8) +
  labs(title="b) Left-slanted Loop",x = "", y = "Median counts") +
  theme_minimal()+ylim(0,30)

p3<-p+coord_flip()+theme_ipsum(axis_title_size = 12)


T4<-subset(dcm,T.FUN=="4.- Verticilo")
median_sex<-aggregate(T4[,measure], list(T4[,"SEX"]), median,na.rm=TRUE)

T_median<-t(data.frame(median_sex[,2:16]))
colnames(T_median)<-c("Male","Female")
Minutiae<-rownames(T_median)
nam<-c("Bifurcation","End","Dock","Overlap","Bridge","Enclosure","Break","Dot_in_ridge","Fragment","Dot_between_ridge","M","Opposite_bifurcation","Trifurcation","Crossbar","Return")
Dats<-data.frame(T_median,Minutiae,nam)

p <- ggplot(Dats) +
  geom_col(aes(x = nam, y = Male), 
           fill = rgb(0.6,0.4,0.9,0.5), width = 0.4) +
  geom_col(aes(x = nam, y = Female), 
           alpha = 0.3, fill = 'gray40', width = 0.8) +
  labs(title="c) Whorl",x = "", y = "Median counts") +
  theme_minimal()+ylim(0,30)

p4<-p+coord_flip()+theme_ipsum(axis_title_size = 12)

dev.new()
library("gridExtra")
plot2<-grid.arrange(p1, p3, p4, ncol = 3, nrow = 1,top = grid::textGrob("2) Index right",gp=gpar(fontsize=16,font=2), x = 0, hjust = 0))

#GENERAL PLOT 
dev.new()
plot<-grid.arrange(plot1, plot2, nrow=2)

