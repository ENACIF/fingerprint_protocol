#-------------------------------------------------------------
#Data
#-------------------------------------------------------------

library(readxl)
dir<-file.choose()
#Records_RIGHT
#Observer1_thumb 
xlsx<-read_excel(dir,sheet=1,col_names=TRUE)
data1<-as.data.frame(xlsx)
head(data1)

#Self-report_thumb
xlsx<-read_excel(dir,sheet=3,col_names=TRUE)
data2<-as.data.frame(xlsx)
head(data2)

#Observer2_thumb 
xlsx<-read_excel(dir,sheet=5,col_names=TRUE)
data3<-as.data.frame(xlsx)
head(data3)

#Observer1_index
xlsx<-read_excel(dir,sheet=2,col_names=TRUE)
data4<-as.data.frame(xlsx)
head(data4)

#Self-report_index
xlsx<-read_excel(dir,sheet=4,col_names=TRUE)
data5<-as.data.frame(xlsx)
head(data5)

#Observer2_index
xlsx<-read_excel(dir,sheet=6,col_names=TRUE)
data6<-as.data.frame(xlsx)
head(data6)


#Records_LEFT
#Observer1_thumb
dir1<-file.choose()
xlsx1<-read_excel(dir1,sheet=1,col_names=TRUE)
data7<-as.data.frame(xlsx1)
head(data7)
#Observer2_index
xlsx2<-read_excel(dir1,sheet=2,col_names=TRUE)
data8<-as.data.frame(xlsx2)
head(data8)

#-------------------------------------------------------------
#Error Intra-observer
#-------------------------------------------------------------

library("irr")
res<-function(data){
j<-1
i <- 1
mat<-matrix(0,18,6)
while(i <=nrow(data)){
	a<-i
	b<-i+1
	x1 <- data[c(a,b),]
	dat<-x1[,8:127]
	datt<-t(dat)
	M<-cor(datt,method = "spearman")
	mat[j,1]<-x1[1,"ID00"]
	mat[j,2]<-x1[1,"DEDO"]
	mat[j,3]<-round(M[1,2],digits=4)
	sum=0
	for (k in 1:nrow(datt)){
		if(datt[k,1]==datt[k,2]){
		 sum=sum+1
		}
	}
	mat[j,4]<-sum
	mat[j,5]<-round(sum/120,digits=4)
	a<-icc(datt, model = "twoway",type = "agreement", unit = "single")
	mat[j,6]<-round(as.numeric(a[7]),digits=2)
	i=b+1
	j=j+1
}

colnames(mat)<-c("ID","Dedo","Rho","Acuerdo","A.Observado","ICC")

dats1<-as.data.frame(mat)
}

a_p<-res(data1)
a_i<-res(data4)
b_p<-res(data2)
b_i<-res(data5)
c_p<-res(data3)
c_i<-res(data6)
c_p2<-res(data7)
c_i2<-res(data8)

#Plot

Pulgar_icc<-data.frame(Observer_1=a_p[6],Observer_2=c_p[6],Auto_report=b_p[6])
names(Pulgar_icc)<-c("Observer_1","Observer_2","Auto_report")
for(i in ncol(Pulgar_icc)){
	Pulgar_icc[,i]<-as.numeric(Pulgar_icc[,i])
}
Pulgar_icc$ID<-a_p[,1]

Indice_icc<-data.frame(Observer_1=a_i[6],Observer_2=c_i[6],Auto_report=b_i[6])
names(Indice_icc)<-c("Observer_1","Observer_2","Auto_report")
for(i in ncol(Indice_icc)){
	Indice_icc[,i]<-as.numeric(Indice_icc[,i])
}
Indice_icc$ID<-a_i[,1]

library(ggplot2)
library(dplyr)
library(hrbrthemes)

for(i in 1:3){
  Pulgar_icc[,i]<-as.numeric(Pulgar_icc[,i])
}
for(i in 1:3){
  Indice_icc[,i]<-as.numeric(Indice_icc[,i])
}

p<-ggplot(Pulgar_icc) + ggtitle("a) Right thumb")+
  geom_segment( aes(x=ID, xend=ID, y=0.20, yend=1),color="gray90") +
  geom_point( aes(x=ID, y=Observer_1), color=rgb(0.3,0.8,0.8,0.5), size=4 ) +
  geom_point( aes(x=ID, y=Observer_2), color=rgb(0.6,0.4,0.9,0.5), size=4 ) +
  geom_point( aes(x=ID, y=Auto_report), color=rgb(0.6,0.7,0.9,0.5), size=4 ) +
  coord_flip()+
  theme_ipsum() +
  theme(legend.position = "none",axis.text.x = element_text(size = 13, hjust = 1),axis.text.y = element_text(size = 13, hjust = 1)) +
  xlab("") + ylab("")

p1<-p+geom_hline(yintercept=0.50, linetype="dashed", color = "gray30", size=.5)+geom_hline(yintercept=0.75, linetype="dashed", color = "gray30", size=.5)+geom_hline(yintercept=0.90, linetype="dashed", color = "gray30", size=.5)+
  geom_text(aes(0, 0.375, label = "Poor", vjust = -48.5))+geom_text(aes(0, 0.625, label = "Moderate", vjust = -48.5))+geom_text(aes(0, 1, label = "Excellent ", vjust = -48.5))+geom_text(aes(0, 0.825, label = "Good", vjust = -48.5))


p<-ggplot(Indice_icc) + ggtitle("b) Right index")+
  geom_segment( aes(x=ID, xend=ID, y=0.20, yend=1),color="gray90") +
  geom_point( aes(x=ID, y=Observer_1), color=rgb(0.3,0.8,0.8,0.5), size=4 ) +
  geom_point( aes(x=ID, y=Observer_2), color=rgb(0.6,0.4,0.9,0.5), size=4 ) +
  geom_point( aes(x=ID, y=Auto_report), color=rgb(0.6,0.7,0.9,0.5), size=4 ) +
  coord_flip()+
  theme_ipsum() +
  theme(text = element_text(size = 13),legend.position = "none",axis.text.x = element_text(size = 13, hjust = 1),axis.text.y = element_blank())+ #element_text(size = 13)) +
  xlab("") + ylab("")
  
p2<-p+geom_hline(yintercept=0.50, linetype="dashed", color = "gray30", size=.5)+geom_hline(yintercept=0.75, linetype="dashed", color = "gray30", size=.5)+geom_hline(yintercept=0.90, linetype="dashed", color = "gray30", size=.5)+
  geom_text(aes(0, 0.375, label = "Poor", vjust = -48.5))+geom_text(aes(0, 0.625, label = "Moderate", vjust = -48.5))+geom_text(aes(0, 1, label = "Excellent ", vjust = -48.5))+geom_text(aes(0, 0.825, label = "Good", vjust = -48.5))#+


dev.new()
library("gridExtra")
grid.arrange(p1, p2, ncol = 2, nrow = 1)

#-------------------------------------------------------------
#Error Inter-observer
#-------------------------------------------------------------

res1<-function(data,data0,dataS){
j<-1
i <- 1
mat<-matrix(0,18,3)
while(i <=nrow(data)){
  a<-i
  b<-i+1
  x1 <- data[c(a,b),]
  dat1<-x1[,8:127]
  datt1<-t(dat1)
  x2 <- data0[c(a,b),]
  dat2<-x2[,8:127]
  datt2<-t(dat2)
  x3 <- dataS[c(a,b),]
  dat3<-x3[,8:127]
  datt3<-t(dat3)
  datt<-cbind(datt1,datt2,datt3)
  mat[j,1]<-x1[1,"ID00"]
  mat[j,2]<-x1[1,"DEDO"]
  a<-icc(datt, model = "twoway",type = "agreement", unit = "single")
  mat[j,3]<-round(as.numeric(a[7]),digits=2)
  i=b+1
  j=j+1
}

colnames(mat)<-c("ID","Dedo","ICC")

dats1<-as.data.frame(mat)
}

All_p<-res1(data1,data2,data3)
All_i<-res1(data4,data5,data6)

res2<-function(data,data0){
j<-1
i <- 1
mat<-matrix(0,18,3)
while(i <=nrow(data)){
  a<-i
  b<-i+1
  x1 <- data[c(a,b),]
  dat1<-x1[,8:127]
  datt1<-t(dat1)
  x2 <- data0[c(a,b),]
  dat2<-x2[,8:127]
  datt2<-t(dat2)
  datt<-cbind(datt1,datt2)
  mat[j,1]<-x1[1,"ID00"]
  mat[j,2]<-x1[1,"DEDO"]
  a<-icc(datt, model = "twoway",type = "agreement", unit = "single")
  mat[j,3]<-round(as.numeric(a[7]),digits=2)
  i=b+1
  j=j+1
}

colnames(mat)<-c("ID","Dedo","ICC")

dats1<-as.data.frame(mat)
}

AC_p<-res2(data1,data3)
AC_i<-res2(data4,data6)

AE_p<-res2(data1,data2)
AE_i<-res2(data4,data5)

EC_p<-res2(data2,data3)
EC_i<-res2(data5,data6)

Pulgar_icc<-data.frame(Todos=All_p[3],Obs1_Obs2=AC_p[3],Obs1_Auto=AE_p[3],Obs2_Auto=EC_p[3])
names(Pulgar_icc)<-c("Todos","Obs1_Obs2","Obs1_Auto","Obs2_Auto")
for(i in ncol(Pulgar_icc)){
  Pulgar_icc[,i]<-as.numeric(Pulgar_icc[,i])
}
Pulgar_icc$ID<-All_p[,1]

Indice_icc<-data.frame(Todos=All_i[3],Obs1_Obs2=AC_i[3],Obs1_Auto=AE_i[3],Obs2_Auto=EC_i[3])
names(Indice_icc)<-c("Todos","Obs1_Obs2","Obs1_Auto","Obs2_Auto")
for(i in ncol(Indice_icc)){
  Indice_icc[,i]<-as.numeric(Indice_icc[,i])
}
Indice_icc$ID<-a_i[,1]

library(ggplot2)
library(dplyr)
library(hrbrthemes)


for(i in 1:4){
  Pulgar_icc[,i]<-as.numeric(Pulgar_icc[,i])
}
for(i in 1:4){
  Indice_icc[,i]<-as.numeric(Indice_icc[,i])
}

mean_p<-colMeans(Pulgar_icc[1:4]) 
mean_i<-colMeans(Indice_icc[1:4]) 

sd_p<-sqrt(diag(var(Pulgar_icc[1:4])))
sd_i<-sqrt(diag(var(Indice_icc[1:4])))

cv_p<-(sd_p/mean_p)*100
cv_i<-(sd_i/mean_i)*100

#Values less than 0.5 are indicative of poor reliability, 
#values between 0.5 and 0.75 indicate moderate reliability, 
#values between 0.75 and 0.9 indicate good reliability, 
#and values greater than 0.90 indicate excellent reliability.
library(ggplot2)
p<-ggplot(Pulgar_icc, aes(x=ID,group = 1)) + #'#41729F' blue
  geom_bar(aes(y = Todos),stat="identity", fill = 'gray90') + 
  geom_point( aes(x=ID, y=Obs1_Obs2),shape=15, color=rgb(0.3,0.8,0.8,0.5), size=4 )+
  geom_point( aes(x=ID, y=Obs1_Auto),shape=15, color=rgb(0.6,0.4,0.9,0.5), size=4 )+
  geom_point( aes(x=ID, y=Obs2_Auto), shape=15,color=rgb(0.6,0.7,0.9,0.5), size=4 )+
  theme_ipsum() +ylim(0, 1)+
  theme(legend.position = "none",axis.text.x = element_text(size = 13, hjust = 1),axis.text.y = element_text(size = 13, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  xlab("") +coord_flip()+
  ylab("")+ggtitle("a) Right thumb")+
  geom_text(aes(0, 0.375, label = "Poor", vjust = -48.5))+geom_text(aes(0, 0.625, label = "Moderate", vjust = -48.5))+geom_text(aes(0, 0.95, label = "Excellent ", vjust = -48.5))+geom_text(aes(0, 0.825, label = "Good", vjust = -48.5))#+

p1<-p+geom_hline(yintercept=0.50, linetype="dashed", color = "gray60", size=.5)+geom_hline(yintercept=0.75, linetype="dashed", color = "gray60", size=.5)+geom_hline(yintercept=0.90, linetype="dashed", color = "gray60", size=.5)+geom_hline(yintercept=1.00, linetype="dashed", color = "gray60", size=.5)

p<-ggplot(Indice_icc, aes(x=ID,group = 1)) + #'#41729F' blue
  geom_bar(aes(y = Todos),stat="identity", fill = 'gray90') + 
  geom_point( aes(x=ID, y=Obs1_Obs2), shape=15,color=rgb(0.3,0.8,0.8,0.5), size=4 )+
  geom_point( aes(x=ID, y=Obs1_Auto),shape=15, color=rgb(0.6,0.4,0.9,0.5), size=4 )+
  geom_point( aes(x=ID, y=Obs2_Auto), shape=15,color=rgb(0.6,0.7,0.9,0.5), size=4 )+
  theme_ipsum() +ylim(0, 1)+
  theme(legend.position = "none",axis.text.x = element_text(size = 13, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 13),axis.text.y = element_blank()) +
  xlab("") +coord_flip()+
  ylab("")+ggtitle("b) Right index")+
  geom_text(aes(0, 0.375, label = "Poor", vjust = -48.5))+geom_text(aes(0, 0.625, label = "Moderate", vjust = -48.5))+geom_text(aes(0, 0.95, label = "Excellent ", vjust = -48.5))+geom_text(aes(0, 0.825, label = "Good", vjust = -48.5))#+


p2<-p+geom_hline(yintercept=0.50, linetype="dashed", color = "gray60", size=.5)+geom_hline(yintercept=0.75, linetype="dashed", color = "gray60", size=.5)+geom_hline(yintercept=0.90, linetype="dashed", color = "gray60", size=.5)+geom_hline(yintercept=1.00, linetype="dashed", color = "gray60", size=.5)
dev.new()
library("gridExtra")
grid.arrange(p1, p2, ncol = 2, nrow = 1)

#-------------------------------------------------------------
#Stability
#-------------------------------------------------------------

c_p<-res(data3)
c_i<-res(data6)
c_p2<-res(data7)
c_i2<-res(data8)
ID_ICC<-c_p[,"ID"]
c_p_ICC<-as.numeric(c_p[,"ICC"])
c_i_ICC<-as.numeric(c_i[,"ICC"])
c_p2_ICC<-as.numeric(c_p2[,"ICC"])
c_i2_ICC<-as.numeric(c_i2[,"ICC"])

DataC<-data.frame(ID_ICC,c_p_ICC,c_i_ICC,c_p2_ICC,c_i2_ICC)
means<-colMeans(DataC[,2:5])
sd<-sqrt(diag(var(DataC[,2:5])))
cv<-(sd/means)*100

c_pz<-res(data3)
c_pz$Time<-c("RT_1")
c_iz<-res(data6)
c_iz$Time<-c("RI_1")
c_p2z<-res(data7)
c_p2z$Time<-c("RT_2")
c_i2z<-res(data8)
c_i2z$Time<-c("RI_2")

c_pz[,c("ID","ICC","Time")]

DataC<-rbind(c_pz[,c("ID","ICC","Time")],c_iz[,c("ID","ICC","Time")])
DataC<-rbind(DataC,c_p2z[,c("ID","ICC","Time")])
DataC<-rbind(DataC,c_i2z[,c("ID","ICC","Time")])
DataC$Time<-as.factor(DataC$Time)
DataC$ICC<-as.numeric(DataC$ICC)
pairwise.wilcox.test(DataC$ICC, DataC$Time,p.adjust.method = "none")
kruskal.test(DataC$ICC ~ DataC$Time)

means<-aggregate(DataC[,"ICC"], list(DataC[,"Time"]), mean,na.rm=TRUE)
sd<-aggregate(DataC[,"ICC"], list(DataC[,"Time"]), sd,na.rm=TRUE)
ma<-aggregate(DataC[,"ICC"], list(DataC[,"Time"]), max,na.rm=TRUE)
mi<-aggregate(DataC[,"ICC"], list(DataC[,"Time"]), min,na.rm=TRUE)
cv<-sd[2]/means[2]
