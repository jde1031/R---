#데이터 불러오기
library("data.table")

for (SKIP in seq(1,100000000,10000000)) {
  if (SKIP == 1) {
    taxi12<-fread("c:/택시/taximatch/TaxiMach_Link_Dataset_Full_201512.txt",sep=",",stringsAsFactor=FALSE,nrows=10000000)
  }  else {
    taxi12<-fread("c:/택시/taximatch/TaxiMach_Link_Dataset_Full_201512.txt",sep=",",stringsAsFactor=FALSE,nrows=10000000,skip=SKIP)
    setnames(taxi12,1:9,c("T_Link_ID","Day","Time","Weather","Dest","CntOn","CntOff","CntEmp","V9"))
  }
  taxi12$Time<-floor(taxi12$Time/2)
  taxi12<-subset(taxi12,select=c("T_Link_ID","CntOn","CntOff","CntEmp","Time"))
  taxi12 <- taxi12[,lapply(.SD,sum,na.rm=TRUE),by=list(T_Link_ID,Time)]
  taxi12$bin <- taxi12$CntEmp-taxi12$CntOn
  write.csv(taxi12,paste0("c:/택시/preprocessing/taxi_",SKIP,".csv"),row.names=FALSE)
  rm(list=ls())
  gc()
}

file <- dir("c:/택시/preprocessing/")

for (i in file) {
  assign(i,fread(paste0("c:/택시/preprocessing/",i),header=TRUE,stringsAsFactors=FALSE))
}

taxi12 <- rbind(taxi_1.csv,taxi_10000001.csv,taxi_20000001.csv,taxi_30000001.csv,taxi_40000001.csv,taxi_50000001.csv,taxi_60000001.csv,taxi_70000001.csv,taxi_80000001.csv,taxi_90000001.csv)
rm(taxi_1.csv,taxi_10000001.csv,taxi_20000001.csv,taxi_30000001.csv,taxi_40000001.csv,taxi_50000001.csv,taxi_60000001.csv,taxi_70000001.csv,taxi_80000001.csv,taxi_90000001.csv,file,i)

taxi12 <- taxi12[,lapply(.SD,sum,na.rm=TRUE),by=list(T_Link_ID,Time)]

link<-fread("C:/택시/Link_WGS84_Link_Info_150M (2).txt", sep=",")
setnames(link,1:7,c("T_Link_ID","X_MAX","Y_MAX","X_MIN","Y_MIN","X_PART","Y_PART"))
#SiGunGu<-fread("C:/택시/dest_code.csv",header=TRUE, stringsAsFactor = F)
kslink<-fread("C:/택시/KSLink_ID_MappingTable_201501.csv",header=TRUE, stringsAsFactor = F)




##현용오빠 link연결, 시각화

link<-fread("C:/택시/Link_WGS84_Link_Info_150M (2).txt", sep=",")
setnames(link,1:7,c("T_Link_ID","X_MAX","Y_MAX","X_MIN","Y_MIN","X_PART","Y_PART"))
link$X <- (link$X_MAX+link$X_MIN)/2
link$Y <- (link$Y_MAX+link$Y_MIN)/2
link <- subset(link,select=c("T_Link_ID","X","Y"))

setkey(taxi12, T_Link_ID)

#승차가 31이하는 제거
taxi12<-taxi12[CntOn>=32,]


#time==0일때 상위 10개data 추ㅊ

taxi12_0 <- taxi12[Time==0,]
grim <- taxi12_0[link,]
grim0<-grim0[order(grim0$bin, decreasing=TRUE), ] 
grim<-grim[1:10,]


#시각화
install.packages("ggmap")
library(ggmap)
lon <- grim$X
lat <- grim$Y
data <-grim$bin
df <- data.frame(lon,lat,data)
seoul_map<-qmap("seoul", zoom=11, maptype="roadmap")
seoul_map+geom_point(data=df, aes(x=lon, y=lat),color="green",size=9)
