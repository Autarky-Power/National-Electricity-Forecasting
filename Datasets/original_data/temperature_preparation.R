
library(openxlsx)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

t <- read.xlsx("temperature.xlsx",sheet = 2)
pop <- read.xlsx("temperature.xlsx",sheet = 3)
colnames(pop)[2]<- "popshare"


for (i in 1:nrow(t)){
 for (k in 1:24){
average[i,k] <- pop$popshare[k]* t[i,(k+1)] 
 }
  print(i)}

average$sum <- sum(average[,1:24])

for (i in 1:nrow(average)){
  average$sum[i] <- sum(average[i,1:24])
print(i)
  }


colMeans(matrix(average$sum[1:52584], nrow=24))


dates<-seq(as.Date("2013-01-01"), as.Date("2020-12-31"), by="days")
df_temp <- as.data.frame(dates)
colnames(df_temp)[1]<-"date"
df_temp$temp<- 0
df_temp$temp[1:2191]<-colMeans(matrix(average$sum[1:52584], nrow=24))
df_temp=df_temp[-c(df_temp$date==as.Date("2020-02-28")),]
df_temp$temp[2192:2921]<-average$sum[52585:53314] 


write.csv(df_temp,"daily_mean_temp.csv",row.names = F)




