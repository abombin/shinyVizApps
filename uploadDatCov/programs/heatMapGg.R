library(tidyr)
#library(hrbrthemes)
library(viridisLite)
library(viridis)
library(ggplot2)



dfSub<-metaDat[, c(19,5)]
#dfSub<-df[, c(5,3)]

colnames(dfSub)<-c("Lin", "Date")
dfSub$Date<-as.Date(dfSub$Date, format="%Y-%m-%d")

dfSub$Date<-format(dfSub$Date, format="%Y-%m")

# group by date
byDate <- split(dfSub, dfSub$Date)
dateNames <- names(byDate)

# empty frames
sumData<-data.frame(matrix(ncol=4, nrow=0))
colnames(sumData)<-c("Lin", "Frequency", "Date", "Percentage")
#loop
for (i in dateNames){
  title0<-i
  data<-byDate[[i]]
  freqData<-data.frame(table(data$Lin))
  freqData$Date<-title0
  total<-sum(freqData$Freq)
  for (j in 1:nrow(freqData)){
    freqData$Percentage[j]<-(freqData$Freq[j]/total)
  }
  colnames(freqData)<-c("Lin", "Frequency", "Date", "Percentage")
  sumData<-rbind(sumData, freqData)
}

df<-sumData[, c(1,3,4)]
df3<-complete(df, Lin, Date , fill = list(Percentage = 0))


df4<-aggregate(df3$Percentage, by = list(df3$Lin, df3$Date), min)

df5<-df4[(df4$x >= 0.1),]

df6<-df3[(df3$Lin%in%df5$Group.1),]

df7<-df6[order(df6$Percentage, decreasing = T),]

ggplot(df7, aes(x=Date, y=Lin, fill=Percentage))+
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) 
  #theme_ipsum()