library(gplots)
library(bipartite)

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

# prepare to spread matrix
dfPrepSpr <- data.frame(higher = c(sumData$Date), 
                        lower = c(sumData$Lin), 
                        freq=c(sumData$Percentage), webID = c("X1_"))

# spread matrix
sprArray<-bipartite::frame2webs(dfPrepSpr,type.out="array")
sprMatrix<-as.data.frame(sprArray)
colnames(sprMatrix) <-  sub(".X1_.*", "", colnames(sprMatrix))

# order matrix by Percentage of abundance

sprMatrix$Order <- rowSums( sprMatrix[,1:ncol(sprMatrix)])
sprMatrixOrd<-sprMatrix[order(sprMatrix$Order, decreasing = T),]
sprMatrixFilt<-sprMatrixOrd[rowSums(sprMatrixOrd[1:(ncol(sprMatrixOrd)-1)] >= 0.1) > 0, ]
# order matrix by Strain name

plotMat<-as.matrix(sprMatrixFilt[, 1:(ncol(sprMatrixFilt)-1)])

heatmap.2(plotMat, scale = "none", col = bluered(100), 
          trace = "none", density.info = "none", dendrogram='none', Rowv=FALSE, Colv=FALSE, 
          lhei=c(2, 12), lwid=c(2,12), margins = c(12, 13), cexCol = 3, cexRow = 3)