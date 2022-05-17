data<-read.delim("/home/ubuntu/strain/ncov/data/hcov_global.tsv", T, sep="\t")
data$strain<-gsub("hCoV-19/", "", data$strain)

tree<-ape::read.tree("/home/ubuntu/strain/ncov/results/forShiny/tree.nwk")

tips<-tree$tip.label

meta<-data[(data$strain%in%tips),]


setwd("/home/ubuntu/github/shinyApp/shinyVizApps/Covid/data")

write.table(meta, "metaData.tsv", col.names = T, row.names = F, quote = F, sep="\t")

metaDat<-read.delim("metaData.tsv", T, sep="\t")

division<-as.data.frame(metaDat$division)
colnames(division)<-"division"

divLoc<-ggmap::mutate_geocode(division, division)

countries<-mapdeck::capitals

abbr<-read.table("/home/ubuntu/shinyApp/Covid/statesAbr.txt", T, sep="\t")

stAbr<-abbr[, c(1,4)]

city<-maps::us.cities

capSt<-city[(city$capital=="2"),]

capSt$name<-gsub("\\ .*", "", capSt$name)

capStSub<-capSt[, c(2,4,5)]
colnames(capStSub)[1]<-"state"

stDat<-plyr::join(capStSub, stAbr, by="state", type="left")
colnames(stDat)[4]<-"division"

addit<-abbr[c(8,40),]
addit[1,4]<-"Washington DC"

colnames(addit)<-colnames(stDat)

stFinal<-rbind(stDat, addit)

write.table(stFinal, "statesGeo.tsv", col.names = T, row.names = F, quote = F, sep="\t")

write.table(countries, "countriesGeo.tsv", col.names = T, row.names = F, quote = F, sep="\t")