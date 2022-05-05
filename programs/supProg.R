# need
# sudo apt install libgdal-dev

# gives the coordinates of major USA cities
city<-maps::us.cities

rawDat<-read.csv("./data/shinyMetadat.csv")

for (i in 1:nrow(rawDat)){
  if (rawDat[i, "State"]=="Alabama"){
    rawDat[i, "lng"]<--86.779633
    rawDat[i, "lat"]<-33.543682
  } else if (rawDat[i, "State"]=="Georgia"){
    rawDat[i, "lng"]<--84.386330
    rawDat[i, "lat"]<-33.543682
  }
}

write.csv(rawDat, "shinyMetadat.csv", row.names = F)


m<-leaflet() %>% 
  addTiles() %>%
  addMarkers(data = metaDat, lat = ~ lat, lng = ~ lng, clusterOptions = markerClusterOptions(), 
             popup = ~uuid)
m


# In case we will replace select option for map or tree for input text option
if (input$mapOption!=""){
  if (input$mapFilter==""){
    mapMeta<-metaDat
  } else {
    colInput<-data.frame(metaDat[, input$mapOption])
    uuid<-data.frame(metaDat$uuid)
    mapOption<-cbind(uuid, colInput)
    colnames(mapOption)<-c("uuid", "mapOption")
    selId<-mapOption[(mapOption$mapOption==input$mapFilter),]
    mapMeta<-metaDat[(metaDat$uuid%in%selId$uuid),]
  }
  m<-leaflet() %>% 
    addTiles() %>%
    addMarkers(data = mapMeta, lat = ~ lat, lng = ~ lng, clusterOptions = markerClusterOptions(), 
               popup = ~uuid)
  m
} else {
  m<-leaflet() %>% 
    addTiles() %>%
    addMarkers(data = metaDat, lat = ~ lat, lng = ~ lng, clusterOptions = markerClusterOptions(), 
               popup = ~uuid)
}

# summary for bactopia run




if (is.numeric(runSum$uuid)==T){
  print("numeric")
} else{
  print("character")
}

# histogram
ggplot(runSum, aes(x=total_contig, color=rank)) +
  geom_histogram(alpha=0.5, position="identity")