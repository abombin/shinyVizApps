data<-ape::read.tree(file = "./data/tree.nwk")

usMap<-read.delim("./data/statesGeo.tsv", T, sep="\t")
colnames(usMap)[3]<-"lng"

worldMap<-read.delim("./data/countriesGeo2.tsv", T, sep="\t")
colnames(worldMap)[4]<-"lng"


rawDat<-read.delim("./data/metaData.tsv", T, sep="\t")
rawDat$Time<-as.Date(rawDat$date, format="%Y-%m-%d")
rawDat$Year<-format(rawDat$Time, format="%Y")
byYear <- split(rawDat, rawDat$Year)
dateNames <- names(byYear)

# empty frames
sumData<-data.frame(matrix(ncol=9, nrow=0))
#colnames(sumData)<-c("PangLin", "Frequency", "Date", "Percentage")

for (year in dateNames){
  subsampDat<-byYear[[year]]
  procDat<-subsampDat[order(subsampDat$Time),]
  dfRow<-c(1:nrow(procDat))
  dfSub2 <- dfRow[seq(from=2, to=length(dfRow), by=2)]
  posSeq<-seq(from=0.25, by=0.25, length.out = (nrow(procDat)/2)) # make sequence that increases by 0.25
  
  procDat$sign<-rep(c(1,-1))
  pos<-vector()
  for (i in posSeq){
    pos<-append(pos, rep(i,2))
  }
  procDat$poisiton<-pos
  
  procDat$PointPos<-procDat$poisiton*procDat$sign
  procDat$TextPos<-(procDat$poisiton+0.8)*procDat$sign
  procDat$Time<-as.Date(procDat$Time, format="%m/%d/%Y")
  sumData<-rbind(sumData, procDat)
}

usDat<-sumData[(sumData$country=="USA"),]
usDatLoc<-plyr::join(usDat, usMap, by="division", type="left")

worldDat<-sumData[!(sumData$country=="USA"),]
worldDatLoc<-plyr::join(worldDat, worldMap, by="country", type="left", match="first")
colnames(worldDatLoc)[36]<-"state"

sumDataUS<-rbind(usDatLoc, worldDatLoc)


metaDat<-subset(sumDataUS, select= -c(authors, originating_lab, submitting_lab))
colnames(metaDat)[1]<-"uuid"

linebreaks <- function(n){HTML(strrep(br(), n))}  # introduce multiple breaks in one function

ui <- navbarPage("Summary",
                 tabsetPanel(
                   tabPanel("Phylogeny", fluid=T,
                            fluidPage(
                              selectInput(inputId = "varOption",
                                          label = "Select Column",
                                          choices = c(names(metaDat[2:ncol(metaDat)]))),
                              splitLayout(cellWidths=c("50%", "50%"), textInput(inputId = "optionB", label= "Group"), 
                                          textInput(inputId = "treeRoot", label= "Root")),
                              selectInput(inputId= "actionOption", label= "Branches Action",
                                          choices=c("Subsample","Highlight")),
                              linebreaks(1),
                              actionButton(inputId ="goRoot", "Make Tree"),
                              linebreaks(3),
                              mainPanel(
                                plotlyOutput(outputId = "tree",width = "200%", height = "1500px"),
                                linebreaks(4),
                                DT::dataTableOutput("metaDat", width = "150%")
                                
                              ))),
                   tabPanel("Summary Stat", fluid=T,
                            fluidPage(
                              selectInput(inputId = "summaryOption",
                                          label = "Select Column",
                                          choices = c(names(metaDat[2:ncol(metaDat)]))),
                              linebreaks(2),
                              
                              textInput(inputId = "varYear", label="Filter by Year"),
                              
                              mainPanel(
                                plotlyOutput(outputId="Time", width="180%"),
                                linebreaks(4),
                                plotOutput("Bar", width = "135%"),
                                linebreaks(4),
                                sliderInput("varAbund", "Relative Abundance",
                                            min = 0, max = 1,
                                            value = 0.1, step = 0.05, width="35%"),
                                linebreaks(2),
                                plotOutput("HM", width = "120%", height = "800px")
                                
                              ))),
                   tabPanel("Map", fluid=T,
                            fluidPage(
                              br(),
                              selectInput(inputId = "mapOption",
                                          label = "Select Column",
                                          choices = c(names(metaDat[2:ncol(metaDat)]))),
                              textInput(inputId = "mapFilter", label= "Filter"),
                              linebreaks(2),
                              
                              mainPanel(
                                uiOutput("leaf")
                              )))
                 ))

server <- function(input, output) {
  
  selTree<- reactive({
    if (input$optionB==""){
      tips<-metaDat
    } else {
      colInput<-data.frame(metaDat[, input$varOption])
      uuid<-data.frame(metaDat$uuid)
      varOption<-cbind(uuid, colInput)
      colnames(varOption)<-c("uuid", "varOption")
      tips<-varOption[(varOption$varOption==input$optionB),]
    }
    selTips<-as.character(tips$uuid)
    treeTips<-ape::keep.tip(data, tip=selTips)
    return(treeTips)
    
  })
  
  
  selTipsTree<-reactive({
    if (input$actionOption=="Highlight"){
      tipsTree<-data
    } else{
      tipsTree<-selTree()
    }
    return(tipsTree)
  })
  
  rootedTree<-eventReactive(input$goRoot, {
    if (input$treeRoot==""){
      rootTree<-selTipsTree()
    } else {
      rootTree<-ape::root(selTipsTree(), outgroup=input$treeRoot)
    }
    return(rootTree)
  })
  
  
  output$tree <- renderPlotly({
    if (input$actionOption=="Subsample"){
      
      plotTree<-ggtree::ggtree(rootedTree())+ 
        theme_tree2()+
        hexpand(0.2, direction = 1)+
        theme(text = element_text(size = 24))+
        ggtitle("Samples Phylogenetic Tree")+
        theme(plot.title = element_text(hjust = 0.5))
      metaTree<-plotTree$data%>% dplyr::inner_join(metaDat, c('label'='uuid'))
      plotMeta<-plotTree+geom_point(data=metaTree, aes( label=label, x = x,
                                                        y = y, color=.data[[input$varOption]]), size=2.5)+
        guides(colour = guide_legend(override.aes = list(size=6)))+
        theme(legend.text=element_text(size=10))
      
      plotlyTree<-ggplotly(plotMeta)
      plotlyTree
    } else if(input$actionOption=="Highlight"){
      colInput<-data.frame(metaDat[, input$varOption])
      uuid<-data.frame(metaDat$uuid)
      varOption<-cbind(uuid, colInput)
      colnames(varOption)<-c("uuid", "varOption")
      tips<-varOption[(varOption$varOption==input$optionB),]
      colTipGr<-as.character(tips$uuid)
      tree<-rootedTree()
      selTipsTree<-ggtree::groupOTU(tree, colTipGr)
      plotTree<-ggtree::ggtree(selTipsTree, aes(color=group)) + 
        scale_color_manual(values=c("black", "red"))+
        theme_tree2()+
        hexpand(0.2, direction = 1)+
        theme(text = element_text(size = 24))+
        guides(colour = guide_legend(override.aes = list(size=6)))+
        ggtitle("Samples Phylogenetic Tree")+
        theme(plot.title = element_text(hjust = 0.5))
      
      metaTree<-plotTree$data%>% dplyr::inner_join(metaDat, c('label'='uuid'))
      plotMeta<-plotTree+geom_point(data=metaTree, aes( label=label, x = x,
                                                        y = y, fill=.data[[input$varOption]]), size=2.5)+
        guides(colour = guide_legend(override.aes = list(size=6)))+
        theme(legend.text=element_text(size=10))
      
      plotlyTree<-ggplotly(plotMeta)
      plotlyTree
    }
    
  })
  
  output$Pie<-renderPlot({
    title0<-as.character(input$summaryOption)
    varSum<-data.frame(table(metaDat[,input$summaryOption]))
    ggplot(varSum, aes(x="", y=Freq, fill=Var1))+
      geom_bar(stat="identity", width=1)+
      coord_polar("y", start=0)+
      theme_void()+
      scale_fill_discrete(name = title0)+
      ggtitle(paste0("Frequency of samples for ", title0))+
      theme(plot.title = element_text(hjust = 0.5))
  }, res = 150)
  
  output$Bar<-renderPlot({
    title0<-as.character(input$summaryOption)
    varSum<-data.frame(table(metaDat[,input$summaryOption]))
    valMax<-max(varSum$Freq)+3
    ggplot(data=varSum, aes(x=Var1, y=Freq, fill=Var1)) +
      geom_bar(stat="identity")+
      theme_classic()+
      theme(text = element_text(size = 24))+ 
      scale_fill_discrete(name = title0)+
      xlab(title0)+
      ggtitle(paste0("Frequency of samples for ", title0))+
      theme(plot.title = element_text(hjust = 0.5))+
      ylim(0, valMax)
    
  })
  
  output$HM<-renderPlot({
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
    sprMatrixFilt<-sprMatrixOrd[rowSums(sprMatrixOrd[1:(ncol(sprMatrixOrd)-1)] >= input$varAbund) > 0, ]
    # order matrix by Strain name
    plotMat<-as.matrix(sprMatrixFilt[, 1:(ncol(sprMatrixFilt)-1)])
    
    heatmap.2(plotMat, scale = "none", col = bluered(100), 
              trace = "none", density.info = "none", dendrogram='none', Rowv=FALSE, Colv=FALSE, 
              lhei=c(2, 12), lwid=c(2,12), margins = c(12, 13), cexCol = 1.5, cexRow = 1.5)
  })
  
  filterTime<-reactive({
    if (input$varYear==""){
      filtMet<-metaDat
    } else {
      filtMet<-metaDat[(metaDat$Year==input$varYear),]
    }
    return(filtMet)
  })
  
  output$Time<-renderPlotly({
    df<-filterTime()
    valMax<-max(df$TextPos)+1
    valMin<-min(df$TextPos)-1
    title0<-as.character(input$summaryOption)
    time_plot<-ggplot(filterTime(), aes(x=Time, y= PointPos))+
      geom_segment(data=filterTime(), aes(y=PointPos,yend=0,xend=Time))+
      geom_point(aes(text=uuid, color=.data[[input$summaryOption]]), size=2)+
      #geom_text(data=filterTime(), aes(y=TextPos, x= Time, label=uuid), size=2)+
      geom_hline(yintercept=0, color = "black", size=0.3)+
      theme_classic()+
      scale_x_date(NULL, date_labels="%b %Y",date_breaks  ="2 month")+
      #scale_x_date(date_labels="%b %Y", breaks = unique(filterTime()$Time))
      theme(axis.title.y = element_blank())+
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      ggtitle("Sampling Time Line")+
      theme(plot.title = element_text(hjust = 0.5))+
      ylim(valMin, valMax)
    plotlyTime<-ggplotly(time_plot)
    plotlyTime
  })
  
  output$metaDat <- DT::renderDataTable(
    metaDat, options = list(scrollX = TRUE), rownames= FALSE)
  
  output$leaf=renderUI({
    leafletOutput('Map', width = "150%", height = 700)
  })
  
  output$Map<-renderLeaflet({
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
    m<-leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      addTiles() %>%
      addMarkers(data = mapMeta, lat = ~ lat, lng = ~ lng, clusterOptions = markerClusterOptions(), 
                 popup = ~uuid)
    m
  })
  
  output$sumFig<-renderPlot({
    
    histTable<-as.data.frame(runSum[,input$sumOption])
    if (is.numeric(runSum[,input$sumOption])==T){
      ggplot(runSum, aes(x=.data[[input$sumOption]])) +
        geom_histogram(fill="#f8766d", alpha=1, position="identity")+
        theme_classic()+
        theme(text = element_text(size = 24))+
        scale_y_continuous(expand = c(0, 0.1))+
        ggtitle(paste0("Bactopia Run Summary"))+
        theme(plot.title = element_text(hjust = 0.5))
    } else{
      title0<-as.character(input$sumOption)
      varSum<-data.frame(table(runSum[,input$sumOption]))
      valMax<-max(varSum$Freq)+3
      ggplot(data=varSum, aes(x=Var1, y=Freq, fill=Var1)) +
        geom_bar(stat="identity")+
        theme_classic()+
        theme(text = element_text(size = 24))+ 
        scale_fill_discrete(name = title0)+
        xlab(title0)+
        #scale_y_continuous(expand = c(0, 0.1))+
        ggtitle(paste0("Bactopia Run Summary"))+
        theme(plot.title = element_text(hjust = 0.5))+
        ylim(0, valMax)
      
      
      
    }
  })
  
  
}

shinyApp(ui, server)
