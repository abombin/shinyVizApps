# map needs on the server: sudo apt install libgdal-dev
# to find capitals coordinates uses free https://simplemaps.com/data/world-cities

#setwd("/home/ubuntu/github/shinyApp/shinyVizApps/uploadDatCov")




library(shiny)
library(ggtree)
library(ggplot2)
library(leaflet)
library(plyr)
library(plotly)
library(gplots) # heatmap
library(bipartite) # spread data table
library(incidence2) # epi curve

linebreaks <- function(n){HTML(strrep(br(), n))}  # introduce multiple breaks in one function

usMap<-read.delim("./data/statesGeo.tsv", T, sep="\t")
colnames(usMap)[3]<-"lng"

worldMap<-read.delim("./data/countriesGeo2.tsv", T, sep="\t")
colnames(worldMap)[4]<-"lng"

ui <- navbarPage("Summary",
                 tabsetPanel(
                   tabPanel("Summary Stat", fluid=T,
                            fluidPage(
                              fileInput('target_upload', 'Upload Your Metadata',
                                        accept = c('.tsv', '.txt')),
                              
                              mainPanel(
                                DT::dataTableOutput("metaDat", width = "150%"),
                                linebreaks(3),
                                tags$div(title="Color plot by the input column",
                                         textInput(inputId = "summaryOption",label = "Write Column Name")),
                                linebreaks(2),
                                plotlyOutput("Epi", width="135%"),
                                linebreaks(2),
                                plotlyOutput("Bar", width = "135%"),
                                linebreaks(4),
                                tags$div(title="Filter samples by the relative abundance of Pangolin lineages",
                                         sliderInput("varAbund", "Relative Abundance",
                                                     min = 0, max = 1,
                                                     value = 0.1, step = 0.05, width="35%")),
                                linebreaks(2),
                                plotOutput("HM", width = "120%", height = "800px")
                                
                              ))),
                   
                   tabPanel("Phylogeny", fluid=T,
                            fluidPage(
                              fileInput('treeUpload', 'Upload Your Newick Tree', accept = c('.nwk', '.txt')),
                              tags$div(title="Color tips by the input column",
                                       textInput(inputId = "varOption",label = "Select Column")),
                              splitLayout(cellWidths=c("50%", "50%"), 
                                          tags$div(title= "Subsample/highlight tree by the input value. 
                                                   Input value should be in the column selected above",
                                                   textInput(inputId = "optionB", label= "Group")), 
                                          tags$div(title="Input the name of the sample to root the tree",
                                                   textInput(inputId = "treeRoot", label= "Root"))
                              ),
                              tags$div(title="Select if based on the Grpup input option the branches should be hilghlited or 
                                       tree should be subsampled",
                                       selectInput(inputId= "actionOption", label= "Branches Action",
                                                   choices=c("Highlight","Subsample"))
                              ),
                              linebreaks(1),
                              actionButton(inputId ="goRoot", "Make Tree"),
                              linebreaks(3),
                              mainPanel(
                                plotlyOutput(outputId = "tree",width = "150%", height = "1000px"),
                              ))),
                   tabPanel("Map", fluid=T,
                            fluidPage(
                              br(),
                              tags$div(title="Input column name based on which to filter the data",
                                       textInput(inputId = "mapOption", label = "Write Column Nme")),
                              tags$div(title="Input column's value to filter data",
                                       textInput(inputId = "mapFilter", label= "Filter")),
                              linebreaks(2),
                              
                              mainPanel(uiOutput("leaf"))
                            ))
                 ))


server <- function(input, output) {
  rawDat <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile)){
      rawDat<-read.delim("./data/metaData.tsv", T, sep="\t")
    } else {
      rawDat<-read.delim(inFile$datapath,  T, sep="\t")
    }
    return(rawDat)
  })
  
  metaTab<-reactive({
    metaDat<-subset(rawDat(), select= -c(authors, originating_lab, submitting_lab))
    return(metaDat)
  })
  
  output$metaDat <- DT::renderDataTable(
    metaTab(), options = list(scrollX = TRUE), rownames= FALSE)
  
  freqTab<-reactive({
    if (input$summaryOption==""){
      title0<-names(rawDat())[2]
      varSum<-data.frame(table(rawDat()[,title0]))
    } else {
      title0<-as.character(input$summaryOption)
      varSum<-data.frame(table(rawDat()[,input$summaryOption]))
    }
    return(varSum)
  })
  
  summOpt<-reactive({
    if (input$summaryOption==""){
      title0<-names(rawDat())[2]
    } else {
      title0<-as.character(input$summaryOption)
    }
    return(title0)
  })
  
  output$Epi<-renderPlotly({
    title0<-summOpt()
    epiGroup <- incidence2::incidence(rawDat(), date_index = date, interval = "month", groups = .data[[title0]],
                                      na_as_group = TRUE)
    epiPlot<-plot(epiGroup, fill = .data[[title0]])+
      theme_classic()+
      theme(text = element_text(size = 16))+ 
      ggtitle(paste0("Frequency of samples per ", title0))+
      theme(plot.title = element_text(hjust = 0.5))
    epiPlotly<-ggplotly(epiPlot)
    epiPlotly
    
    
  })
  
  output$Bar<-renderPlotly({
    title0<-summOpt()
    varSum<-freqTab()
    valMax<-max(varSum$Freq)+3
    pBar<-ggplot(data=varSum, aes(x=Var1, y=Freq, fill=Var1)) +
      geom_bar(stat="identity")+
      theme_classic()+
      theme(text = element_text(size = 16))+ 
      scale_fill_discrete(name = title0)+
      xlab(title0)+
      ggtitle(paste0("Frequency of samples per ", title0))+
      theme(plot.title = element_text(hjust = 0.5))+
      ylim(0, valMax)
    barPlotly<-ggplotly(pBar)
    barPlotly
  })
  
  output$HM<-renderPlot({
    metaDat<-rawDat()
    dfSub<-metaDat[, c('pango_lineage','date')]
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
  
  upTree <- reactive({
    inFile <- input$treeUpload
    if (is.null(inFile)){
      rawTree<-ape::read.tree(file = "./data/tree.nwk")
    } else {
      rawTree<-ape::read.tree(file=inFile$datapath)
    }
    return(rawTree)
  })
  
  selTree<- reactive({
    if (input$optionB==""){
      tips<-rawDat()
    } else {
      rawDat=rawDat()
      colInput<-data.frame(rawDat[, input$varOption])
      uuid<-data.frame(rawDat$strain)
      varOption<-cbind(uuid, colInput)
      colnames(varOption)<-c("strain", "varOption")
      tips<-varOption[(varOption$varOption==input$optionB),]
    }
    selTips<-as.character(tips$strain)
    return(selTips)
    
  })
  
  selTips<-reactive({
    tips<- selTree()
    treeTips<-ape::keep.tip(upTree(), tip=tips)
    return(treeTips)
  })
  
  selTipsTree<-reactive({
    if (input$actionOption=="Highlight"){
      tipsTree<-upTree()
    } else{
      tipsTree<-selTips()
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
  
  varOpt<-reactive({
    if (input$varOption==""){
      title0<-names(rawDat())[2]
    } else {
      title0<-as.character(input$varOption)
    }
    return(title0)
  })
  
  output$tree <- renderPlotly({
    metaDat<-rawDat()
    title0<-varOpt()
    title1<-selTree()
    if (input$actionOption=="Subsample"){
      plotTree<-ggtree::ggtree(rootedTree())+
        theme_tree2()+
        hexpand(0.2, direction = 1)+
        theme(text = element_text(size = 24))+
        ggtitle("Samples Phylogenetic Tree")+
        theme(plot.title = element_text(hjust = 0.5))
      
      metaTree<-plotTree$data%>% dplyr::inner_join(metaDat, c('label'='strain'))
      plotMeta<-plotTree+geom_point(data=metaTree, aes( label=label, x = x,
                                                        y = y, color=.data[[title0]]), size=2)+
        guides(colour = guide_legend(override.aes = list(size=6)))+
        theme(legend.text=element_text(size=10))
      plotlyTree<-ggplotly(plotMeta)
      plotlyTree
      
    } else if(input$actionOption=="Highlight"){
      selTipsTree<-ggtree::groupOTU(rootedTree(), title1)
      plotTree<-ggtree::ggtree(selTipsTree, aes(color=group)) + 
        scale_color_manual(values=c("black", "red"))+
        theme_tree2()+
        hexpand(0.2, direction = 1)+
        theme(text = element_text(size = 24))+
        guides(colour = guide_legend(override.aes = list(size=6)))+
        ggtitle("Samples Phylogenetic Tree")+
        theme(plot.title = element_text(hjust = 0.5))
      
      metaTree<-plotTree$data%>% dplyr::inner_join(metaDat, c('label'='strain'))
      plotMeta<-plotTree+geom_point(data=metaTree, aes( label=label, x = x,
                                                        y = y, fill=.data[[title0]]), size=2)+
        guides(colour = guide_legend(override.aes = list(size=6)))+
        theme(legend.text=element_text(size=10))
      
      plotlyTree<-ggplotly(plotMeta)
      plotlyTree
    }
    
  })
  
  mapDat<-reactive({
    sumData<-rawDat()
    usDat<-sumData[(sumData$country=="USA"),]
    usDatLoc<-plyr::join(usDat, usMap, by="division", type="left")
    worldDat<-sumData[!(sumData$country=="USA"),]
    worldDatLoc<-plyr::join(worldDat, worldMap, by="country", type="left", match="first")
    colnames(worldDatLoc)[30]<-"state"
    sumDataAll<-rbind(usDatLoc, worldDatLoc)
    return(sumDataAll)
  })
  
  mapColumn<-reactive({
    if ((input$mapOption=="") | (input$mapFilter=="" )){
      mapMeta<-mapDat()
    } else {
      metaDat<-mapDat()
      colInput<-data.frame(metaDat[, input$mapOption])
      uuid<-data.frame(metaDat$strain)
      mapOption<-cbind(uuid, colInput)
      colnames(mapOption)<-c("strain", "mapOption")
      selId<-mapOption[(mapOption$mapOption==input$mapFilter),]
      mapMeta<-metaDat[(metaDat$strain%in%selId$strain),]
    }
    return(mapMeta)
  })
  
  output$Map<-renderLeaflet({
    mapMeta<-mapColumn()
    m<-leaflet(options = leafletOptions(zoomControl = T)) %>% 
      addTiles() %>%
      addMarkers(data = mapMeta, lat = ~ lat, lng = ~ lng, clusterOptions = markerClusterOptions(), 
                 popup = ~strain)
    m
  })
  
  output$leaf=renderUI({
    leafletOutput('Map', width = "150%", height = 700)
  })
}

shinyApp(ui, server)