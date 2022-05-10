# map needs on the server: sudo apt install libgdal-dev

#setwd("/home/ubuntu/github/shinyApp/IcmcVizApp")



library(shiny)
library(ggtree)
library(ggplot2)
library(leaflet)

data<-ape::read.tree(file = "./data/staphylococcus-aureus_consensus.nwk")

rawDat<-read.csv("./data/shinyMetadat.csv")
rawDat$Time<-as.Date(rawDat$Time, format="%m/%d/%Y")
rawDat$Year<-format(rawDat$Time, format="%Y")
byYear <- split(rawDat, rawDat$Year)
dateNames <- names(byYear)

# empty frames
sumData<-data.frame(matrix(ncol=9, nrow=0))
#colnames(sumData)<-c("PangLin", "Frequency", "Date", "Percentage")

for (year in dateNames){
  procDat<-byYear[[year]]
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
  procDat$TextPos<-(procDat$poisiton+0.2)*procDat$sign
  procDat$Time<-as.Date(procDat$Time, format="%m/%d/%Y")
  sumData<-rbind(sumData, procDat)
}

metaDat<-sumData

runSum<-read.table("./data/bactopia-report.txt", T, sep="\t")
colnames(runSum)[1]<-"uuid"

linebreaks <- function(n){HTML(strrep(br(), n))}  # introduce multiple breaks in one function

ui <- navbarPage("Summary",
                 tabsetPanel(
                   tabPanel("Phylogeny", fluid=T,
                            fluidPage(
                              selectInput(inputId = "varOption",
                                          label = "Select Column",
                                          choices = c(names(metaDat[2:ncol(metaDat)]))),
                              splitLayout(cellWidths=c("50%", "50%"), textInput(inputId = "optionB", label= "Filter"), 
                                          textInput(inputId = "treeRoot", label= "Root")),
                              br(),
                              br(),
                              br(),
                              mainPanel(
                                plotOutput(outputId = "tree",width = "155%"),
                                br(),
                                br(),
                                br(),
                                br(),
                                plotOutput(outputId="Time", width="150%"),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                plotOutput("Bar", width = "150%"), 
                                br(),
                                br(),
                                br(),
                                plotOutput("Pie", width="150%")
                              )
                            )),
                   tabPanel("Meta Data", fluid=T,
                            fluidPage(
                              mainPanel(DT::dataTableOutput("metaDat", width = "150%"))
                            )),
                   tabPanel("Map", fluid=T,
                            fluidPage(
                              br(),
                              selectInput(inputId = "mapOption",
                                          label = "Select Column",
                                          choices = c(names(metaDat[2:ncol(metaDat)]))),
                              textInput(inputId = "mapFilter", label= "Filter"),
                              br(),
                              br(),
                              
                              mainPanel(
                                uiOutput("leaf")
                              ))),
                   tabPanel("Bactopia Summary", fluid=T,
                            fluidPage(
                              selectInput(inputId= "sumOption", label="Select Column", choices = c(names(runSum[2:ncol(runSum)]))),
                              br(),
                              br(),
                              br(),
                              mainPanel(
                                br(),
                                br(),
                                br(),
                                plotOutput("sumFig", width="150%"),
                                br(),
                                br(),
                                br(),
                                DT::dataTableOutput("bactSumDat", width = "150%")
                              )
                            ))
                   
                 )
)

server <- function(input, output) {
  
  output$tree <- renderPlot({
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
    selTree<-ape::keep.tip(data, tip=selTips)
    if (input$treeRoot==""){
      rootTree<-selTree
    } else {
      rootTree<-ape::root(selTree, outgroup=input$treeRoot)
    }
    ggtree::ggtree(rootTree)%<+% metaDat + 
      #geom_treescale() +
      geom_tiplab(aes(color = .data[[input$varOption]])) + # size of label border 
      #xlim(0, 0.006)+
      theme_tree2()+
      hexpand(0.2, direction = 1)+
      #theme(legend.position = c(0.5,0.2), 
      #legend.title = element_blank(), # no title
      #legend.key = element_blank())+
      theme(text = element_text(size = 24))+
      guides(colour = guide_legend(override.aes = list(size=10)))+
      ggtitle("Samples Phylogenetic Tree")+
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  
  output$Pie<-renderPlot({
    title0<-as.character(input$varOption)
    varSum<-data.frame(table(metaDat[,input$varOption]))
    ggplot(varSum, aes(x="", y=Freq, fill=Var1))+
      geom_bar(stat="identity", width=1)+
      coord_polar("y", start=0)+
      theme_void()+
      scale_fill_discrete(name = title0)+
      ggtitle(paste0("Frequency of samples for ", title0))+
      theme(plot.title = element_text(hjust = 0.5))
  }, res = 150)
  
  output$Bar<-renderPlot({
    title0<-as.character(input$varOption)
    varSum<-data.frame(table(metaDat[,input$varOption]))
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
  
  output$Time<-renderPlot({
    title0<-as.character(input$varOption)
    time_plot<-ggplot(metaDat, aes(x=Time, y= PointPos))+
      geom_segment(data=metaDat, aes(y=PointPos,yend=0,xend=Time))+
      geom_point(aes(color=.data[[input$varOption]]), size=3)+
      geom_text(data=metaDat, aes(y=TextPos, x= Time, label=uuid), size=2)+
      geom_hline(yintercept=0, color = "black", size=0.3)+
      theme_classic()+
      scale_x_date(NULL, date_labels="%b %Y",date_breaks  ="2 month")+
      #scale_x_date(date_labels="%b %Y", breaks = unique(metaDat$Time))
      theme(axis.title.y = element_blank())+
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      ggtitle("Sampling Time Line")+
      theme(plot.title = element_text(hjust = 0.5))
    time_plot
  },res = 150)
  
  output$metaDat <- DT::renderDataTable(
    metaDat, options = list(scrollX = TRUE))
  
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
  
  output$bactSumDat <- DT::renderDataTable(
    runSum, options = list(scrollX = TRUE))
  
}

shinyApp(ui, server)