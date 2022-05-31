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

ui <- navbarPage("Summary",
                 tabsetPanel(
                   tabPanel("Summary Stat", fluid=T,
                            fluidPage(
                              fileInput('target_upload', 'Upload Metadata',
                                        accept = c('.tsv', '.txt')),
                              textInput(inputId = "summaryOption",
                                          label = "Write Column Name"),
                              linebreaks(2),
                              
                              mainPanel(
                                plotOutput("Bar", width = "135%")

                              )))
                 ))


server <- function(input, output) {
  metaDat <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    rawDat<-read.delim(inFile$datapath,  T, sep="\t")
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
    
    return(sumData)
  })
  
  output$Bar<-renderPlot({
    if (input$summaryOption==""){
      title0<-names(metaDat())[2]
      varSum<-data.frame(table(metaDat()[,title0]))
      valMax<-max(varSum$Freq)+3
      ggplot(data=varSum, aes(x=Var1, y=Freq, fill=Var1)) +
        geom_bar(stat="identity")+
        theme_classic()+
        theme(text = element_text(size = 24))+ 
        scale_fill_discrete(name = title0)+
        xlab(title0)+
        ggtitle(paste0("Frequency of samples per ", title0))+
        theme(plot.title = element_text(hjust = 0.5))+
        ylim(0, valMax)
    } else {
      title0<-as.character(input$summaryOption)
      varSum<-data.frame(table(metaDat()[,input$summaryOption]))
      valMax<-max(varSum$Freq)+3
      ggplot(data=varSum, aes(x=Var1, y=Freq, fill=Var1)) +
        geom_bar(stat="identity")+
        theme_classic()+
        theme(text = element_text(size = 24))+ 
        scale_fill_discrete(name = title0)+
        xlab(title0)+
        ggtitle(paste0("Frequency of samples per ", title0))+
        theme(plot.title = element_text(hjust = 0.5))+
        ylim(0, valMax)
    }
    
  })
}

shinyApp(ui, server)
