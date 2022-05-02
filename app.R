#setwd("/home/ubuntu/shinyApp/ICMC/")

library(shiny)
library(ggtree)
library(ggplot2)

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
  procDat$TextPos<-(procDat$poisiton+0.1)*procDat$sign
  procDat$Time<-as.Date(procDat$Time, format="%m/%d/%Y")
  sumData<-rbind(sumData, procDat)
}

metaDat<-sumData


ui <- navbarPage("Summary",
                 tabsetPanel(
                   tabPanel("Phylogeny", fluid=T,
                            fluidPage(
                              selectInput(inputId = "varOption",
                                          label = "Option",
                                          choices = c(names(metaDat[2:ncol(metaDat)]))),
                              textInput(inputId = "optionB", label= "Filter"),
                              mainPanel(
                                plotOutput(outputId = "tree"),
                                br(),
                                br(),
                                br(),
                                plotOutput(outputId="Time"),
                                br(),
                                br(),
                                br(),
                                splitLayout(cellWidths = c("50%", "50%"), plotOutput("Bar"), plotOutput("Pie"))
                              )
                            )),
                   tabPanel("Meta Data", fluid=T,
                            fluidPage(
                              mainPanel(DT::dataTableOutput("metaDat", width = 500))
                            )
                   )
                   
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
    ggtree::ggtree(selTree)%<+% metaDat + 
      geom_treescale() +
      geom_tiplab(aes(color = .data[[input$varOption]])) + # size of label border  
      theme(legend.position = c(0.5,0.2), 
            legend.title = element_blank(), # no title
            legend.key = element_blank())+
      theme(text = element_text(size = 24))+
      guides(colour = guide_legend(override.aes = list(size=10)))
    
  })
  
  
  output$Pie<-renderPlot({
    title0<-as.character(input$varOption)
    varSum<-data.frame(table(metaDat[,input$varOption]))
    ggplot(varSum, aes(x="", y=Freq, fill=Var1))+
      geom_bar(stat="identity", width=1)+
      coord_polar("y", start=0)+
      theme_void()+
      scale_fill_discrete(name = title0)
  }, res = 150)
  
  output$Bar<-renderPlot({
    title0<-as.character(input$varOption)
    varSum<-data.frame(table(metaDat[,input$varOption]))
    ggplot(data=varSum, aes(x=Var1, y=Freq, fill=Var1)) +
      geom_bar(stat="identity")+
      theme_classic()+
      theme(text = element_text(size = 24))+ 
      scale_fill_discrete(name = title0)+
      xlab(title0)
    
  })
  
  output$Time<-renderPlot({
    title0<-as.character(input$varOption)
    time_plot<-ggplot(metaDat, aes(x=Time, y= PointPos))+
      geom_segment(data=metaDat, aes(y=PointPos,yend=0,xend=Time))+
      geom_point(aes(color=.data[[input$varOption]]), size=3)+
      geom_text(data=metaDat, aes(y=TextPos, x= Time, label=uuid), size=2)+
      geom_hline(yintercept=0, color = "black", size=0.3)+
      theme_classic()+
      scale_x_date(NULL, date_labels="%b %Y",date_breaks  ="2 month")
    #scale_x_date(date_labels="%b %Y", breaks = unique(metaDat$Time))
    time_plot
  },res = 150)
  
  output$metaDat <- DT::renderDataTable(
    metaDat,
    options = list(scrollX = TRUE)
  )
  
}

shinyApp(ui, server)