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
                                plotlyOutput(outputId = "tree",width = "180%", height = "1200px"),
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
                                plotOutput("HM", width = "120%", height = "800px"),
                                linebreaks(2),
                                selectInput(inputId = "varEpi", label= "Select Column", choices=c(names(metaDat[2:ncol(metaDat)]))),
                                linebreaks(2),
                                plotlyOutput("Epi", width="135%")
                                
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
                                                        y = y, color=.data[[input$varOption]]), size=2)+
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
                                                        y = y, fill=.data[[input$varOption]]), size=2)+
        guides(colour = guide_legend(override.aes = list(size=6)))+
        theme(legend.text=element_text(size=10))
      
      plotlyTree<-ggplotly(plotMeta)
      plotlyTree
    }
    
  })
}

shinyApp(ui, server)