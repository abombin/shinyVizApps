setwd("C:/Users/abomb/OneDrive - Emory University/bacteria/ICMC")
library(shiny)
library(ggtree)
library(ggplot2)
metaDat<-read.csv("C:/Users/abomb/OneDrive - Emory University/bacteria/ICMC/shinyMetadat.csv")
data<-ape::read.tree(file = "C:/Users/abomb/OneDrive - Emory University/bacteria/ICMC/staphylococcus-aureus_consensus.nwk")


# Define UI for miles per gallon app ----
ui <- navbarPage("Summary",
                 tabsetPanel(
                   tabPanel("Phylogeny", fluid=T,
                            fluidPage(
                              sidebarPanel(selectInput(inputId = "varColor",
                                                       label = "Color",
                                                       choices = c("State", "Streptomycin"),
                                                       selected = "State"), width=5),
                              mainPanel(
                                plotOutput(outputId = "tree"),
                                br(),
                                br(),
                                
                                plotOutput(outputId = "Pie"))
                            )),
                   tabPanel("Meta Data", fluid=T,
                            fluidPage(
                              mainPanel(DT::dataTableOutput("metaDat", width = 500))
                            )
                   )
                   
                 )
)


server <- function(input, output) {
  
  make_tree<-reactive({ggtree::ggtree(data)%<+% metaDat + 
      geom_treescale() +
      geom_tiplab(aes(color = .data[[input$varColor]])) + # size of label border  
      theme(legend.position = c(0.5,0.2), 
            legend.title = element_blank(), # no title
            legend.key = element_blank())
  })
  
  output$tree <- renderPlot({
    make_tree()
  })
  
  output$Pie<-renderPlot({
    title0<-as.character(input$varColor)
    varSum<-data.frame(table(metaDat[,input$varColor]))
    ggplot(varSum, aes(x="", y=Freq, fill=Var1))+
      geom_bar(stat="identity", width=1)+
      coord_polar("y", start=0)+
      theme_void()+
      scale_fill_discrete(name = title0)
  }, res = 150)
  
  output$metaDat <- DT::renderDataTable(
    metaDat,
    options = list(scrollX = TRUE)
  )
  
}

shinyApp(ui, server)