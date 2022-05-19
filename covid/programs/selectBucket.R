library(shiny)

buckets<-aws.s3::bucketlist()



ui <-   fluidPage(
  selectInput('selectfile','Select bucket', choice = buckets$Bucket),
  plotOutput('tree')
)

server <- function(input,output){
  
  treeFile<-reactive({
    #dir <- file.path("./data")
    #treeFile<-input$selectfile
    #treePath<-file.path(dir, treeFile)
    tree<-aws.s3::s3read_using(FUN = ape::read.tree, bucket = input$selectfile, object = "tree.nwk") 
    return(tree)
  })  
  output$tree<-renderPlot({
    ggtree::ggtree(treeFile())
  })  
  
}

shinyApp(ui,server)
