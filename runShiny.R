library(shiny)
setwd("C:/Users/abomb/OneDrive - Emory University/bacteria/ICMC")

runApp(appDir="shinyApp", host = getOption("shiny.host", "127.0.0.1"), port=3269)