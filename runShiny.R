library(shiny)
setwd("./")
print(" app is running on:  http://10.66.123.250:4000/")
runApp(appDir="./", host = getOption("shiny.host", "10.66.123.250"), port=4000)