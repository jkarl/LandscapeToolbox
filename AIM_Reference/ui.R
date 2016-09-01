
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

shinyUI(fluidPage(

  # Application title
  titlePanel("TerrADat Query"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("query",
                label="TerrADat Query",
                value="ProjectName == \"California NorCal 2013\"",
                width='100%'),
      actionButton("goButton",
                   label="Query TerrADat")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("AIMmap")
    )
  )
))
