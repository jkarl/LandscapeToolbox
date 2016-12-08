library(shiny)

shinyUI(fluidPage(
  titlePanel("DIMA Database Analysis Tools"),
  sidebarLayout(
    sidebarPanel(
      fileInput('infile', h5('Choose DIMA File'),accept='application/x-msaccess'),
      submitButton("Update View")
      ),
    
    mainPanel(
      h4("Summary"),
      verbatimTextOutput("test"),
      
      h4("Plots"),
      plotOutput("DIMAPlot", height="300px")
    )
    
  )  
))