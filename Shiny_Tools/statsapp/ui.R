library(shiny)


shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Assessment and Monitoring Analysis Tools"),
  
  sidebarPanel(
    tags$head(
      tags$style(type="text/css", "select { max-width: 150px; }"),
      tags$style(type="text/css", "textarea { max-width: 135px; }"),
      tags$style(type="text/css", ".jslider { max-width: 150px; }"),
      tags$style(type='text/css', ".well { max-width: 200px; }"),
      tags$style(type='text/css', ".span4 { max-width: 200px; }")
    ),
    
    fileInput('file1', 'Choose an XLS or XLSX File',
              accept=c('binary'))
   # tags$hr(),
    
  ),
      
  mainPanel(
    tabsetPanel(
      tabPanel("plot", helpText("summary for first two groups only"),
               verbatimTextOutput("summary1"),
               verbatimTextOutput("summary2"),
               plotOutput("plot"),
               p(paste("Histogram of values from Excel"))),
      
      tabPanel("Data Table", tableOutput('contents1')),
                #tableOutput('contents2')),
      
      tabPanel("Threshold Analysis", 
              numericInput("thresh", "enter a threshold value:", 10),
             #  uiOutput("thresholdControl")
              plotOutput("threshPlot")),
      
      tabPanel("2 Sample T-Test", 
               verbatimTextOutput("twoSampTtestOutput"),
               plotOutput("twoSampTtestPlot")), 
      
      tabPanel("Paired T-Test", 
               verbatimTextOutput("pairedSampTtestOutput"),
               plotOutput("pairedSampTtestPlot")),
      
      tabPanel("ANOVA",
               verbatimTextOutput("AOVsummary"), 
               plotOutput("AOVplot")), 
      
      tabPanel("Instructions", 
               includeHTML("instructions.html"))
               
              )
    
    )
))
