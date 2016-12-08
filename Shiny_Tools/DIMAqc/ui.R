#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("DIMA Quality Control Analysis Tool"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       fileInput("DIMAfile",
                  "Select DIMA File"
       ),
       hr(),
       h3("Filter DIMA Data"),
       selectInput("Year","Select year",choices=""),
       selectInput("Site","Select Site",choices=""),
       downloadButton('runReport',"Run Report")
    ),
    
    # Main panel with tabs
    mainPanel(
      HTML("<p>It is important to understand the quality of monitoring data in order to be able to successfully use it to support management decision making. 
          This tool runs a series of Quality Control assessments on data contained in a DIMA database and produces a downloadable report. The purpose of the
          report is to identify statistical patterns that exist 
          in core monitoring data (LPI, Vegetation Height, Canopy Gap, Soil Stability, Species Inventory) and to flag data values that are outside of what is expected 
          (i.e., potential errors or extreme events). This report consists of the following sections:</p>
          <ol>
          <li>DIMA Data Summary</li>
          <li>Crew Variability Checks</li>
          <li>Missing Data Checks</li>
          <li>Suspect Value Checks</li>
          <li>Temporal Variability in Indicators</li>
          </ol>
           <p> To use this tool, upload your DIMA Database and filter the data if necessary. When you click <em>Run Report</em> Shiny/R will open a new, blank browser tab and compile the report and download it.
          Compiling the report can take several minutes. Please be patient.</p>
           ")
    )
  ),
  fluidRow(
    width="100%", height="100px",
    hr(),
    h4("App Log: "),
    verbatimTextOutput("Log")
  )
))
