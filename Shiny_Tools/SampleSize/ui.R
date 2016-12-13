library(shiny)

htmlText = '<p>This tool allows for calculation of sampling sufficiency based on observed and desired minimum detectable difference (MDD or effect size) from a set of sample data. This tool can also be used to estimate sample sizes necessary to meet monitoring objectives from pilot data.</p><p>To use this tool, upload a DIMA indicator report file (Excel). Select the Results tab to calculate sampling sufficiency and see tabular and graphical results.</p>'


# Define UI for application that calculates sample sizes and sampling sufficiency from a set of input data
shinyUI(fluidPage(
  #tags$head(includeScript('google_analytics.js')),  
  #Application Title
  titlePanel("Sampling Sufficiency Calculator"),
  
  #######################################################################################
  ## Sidebar for inputs and options
  #######################################################################################
  sidebarLayout(   #Tells Shiny that we're using a page layout with a Sidebar
    sidebarPanel(  #Initializes the sidebar      
      h4("Load Indicator File..."),
      selectInput("fileType","File Type",c("DIMA Indicator File","Generic Indicator File"),selected="DIMA Indicator File"),
      fileInput("file1",h5("(Indicator File (.xls, .xlsx, or .csv)"), multiple=FALSE),
      HTML("<br>"),
      h4("Input Options"),
      uiOutput("ChooseStrata"),
      uiOutput("ChoosePlotID"),
      uiOutput("ChooseIndicatorField"),
      uiOutput("ChooseIndicator"),
      uiOutput("ChooseValueField"),
      HTML("<br>"),
      h4("Analysis Options"),
      selectInput("analysisType","Analysis Type",c("Threshold Test","Two Independent Samples","Repeated Measures")),
      checkboxInput("PropData","Proportion Data?",value=FALSE),
      numericInput("MDD","Desired MDD (% change)",0.15,min=0.01,max=9999,step=0.01),
      numericInput("Alpha","Significance Level (Alpha)",0.05,min=0.01,max=0.99,step=0.01),
      numericInput("Rho","Intraobservation Correlation",0.6,min=0.0,max=0.99,step=0.01),
      
      HTML("<br>"),
      actionButton("goButton",h4("Rerun Analysis")),
      hr(),
      img(src="http://dev.landscapetoolbox.org/wp-content/uploads/2015/01/JER_BLM_logos.png")
      
      
      ),
  #######################################################################################
  ## Main panel for outputs
  #######################################################################################
  mainPanel(  #Initializes the main panel
    tabsetPanel(
      tabPanel('Instructions',HTML(htmlText)),
      tabPanel('Input Data Table', dataTableOutput("DataTable")),
      tabPanel('Results Table', dataTableOutput("ssResults"),
               plotOutput("powerPlot"),
               HTML("<hr>"),
               downloadButton("downloadTable","Download Tabular Results"),
               downloadButton("downloadPlot","Save Plot to Image")
               )
      )
    
    )
  )
  ))