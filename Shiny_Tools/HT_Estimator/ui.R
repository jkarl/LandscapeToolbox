library(shiny)

shinyUI(fluidPage(
  tags$head(includeScript("google_analytics.js")),
  titlePanel("Simple Horwitz-Thompson Indicator Estimator"),
  
  sidebarLayout(

    sidebarPanel(
      fileInput('infile', h5('Choose CSV File'),
          accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      hr(),
      h4('Input Options'),
      selectInput("ind_field",label="Select indicator field","no file loaded"),
      selectInput("weight_field",label="Weights field (optional)","no file loaded"),
      selectInput("strat_field",label="Strata field (optional)","no file loaded"),
      selectInput("strat_sel",label="Stratum (optional)","no stratum field selected"),
      selectInput("type",label="Variance estimation type",c("Simple Random Selection"="SRS","Local Means Estimation"="local"),selected=("SRS")),
      selectInput("x_field",label="Longitude (X) field (optional)","no file loaded"),
      selectInput("y_field",label="latitude (Y) field (optional)","no file loaded"),
      sliderInput("conf",label="Confidence level (%)",min=50,max=100,step=5,format="#",value=90,ticks=TRUE),
      hr(),
      img(src="http://dev.landscapetoolbox.org/wp-content/uploads/2015/01/JER_BLM_logos.png")
      ), #close sidebarPanel
    
    mainPanel(
      tabsetPanel(
        tabPanel('Instructions',h4("Background"),
                 div("The Horwitz-Thompson Estimator tool can be used to produce indicator estimates from survey data either 1) for a study area without strata or 2) within a stratum. Sample points can be unweighted (most typical) or individually weighted (i.e., unequal probability sampling)."),
        h5("Note that this tool does not take into consideration any survey/monitoring design information (e.g., stratum weights)."),div("It is appropriate to use this tool for exploratory data analysis and generating quick, preliminary estimates from monitoring data. For final indicator estimates, you should use a tool (or stats package) that accounts for survey design information."),
        div("The input to this tool is a comma-delimited (CSV) spreadsheet file with columns for indicator values, weights (optional), strata, and point coordinates like in the example below."),
        img(src="http://dev.landscapetoolbox.org/wp-content/uploads/2015/01/HT_input_example.png"),
        h4("To use this tool:"),
        HTML('<ol><li>Click the <strong>Choose File</strong> button at the top left of the window. Once the file has loaded you can switch to the <strong>Input Data Table</strong> tab to view the data.</li>
                 <li>Use the drop-down boxes at the left of the window to select an indicator to calculate estimates for.</li>
                 <li>Choose other analysis options using the drop down boxes. If no strata field is selected, estimates will be calculated across all input data. If a <strong>Strata field</strong> is selected, choosing a <strong>Strata</strong> will calculate indicator estimates for only that stratum. If the <strong>Variance estimation type</strong> is changed to "local means estimation", fields for the longitude and latitude of the sample points must also be specified.</li>
                 <li>Choose a confidence level for the indicator estimates or use the default value.</li>
                 <li>Click on the <strong>Results Table</strong> tab to see the output.</li>')),
        tabPanel('Input Data Table', h3("Input Data Table"), dataTableOutput('contents')),
        tabPanel('Results Table', h3("Indicator Estimates"),h4(textOutput('subtitle')),h4(textOutput('message')),dataTableOutput('HTresults'),
                 h5('*Note: These estimates do not take into account any survey design-related information that may be important to calculating indicator estimates (e.g., stratum weights, non-response).'),hr(), plotOutput("histogram")),
        tabPanel('More Info',h4('Additional Background and Information'),p("This tool uses the total.est command in the spsurvey package for R version 3.1.2 to produce indicator estimates. This tool is appropriate for analyzing continuous data, not categorical or ordinal data. Depending on the options selected for the tool, one of the following commands is used: "),
                 pre("total.est(indicator[obs],weights[obs],vartype='SRS',conf=a)"),p("or"),pre("total.est(indicator[obs],weights[obs],x[obs],y[obs],vartype='local',conf=a)"),
                 p("where obs is a vector denoting which records are to be used for analysis within a particular stratum (all records otherwise) and a is the selected confidence level."),
                 p("For more information on the total.est routine and the spsurvey package, see the following resources:"),
                 HTML("<ul><li><a href='http://cran.r-project.org/web/packages/spsurvey/index.html'>spsurvey package description on r-project.org</a></li>
                      <li><a href='http://www.epa.gov/nheerl/arm/documents/intro.pdf'>US-EPA EMAP Statistical Methods Manual</a></li></ul>"))
        ) # close tabsetPanel
      ) #close mainPanel
    
    ) #close sidebarLayout
  )) #close shinyUI and fluidPage