fluidPage(theme = shinytheme("united"),
  titlePanel("Indicator Distribution Visualizer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "domain",
                  label = "TerrADat or AquADat?",
                  choices = list("", "AquADat" = "aquatic", "TerrADat" = "terrestrial"),
                  selected = ""
      ),
      
      conditionalPanel(condition = "input.domain != ''",
                       tabsetPanel(id = "compref",
                                   "Use the options below to define your comparison or reference data by shapefile or query.",
                                   tabPanel(title = "Comparison",
                                            tabsetPanel(id = "filtertype",
                                                        tabPanel(title = "Shapefile",
                                                                 fileInput(inputId = "uploadzip",
                                                                           label = "Upload a polygon shapefile in a .ZIP",
                                                                           multiple = F,
                                                                           accept = c("application/zip")
                                                                 ),

                                                                 ## Select the field in the uploaded shapefile to filter with
                                                                 selectInput(inputId = "Comparisonfieldname",
                                                                             label = "Select the relevant attribute field in the shapefile:",
                                                                             choices = c("") ## The server has an observeEvent that populates this once there's a shapefile
                                                                 ),

                                                                 ## Select the values in the field selected above to use to filter by
                                                                 selectizeInput(inputId = "Comparisonfieldvalues",
                                                                                label = "Select the attribute field values to filter by:",
                                                                                choices = c(""), ## The server has an observeEvent that populates this once there's a field
                                                                                multiple = T
                                                                 ),
                                                                 actionButton(inputId = "terrafilter",
                                                                              label = "Filter TerrADat by selection"
                                                                 )
                                                        ),
                                                        tabPanel(title = "Query",
                                                                 textInput("Comparisonquery",
                                                                           label = "TerrADat Query",
                                                                           value = "ProjectName == \"California NorCal 2013\"",
                                                                           width = '100%'
                                                                 ),

                                                                 actionButton(inputId = "terraquery",
                                                                              label = "Filter TerrADat by query"
                                                                 )
                                                        )
                                            )
                                   ),
                                   tabPanel(title = "Reference",
                                            tabsetPanel(id = "filtertype",
                                                        tabPanel(title = "Shapefile",
                                                                 fileInput(inputId = "uploadzip",
                                                                           label = "Upload a polygon shapefile in a .ZIP",
                                                                           multiple = F,
                                                                           accept = c("application/zip")
                                                                 ),

                                                                 selectInput(inputId = "Referencefieldname",
                                                                             label = "Select the relevant attribute field in the shapefile:",
                                                                             choices = c("")
                                                                 ),


                                                                 selectizeInput(inputId = "Referencefieldvalues",
                                                                                label = "Select the attribute field values to filter by:",
                                                                                choices = c(""),
                                                                                multiple = T
                                                                 ),
                                                                 actionButton(inputId = "terrafilter",
                                                                              label = "Filter TerrADat by selection"
                                                        )
                                                        ),
                                                        tabPanel(title = "Query",
                                                                 textInput("query",
                                                                           label = "TerrADat Query",
                                                                           value = "ProjectName == \"California NorCal 2013\"",
                                                                           width = '100%'
                                                                 ),

                                                                 actionButton(inputId = "terraquery",
                                                                              label = "Filter TerrADat by query"
                                                                 )
                                                        )
                                            )
                                   )
                       ),
                       
                       selectInput(inputId = "comparisonplotdata",
                                   label = "Data to plot",
                                   choices = c("")
                       ),
                       
                       selectInput(inputId = "referenceplotdata",
                                   label = "Optional reference data to plot against",
                                   choices = c("None"),
                                   selected = "None"
                       ),
                       
                       conditionalPanel(condition = "input.comparisonplotdata == ''",
                                        helpText("Once you've defined and selected at least data you want to plot, you can generate a figure")
                       ),
                       
                       conditionalPanel(condition = "input.comparisonplotdata != ''",
                                        actionButton(inputId = "plotbutton",
                                                     label = "Plot selected data"
                                        )
                       )
      )
      

    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Instructions",
                 "Select stuff in the side panel to get a histogram of that stuff.",
                 dataTableOutput("filteredtable")
        ),
        
        tabPanel(title = "Histogram",
                 textOutput("emptyframe"),
                 plotOutput("plot"),
                 checkboxGroupInput(label = "Indicators to plot",
                                    inputId = "plotindicators",
                                    choices = c("No indicators available until data are defined" = "none")
                 )
        ),
        
        tabPanel(title = "Map",
                 leafletOutput("AIMmap")
        )
      )
    )
  )
)