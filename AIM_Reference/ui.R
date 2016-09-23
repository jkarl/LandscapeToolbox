
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

shinyUI(fluidPage(

  # Application title
  titlePanel("Indicator Distribution Visualizer"),

  ## Setting up the panels
  sidebarLayout(
    sidebarPanel(
      ## Choosing whether you want to look at terrestrial or aquatic data
      selectInput(inputId = "domain",
                  label = "TerrADat or AquADat?",
                  choices = list("", "AquADat" = "aquatic", "TerrADat" = "terrestrial"),
                  selected = ""
                  ),

#### The panel of AquADat options visible only when input$domain == "AquADat" ----
      conditionalPanel(
        condition = "input.domain == 'aquatic'",
## Choose between riparian and in-stream indicators ====
        selectInput(inputId = "indicatortype",
                    label = "In-stream or riparian indicator?",
                    choices = list("In-stream" = "instream", "Riparian" = "riparian"),
                    selected = "instream"
                    ),
## Options for when working with in-stream indicators ====
        conditionalPanel(
          condition = "input.indicatortype == 'instream'",
          ## Indicators to select
          selectInput(inputId = "aquaticindicatorinstream",
                      label = "Indicator to compare.",
                      choices = instream.indicators,
                      selected = instream.indicators[1]
                      )
          ),
## Options for when working with riparian indicators =====
        conditionalPanel(
          condition = "input.indicatortype == 'riparian'",
          ## Indicators to select
          selectInput(inputId = "aquaticindicatorriparian",
                      label = "Indicator to compare.",
                      choices = riparian.indicators,
                      selected = riparian.indicators[1]
                      )
          ),
        
## What THRESH values to use ====
        selectizeInput(inputId = "thresh",
                       label = "Select one or more THRESH values to filter data by.",
                       choices = unique(aquatic.reference$THRESH),
                       multiple = T
                       ),
## Display the button to use the entered query ====
        actionButton(inputId = "aquagobutton",
                     label="Query AquADat")
      ), ## Closure for the AquADat panels
      
            # 
#### The panel of TerrADat options visible only when input$domain == "TerrADat" ----
      conditionalPanel(
        condition = "input.domain == 'terrestrial'",
        
        radioButtons(inputId = "compref",
                     label = "Defining comparison data or reference data?",
                     choices = c("Comparison" = "comparison", "Reference" = "reference"),
                     selected = "comparison",
                     inline = T
        ),
        
        radioButtons(inputId = "filtertype",
                     label = "Defining with a shapefile or a query?",
                     choices = c("Shapefile" = "shape", "Query" = "query"),
                     selected = "shape",
                     inline = T
        ),
        
        ## Display the option to enter a TerrADat query ====
        conditionalPanel(condition = "input.filtertype == 'query'",
                         
                         textInput("query",
                                   label = "TerrADat Query",
                                   value = "ProjectName == \"California NorCal 2013\"",
                                   width = '100%'
                                  ),
                         
                         actionButton(inputId = "terraquery",
                                      label = "Filter TerrADat by query"),
                         
                         helpText(textOutput("queryerror")) ## Jury-rigging because output can't have logical values? If there's an error, output$queryerror is changed to an error message letting the user know they screwed up
                         ),
        
        conditionalPanel(condition = "input.filtertype == 'shape'",
                         ## Upload a .zip containing the .shp and attendant files
                         fileInput(inputId = "uploadzip",
                                   label = "Upload a polygon shapefile in a .ZIP",
                                   multiple = F,
                                   accept = c("application/zip")
                                   ),
                         
                         ## Select the field in the uploaded shapefile to filter with
                         selectInput(inputId = "fieldname",
                                     label = "Select the relevant attribute field in the shapefile:",
                                     choices = c("") ## The server has an observeEvent that populates this once there's a shapefile
                                     ),
                         
                         ## Select the values in the field selected above to use to filter by
                         selectizeInput(inputId = "fieldvalues",
                                        label = "Select the attribute field values to filter by:",
                                        choices = c(""), ## The server has an observeEvent that populates this once there's a field
                                        multiple = T
                                        ),
                         
                         actionButton(inputId = "terrafilter",
                                      label = "Filter TerrADat by selection")
                         )#,

        ## Display the button to use the entered query ====
        # actionButton(inputId = "terragobutton",
        #              label = "Query TerrADat"),
        # ## Display the button to filter with
        # actionButton(inputId = "terrapolygobutton",
        #              label = "Filter TerrADat with shapefile"),
        ## If the query threw an error, this will let you know
      ) ## Closure for the TerrADat panels
    ), ## Closure for the sidebar definitions

## The main panel ----
    mainPanel(
      ## We're going to make a set of tabs to organize things
      tabsetPanel(
## The landing tab that explains a bit of what's going on ====
        tabPanel(title = "Instructions",
                 "Select stuff in the side panel to get a histogram of that stuff.",
                 dataTableOutput("filteredtable")
                 ),
## A tab for the histogram ====
        tabPanel(title = "Histogram",
                 plotOutput("histogram")
                 ),
## A tab for the TerrADat map ====
        ## Maybe make this a map of all the AIM points, terrestrial and aquatic, in different colors?
        tabPanel(title = "Map",
                 leafletOutput("AIMmap")
                 )
      ) ## Closure for tabsetpanel
    ) ## Closure for mainpanel
  ) ## Closure for sidebarlayout
) ## Closure for fluidpage
) ## Closure for shinyui
