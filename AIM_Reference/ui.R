
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
      ## Choosing whether you want to look at terrestrial or aquatic data
      selectInput(inputId = "domain",
                  label = "TerrADat or AquADat?",
                  choices = list("AquADat" = "aquadat", "TerrADat" = "terradat"),
                  selected = "aquadat"
                  ),

      #### The panel of AquADat options visible only when input$domain == "AquADat" ####
      conditionalPanel(
        condition = "input.domain == 'aquadat'",
        ## Choose between riparian and in-stream indicators
        selectInput(inputId = "indicatortype",
                    label = "In-stream or riparian indicator?",
                    choices = list("In-stream" = "instream", "Riparian" = "riparian"),
                    selected = "instream"
                    ),
        ## Options for when working with in-stream indicators
        conditionalPanel(
          condition = "input.indicatortype == 'instream'",
          ## Indicators to select
          selectInput(inputId = "aquaticindicatorinstream",
                      label = "Indicator to compare.",
                      choices = instream.indicators,
                      selected = instream.indicators[1]
                      )
          ),
        ## Options for when working with riparian indicators
        conditionalPanel(
          condition = "input.indicatortype == 'riparian'",
          ## Indicators to select
          selectInput(inputId = "aquaticindicatorriparian",
                      label = "Indicator to compare.",
                      choices = riparian.indicators,
                      selected = riparian.indicators[1]
                      )
          ),
        
        ## What THRESH values to use
        selectizeInput(inputId = "thresh",
                       label = "Select one or more THRESH values to filter data by.",
                       choices = unique(aquatic.reference$THRESH),
                       multiple = T
                       ),
        ## Display the button to use the entered query
        actionButton(inputId = "aquagobutton",
                     label="Query AquADat")
      ), ## Closure for the AquADat panels
      
            
      #### The panel of TerrADat options visible only when input$domain == "TerrADat"
      conditionalPanel(
        condition = "input.domain == 'terradat'",
        
        ## Display the option to enter a TerrADat query
        textInput("query",
                  label="TerrADat Query",
                  value="ProjectName == \"California NorCal 2013\"",
                  width='100%'
                  ),
        ## Display the button to use the entered query
        actionButton(inputId = "terragobutton",
                     label="Query TerrADat")
      ) ## Closure for the TerrADat panels
    ), ## Closure for the sidebar definitions

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("histogram")
      # leafletOutput("AIMmap")
    ) ## Closure for mainpanel
  ) ## Closure for sidebarlayout
) ## Closure for fluidpage
) ## Closure for shinyui
