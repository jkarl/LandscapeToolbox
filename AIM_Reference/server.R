
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(sp)
library(rgdal)
library(dplyr)
library(ggplot2)

datapath <- getwd()
TerrADat.gdd <- "Terradat_data_8.17.15_complete.gdb"

## Load TerrADat points
TerrADat.gdb <- paste(datapath,TerrADat.gdd,sep="/")
ogrListLayers(TerrADat.gdb)
tdat.point.fc <- readOGR(dsn=TerrADat.gdb, layer="SV_IND_TERRESTRIALAIM",stringsAsFactors=F)
tdat.prj <- proj4string(tdat.point.fc)

## Load AquADat reference points
aquatic.reference <- read.csv("aquatic_aim_priority_reference.csv", stringsAsFactors = F)

## Lists of aquatic indicators
riparian.indicators <- list("Percent overhead cover" = "XCDENMID", "Bank overhead cover" = "XCDENBK", "Vegetation Complexity" = "XCMG")
instream.indicators <- list("Habitat complexity" = "XFC_NAT", "Percent fines" = "PCT_SAFN", "Floodplain connectivity" = "LINCIS_H", "Residual pool depth" = "RP100")

shinyServer(function(input, output) {

    # filter terrestrial data
    filteredData <- reactive({
      q.string <- input$query
      eval.string <- paste('filter(tdat.point.fc@data, ',q.string,')',sep="")
      print(eval.string)
      tmp <- eval(parse(text=eval.string))
      return(tmp)
    })
    
    ## Filter the aquatic data. We're defining a reactive function that we can use to call aquatic.filtered() and get the currently-defined dataset
    aquatic.filtered <- reactive({
      ## First up is grabbing the correct indicator, either riparian or in-stream
      if (input$indicatortype == "riparian"){
        aquatic.indicator <- input$aquaticindicatorriparian
      } else if (input$indicatortype == "instream"){
        aquatic.indicator <- input$aquaticindicatorinstream
      }
      ## Then we filter for it and remove all the NAs
      tmp <- aquatic.reference %>% filter(indicator == aquatic.indicator) %>% filter(!is.na(value), THRESH %in% input$thresh)
      return(tmp)
    })
    

    ## Creating a histogram from the filtered reference data when the query button is hit
    observeEvent(input$aquagobutton,
                 {
                   ## Plotting the figure. This needs further expansion, but is generalized to pull the relevant information in to label the figure
                   figure.dataset <- aquatic.filtered()
                   output$histogram <- renderPlot(
                     figure.dataset %>%
                     ggplot(.data = figure.dataset, aes(x = value)) +
                     ## Arbitrarily decided that the resolution should be 20 bins for the figure. This can and should be flexible
                     geom_histogram(binwidth = {(max(figure.dataset$value) - min(figure.dataset$value)) / 20}) #+ 
                     #geom_density()
                   )
                 }
                 )
    
    
    # Create base leaflet map and add the base TerrADat points
    output$AIMmap <- renderLeaflet({
      leaflet(tdat.point.fc) %>%
        addProviderTiles("Stamen.TonerLite",
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addCircles(data=tdat.point.fc,radius=1,color="#777777",popup=~paste("PlotID:",PlotID)) %>%
        #setView(-115,41,zoom=5)
        fitBounds(tdat.point.fc@bbox[1,1],tdat.point.fc@bbox[2,1],tdat.point.fc@bbox[1,2],tdat.point.fc@bbox[2,2])
    })

    # add points to leaflet map
    #input$goButton
    observeEvent(input$terragobutton,{
        filtData <- filteredData()
        leafletProxy("AIMmap",data=filtData) %>%
        clearMarkers() %>%
        addCircleMarkers(radius=2,color="#DD7777",layerId=paste("q",1:nrow(filtData),sep=''),popup=~paste("PlotID:",PlotID))
    })

})
