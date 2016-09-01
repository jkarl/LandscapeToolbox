
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(sp)
library(rgdal)

datapath <- getwd()
TerrADat.gdd <- "Terradat_data_8.17.15_complete.gdb"

## Load TerrADat points
TerrADat.gdb <- paste(datapath,TerrADat.gdd,sep="/")
ogrListLayers(TerrADat.gdb)
tdat.point.fc <- readOGR(dsn=TerrADat.gdb, layer="SV_IND_TERRESTRIALAIM",stringsAsFactors=F)
tdat.prj <- proj4string(tdat.point.fc)

shinyServer(function(input, output) {

    # filter data
    filteredData <- reactive({
      q.string <- input$query
      eval.string <- paste('filter(tdat.point.fc@data, ',q.string,')',sep="")
      print(eval.string)
      tmp <- eval(parse(text=eval.string))
      return(tmp)
      })
    

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
    observeEvent(input$goButton,{
        filtData <- filteredData()
        leafletProxy("AIMmap",data=filtData) %>%
        clearMarkers() %>%
        addCircleMarkers(radius=2,color="#DD7777",layerId=paste("q",1:nrow(filtData),sep=''),popup=~paste("PlotID:",PlotID))
    })

})
