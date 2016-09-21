
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
library(purrr)
library(stringr)

#setwd(paste0(getwd(), "/AIM_Reference/")) ## Because the working directory is one level up when I run this
datapath <- getwd()
TerrADat.gdd <- "Terradat_data_8.17.15_complete.gdb"

#### Load TerrADat points ####
TerrADat.gdb <- paste(datapath,TerrADat.gdd,sep="/")
ogrListLayers(TerrADat.gdb)
tdat.point.fc <- readOGR(dsn=TerrADat.gdb, layer="SV_IND_TERRESTRIALAIM",stringsAsFactors=F)
tdat.prj <- proj4string(tdat.point.fc)

#### Load AquADat reference points ####
aquatic.reference <- read.csv("aquatic_aim_priority_reference.csv", stringsAsFactors = F)

#### Lists of aquatic indicators ####
riparian.indicators <- list("Percent overhead cover" = "XCDENMID", "Bank overhead cover" = "XCDENBK", "Vegetation Complexity" = "XCMG")
instream.indicators <- list("Habitat complexity" = "XFC_NAT", "Percent fines" = "PCT_SAFN", "Floodplain connectivity" = "LINCIS_H", "Residual pool depth" = "RP100")

#### Starting up that Shiny backend! ####
shinyServer(function(input, output, session) {

  temp <- reactiveValues()
#### We need to handle the extraction and reading-in of the shapefile from a .zip. This is a pain. ####
  ## The solution is a to wrap the process in an observe() so that if any of the inputs it depends on (specifically the uploaded file) changes,
  ## then the chunk of code will execute and read in the shapefile from the upload
  observe({
    file <- input$uploadzip ## There should've been an uploaded file and we're storing it as file
    if (is.null(file)) {return(NULL)} ## Emphasis on 'should' so we take precautions in case it's not been uploaded
    if (!grepl(".zip", file$name, fixed = T)) {return(NULL)} ## Check to see if grepl()ing the file name for ".zip" returns FALSE. Because we want to return NULL if that's the case, we use the ! to flip the F to T to get a response when some fool's uploaded a non-.zip
    ## Theoretically, the preceding two lines caught the worst case scenarios and we have a .zip, so now we can extract its contents
    print("File exists and ends in .zip")
    print("The value in file$datapath is:")
    print(dirname(file$datapath)) ## Just some diagnostic output in the terminal, not that an end-user will ever see it
    ## This was adapted from the LandscapeToolbox spatially-balanced points tool because that was broken(?????)
    switch(Sys.info()[["sysname"]], ## switch() takes as its first argument an expression that resolves to a number or character string, then executes the subsequent argument that matches that number or character string
           Windows = { ## For a Windows system, which should happen when the named value "sysname" in the list resulting from Sys.info() is "Windows", do the following
             print("This is Windows.")
             origdir = getwd() ## Save what the current working directory is so we can set it back at the end of this
             setwd(dirname(file$datapath)) ## Set the new working directory to the uploaded file's datapath. Not sure why this is here, but removing it breaks stuff
             system(paste0("cmd.exe /c cd ", dirname(file$datapath))) ## Pass this argument to the OS. It changes directories. When making Windows system calls, you need to invoke "cmd.exe /c" first
             system(paste0("cmd.exe /c \"C:\\Program Files\\7-Zip\\7z\".exe e -aoa ", file$datapath)) ## Pass the extraction argument to the OS. I had to aim it at my 7zip install. If yours is elsewhere, change the filepath to it, but know that those escaped quotation marks are necessary if there are spaces in your folder names. Thanks, Microsoft
             setwd(origdir) ## Restoring the working directory
             print("Resetting working directory to:") ## Diagnostic terminal output to reassure a debugger that it is in fact reset to the original working directory
             print(getwd())
           },
           Linux = { ## If the OS is Linux then:
             print("This is Unix. I know this.")
             origdir = getwd() ## Store the working directory as is, so we can restore it at the end of unzipping
             setwd(dirname(file$datapath)) ## Setting the working directory
             system(sprintf("cd %s", dirname(file$datapath))) ## Passing this to the OS
             print(getwd()) ## Just checking for debugging
             system(sprintf("unzip -u %s", inFile$datapath)) ## The unzipping argument to pass to the OS
             setwd(origdir) ## Set the working directory back
           }
        )
    ## We need the shapefile name and for it to not have the file extension
    shapename <- list.files(path = dirname(file$datapath), pattern = ".shp$") %>% ## List all the files in the extracted folder that end in .shp. If there's more then one this is almost certainly going to burn.
      str_replace(., ".shp", "") ## Strip out the .shp
    print("The shapefile name, sans extension, is:") ## Just diagnostic reassurance.
    print(shapename) ## If you see "character(0)" then something's wrong, but you probably already knew that
    ## I suspect the next bit is overcomplicated because SOMEONE didn't know an easier way to use readOGR(), but I can't be bothered to do more than copy/paste right now and it works
    shape <- readOGR(dsn = dirname(file$datapath), layer = shapename) ## Read in the shapefile using the file location and the shapefile name. Normally I'd provide the full filepath down to the file extension for the dsn argument but this doesn't, so that may explain why someone wrote the next line
    shape$dirname <- substr(file$datapath, 1, nchar(file$datapath) - 2) %>% as.character() %>% paste() ## I rewrote this just to use pipes, but I don't understand why it's here or the specifics of substr()
    shape <- shape %>% spTransform(., CRS(tdat.prj)) ## Pre-emptively get the shapefile into the same projection as the TerrADat data
    print("The structure of shape before trying to write it from within the observe({})")
    str(shape@data)
    temp$shape <- shape
    print("The structure of the shape now that it's stored in the reactive value object temp")
    str(temp$shape@data)
  })
  
  
#### filter terrestrial data ####
  ## This is defining a reactive function that takes the query from ui.R and filters by it
    filteredData <- reactive({
      q.string <- input$query ## Pull in the query (a string)
      eval.string <- paste('filter(tdat.point.fc@data, ',q.string,')',sep="") ## Construct a string that constitutes the filter() function and its arguments
      print(eval.string)
      ## Creating a version of eval() using the purrr adverb safely(). safe.eval() returns a named list with "result" and "error", one of which will be NULL
      ## Calling safe.eval won't crash the app and we can check to see if is.null(output[["result"]]) elsewhere to decide how to procede
      safe.eval <- safely(eval)
      tmp <- safe.eval(parse(text=eval.string))[["result"]] ## Run the string as though it were the function+arguments. Because it is, theoretically
      return(tmp)
    })
    
#### Filter the aquatic data ####
    ## We're defining a reactive function that we can use eslewhere so that calling aquatic.filtered()
    ## is equivalent to just inserting the currently-defined dataset
    aquatic.filtered <- reactive({
      ## First up is grabbing the correct indicator, either riparian or in-stream
      ## I wasn't getting the expected results when both the in-stream and riparian panels in the UI were writing to the same input object
      ## so this takes takes the appropriate one based on the indicatortype selection and writes it to an object I can use
      if (input$indicatortype == "riparian"){
        aquatic.indicator <- input$aquaticindicatorriparian
      } else if (input$indicatortype == "instream"){
        aquatic.indicator <- input$aquaticindicatorinstream
      }
      ## Then we filter for THRESH selection(s) and remove all the NAs
      tmp <- aquatic.reference %>% filter(x = ., indicator == aquatic.indicator) %>%
        filter(., !is.na(value), THRESH %in% input$thresh)
      return(tmp)
    })
    

#### Creating a histogram from the filtered reference data when the query button is hit ####
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
    
#### Updating the values of the field selectizeInput() in the UI from our shapefile
    observeEvent(eventExpr = temp$shape, ## Theoretically, when there's a new shapefile, this becomes true
                 handlerExpr = {
                   print("The structure of temp$shape is:")
                   str(temp$shape)
                   print("The column names of temp$shape@data are:")
                   print(paste(colnames(temp$shape@data), collapse = ", "))
                   updateSelectInput(session = session,
                                     inputId = "fieldname",
                                     choices = as.list(colnames(temp$shape@data)),
                                     selected = head(colnames(temp$shape@data)))
                   print("Party hard") ## Diagnostic to make sure we made it this far
                   }
                 )
    
    observeEvent(eventExpr = !is.null(input$fieldname),
                 handlerExpr = {
                   if (input$fieldname != "") {
                     print("The selected field name is:")
                     print(paste(input$fieldname))
                     print("The values in that field in the shapefile are:")
                     print(paste(temp$shape@data[,input$fieldname], collapse = ", "))
                     updateSelectizeInput(session = session,
                                       inputId = "fieldvalues",
                                       choices = as.character(temp$shape@data[, (colnames(temp$shape@data) %in% input$fieldname)]) %>% unique()
                                       )
                     str(as.character(temp$shape@data[, (colnames(temp$shape@data) %in% input$fieldname)]))
                     str(input$fieldvalues)
                     print("Rock on") ## Diagnostic to make sure we made it this far
                   }
                 }
    )
    
    observeEvent(eventExpr = input$terrapolygobutton, ## When they hit the button
                 handlerExpr = {
                   print(paste0("Current selected values in the field ", input$fieldname, " are:"))
                   print(paste(input$fieldvalues, collapse = ", "))
                   restrictedshape <- temp$shape[(temp$shape@data[, paste(input$fieldname)] %in% as.vector(input$fieldvalues)),] ## Slice the polygons down to the areas where the values in the selected field match the values that the user chose
                   filterterradatindices <- over(tdat.point.fc, restrictedshape)[, paste(input$fieldname)] %>% is.na() %>% !. ## Get the column from the data frame where the points were intersected with the polygons, turn it logical, and flip the values because we want the indices where they overlapped
                   filterterradat <- tdat.point.fc[filterterradatindices,] ## Couldn't be part of the above line because adding it caused !. to think that it was getting a vector with two dimensions(???)
                   output$filteredtable <- renderTable(filterterradat@data[])
                   # if (!is.null(filterterradat)){ ## If there was an error in filtering the data, this will be NULL
                   #   print("Doesn't look like there was an error") ## For debug purposes
                   #   leafletProxy("AIMmap", data = filterterradat@data) %>% ## Adding it to the map
                   #     clearMarkers() %>%
                   #     addCircleMarkers(radius = 2,
                   #                      color = "#DD7777",
                   #                      layerId = paste("q", 1:nrow(filterterradat@data), sep = ''),
                   #                      popup = ~paste("PlotID:",PlotID)
                   #                      )
                   #    }
                   }
                 )

    
    
#### Create base leaflet map and add the base TerrADat points ####
    output$AIMmap <- renderLeaflet({
      leaflet(tdat.point.fc) %>%
        addProviderTiles("Stamen.TonerLite",
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addCircles(data=tdat.point.fc,radius=1,color="#777777",popup=~paste("PlotID:",PlotID)) %>%
        #setView(-115,41,zoom=5)
        fitBounds(tdat.point.fc@bbox[1,1],tdat.point.fc@bbox[2,1],tdat.point.fc@bbox[1,2],tdat.point.fc@bbox[2,2])
    })

#### add points to leaflet map ####
    #input$goButton
    observeEvent(input$terragobutton,{
        filtData <- filteredData() ## The output of filteredData comes from a safe()-wrapped function where we already requested just the "result" from the list
        filtData %>% str() ## Part of debugging
        if (!is.null(filtData)){ ## If there was an error in filtering the data, the filtData will be NULL
          print("Doesn't look like there was an error") ## For debug purposes
          output$queryerror <- renderText("") ## There wasn't an error!
          leafletProxy("AIMmap",data=filtData) %>% ## Adding it to the map
          clearMarkers() %>%
          addCircleMarkers(radius=2,color="#DD7777",layerId=paste("q",1:nrow(filtData),sep=''),popup=~paste("PlotID:",PlotID))
        } else if (is.null(filtData)) { ## The alternative to "we avoided an error" is "an error happened"
            output$queryerror <- renderText("There was an error in subsetting the data. Check to make sure your query isn't malformed.") ## We can use this value on the UI side to let the user know the query was borked. Logical values aren't allowed, so we're just writing the whole error message
        }
        # leafletProxy("AIMmap",data=filtData) %>%
        # clearMarkers() %>%
        # addCircleMarkers(radius=2,color="#DD7777",layerId=paste("q",1:nrow(filtData),sep=''),popup=~paste("PlotID:",PlotID))
    })

})
