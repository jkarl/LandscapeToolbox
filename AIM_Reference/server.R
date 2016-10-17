library(shiny)
library(shinythemes)
library(leaflet)
library(sp)
library(rgdal)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)

#setwd(paste0(getwd(), "/AIM_Reference/")) ## Because the working directory is one level up when I run this
datapath <- getwd()
TerrADat.gdd <- "Terradat_data_8.17.15_complete.gdb"

#### Load TerrADat points ####
TerrADat.gdb <- paste(datapath, TerrADat.gdd, sep = "/")
ogrListLayers(TerrADat.gdb)
tdat.point.fc <- readOGR(dsn = TerrADat.gdb, layer = "SV_IND_TERRESTRIALAIM", stringsAsFactors = F)
tdat.prj <- proj4string(tdat.point.fc)

#### Load AquADat reference points ####
aquatic.reference <- read.csv("aquatic_aim_priority_reference.csv", stringsAsFactors = F)

#### Lists of aquatic indicators ####
riparian.indicators <- list("Percent overhead cover" = "XCDENMID", "Bank overhead cover" = "XCDENBK", "Vegetation Complexity" = "XCMG")
instream.indicators <- list("Habitat complexity" = "XFC_NAT", "Percent fines" = "PCT_SAFN", "Floodplain connectivity" = "LINCIS_H", "Residual pool depth" = "RP100")

#### List of terrestrial indicators ####
terrestrial.indicators <- {c(GapPct_25_50 = "Gaps in foliar cover between 25 and 50 cm",
                             GapPct_51_100 = "Gaps in foliar cover between 51 and 100 cm",
                             GapPct_101_200 = "Gaps in foliar cover between 101 and 200 cm",
                             GapPct_200_plus = "Gaps in foliar cover greater than 200 cm",
                             GapPct_25_plus = "Gaps in foliar cover greater than 25 cm",
                             BareSoilCover_FH = "Bare soil",
                             TotalFoliarCover_FH = "Total foliar cover",
                             NonInvPerenForbCover_AH = "Non-invasive perennial forb cover",
                             NonInvAnnForbCover_AH = "Non-invasive annual forb cover",
                             NonInvPerenGrassCover_AH = "Non-invasive perennial grass cover",
                             NonInvAnnGrassCover_AH = "Non-invasive annual grass cover",
                             NonInvAnnForbGrassCover_AH = "Non-invasive annual forb and grass cover",
                             NonInvPerenForbGrassCover_AH = "Non-invasive perennial forb and grass cover",
                             NonInvSucculentCover_AH = "Non-invasive succulent cover",
                             NonInvShrubCover_AH = "Non-invasive shrub cover",
                             NonInvSubShrubCover_AH = "Non-invasive subshrub cover",
                             NonInvTreeCover_AH = "Non-invasive tree cover",
                             InvPerenForbCover_AH = "Invasive perennial forb cover",
                             InvAnnForbCover_AH = "Invasive annual forb cover",
                             InvPerenGrassCover_AH = "Invasive perennial grass cover",
                             InvAnnGrassCover_AH = "Invasive annual grass cover",
                             InvAnnForbGrassCover_AH = "Invasive annual forb and grass cover",
                             InvPerenForbGrassCover_AH = "Invasive perennial forb and grass cover",
                             InvSucculentCover_AH = "Invasive succulent cover",
                             InvShrubCover_AH = "Invasive shrub cover",
                             InvSubShrubCover_AH = "Invasive subshrub cover",
                             InvTreeCover_AH = "Invasive tree cover",
                             SagebrushCover_AH = "Sagebrush cover",
                             WoodyHgt_Avg = "Average woody plant height",
                             HerbaceousHgt_Avg = "Average herbaceous plant height",
                             SagebrushHgt_Avg = "Average sagebrush height",
                             OtherShrubHgt_Avg = "Average non-sagebrush shrub height",
                             NonInvPerenGrassHgt_Avg = "Average non-invasive perennial grass height",
                             InvPerenGrassHgt_Avg = "Average invasive perennial grass height",
                             InvPlantCover_AH = "Invasive plant cover",
                             InvPlant_NumSp = "Invasive plant species count",
                             SoilStability_All = "Average soil stability rating",
                             SoilStability_Protected = "Average protect soil stability rating",
                             SoilStability_Unprotected = "Average unprotected soil stability rating"
)}

shinyServer(function(input, output, session) {
  
  ## Creating a reactive values object to use as a workspace for various functions here
  temp <- reactiveValues(
    terrindicators = {names(names(terrestrial.indicators)) <- unname(terrestrial.indicators)} ## Populating first terrindicators with terrestrial.indicators (just where the names and values swapped) for use in the UI
  )
  
  ## Function to decide what the name of the object the user is working with/on is called based on what tabs are selected
  ## Returns a string, e.g. "terrestrialShapefileComparison" or "terrestrialQueryReference"
  currentobjectname <- reactive({
    print("Trying to make the current object name")
    if (input$domain == "") {
      print("Your domain was somehow ''???")
      return(NULL)
    } else {
      objname <- paste0(
        input$domain,
        input$filtertype,
        input$compref)
      print(paste0("The current object is named: ", objname))
      return(objname)
    }
  })
  
  ## Function that will take the current uploaded file, check to make sure it's a valid .zip, unzip it, and read in the shapefile
  ## Returns a spatial data frame, but doesn't make sure that it's polygons
  shapeextract <- reactive({ ## TODO: Make this work when subsequent files are uploaded and not just the first
    file <- input$uploadzip ## There should've been an uploaded file and we're storing it as file
    if (is.null(file)) {return(NULL)} ## Emphasis on 'should' so we take precautions in case it's not been uploaded
    else if (!grepl(".zip", file$name, fixed = T)) {return(NULL)} ## Check to see if grepl()ing the file name for ".zip" returns FALSE. Because we want to return NULL if that's the case, we use the ! to flip the F to T to get a response when some fool's uploaded a non-.zip
    else ## Theoretically, the preceding two lines caught the worst case scenarios and we have a .zip, so now we can extract its contents
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
      str_replace(".shp", "") ## Strip out the .shp
    print("The shapefile name, sans extension, is:") ## Just diagnostic reassurance.
    print(shapename) ## If you see "character(0)" then something's wrong, but you probably already knew that
    ## I suspect the next bit is overcomplicated because SOMEONE didn't know an easier way to use readOGR(), but I can't be bothered to do more than copy/paste right now and it works
    shape <- readOGR(dsn = dirname(file$datapath), layer = shapename) ## Read in the shapefile using the file location and the shapefile name. Normally I'd provide the full filepath down to the file extension for the dsn argument but this doesn't, so that may explain why someone wrote the next line
    shape$dirname <- substr(file$datapath, 1, nchar(file$datapath) - 2) %>% as.character() %>% paste() ## I rewrote this just to use pipes, but I don't understand why it's here or the specifics of substr()
    shape <- shape %>% spTransform(., CRS(tdat.prj)) ## Pre-emptively get the shapefile into the same projection as the TerrADat data
    print("The structure of shape before trying to write it from within the observe({})")
    str(shape@data)
    return(shape)
  })
  
  ## Automatically read in the shapefile when it's uploaded and store it as the correctly named value in temp
  observeEvent(eventExpr = input$uploadzip,
               handlerExpr = {
                 temp[[currentobjectname()]] <- shapeextract()
                 temp$newshape <- T ## A flag so we can trigger subsequent things
                 print("The current contents of temp are:")
                 print(paste(names(temp), collapse = ", "))
               })
  
  ## Update the options in the appropriate tab for the fields available in the uploaded shapefile
  observeEvent(eventExpr = {temp$newshape},
               handlerExpr = {
                 if (temp$newshape == T) {
                   print(paste("Current value of temp$newshape is", temp$newshape, sep = " "))
                   temp$newshape <- F
                   print(paste("The new value of temp$newshape is", temp$newshape, sep = " "))
                   print("The structure of the shape being worked with is:")
                   str(temp[[currentobjectname()]])
                   print("The column names of the that shape's data frame are:")
                   print(paste(colnames(temp[[currentobjectname()]]@data), collapse = ", "))
                   print(paste0("Updating options for selectInput() with the id ", paste0(input$compref, "fieldname")))
                   updateSelectInput(session = session,
                                     inputId = paste0(input$compref, "fieldname"),
                                     choices = as.list(colnames(temp[[currentobjectname()]]@data)),
                                     selected = (colnames(temp[[currentobjectname()]]@data))[1]
                   )
                   print("Updated fieldname options")
                 }
               }
  )
  
  ## Update the options in the appropriate tab for the values in the selected field in the uploaded shapefile
  observeEvent(eventExpr = input[[paste0(input$compref, "fieldname")]],
               handlerExpr = {
                 if (input[[paste0(input$compref, "fieldname")]] != "") {
                   print("The selected field name is:")
                   print(paste(input[[paste0(input$compref, "fieldname")]]))
                   print("The values in that field in the shapefile are:")
                   print(paste(temp[[currentobjectname()]]@data[, input[[paste0(input$compref, "fieldname")]]], collapse = ", "))
                   print("Updating the options for the field values to:")
                   print(paste(as.character(temp[[currentobjectname()]]@data %>% .[, (colnames(temp[[currentobjectname()]]@data) %in% input[[paste0(input$compref, "fieldname")]])]) %>% unique(), collapse = ", "))
                   updateSelectizeInput(session = session,
                                        inputId = paste0(input$compref, "fieldvalues"),
                                        choices = as.character(temp[[currentobjectname()]]@data %>% .[, (colnames(temp[[currentobjectname()]]@data) %in% input[[paste0(input$compref, "fieldname")]])]) %>% unique()
                   )
                 }
               }
  )
  
  # observeEvent(eventExpr = input[[paste0(input$compref, "fieldvalues")]],
  #              handlerExpr = {
  #                if (input[[paste0(input$compref, "fieldvalues")]] != "") {
  #                  print("The current selected 'strata' are:")
  #                  print(paste(input[[paste0(input$compref, "fieldvalues")]], collapse = ", "))
  #                  print("Making a long form of the data frame")
  #                  temp[[paste0(currentobjectname(), "longdf")]] <- switch(input$domain,
  #                                                                          terrestrial = {
  #                                                                            gather(data = temp[[currentobjectname()]]@data %>%
  #                                                                                     .[temp[[currentobjectname()]]@data[, input[[paste0(input$compref, "fieldname")]]] %in% input[[paste0(input$compref, "fieldvalues")]]],
  #                                                                                   indicator,
  #                                                                                   value,
  #                                                                                   GapPct_25_50,GapPct_51_100,GapPct_101_200,GapPct_200_plus,GapPct_25_plus,BareSoilCover_FH,TotalFoliarCover_FH,NonInvPerenForbCover_AH,NonInvAnnForbCover_AH,NonInvPerenGrassCover_AH,NonInvAnnGrassCover_AH,NonInvAnnForbGrassCover_AH,NonInvPerenForbGrassCover_AH,NonInvSucculentCover_AH,NonInvShrubCover_AH,NonInvSubShrubCover_AH,NonInvTreeCover_AH,InvPerenForbCover_AH,InvAnnForbCover_AH,InvPerenGrassCover_AH,InvAnnGrassCover_AH,InvAnnForbGrassCover_AH,InvPerenForbGrassCover_AH,InvSucculentCover_AH,InvShrubCover_AH,InvSubShrubCover_AH,InvTreeCover_AH,SagebrushCover_AH,WoodyHgt_Avg,HerbaceousHgt_Avg,SagebrushHgt_Avg,OtherShrubHgt_Avg,NonInvPerenGrassHgt_Avg,InvPerenGrassHgt_Avg,InvPlantCover_AH,InvPlant_NumSp,SoilStability_All,SoilStability_Protected,SoilStability_Unprotected
  #                                                                            )
  #                                                                          },
  #                                                                          aquatic = {
  #                                                                            gather(data = temp[[currentobjectname()]]@data %>%
  #                                                                                     .[temp[[currentobjectname()]]@data[, input[[paste0(input$compref, "fieldname")]]] %in% input[[paste0(input$compref, "fieldvalues")]]],
  #                                                                                   indicator,
  #                                                                                   value#, TODO: Need to handle an aquatic situation by listing the indicators on the next line. Should be one set for riparian and one for in-stream
  #                                                                                   # GapPct_25_50,GapPct_51_100,GapPct_101_200,GapPct_200_plus,GapPct_25_plus,BareSoilCover_FH,TotalFoliarCover_FH,NonInvPerenForbCover_AH,NonInvAnnForbCover_AH,NonInvPerenGrassCover_AH,NonInvAnnGrassCover_AH,NonInvAnnForbGrassCover_AH,NonInvPerenForbGrassCover_AH,NonInvSucculentCover_AH,NonInvShrubCover_AH,NonInvSubShrubCover_AH,NonInvTreeCover_AH,InvPerenForbCover_AH,InvAnnForbCover_AH,InvPerenGrassCover_AH,InvAnnGrassCover_AH,InvAnnForbGrassCover_AH,InvPerenForbGrassCover_AH,InvSucculentCover_AH,InvShrubCover_AH,InvSubShrubCover_AH,InvTreeCover_AH,SagebrushCover_AH,WoodyHgt_Avg,HerbaceousHgt_Avg,SagebrushHgt_Avg,OtherShrubHgt_Avg,NonInvPerenGrassHgt_Avg,InvPerenGrassHgt_Avg,InvPlantCover_AH,InvPlant_NumSp,SoilStability_All,SoilStability_Protected,SoilStability_Unprotected
  #                                                                            )
  #                                                                          }
  #                  )
  #                }
  #              }
  # )
  
  observeEvent(eventExpr = input$terrafilter,
               handlerExpr = {
                 print("Restricting the polygons")
                 restrictedshape <- temp[[currentobjectname()]] %>%
                   .[(temp[[currentobjectname()]]@data[, input[[paste0(input$compref, "fieldname")]]] %in% as.vector(input[[paste0(input$compref, "fieldvalues")]])),] ## Slice the polygons down to the areas where the values in the selected field match the values that the user chose
                 print("Here's the structure of restrictedshape@data:")
                 print(str(restrictedshape@data))
                 print("Getting ready to execute the over(), so make yourself comfortable.")
                 filterterradat <- over(tdat.point.fc, restrictedshape)
                 print("The over() is finally finished! The result was:")
                 print(str(filterterradat))
                 print("Making a logical vector of points that inherited values from the polygon so we can slice by that")
                 filterterradatindices <- filterterradat[, input[[paste0(input$compref, "fieldname")]]] %>% as.character() %>% is.na() %>% !. ## Get the column from the data frame where the points were intersected with the polygons, turn it logical, and flip the values because we want the indices where they overlapped
                 print("The number of points that fell in the polygons was:")
                 print(filterterradatindices[filterterradatindices] %>% length())
                 print("Making the long data frame")
                 temp[[paste0(currentobjectname(), "longdf")]] <- switch(input$domain,
                                                                         terrestrial = {
                                                                           gather(data = tdat.point.fc@data[filterterradatindices,],
                                                                                  indicator,
                                                                                  value,
                                                                                  GapPct_25_50,GapPct_51_100,GapPct_101_200,GapPct_200_plus,GapPct_25_plus,BareSoilCover_FH,TotalFoliarCover_FH,NonInvPerenForbCover_AH,NonInvAnnForbCover_AH,NonInvPerenGrassCover_AH,NonInvAnnGrassCover_AH,NonInvAnnForbGrassCover_AH,NonInvPerenForbGrassCover_AH,NonInvSucculentCover_AH,NonInvShrubCover_AH,NonInvSubShrubCover_AH,NonInvTreeCover_AH,InvPerenForbCover_AH,InvAnnForbCover_AH,InvPerenGrassCover_AH,InvAnnGrassCover_AH,InvAnnForbGrassCover_AH,InvPerenForbGrassCover_AH,InvSucculentCover_AH,InvShrubCover_AH,InvSubShrubCover_AH,InvTreeCover_AH,SagebrushCover_AH,WoodyHgt_Avg,HerbaceousHgt_Avg,SagebrushHgt_Avg,OtherShrubHgt_Avg,NonInvPerenGrassHgt_Avg,InvPerenGrassHgt_Avg,InvPlantCover_AH,InvPlant_NumSp,SoilStability_All,SoilStability_Protected,SoilStability_Unprotected
                                                                           )
                                                                         }#,
                                                                         # aquatic = { ## TODO: EVERYTHING AQUATIC. THIS WILL BREAK IF ANYTHING TRIES TO USE IT
                                                                         #   gather(data = temp[[currentobjectname()]]@data %>%
                                                                         #            .[temp[[currentobjectname()]]@data[, input[[paste0(input$compref, "fieldname")]]] %in% input[[paste0(input$compref, "fieldvalues")]]],
                                                                         #          indicator,
                                                                         #          value#, TODO: Need to handle an aquatic situation by listing the indicators on the next line. Should be one set for riparian and one for in-stream
                                                                         #          # GapPct_25_50,GapPct_51_100,GapPct_101_200,GapPct_200_plus,GapPct_25_plus,BareSoilCover_FH,TotalFoliarCover_FH,NonInvPerenForbCover_AH,NonInvAnnForbCover_AH,NonInvPerenGrassCover_AH,NonInvAnnGrassCover_AH,NonInvAnnForbGrassCover_AH,NonInvPerenForbGrassCover_AH,NonInvSucculentCover_AH,NonInvShrubCover_AH,NonInvSubShrubCover_AH,NonInvTreeCover_AH,InvPerenForbCover_AH,InvAnnForbCover_AH,InvPerenGrassCover_AH,InvAnnGrassCover_AH,InvAnnForbGrassCover_AH,InvPerenForbGrassCover_AH,InvSucculentCover_AH,InvShrubCover_AH,InvSubShrubCover_AH,InvTreeCover_AH,SagebrushCover_AH,WoodyHgt_Avg,HerbaceousHgt_Avg,SagebrushHgt_Avg,OtherShrubHgt_Avg,NonInvPerenGrassHgt_Avg,InvPerenGrassHgt_Avg,InvPlantCover_AH,InvPlant_NumSp,SoilStability_All,SoilStability_Protected,SoilStability_Unprotected
                                                                         #   )
                                                                         # }
                 )
                 print(paste0("Just to prove that it worked, here's the structure of the freshly-generated ", paste0(currentobjectname(), "longdf")))
                 print(str(temp[[paste0(currentobjectname(), "longdf")]]))
                 print("Updating the options for comparison plotting data")
                 updateSelectInput(session = session,
                                   inputId = "comparisonplotdata",
                                   choices = c("", names(temp)[grepl(pattern = "longdf$", x = names(temp)) & grepl(pattern = "comparison", x = names(temp), ignore.case = T)])
                 )
                 print("Updating the options for reference plotting data")
                 updateSelectInput(session = session,
                                   inputId = "referenceplotdata",
                                   choices = c("None", names(temp)[grepl(pattern = "longdf$", x = names(temp)) & grepl(pattern = "reference", x = names(temp), ignore.case = T)])
                 )
                 print("Updating the options for indicators to filter and facet by")
                 updateCheckboxGroupInput(session = session,
                                          inputId = "plotindicators",
                                          choices = temp$terrindicators,
                                          selected = unname(temp$terrindicators)[1]
                                          )
               }
  )
  
  observeEvent(eventExpr = input$plotbutton,
               handlerExpr = { ## TODO: Add in something to catch when the data frame is empty because there was nothing that fit the filtering
                 print("You clicked the plot button, so things're about to get real")
                 print(paste0("Defining compdata as the dataframe ", input$comparisonplotdata))
                 compdata <- temp[[input$comparisonplotdata]]
                 print("Adding a variable to mark these as comparison")
                 compdata$type <- "Comparison"
                 print("Its structure is:")
                 print(str(compdata))
                 if (input$referenceplotdata != "None") {
                   print(paste0("Defining compdata as the dataframe ", input$referenceplotdata))
                   refdata <- temp[[input$referenceplotdata]]
                   print("Adding a variable to mark these as reference")
                   refdata$type <- "Reference"
                   print("Its structure is:")
                   str(refdata)
                 } else {
                   print("No reference data defined, so we're making an empty dataframe with the same column names as compdata")
                   refdata <- data.frame(matrix(ncol = (ncol(compdata)), nrow = 0))
                   names(refdata) <- names(compdata)
                   print("Its structure is:")
                   str(refdata)
                 }
                 print("Rbinding the compdata and refdata data frames into plotdata")
                 plotdata <- rbind(compdata, refdata)
                 print("Its structure is:")
                 str(plotdata)
                 print("Filtering to only the indicators that the user wants")
                 if (input$plotindicators != "none") {
                   print("Current indicators selected are:")
                   print(paste(terrestrial.indicators[input$plotindicators], collapse = ", "))
                   plotdata <- plotdata[plotdata$indicator %in% names(terrestrial.indicators[terrestrial.indicators %in% input$plotindicators]),]
                 }
                 print("Its structure now is:")
                 str(plotdata)
                 print("Attempting to generate figures")
                 if (nrow(plotdata) > 0) {
                   if (plotdata$PlotID[plotdata$type == "Comparison"] %>% unique() %>% length() > 1) {
                     print("More than one comparison plot selected, so we'll plot two histograms")
                     output$plot <- renderPlot(
                       ggplot(plotdata, aes(x = value, fill = type)) +
                         geom_histogram(alpha = 0.5) +
                         facet_wrap(~indicator)
                     )
                   } else if (plotdata$PlotID[plotdata$type == "Comparison"] %>% unique() %>% length() == 1) {
                     print("Just one comparison plot selected, so we'll put a vertical line where its value fell")
                     output$plot <- renderPlot(
                       ggplot(plotdata, aes(x = value, fill = type)) +
                         geom_histogram(plotdata[plotdata$type == "Reference",], alpha = 0.5) +
                         geom_vline(data = plotdata[plotdata$type == "Comparison",], aes(xintercept = value)) +
                         facet_wrap(~indicator)
                     )
                   }
                 }
                 print("Figures should be done!")
               }
  )
  
})