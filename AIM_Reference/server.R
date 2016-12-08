library(shiny)
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
  
  ## Priming output with a vector to store the shapefile names in as they're uploaded so that they can be used to populate and option in the UI
  temp$shapefiles <- as.vector("")
  
  ## Priming our newshape logical value
  temp$newshape <- F
  
  ## Function to decide what the name of the object the user is working with/on is called based on what tabs are selected
  ## Returns a string, e.g. "terrestrialShapefileComparison" or "terrestrialQueryReference"
  currentobjectname <- reactive({
    print("Trying to make the current object name")
    if (input$domain == "") {
      print("Your domain was somehow an empty string???")
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
    shapes <- input$uploadzip ## There should've been an uploaded file and we're storing it as a new object so that this function will trigger on it
    if (is.null(shapes)) {return(NULL)} ## Emphasis on 'should' so we take precautions in case it's not been uploaded
    else if (!grepl(".zip", shapes$name, fixed = T)) {return(NULL)} ## Check to see if grepl()ing the file name for ".zip" returns FALSE. Because we want to return NULL if that's the case, we use the ! to flip the F to T to get a response when some fool's uploaded a non-.zip
    else ## Theoretically, the preceding two lines caught the worst case scenarios and we have a .zip, so now we can extract its contents
      print("File exists and ends in .zip")
    print("The value in shapes$datapath is:")
    print(dirname(shapes$datapath)) ## Just some diagnostic output in the terminal, not that an end-user will ever see it
    ## This was adapted from the LandscapeToolbox spatially-balanced points tool because that was broken(?????)
    switch(Sys.info()[["sysname"]], ## switch() takes as its first argument an expression that resolves to a number or character string, then executes the subsequent argument that matches that number or character string
           Windows = { ## For a Windows system, which should happen when the named value "sysname" in the list resulting from Sys.info() is "Windows", do the following
             print("This is Windows.")
             origdir = getwd() ## Save what the current working directory is so we can set it back at the end of this
             setwd(dirname(shapes$datapath)) ## Set the new working directory to the uploaded file's datapath. Not sure why this is here, but removing it breaks stuff
             system(paste0("cmd.exe /c cd ", dirname(shapes$datapath))) ## Pass this argument to the OS. It changes directories. When making Windows system calls, you need to invoke "cmd.exe /c" first
             system(paste0("cmd.exe /c \"C:\\Program Files\\7-Zip\\7z\".exe e -aoa ", shapes$datapath)) ## Pass the extraction argument to the OS. I had to aim it at my 7zip install. If yours is elsewhere, change the filepath to it, but know that those escaped quotation marks are necessary if there are spaces in your folder names. Thanks, Microsoft
             setwd(origdir) ## Restoring the working directory
             print("Resetting working directory to:") ## Diagnostic terminal output to reassure a debugger that it is in fact reset to the original working directory
             print(getwd())
           },
           Linux = { ## If the OS is Linux then:
             print("This is Unix. I know this.")
             origdir = getwd() ## Store the working directory as is, so we can restore it at the end of unzipping
             setwd(dirname(shapes$datapath)) ## Setting the working directory
             system(sprintf("cd %s", dirname(shapes$datapath))) ## Passing this to the OS
             print(getwd()) ## Just checking for debugging
             system(sprintf("unzip -u %s", inFile$datapath)) ## The unzipping argument to pass to the OS
             setwd(origdir) ## Set the working directory back
           }
    )
    ## We need the shapefile name and for it to not have the file extension
    shapename <- list.files(path = dirname(shapes$datapath), pattern = ".shp$") %>% ## List all the files in the extracted folder that end in .shp. If there's more then one this is almost certainly going to burn.
      str_replace(".shp", "") ## Strip out the .shp
    temp$currentshapename <- shapename ## We need this to rename with outside this function later
    print("The shapefile name, sans extension, is:") ## Just diagnostic reassurance.
    print(shapename) ## If you see "character(0)" then something's wrong, but you probably already knew that
    ## I suspect the next bit is overcomplicated because SOMEONE didn't know an easier way to use readOGR(), but I can't be bothered to do more than copy/paste right now and it works
    shape <- readOGR(dsn = dirname(shapes$datapath), layer = shapename, stringsAsFactors = F) ## Read in the shapefile using the file location and the shapefile name. Normally I'd provide the full filepath down to the file extension for the dsn argument but this doesn't, so that may explain why someone wrote the next line
    shape$dirname <- substr(shapes$datapath, 1, nchar(shapes$datapath) - 2) %>% as.character() %>% paste() ## I rewrote this just to use pipes, but I don't understand why it's here or the specifics of substr()
    shape <- shape %>% spTransform(., CRS(tdat.prj)) ## Pre-emptively get the shapefile into the same projection as the TerrADat data
    print("The structure of shape@data before trying to write it from within the observe({})")
    str(shape@data)
    return(shape)
  })
  
  ## Automatically read in the shapefile when it's uploaded and store it as the correctly named value in temp
  observeEvent(eventExpr = input$uploadzip,
               handlerExpr = {
                 
                 temp$file <- input$uploadzip
                 temp$shapefile <- shapeextract()
                 print(paste0("Attempting to assign the value of temp$shapefile to temp$", temp$currentshapename))
                 temp[[temp$currentshapename]] <- temp$shapefile
                 print("Is that a spatial data frame?")
                 is.data.frame(temp[[temp$currentshapename]]@data)
                 temp$shapefiles <- c(temp$shapefiles, temp$currentshapename)
                 ## Updating the shapefile options each time a new shapefile is uploaded
                 print(paste0("Updating the input$shapefile options to: ", paste(temp$shapefiles, collapse = ", ")))
                 updateSelectInput(session = session,
                                   inputId = "shapefile",
                                   choices = temp$shapefiles,
                                   selected = temp$currentshapename)
                 print(paste0("The current value of input$shapefile is ", input$shapefile, " and it should be ", temp$currentshapename))
                 temp$newshape <- T ## A flag so we can trigger subsequent things
                 print("The current contents of temp are:")
                 print(paste(names(temp), collapse = ", "))
               })
  
  ## Update the options in the appropriate tab for the fields available in the uploaded shapefile
  observeEvent(eventExpr = {input$shapefile},
               handlerExpr = {
                 if (input$shapefile != "") {
                   print(paste("Current value of temp$newshape is", temp$newshape, sep = " "))
                   temp$newshape <- F
                   print(paste("The new value of temp$newshape is", temp$newshape, sep = " "))
                   print(paste0("The length of the @data slot in, ", input$shapefile, ", is:"))
                   print(nrow(temp[[input$shapefile]]@data))
                   print("The column names of the that shape's data frame are:")
                   print(paste(colnames(temp[[input$shapefile]]@data), collapse = ", "))
                   print(paste0("Updating options for selectInput() for input$fieldname"))
                   print(paste0("Which should be: ", paste(colnames(temp[[input$shapefile]]@data), collapse = ", ")))
                   updateSelectInput(session = session,
                                     inputId = "fieldname",
                                     choices = as.list(colnames(temp[[input$shapefile]]@data)),
                                     selected = (colnames(temp[[input$shapefile]]@data))[1]
                   )
                   print("Updated fieldname options")
                 }
               }
  )
  
  ## Update the options in the appropriate tab for the values in the selected field in the uploaded shapefile
  observeEvent(eventExpr = input$fieldname,
               handlerExpr = {
                 if (input$fieldname != "") {
                   print("The selected field name is:")
                   print(input$fieldname)
                   print("The values in that field in the shapefile are:")
                   print(paste(temp[[input$shapefile]]@data[, input$fieldname], collapse = ", "))
                   print("Updating the options for the field values to:")
                   print(paste(as.character(temp[[input$shapefile]]@data %>% .[, (colnames(temp[[input$shapefile]]@data) %in% input$fieldname)]) %>% unique(), collapse = ", "))
                   updateSelectizeInput(session = session,
                                        inputId = "fieldvalues",
                                        choices = as.character(temp[[input$shapefile]]@data %>% .[, (colnames(temp[[input$shapefile]]@data) %in% input$fieldname)]) %>% unique()
                   )
                 }
               }
  )
  
  ## TODO: Figure out a way to make the names of the stored TerrADat subsets sane rather than purely functional (Renaming function?)
  
  ## Filtering by query
  observeEvent(eventExpr = input$terraquery,
               handlerExpr = {
                 if (nchar(input$query) > 3) {
                   eval.string <- paste0('filter(tdat.point.fc@data, ', input$query, ')') ## Construct a string that constitutes the filter() function and its arguments
                   print(eval.string)
                   ## Creating a version of eval() using the purrr adverb safely(). safe.eval() returns a named list with "result" and "error", one of which will be NULL
                   ## Calling safe.eval won't crash the app and we can check to see if is.null(output[["result"]]) elsewhere to decide how to procede
                   safe.eval <- safely(eval)
                   queryfilteredterradat <- safe.eval(parse(text = eval.string))[["result"]] ## Run the string as though it were the function+arguments. Because it is, theoretically
                   temp[[paste0(input$query, input$compref, "longdf")]] <- switch(input$domain,
                                                                                      terrestrial = {
                                                                                        gather(data = queryfilteredterradat,
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
                   print(paste0("Just to prove that it worked, here's the structure of the freshly-generated longdf"))
                   print(str(temp[[paste0(input$query, input$compref, "longdf")]]))
                   output$currenttable <- renderTable(temp[[paste0(input$query, input$compref, "longdf")]])
                   print("Updating the options for comparison plotting data")
                   updateSelectInput(session = session,
                                     inputId = "comparisonplotdata",
                                     choices = c("", names(temp)[grepl(pattern = "longdf$", x = names(temp)) & grepl(ignore.case = T, pattern = "comparison", x = names(temp))])
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
               }
               )
  
  ## Filtering by polygons
  observeEvent(eventExpr = input$terrafilter,
               handlerExpr = {
                 print("Restricting the polygons")
                 restrictedshape <- temp[[input$shapefile]] %>%
                   .[(temp[[input$shapefile]]@data[, input$fieldname] %in% as.vector(input$fieldvalues)),] ## Slice the polygons down to the areas where the values in the selected field match the values that the user chose
                 print(paste("Here's the structure of restrictedshape, which is class ", class(restrictedshape), ":"))
                 print(str(restrictedshape))
                 print("Is proj4string the same for the restrictedshape and tdat.point.fc?")
                 print(paste0("For tdat.point.fc it's ", tdat.point.fc@proj4string@projargs))
                 print(paste0("For restrictedshape it's ", restrictedshape@proj4string@projargs))
                 print("Getting ready to execute the over(), so make yourself comfortable.")
                 filterterradat <- over(tdat.point.fc, restrictedshape)
                 print("The over() is finally finished! The result was:")
                 print(str(filterterradat))
                 # print(paste0("Unique values in the column ", input$fieldname, ": ", paste(unique(filterterradat[, input$fieldname]), collapse = ", ")))
                 print("Making a logical vector of points that inherited values from the polygon so we can slice by that")
                 filterterradatindices <- filterterradat[, input$fieldname] %>% as.character() %>% is.na() %>% !. ## Get the column from the data frame where the points were intersected with the polygons, turn it logical, and flip the values because we want the indices where they overlapped
                 print("The number of points that fell in the polygons was:")
                 print(filterterradatindices[filterterradatindices] %>% length())
                 print("Making the long data frame")
                 temp[[paste0(input$shapefile, input$compref, "longdf")]] <- switch(input$domain,
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
                 print(paste0("Just to prove that it worked, here's the structure of the freshly-generated ", paste0(input$shapefile, "longdf")))
                 print(str(temp[[paste0(input$shapefile, input$compref, "longdf")]]))
                 output$currenttable <- renderTable(temp[[paste0(input$shapefile, input$compref, "longdf")]])
                 print("Updating the options for comparison plotting data")
                 updateSelectInput(session = session,
                                   inputId = "comparisonplotdata",
                                   choices = c("", names(temp)[grepl(pattern = "longdf$", x = names(temp)) & grepl(ignore.case = T, pattern = "comparison", x = names(temp))])
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
               handlerExpr = {
                 print("You clicked the plot button, so things're about to get real")
                 print(paste0("Defining compdata as the dataframe ", input$comparisonplotdata))
                 compdata <- temp[[input$comparisonplotdata]]
                 if (nrow(compdata) < 1) {
                   print("There are no values in this data frame. ABORTING")
                   output$emptyframe <- renderText(print("The data frame was empty. Your filtering didn't find any matching plots. If you have small polygons or a very specific query within a project area, there may be no plots that meet those criteria. You may also have tried to select part of the landscape where no data have been collected or misspelled part of your query."))
                 } else {
                   output$emptyframe <- renderText(print(""))
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
                   
                   ## TODO: Maybe normalize the histogram y-axis (percentage?)
                   ## TODO: Beautify the figures
                   ## TODO: Make histogram tab open when plots are rendered
                   ## TODO: Fix all errors when there's just one plot in the comparison data
                   ## TODO: Set up appropriate failsafes for all plot situations (no data in either type of data frame, only one plot in either data frame)
                   
                   print("Attempting to generate figures")
                   if (nrow(plotdata) > 0) {
                     if (plotdata$PlotID[plotdata$type == "Comparison"] %>% unique() %>% length() > 1) {
                       print("More than one comparison plot selected, so we'll plot two histograms")
                       output$plot <- renderPlot(
                         ggplot(plotdata, aes(x = value, fill = type)) +
                           geom_histogram(aes(y = ..count../sum(..count..)), alpha = 0.5) + ## That aes(y) argument normalizes the histogram to proportion
                           facet_wrap(~indicator)
                       )
                       ## This logic is broken currently somehow, I think. TODO: Fix it!
                     } else if (plotdata$PlotKey[plotdata$type == "Comparison"] %>% unique() %>% length() == 1 & plotdata$PlotKey[plotdata$type == "Reference"] %>% unique() %>% length() > 1) {
                       print("Just one comparison plot selected, so we'll put a vertical line where its value fell")
                       output$plot <- renderPlot(
                         ggplot(plotdata, aes(x = value, fill = type)) +
                           geom_histogram(plotdata[plotdata$type == "Reference",],aes(y = ..count../sum(..count..)), alpha = 0.5) +
                           geom_vline(data = plotdata[plotdata$type == "Comparison",], aes(xintercept = value)) +
                           facet_wrap(~indicator)
                       )
                     }
                   }
                   print("Figures should be done!") 
                 }
               }
  )
  
})