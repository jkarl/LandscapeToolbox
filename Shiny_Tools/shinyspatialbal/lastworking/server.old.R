
library(tools)
library(shiny)
library(rgdal)
library(spsurvey)
library(RColorBrewer)
library(latticeExtra)

source("stratumdesign.R")
source("grts2.R")

enable <- function(x) {
    
    if (inherits(x, "shiny.tag")) {
        if (x$name %in% c("input", "select")) 
            x$attribs$disabled <- NULL
        x$children <- enable(x$children)
        
    } else if (is.list(x) && length(x) > 0) {
        for (i in 1:length(x)) x[[i]] <- enable(x[[i]])
    }
    x
}

# Define server logic
shinyServer(function(input, output, session) {
    
    # allow for wonking big files
    options(shiny.maxRequestSize = 30 * 1024^2)
    
    observe({
        x <- input$controller
    })
    
    observe({
        theSHP <- readSHPdata()
        if (is.null(theSHP)) {
            fieldnames <- "strata field"
        } else {
            fieldnames <- names(theSHP)
        }
        enable(updateSelectInput(session, "strataname", "select:", c(fieldnames)))
    })
    
    
    observe({
        thepress <- input$strataname
        if (input$strataname == "strata field") 
            return()
        # theSHP<-mySHPdata()
        updateTabsetPanel(session, "inTabSet", selected = "Plot")
    })
    
    observe({
        thepts <- input$pts
        grtsPointData()
        
    })
    
    readSHPdata <- reactive({
        x <- 0
        enable(updateNumericInput(session, "pts", value = x))
        enable(updateNumericInput(session, "oversamppts", value = x))
        enable(updateSelectInput(session, "strataname", "select:", "strata"))
        inFile <- input$file1
        
        if (is.null(inFile)) 
            return(NULL)
        print(inFile$name)
        whatami <- grep(pattern = ".zip", inFile$name, fixed = TRUE)
        if (length(whatami) == 0) 
            return(NULL)
        print(inFile$datapath)
        # unzipping upload is a sys call and depends on platform
        switch(Sys.info()[["sysname"]], Windows = {
            origdir = getwd()
            setwd(dirname(inFile$datapath))
            system(sprintf(" cd %s", dirname(inFile$datapath)))
            print(getwd())
            system(sprintf("7z e -aoa %s", inFile$datapath))
            setwd(origdir)
            print(getwd())
        }, Linux = {
            origdir = getwd()
            setwd(dirname(inFile$datapath))
            system(sprintf(" cd %s", dirname(inFile$datapath)))
            print(getwd())
            system(sprintf("unzip -u %s", inFile$datapath))
            setwd(origdir)
        })
        shapename <- list.files(dirname(inFile$datapath), pattern = "\\.shp$")
        shapename <- file_path_sans_ext(shapename, compression = FALSE)
        print(shapename)
        
        require(rgdal)
        theSHP <- readOGR(dirname(inFile$datapath), shapename)
        # store datapath of uploaded file, save into dataframe, so as to store results there
        myDatapath <- substr(inFile$datapath, 1, nchar(inFile$datapath) - 2)
        theSHP$dirname <- paste(as.character(myDatapath))
        
        return(theSHP)
    })
    
    
    # alter shapefile fields for stratum
    mySHPdata <- reactive({
        theSHP <- readSHPdata()
        myDirname <- as.character(theSHP$dirname[[1]])
        
        # now add a stratum field name to have it always the same
        print("WHAT IS THE INPUT STRATA NAME")
        print(input$strataname)
        
        # first, deal with the strata field if it is numeric by making new column
        if (is.numeric(theSHP[[input$strataname]])) {
            theSHP$shnystr <- paste(as.character(theSHP[[input$strataname]]), sep = " ")
        } else {
            # just rename the current text column
            names(theSHP)[names(theSHP) == input$strataname] <- "shnystr"
            
        }
        print("OUR STRATUM FIELD CONTAINS : ")
        print(theSHP$shnystr)
        # Diagnostic save
        writeOGR(theSHP, myDirname, "stratatest", driver = "ESRI Shapefile", overwrite_layer = TRUE, verbose = TRUE)
        
        print(theSHP$shnystr)
        (updateTabsetPanel(session, "inTabSet", selected = "Plot"))
        return(theSHP)
        
    })
    
    getHeight <- function() {
        a = as.integer(input$plotsize)
    }
    
    
    output$plot <- renderPlot({
        
        lookatit <- input$strataname
        lookatittoo <- input$pts
        lookatitthree <- input$plotsize
        
        isolate({
            theSHP <- mySHPdata()
            if (is.null(theSHP)) {
                
                return(NULL)
            }
            # a variable we can play with, vs the true reactive?
            
            print(proj4string(theSHP))
            # ok so whereabouts are we
            limitspoly = bbox(theSHP)
            minx = limitspoly[1]
            miny = limitspoly[2]
            maxx = limitspoly[3]
            maxy = limitspoly[4]
            
            # factor strata values
            theSHP$shnystr = as.factor(theSHP$shnystr)
            # plotstrata<-as.character(theSHP$shinystrata)
            print("we have how many strata?")
            print(nlevels(theSHP$shnystr))
            colorMyWorld <- terrain.colors(nlevels(theSHP$shnystr), alpha = 1)
            # print()
            layer1 <- spplot(theSHP, c("shnystr"), xlim = c(minx, maxx), ylim = c(miny, maxy), col.regions = colorMyWorld)
            # col.regions=brewer.pal(nlevels(as.character(theSHP$shnystr)), 'Accent'))
            
            if (input$pts >= 1) {
                # print(theSHP$shnystr) print('PLOT POINTS DAMMIT')
                
                layer1 <- spplot(theSHP, c("shnystr"), xlim = c(minx, maxx), ylim = c(miny, maxy), col.regions = colorMyWorld)
                # col.regions=brewer.pal(nlevels(as.character('shnystr')), 'Accent'))
                
                gimmie <- grtsPointData()
                oversamp <- subset(gimmie, gimmie$panel == "OverSamp", select = c(xcoord, ycoord, stratum))
                regpts <- subset(gimmie, gimmie$panel != "OverSamp", select = c(xcoord, ycoord, stratum))
                layer2 <- spplot(oversamp, c("stratum"), col.regions = "red")
                layer3 <- spplot(regpts, c("stratum"), col.regions = "black")
                print(layer1 + layer2 + layer3)
            } else print(layer1)
        })
        
    }, height = getHeight)
    
    mySHPClass <- reactive({
        # the return from here will tell us if we have a line or poly feature SHP
        theSHP <- mySHPdata()
        myclass <- class(theSHP[])
        print(myclass[1])
        return(myclass[1])
    })
    
    
    
    # make our sample design and invoke grts from spsurvey
    grtsPointData <- reactive({
        if (input$pts == 0) 
            return(NULL)
        isolate({
            # non-reactive shp we can manipulate
            theSHP <- mySHPdata()
            if (is.null(theSHP)) 
                return(NULL)
            # set dir to where upload data is
            myDirname <- (as.character(theSHP$dirname[[1]]))
            setwd(myDirname)
            print(length(theSHP$shnystr))
            numstratavec <- theSHP$shnystr
            
            numstrata <- unique(numstratavec)
            print("number of strata is ")
            print(numstrata)
            
            # make dummy first entry for list we will concat to
            wholedesign = list(NULL)
            for (i in 1:length(numstrata)) {
                print(numstrata[i])
                
                temp <- list(stratumdesign(numstrata[i], input$pts, input$oversamppts))
                names(temp) <- numstrata[i]
                wholedesign <- c(wholedesign, temp)
                
            }
            
            wholedesign[[1]] <- NULL
            print(wholedesign)
            # diagnostic
            save(list = "wholedesign", file = paste("wholedesign", ".Rdata", sep = ""), ascii = TRUE)
            # save(wholedesign, file='wholedesign')
            
            if (mySHPClass() == "SpatialPolygonsDataFrame") 
                (myTypeFrame <- "area") else (myTypeFrame <- "linear")
            print("type frame to go to grts...")
            print(myTypeFrame)
            
            # create numbered list of entries in shapefile
            howmany <- length(theSHP)
            ids <- 1:howmany
            dat <- theSHP@data
            dat$ID <- ids
            theSHP@data <- dat
            save(theSHP, file = "shapedata")
            writeOGR(theSHP, myDirname, "rgdaltest2", driver = "ESRI Shapefile", overwrite_layer = TRUE, verbose = TRUE)
            test.attframe <- read.dbf("rgdaltest2")
            grtsstrata <- "shnystr"
            ################################################## debug me browser() try this bit of nonsense
            
            result <- grts2(design = as.list(wholedesign), src.frame = "sp.object", sp.object = theSHP, type.frame = myTypeFrame, stratum = grtsstrata, 
                att.frame = test.attframe, prjfilename = "rgdaltest2", out.shape = "grtstest")
            
            # just rename the siteID column
            names(result)[names(result) == "siteID"] <- "sample"
            
            file.copy("./rgdaltest2.prj", "./grtsresults.prj", overwrite = TRUE, copy.mode = TRUE)
            
            # no idea why he above doesn't work in CentOS; brute force method
            system("cp -f ./gdaltest2.prj ./grtsresults.prj")
            # textOutput('should be copying')
            
            save(result, file = "result")
            writeOGR(result, myDirname, "grtsresults", driver = "ESRI Shapefile", overwrite_layer = TRUE, verbose = TRUE)
            print("DONE creating sample locations!")
            
            # have to make latlong version here
            theResults <- readOGR(myDirname, "grtsresults")
            projit <- spTransform(theResults, CRS("+proj=longlat"))
            writeOGR(projit, myDirname, "latlongresults", driver = "ESRI Shapefile", overwrite_layer = TRUE)
            
            # create zip fle in case user wants the points
            types <- list.files(pattern = "grtsresults")
            latlongtypes <- list.files(pattern = "latlongresults")
            types_as_string = as.character(types[[1]])
            latlong_types_as_string = as.character(latlongtypes[[1]])
            
            if (length(types) > 1) 
                for (j in 2:length(types)) types_as_string = paste(types_as_string, " ", as.character(types[[j]]), sep = "")
            if (length(latlongtypes) > 1) 
                for (j in 2:length(latlongtypes)) latlong_types_as_string = paste(latlong_types_as_string, " ", as.character(latlongtypes[[j]]), 
                  sep = "")
            
            
            # zipping is a sys call and depends on platform
            switch(Sys.info()[["sysname"]], Windows = {
                system(sprintf("7z a -tzip grtsresults.zip %s", types_as_string))
                system(sprintf("7z a -tzip latlongresults.zip %s", latlong_types_as_string))
            }, Linux = {
                system(sprintf("zip grtsresults %s", types_as_string))
                system(sprintf("zip latlongresults %s", latlong_types_as_string))
            })
        })
        return(result)
    })
    
    output$pointdata <- renderTable({
        temp <- as.data.frame(grtsPointData())
        temp2 <- temp[, c("sample", "xcoord", "ycoord", "stratum")]
    })
    plotpoints <- reactive({
        save(grtsPointData, file = "grtsPointData")
        return(grtsPointData())
        
        
        
    })
    
    output$downloadData <- downloadHandler(filename = function() {
        paste("grtsresults-", Sys.Date(), ".zip")
    }, content = function(file) {
        file.copy("grtsresults.zip", file)
    })
    
    output$downloadLatlong <- downloadHandler(filename = function() {
        paste("latlongresults-", Sys.Date(), ".zip")
    }, content = function(file) {
        file.copy("latlongresults.zip", file)
    })

 # make a dima file to download
    
    dimaResults = function(){
      latlongdata<-readOGR(".", "latlong")
      template <- loadWorkbook("./DimaTemplate.xls", create=FALSE)
      XLresults<-loadWorkbook("./Dimaresults", create=TRUE)
      writeWorksheet(template, latlong$ycoord, 1, startRow=2, startCol=7)
      writeWorksheet(template, latlong$xcoord, 1, startRow=2, startCol=8)
      writeWorksheet(template, latlong$siteID, 1, startRow=2, startCol=1)
      saveWorkbook(template)
    }

# make an output obj that can return dima file

 output$downloadDima <- downloadHandler(filename = function() {
        paste("/srv/shiny-server/shinyspatialbal/DimaTemplate", ".xls")
    }, content = function(file) {
        file.copy("/srv/shiny-srver/shinyspatialbal/DimaTemplate.xls", file)
    })    
    
})

