
library(tools)
library(shiny)
library(rgdal)
library(spsurvey)
library(RColorBrewer)
library(latticeExtra)
#library(XLConnect)
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
      uneq <- input$unequalpts
      if (is.null(readSHPdata())) {
        return()
      }
      if (uneq == "no") {
        return()
      }
      updateTabsetPanel(session, "inTabSet", selected = "Unequal")
      grtsUNEQPointData()
    })
    
    observe({
      if (input$strataname == "strata field") 
        return()
      updateTabsetPanel(session, "inTabSet", selected = "Plot")
    })
    
    observe({
      thepts <- input$pts
      if(thepts == 0) {
        return()
      }
      
      if (is.null(readSHPdata())) {
        return()
      }
      grtsPointData()
      
    })
    
    
  readSHPdata <- reactive({
        
        x <- 0
        enable(updateNumericInput(session, "pts", value = x))
        enable(updateNumericInput(session, "oversamppts", value = x))
        enable(updateSelectInput(session, "strataname", "select:", "strata field"))
        enable(updateSelectInput(session, "unequalpts", "unequal pts?", c("yes","no"), selected="no"))
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
      #  print(shapename)
        
        require(rgdal)
        theSHP <- readOGR(dirname(inFile$datapath), shapename)
        myDatapath<-substr(inFile$datapath, 1, nchar(inFile$datapath)-2)
        theSHP$dirname <- paste(as.character(myDatapath))
                           #     print('dir for this shapefiles data is')
                            #    print(theSHP$dirname)
        return(theSHP)
    })
    
  #basename reactive
  theBaseName <- reactive({
    readSHPdata()
 inFile <- input$file1   
 
  nameme <- strsplit(as.character(inFile$name), "[.]")
  print("from basename")
  print(nameme[[1]][1])
 print(inFile$datapath)
 return(nameme[[1]][1])
    
  })
    
    # alter shapefile fields for stratum
  # IF shnystr exists already, DON"T DO THIS
    mySHPdata <- reactive({
       theSHP <- readSHPdata()
       myDirname <- as.character(theSHP$dirname[[1]])
         
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
        writeOGR(theSHP, myDirname, "stratatest", driver = "ESRI Shapefile", overwrite_layer = TRUE)
        
        print(theSHP$shnystr)
        (updateTabsetPanel(session, "inTabSet", selected = "Plot"))
        return(theSHP)
        
    })
    mySHPClass <- reactive({
      # the return from here will tell us if we have a line or poly feature SHP
      theSHP <- mySHPdata()
      myclass <- class(theSHP[])
      print(myclass[1])
      return(myclass[1])
    })
    
    
    
    getHeight <- function() {
        a = as.integer(input$plotsize)
    }
    
    output$plot <- renderPlot({
            
        lookatit <- input$strataname
        lookatittoo <- input$pts
        lookatitthree <- input$plotsize
        lookatitfour <- grtsPointData()
        lookatitfive <-grtsUNEQPointData()
        
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
            layer1 <- spplot(theSHP, c("shnystr"), xlim = c(minx, maxx), ylim = c(miny, maxy), 
                col.regions = colorMyWorld)
            # col.regions=brewer.pal(nlevels(as.character(theSHP$shnystr)), 'Accent'))
            
            if (input$pts >= 1 || !(is.null(grtsUNEQPointData())) ) {
                
                layer1 <- spplot(theSHP, c("shnystr"), xlim = c(minx, maxx), ylim = c(miny, maxy), 
                  col.regions = colorMyWorld)
                # col.regions=brewer.pal(nlevels(as.character('shnystr')), 'Accent')) gimmie<-plotpoints()
                if (input$hitme != 0) {
                  gimmie<-grtsUNEQPointData()
                }
                else {
                gimmie <- grtsPointData()
                }
                getit <- gimmie$panel
                oversamp <- subset(gimmie, getit == "OverSamp", select = c(xcoord, ycoord, 
                  stratum))
                regpts <- subset(gimmie, getit != "OverSamp", select = c(xcoord, ycoord, 
                  stratum))
                layer2 <- spplot(oversamp, c("stratum"), col.regions = "red")
                layer3 <- spplot(regpts, c("stratum"), col.regions = "black")
                print(layer1 + layer2 + layer3)
            } else print(layer1)
        })
        
    }, height = getHeight)
    
    
    # **** NEW **** for unequal point selection in strata
    output$sliders <- renderUI({
      if (input$unequalpts == "no")
        return(NULL)
      updateTabsetPanel(session, "inTabSet", selected = "Unequal")
      theSHP <- mySHPdata()
      thestrata <- unique(as.character(theSHP$shnystr))
      numstrata <- nlevels(theSHP$shnystr)
      print("in slider ui; thestrata is?")
      print(thestrata)
      gimmie="blah"
      gimmie<- lapply(1:numstrata, function(i) {
      sliderInput(inputId = paste0("strata", i), label = paste("strata ", thestrata[i]),
                  min = 0, max = 100, value = 0, step = 1)
      })
      
    return(gimmie)
   })

   
    
    # make our sample design and invoke grts from spsurvey
    grtsPointData <- reactive({
      if (input$pts == 0)
        return(NULL)
   print("we are in grtspointdata now......")
   
   theBaseName()
      
      isolate({
        # non-reactive shp we can manipulate
        theSHP <- mySHPdata()
      
        if (is.null(theSHP)) 
          return(NULL)
        touchme <-input$pts
        
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
#        save(theSHP, file = "shapedata")
        writeOGR(theSHP, myDirname, "rgdaltest2", driver = "ESRI Shapefile", overwrite_layer = TRUE)
        test.attframe <- read.dbf("rgdaltest2")
        grtsstrata <- "shnystr"
              
        result <- grts2(design = as.list(wholedesign), src.frame = "sp.object", sp.object = theSHP, 
                        type.frame = myTypeFrame, stratum = grtsstrata, att.frame = test.attframe, prjfilename = "rgdaltest2", 
                        out.shape = "grtstest")
      
        # for Sarah McC request
        names(result)[names(result) == "siteID"] <- "sample"
        
        save(result, file = "results")
        writeOGR(result, myDirname, "results", driver = "ESRI Shapefile", overwrite_layer = TRUE)
        print("DONE creating sample locations!")
        file.copy( "./rgdaltest2.prj", "./results.prj", overwrite = TRUE, copy.mode = TRUE)
      
      # was necessary in CentOS in previous server.R file
      system("cp -f ./rgdaltest2.prj ./results.prj")
      
      # have to make latlong version here
        theResults <- readOGR(myDirname, "results")
        projit <- spTransform(theResults, CRS("+proj=longlat +ellps=WGS84"))
        writeOGR(projit, myDirname, paste(theBaseName(),"latlong", sep=""), 
                 driver = "ESRI Shapefile", overwrite_layer = TRUE)
        
        # create zip fle in case user wants the points
        types <- list.files(pattern = "^results")
        types_as_string = as.character(types[[1]])
        if (length(types) > 1) 
          for (j in 2:length(types)) types_as_string = paste(types_as_string, " ", 
                                                             as.character(types[[j]]), 
                                                             sep = "")
        typesll <- list.files(pattern = "latlong")
        typesll_as_string = as.character(typesll[[1]])
        if (length(typesll) > 1) 
          for (j in 2:length(typesll)) typesll_as_string = paste(typesll_as_string, " ", 
                                                                 as.character(typesll[[j]]), 
                                                                 sep = "")
        
        # zipping is a sys call and depends on platform
        setwd(myDirname)
      print(getwd())
        switch(Sys.info()[["sysname"]], Windows = {
          system(sprintf("7z a -tzip results.zip %s", types_as_string))
          system(sprintf("7z a -tzip latlong.zip %s", typesll_as_string))
          
        }, Linux = {
          allshp <- list.files(pattern = "results")
          system(sprintf("zip results %s", types_as_string))
          allshp <- list.files(pattern = "latlong")
          system(sprintf("zip latlong %s", typesll_as_string))
        })
      })
      x<- 0
      enable(updateNumericInput(session, "pts", value = x))
      enable(updateNumericInput(session, "oversamppts", value = x))
      
    #  enable(updateSelectInput(session, "strataname", "select:", c(fieldnames)))
    #  enable(updateSelectInput(session, "strataname", "select:", "strata field"))
    #  enable(updateSelectInput(session, "unequalpts", "unequal pts?", c("yes","no"), selected="no"))
   updateTabsetPanel(session, "inTabSet", selected = "Plot")
      return(result)
    })
    
   grtspts<- reactive ({
     input$hitme
     if (input$unequalpts == "no")
       return(NULL)
  #  isolate({
       print("getting slider values...")
       mysliders<- NULL
       theSHP <- mySHPdata() 
       numstrata <- nlevels(theSHP$shnystr)
       
       mysliders <- lapply(1:(numstrata), function(i) {
         input[[paste0("strata", i)]]
       })
       pts <-as.numeric(mysliders)
       print("input sliders are")
       print(pts)
  #   })
     return(pts)
   })
   
    # NUMBER 2
    # make our sample design and invoke grts from spsurvey
    grtsUNEQPointData <- reactive({
      if(input$hitme == 0)
        return(NULL)
      grtspts()
      print("we are in grtsUNEQpointdata now......")  
      isolate({
        # non-reactive shp we can manipulate
        theSHP <- mySHPdata()
        if (is.null(theSHP)) 
          return(NULL)
        myDirname <- as.character(theSHP$dirname[[1]])
        setwd(myDirname)
        print(length(theSHP$shnystr))
        numstratavec <- theSHP$shnystr
        
        numstrata <- unique(numstratavec)
        print("number of strata is ")
        print(numstrata)
        
        # make dummy first entry for list we will concat to
        wholedesign = list(NULL)
        for (i in 1:length(numstrata)) {
          
          temp <- list(stratumdesign(numstrata[i], grtspts()[i], input$oversamppts))
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
#        save(theSHP, "shapedata")
        writeOGR(theSHP, myDirname, "rgdaltest2", driver = "ESRI Shapefile", overwrite_layer = TRUE)
        test.attframe <- read.dbf("rgdaltest2")
        grtsstrata <- "shnystr"
   
      resultuneq <- grts2(design = as.list(wholedesign), src.frame = "sp.object", sp.object = theSHP, 
                      type.frame = myTypeFrame, stratum = grtsstrata, att.frame = test.attframe, prjfilename = "rgdaltest2", 
                      out.shape = "grtstest")
      names(resultuneq)[names(resultuneq) == "siteID"] <- "sample"
      
      save(resultuneq, file = "results")
      writeOGR(resultuneq, myDirname, "results", driver = "ESRI Shapefile", overwrite_layer = TRUE)
      print("DONE creating sample locations!")
      file.copy("./rgdaltest2.prj", "./results.prj", overwrite = TRUE)
      
      # was necessary in CentOS in previous server.R file
      system("cp -f ./rgdaltest2.prj ./results.prj")
      
      # have to make latlong version here
      theResults <- readOGR(myDirname, "results")
      projit <- spTransform(theResults, CRS("+proj=longlat +ellps=WGS84"))
      writeOGR(projit, myDirname , "latlong", driver = "ESRI Shapefile", overwrite_layer = TRUE)
      setwd(myDirname)
      # switch tab panels
      updateTabsetPanel(session, "inTabSet", selected = "Plot")
      # create zip fle in case user wants the points
      types <- list.files(pattern = "^results")
      types_as_string = as.character(types[[1]])
      if (length(types) > 1) 
        for (j in 2:length(types)) types_as_string = paste(types_as_string, " ", 
                                                           as.character(types[[j]]), 
                                                           sep = "")
      typesll <- list.files(pattern = "latlong")
      typesll_as_string = as.character(typesll[[1]])
      if (length(typesll) > 1) 
        for (j in 2:length(typesll)) typesll_as_string = paste(typesll_as_string, " ", 
                                                               as.character(typesll[[j]]), 
                                                               sep = "")
      
      
      # zipping is a sys call and depends on platform
      setwd(theSHP$dirname[[1]])
      print (getwd())
      switch(Sys.info()[["sysname"]], Windows = {
        system(sprintf("7z a -tzip results.zip %s", types_as_string))
        system(sprintf("7z a -tzip latlong.zip %s", typesll_as_string))
        
      }, Linux = {
        allshp <- list.files(pattern = "results")
        system(sprintf("zip results %s", types_as_string))
        allshp <- list.files(pattern = "latlong")
        system(sprintf("zip latlong %s", typesll_as_string))
      })
      })
      x<- 0
      enable(updateNumericInput(session, "pts", value = x))
      enable(updateNumericInput(session, "oversamppts", value = x))
      
    # enable(updateSelectInput(session, "strataname", "select:", c(fieldnames)))
     #enable(updateSelectInput(session, "strataname", "select:", "strata field"))
     #enable(updateSelectInput(session, "unequalpts", "unequal pts?", c("yes","no"), selected="no"))
     
     #experimental
     thestrata <- unique(as.character(theSHP$shnystr))
     numstrata <- nlevels(theSHP$shnystr)
     mysliders <- lapply(1:(numstrata), function(i) {
       updateSliderInput(session=session, inputId = paste0("strata", i), label = paste("strata ", thestrata[i]),
                   value = 0 )
     })
     
      return(resultuneq)
     
    })
  
    output$pointdata <- renderTable({
      if (input$unequalpts == "yes") {
        temp <- as.data.frame(grtsUNEQPointData())
      } else {
        temp <- as.data.frame(grtsPointData())
        temp2 <- temp[, c("sample", "xcoord", "ycoord", "stratum")]
    }
    })

    plotpoints <- reactive({
      if (input$unequalpts == "yes") {
        save(grtsPointData, file = "grtsPointData")
      } else {
        save(grtsPointData, file = "grtsPointData")
        return(grtsPointData())
      }
    })
    
output$downloadData <- downloadHandler(filename = function() {
  paste(theBaseName(), "_results-", Sys.Date(), ".zip")
}, content = function(file) {
  file.copy("results.zip", file)
})

output$downloadLatlong <- downloadHandler(filename = function() {
  paste(theBaseName(), "latlong-", Sys.Date(), ".zip")
}, content = function(file) {
  file.copy("latlong.zip", file)
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
   

    
