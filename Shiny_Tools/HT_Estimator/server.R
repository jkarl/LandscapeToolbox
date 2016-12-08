library(shiny)
library(datasets)
library(spsurvey)

# Define server logic

shinyServer(function(input,output,session) {
  
  data <- reactiveValues()
  
  observe({
    # read input file
    inFile <- input$infile
    if(is.null(inFile))
      return(NULL)
    data$table <- read.csv(inFile$datapath, header=TRUE)
    
    # update the selectInput controls
    fields <- append("none",names(data$table))
    updateSelectInput(session,"ind_field",choices=fields)
    updateSelectInput(session,"weight_field",choices=fields)
    updateSelectInput(session,"strat_field",choices=fields)
    updateSelectInput(session,"x_field",choices=fields)
    updateSelectInput(session,"y_field",choices=fields)    
  })
  
  observe({
    # Update the stratum field with the levels from the strata field if selected
    if (input$strat_field=="none"||input$strat_field=="no file loaded") {
      updateSelectInput(session,"strat_sel",choices="no stratum field selected")
    } else {
      strata <- append("all strata",levels(data$table[[input$strat_field]]))
      updateSelectInput(session,"strat_sel",choices=strata)
    }
  })
  
  ##########################################################################
  ## Render input data table
  ##########################################################################
  output$contents <- renderDataTable({
      data$table    
  }, options=list(pageLength=10))

  output$message <- renderText({
    if(is.null(input$infile)) {
      "No input file has been uploaded."
    } else {
      ""
    }
  })
  
  output$subtitle <- renderText({
    if(is.null(input$infile)) {
      return()
    }
    if(input$strat_sel=="all strata"||input$strat_sel=="no stratum field selected") {
      strat=""
    } else {
      strat=paste("in stratum ",input$strat_sel)
    }
    paste("Estimates for ",input$ind_field,strat," using a ",switch(input$type,"SRS"="simple random selection","local"="local means estimation")," variance estimation.")
  })
  
  output$HTresults <- renderDataTable({
  
      ## Set up selection for a single stratum
      if(input$strat_sel=="no stratum field selected"||input$strat_sel=="all strata") {
        obs <- seq(1,length(data$table[,1]))
      } else {
        strata <- data$table[[input$strat_field]]
        stratum <- input$strat_sel
        obs <- which(strata==stratum)
      }
    
      ## Get the indicator values and the weights    
      ind.values <- data$table[[input$ind_field]][obs]
      if(input$weight_field=="none")
        wgts <- rep(1,length(ind.values))
      else
        wgts <- data$table[[input$weight_field]][obs]
      
      ## Set up the string to evaluate for the command
      func <- "total.est(ind.values,wgts"
  
      ## Set up the vartype
      if(input$type=="SRS") {
        vartype <- "vartype='SRS'"
      } else {  # local
        if(input$x_field=="none"||input$y_field=="none") {
          data$results <- ''
          return()
        }
        x <- data$table[[input$x_field]][obs]
        y <- data$table[[input$y_field]][obs]
        vartype <- "x=x,y=y,vartype='local'"
      }
      func <- paste(func,vartype,sep=",")
    
      ## Close off the function string and evaluate it
      func <- paste(func,"conf=input$conf)",sep=",")
      ex <- parse(text=func)
      data$results <- eval(ex)
    isolate(data$results)
  })
  
  output$histogram <- renderPlot({
    hist(data$table[[input$ind_field]],breaks=20,main=paste("Histogram of ",input$ind_field),xlab=input$ind_field,col="steelblue")
  })
  
  }) #close shinyServer