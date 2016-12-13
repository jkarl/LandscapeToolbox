library(shiny)
library(ggplot2)
library(gdata)

# Define server logic
shinyServer(function(input, output) {
  
    mycsvData <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    require(gdata)
    #wb <- loadWorkbook(inFile$datapath)
    #sheets <- getSheets(wb)
    return(
    read.xls(inFile$datapath, verbose=FALSE, method=c("csv"), perl="perl")
    #dat <- readWorksheet(wb, 1)
    )
    })
    
# make a reactive of the column name of the values we want, as splitting changes the names
  valuenum <- reactive({
    match("value", names(mycsvData()))
  })
    
# also make reactive of the number of groups, so we know which tests are valid
    numgroups <- reactive({
      howmany <- length(unique(mycsvData()$group))
      return(howmany)
    })
    
# display data(s) in a table
# !!!!!!!!!!!!  do we want to merge this back to one table or allow many (>2) tables split out    
  splitme1 <- reactive({
    if (is.null(mycsvData()))
      return(NULL)
    doit <- (split(mycsvData(), mycsvData()$group))
    doit1 <- as.data.frame(doit[1])
    return (
    doit1
    )
  })
  splitme2 <- reactive({
    if (is.null(mycsvData()))
      return(NULL)
    doit <- (split(mycsvData(), mycsvData()$group))
    doit2 <- as.data.frame(doit[2])
    return ( 
    doit2
    )
    })   
  
     
    output$contents1 <- renderTable({
      if(is.null(mycsvData()))
        return(NULL)
      return(
        (mycsvData())
      )
    })
  # output$contents2 <- renderTable({
  #    if(is.null(splitme2()))
  #      return(NULL)
  #  return(
  #      (splitme2())
  #    )
  #  })  
    
   output$summary1 <-renderPrint({
     doit1 <- summary(splitme1()[[valuenum()]])
     return (doit1)
   })    
   output$summary2 <-renderPrint({
     doit2 <- summary(splitme2()[[valuenum()]])
     return (doit2)
   })
    
   output$plot <- renderPlot({
   if(is.null(mycsvData()))
     return(NULL)
    p<- qplot(mycsvData()$value, data = mycsvData(), group = mycsvData()$group, fill = factor(mycsvData()$group))
      print(p)    
  
   }, type = "cairo")
  
 
    # two sample T-test
    
    output$twoSampTtestOutput <- renderPrint({
    if( numgroups() != 2 )       
        return(NULL)
    sample1 <- as.vector(splitme1()[[valuenum()]])
    sample2 <- as.vector(splitme2()[[valuenum()]])
    teeme <- t.test(sample1, sample2)
    return (
      teeme
    )
  })
  output$twoSampTtestPlot <- renderPlot({
        if(is.null(mycsvData()))
      return(NULL)
        if( numgroups() != 2 )
          return(NULL)
     #q <- qplot(factor(group),value,data=mycsvData(), geom="boxplot")
     #print(q)
      boxplot(value ~ factor(group), data=mycsvData())
  })  

    # Paired Sample T-Test
    output$pairedSampTtestOutput <- renderPrint({
      if(numgroups() != 2)
        return(NULL)
      sample1 <- as.vector(splitme1()[[valuenum()]])
      sample2 <- as.vector(splitme2()[[valuenum()]])
      teeme <- t.test(sample1, sample2, paired = TRUE)
      return (
        teeme
      ) 
    })
    output$pairedSampTtestPlot <- renderPlot({
      if(is.null(mycsvData()))
        return(NULL)
      if(numgroups() != 2 )
        return(NULL)
      #q <- qplot(factor(group),value,data=mycsvData(), geom="boxplot")
      #print(q)
      boxplot(value ~ factor(group), data=mycsvData())
    })  
    
  # now for ANOVA
    output$AOVsummary <- renderPrint({
      if(is.null(mycsvData()))
        return(NULL)
      if(numgroups() <= 2)
        return(NULL)
      factorme <- as.factor(mycsvData()$group)
      aovout <- aov(value ~ factor(group), data=mycsvData())
      return(aovout)
    })
   output$AOVplot <- renderPlot({
     boxplot(value ~ factor(group), data=mycsvData())
   })
    
# dynamic (we hope) thresholding
    output$threshPlot <- renderPlot({
      
    boxplot(value ~ factor(group), data=mycsvData())
    abline(h=input$thresh, col="red")
    })
    
   # output$thresholdControl <- renderUi({
    #  thresholdlevel <- input$threshold
      
  # directions?
    #output$instructions <- includeHTML({
     # "instructions.html"
    #})
  
})

