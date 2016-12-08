library(shiny)
library(xlsx)
library(pwr)
library(ggplot2)
library(reshape2)

#Define server logic for Shiny app
shinyServer(function(input,output,session) {
  
  a <- reactiveValues()
  
  #######################################################################################
  ## Define function for calculating power
  #######################################################################################
  power <- reactive({
    input$goButton  # Dependency on goButton click
    
    if(is.null(input$file1)) {
      return(NULL)
    }
    
    #input$goButton  # Dependency on goButton click
    progress <- shiny::Progress$new(session)
    on.exit(progress$close())
    progress$set(message = 'Calculation in progress...')
    
    #Set up inputs and containers
    mddPct = input$MDD; alpha = input$Alpha; rho = input$Rho; prop = input$PropData
    indField = input$IndicatorField
    valField = input$Values
    stratField = input$StrataField
    anType = input$analysisType
    rho = input$Rho
    prop = input$PropData
    nRange = seq(3,100,by=3)
    a$results.tab = data.frame(matrix(nrow=1,ncol=9))
    names(a$results.tab)=c("stratum","indicator","mean","stdev","n.actual","coef.var","actualMDD","power","n.desired.mdd")
    a$sim.tab <- data.frame("stratum"="", "indicator"="","n"=0,"pwr"=0)
    a$sim.tab$indicator <- factor(a$sim.tab$indicator,levels=levels(a$data$Indicator))
    
    for (strat in levels(a$data[[stratField]])) {
      for (l in levels(a$data[[indField]])) {
        data.sub <- a$data[a$data[[indField]]==l&a$data[[stratField]]==strat,]
        x.i = data.sub[[valField]]
        x.i[is.na(x.i)] <- 0
        #Calculate power stats
        xbar1 <- mean(x.i, na.rm=TRUE)
        s1 <- var(x.i, na.rm=TRUE)
        if (!(xbar1==0 & s1==0)) { #break  ## trap for indicators not measured by stratum
          n <- length(x.i)
          cv <- sqrt(s1)/xbar1
          xbar2 = xbar1 + mddPct*xbar1
          if (xbar2<0) xbar2=0; if(xbar2>1) xbar2=1
          
          ## Calculate effect size (actual MDD) and power based on selected analysis type: "Threshold Test","Two Independent Samples","Repeated Measures"
          if (anType=="Threshold Test") {
            sp = sqrt( (n-1)*s1/n  )
            d = (xbar2-xbar1)/sp
            type = "one.sample"
            altern = "two.sided"
          } else if (anType=="Two Independent Samples") {
            sp = sqrt( (n-1)*s1/n  )
            d = (xbar2-xbar1)/sp
            type = "two.sample"
            altern = "two.sided"
          } else {  # assume paired
            sp = sqrt( (n-1)/n*s1*2*(1-rho) )
            d = (xbar2-xbar1)/sp
            type = "paired"
            altern = "two.sided"
          }
          
          if (prop) {
            p = pwr.p.test(n=n,h=d,sig.level=alpha,alternative=altern)$power
            desired.n = pwr.p.test(power=0.8,h=d,sig.level=alpha,alternative=altern)$n
          } else {
            p = pwr.t.test(n=n,d=d,sig.level=alpha,type=type,alternative=altern)$power
            desired.n = pwr.t.test(power=0.8,d=d,sig.level=alpha,type=type,alternative=altern)$n        
          }
          
          
          
          #Write results to the table
          a$results.tab = rbind(a$results.tab,c(strat,l,xbar1,sqrt(s1),n,cv,d,p,desired.n))
          
          # Generate power curve data and write to sim.tab
          for (nSim in nRange) {
            pSim = pwr.t.test(n=nSim,d=d,sig.level=alpha,type="one.sample",alternative="two.sided")
            #print(paste(l,nSim,pSim$power,sep=", "))
            a$sim.tab = rbind(a$sim.tab,data.frame("stratum"=strat,"indicator"=l,"n"=nSim,"pwr"=pSim$power))
            #plotArr[nSim-2]=pSim$power
          }
        }
      } 
    }  

    a$results.tab = a$results.tab[-1,]
    a$sim.tab = a$sim.tab[-1,]
    a$results.tab
    
  })
  
  makePlot <- reactive({
    g = ggplot(data=a$sim.tab, aes(x=n,y=pwr,color=indicator))+geom_line() #+geom_vline(data=a$results.tab,aes(xintercept=as.numeric(15),linetype="dashed"))
    g+facet_wrap(~stratum)
  })
  
  #######################################################################################
  ## Load the data and display messages for errors
  #######################################################################################
  #output$testText <- renderPrint({
  #  infile = input$file1
  #  
  #  if(is.null(infile))
  #    return("No file selected.")
  #  
  #  if(infile$type=="text/csv") {
  #    a$data = read.csv(infile$datapath,header=TRUE)
  #  } else {
  #    if(infile$type=="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
  #      a$data = read.xlsx(infile$datapath,sheetName="Line Totals")
  #    } else {
  #      return(paste("Cannot load file type:",infile$type,sep=' '))
  #    }
  #  }    
  #  return("Data file successfully loaded.")  
  #})
  
  #  loadData <- reactive({
  #    infile = input$file1
  #    
  #    if(is.null(infile))
  #      return(NULL)
  #    
  #    if(infile$type=="text/csv") {
  #      a$data = read.csv(infile$datapath,header=TRUE)
  #    } else {
  #      if(infile$type=="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
  #        a$data = read.xlsx(infile$datapath,sheetName="Line Totals")
  #      }
  #    }    
  #    
  #  })
    

  
  #######################################################################################
  ## Finish UI Setup - populate combo boxes depending on file upload
  #######################################################################################
  output$ChooseStrata <- renderUI({
    infile = input$file1
    
    if(is.null(infile))
      return(NULL)
    
    if(infile$type=="text/csv") {
      if(input$fileType=="DIMA Indicator File") {
        a$data = read.csv(infile$datapath,header=TRUE)  
      } else {
        temp = read.csv(infile$datapath,header=TRUE)
        temp2 = melt(temp,id.vars=c(1,2,3))
        names(temp2)[4:5] = c("Indicators","Ind_Values")
        a$data = temp2
      }
      
    } else {
      if(infile$type=="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
        if(input$fileType=="DIMA Indicator File") {
          a$data = read.xlsx(infile$datapath,sheetName="Plot Totals")  
        } else {
          temp = read.xlsx(infile$datapath,1)  ## Read the first sheet in the XLS(X) file
          a$data = melt(temp,id.vars=c(1,2,3))
          names(a$data)[4:5] = c("Indicators","Ind_Values")
        }
        
      }
    }    
    
    if(is.null(input$file1))
      { fields <- '' }
    else
    { fields <- names(a$data) }
    selectInput("StrataField", "Strata Field", choices = cbind("None",fields), multiple=FALSE,selected="Site")
  })
  
  output$ChoosePlotID <- renderUI({
    if(is.null(input$file1))
    { fields <- '' }
    else
    { fields <- names(a$data) }
    selectInput("PlotIDField", "Plot ID Field", choices = cbind("None",fields),selected="Plot")
  })
  
  output$ChooseIndicatorField <- renderUI({
    if(is.null(input$file1))
    { fields <- '' }
    else
    { fields <- names(a$data) }
    selectInput("IndicatorField", "Field Specifying Indicators", choices = (fields),selected="Indicator")
  })
  
#  output$ChooseIndicator <- renderUI({
#    if(is.null(input$file1))
#      { fields <- ''}
#    else
#    { if(is.null(input$IndicatorField))
#      { fields <- '' }
#      else
#      { fields <- levels(a$data[[input$IndicatorField]])
#      selectInput("Indicator","Select Indicator",choices=(cbind("All",fields)))}
#    }
#  })
  
  output$ChooseValueField <- renderUI({
    if(is.null(input$IndicatorField)) {
      fields <- ''
    } else {
      fields <- rbind("",names(a$data))
    }
    selectInput("Values","Indicator Value Field",choices=fields)
  })

  
  #######################################################################################
  ## Run the sample sufficiency calculations and display the results as a table
  #######################################################################################
  
  ## Display the loaded data table
  output$DataTable <- renderDataTable({
    if(is.null(input$file1)) {
      return(NULL)
    }
    a$data
    }, options = list(pageLength = 10))
  
  ## Run and display the sample sufficiency analysis results
  output$ssResults <- renderDataTable({
    if(is.null(input$file1)) {
      return(NULL)
    }
    input$goButton
    data <- isolate(power())
    data
  }, options=list(pageLength=10))
  
  ## Display the power analysis results
  output$powerPlot <- renderPlot({
    if(is.null(a$sim.tab)) {
      return(NULL)
    }
    print(makePlot())
  })
  
output$downloadTable <- downloadHandler(
  filename = function() {
    paste('sample_sufficiency_results-',Sys.Date(),'.csv',sep='')
  },
  content = function(file) {
    write.csv(a$results.tab, file)
  },
  contentType="text/csv"
  )
  
output$downloadPlot <- downloadHandler(
  filename = function() {
    paste('sample_sufficiency_results-',Sys.Date(),'.png',sep='')
  },
  content = function(file) {
    device <- function(..., width, height) {
      grDevices::png(..., width = width, height = height,
                     res = 300, units = "in")
    }
    ggsave(file, plot = makePlot(), device = device)
  },
  contentType="image/png"
)

#output$downloadPlot <- plotPNG(
#  makePlot(),
#  filename = paste('sample_sufficiency_results-',Sys.Date(),'.png',sep=''),
#  width=6,
#  height=6
#  dpi=200
#  )

})