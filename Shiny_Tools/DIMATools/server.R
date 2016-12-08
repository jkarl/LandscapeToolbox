library(shiny)

options(shiny.maxRequestSize = 50*1024^2)

shinyServer(function(input,output) {
  
  data <- reactiveValues()
  
  observe({
    # read input file
    inFile <- input$infile
    if(is.null(inFile))
      return(NULL)
    print(inFile$datapath)
    print(inFile$name)
    read.string <- paste("mdb-export -d '|' '",inFile$datapath,"' tblPlots",sep="")
    print(read.string)
    data$table <- read.table(pipe(read.string), sep="|", header=TRUE)
  })
  
  output$test <- renderPrint({
    print(data$table[1:10,1:5])
  })
  
})