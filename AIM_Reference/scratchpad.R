current.shapefile <- readshp()
output$fieldnames <- names(current.shapefile)

output$fieldvalues <- current.shapefile[,input$fieldname] %>% unique()

filtered.shapefile <- current.shapefile[current.shapefile[,input$fieldname] %in% input$filtervalues, input$fieldname]
over()