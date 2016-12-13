library(shiny)



shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Spatially Balanced Sampling Tool"),
  
  sidebarPanel(
    tags$head(
      tags$style(type="text/css", "select { max-width: 250px; }"),
      tags$style(type="text/css", "textarea { max-width: 250px; }"),
      tags$style(type="text/css", ".jslider { max-width: 250px; }"),
      tags$style(type='text/css', ".well { max-width: 250px; }"),
      tags$style(type='text/css', ".span4 { max-width: 250px; }")
    ),
      
    helpText("PLEASE RUN STEPS IN ORDER"),
    helpText(""),
   
    fileInput('file1', '1) Choose .ZIP Shapefile:', multiple=TRUE,
          accept=c('binary')),
    helpText("2) Select a stratum field:"),
   selectInput("strataname","2) Select a stratum field:", choices=c("strata field")), 
   submitButton("3) Press to select field & plot"),
   helpText("    "),
   numericInput("pts", "4) enter # of sampling points per stratum:", 0),
   helpText("And - (optional)"),
   numericInput("oversamppts", "enter # to OVER sample", 0),    
   submitButton( "5) Fetch Points!"),
  
   downloadButton('downloadData', "Download Points as shapefile"),
   downloadButton('downloadLatlong', "Download Points as lat/long shapefile"), 
   downloadButton('downloadDima', "Download lat/long points in Dima XLS")   
  ),
  
  mainPanel(
     
    tabsetPanel(
           
      tabPanel("Instructions",
              includeHTML("instructions.html"), 
              div(id="linkToMap", tags$a("Click here to see a map of your input data and create points")), 
              div(id="linkToPoints", tags$a("Click here to see table of created points")),
              value=1, 
              HTML('<img style="float: right"; src="JornadaLogo_1.png"/>')
      ),
      
      tabPanel("Plot", 
               numericInput("plotsize", "enter plot size in pixels (200-1000):", "400"),
               submitButton("change plot size"),  
               plotOutput(outputId = "plot"),   
               
  conditionalPanel(
    condition="$('div#plot').hasClass('recalculating') || $('div#plot').hasClass('calculating')",
    img(src="dogfetcher2.gif"), h5("fetching data; please wait...") 
  ),
             #  conditionalPanel(
              #   condition="output$plot == NULL",
               #  img(src="dogfetcher2.gif"), h5("fetching data; please wait...") 
              # ), 
                
  conditionalPanel(
    condition="!($('div#plot').hasClass('recalculating')", br()
  ),
          
            #   plotOutput(outputId = "plot"),          
               
    helpText("Map of input polygons by strata")
           
      ),
      
      tabPanel("View Points", helpText("suggested sampling points"),
               tableOutput(outputId = "pointdata"),
           
               HTML("<script>$('#linkToMap').click(function() {
                    tabs = $('.tabbable .nav.nav-tabs li')
                    tabs.each(function() {
                    $(this).removeClass('active')
                    })
                    $(tabs[1]).addClass('active')
                    tabsContents = $('.tabbable .tab-content .tab-pane')
                    tabsContents.each(function() {
                    $(this).removeClass('active')
                    })
                    $(tabsContents[1]).addClass('active')
                    
                    $('#plot').trigger('change').trigger('shown')
                    
                    })</script>
                    "),
               HTML("<script>$('#linkToPoints').click(function() {
                    tabs = $('.tabbable .nav.nav-tabs li')
                    tabs.each(function() {
                    $(this).removeClass('active')
                    })
                    $(tabs[2]).addClass('active')
                    tabsContents = $('.tabbable .tab-content .tab-pane')
                    tabsContents.each(function() {
                    $(this).removeClass('active')
                    })
                    $(tabsContents[2]).addClass('active')
                    
                    $('#pointdata').trigger('change').trigger('shown')
                    
               })</script>
                    ")), 
      id = "inTabSet"
      )
    ))
    )
