#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


crewReportChoices = c("LPI:Total Foliar","LPI:Bare Ground","LPI:Total Litter","LPI:Percent Checked","LPI:Basal Cover","Height:Average Woody","Height:Average Herbaceous","Gap:Percent 25-50cm","Gap:Percent 50cm-1m","Gap:Percent 1-2m","Gap:Percent >2m","Species Inventory:Number Species")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("DIMA Quality Control Analysis Tool"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       fileInput("DIMAfile",
                  "Select DIMA File"
       ),
       hr(),
       h3("Filter DIMA Data"),
       selectInput("Year","Select year",choices=""),
       selectInput("Site","Select Site",choices=""),
       downloadButton('runReport',"Run Report")
    ),
    
    # Main panel with tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Introduction",
                 p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec nisi eros, finibus convallis odio vitae, hendrerit scelerisque lectus. Praesent lacus lectus, vestibulum ac cursus non, consectetur vitae magna. Nulla dui dui, interdum varius ultricies quis, tempor eget orci. Aliquam nunc tortor, maximus sed diam at, tempor consequat lectus. Sed ac tortor quis ante lobortis euismod. Curabitur tincidunt, nulla non malesuada tincidunt, sem leo maximus justo, et egestas est mauris at mauris. Vestibulum vitae metus elit. Maecenas et aliquam diam. Fusce magna libero, rhoncus eu dapibus non, euismod ut ligula. Proin tincidunt convallis laoreet. Nulla euismod dapibus vulputate."),
                 hr(),
                 checkboxInput("chkDataSum","Data Summary"),
                 checkboxInput("chkCrewVar","Crew Varibility"),
                 checkboxInput("chkSeasons","Data Timing"),
                 checkboxInput("chkDataValues","Data Values Check")
                 ),
        tabPanel("More Info",
                 p("Aliquam commodo fermentum arcu, id interdum tellus tincidunt et. Maecenas semper non mi id scelerisque. Cras molestie, magna id maximus venenatis, nunc justo feugiat lectus, elementum volutpat sem ligula et nibh. Praesent facilisis in magna nec dictum. Suspendisse blandit nibh quis nunc gravida, sed blandit ipsum blandit. Pellentesque pellentesque ornare magna, finibus sollicitudin arcu iaculis ullamcorper. Proin quis augue malesuada felis vehicula tempus. Proin elementum erat vitae egestas suscipit. Praesent ornare nisi sapien, et euismod lorem ultricies ac. Donec ac nisi sed tellus accumsan posuere vitae ac libero. Sed et finibus ligula. Sed vehicula mauris non libero aliquet, vel accumsan dolor rutrum. Curabitur id turpis eget purus pulvinar tempus sed gravida tortor.")
                 )
      )
    )
  ),
  fluidRow(
    width="100%", height="100px",
    hr(),
    h4("App Log: "),
    verbatimTextOutput("Log")
  )
))
