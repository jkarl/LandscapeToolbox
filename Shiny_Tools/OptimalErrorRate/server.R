library(shiny)
source('optimal_alpha.R')

shinyServer(function(input,output) {
  output$optabOut <- renderPrint(optab(n1=input$n1,n2=input$n2,d=input$d,type=input$type,tails=input$tails,T1T2cratio=input$T1T2cratio,HaHopratio=input$HaHopratio)$output)
  output$optabPlot <- renderPlot(optab.plot(n1=input$n1,n2=input$n2,d=input$d,type=input$type,tails=input$tails,T1T2cratio=input$T1T2cratio,HaHopratio=input$HaHopratio))
  output$CohensD <- renderText(effect.size(x1=input$x1,n1=input$esn1,s1=input$s1,pctChg=input$pctChg,paired=input$paired,type=input$EStype,x2=input$x2,n2=input$esn2,s2=input$s2,threshold=input$threshold))
  output$calcCR <- renderText((input$IaddMonit+input$IlostUse+input$IotherDirect+input$Iindirect)/(input$IIrestoration+input$IIotherDirect+input$IIindirect))
  })