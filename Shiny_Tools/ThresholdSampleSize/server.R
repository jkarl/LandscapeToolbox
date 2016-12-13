library(shiny)
library(ggplot2)

# Function to calculate binomial sample sizes
calcN <- function(p=0.5,p0=0.4,alpha=0.2,beta=0.3) {
  (n=p*(1-p)*((qnorm(1-alpha/2)+qnorm(1-beta))/(p-p0))^2)
  return(ceiling(n)) # 50
}

# calculate sample size required to achieve a specified margin of error
n.moe <- function(p=0.2,alpha=0.2,moe=0.05) {
  m<-p*(1-p)/(moe/qnorm(1-alpha/2))^2
  return(ceiling(m))
}

N.plot <- function(p=0.5,p0=0.2,alpha=0.2,beta=0.2,moe=0.1) {
  n.array<-data.frame()
  m.n<-p*(1-p)/(moe/qnorm(1-alpha/2))^2
  for (i in seq(0.01,0.99,by=0.01)) {
    n<-calcN(p=p,p0=i,alpha=alpha,beta=beta)
    if (n>m.n) {n<-m.n}
    n.array<-rbind(n.array,data.frame("p"=p,"p0"=i,"n"=n)) 
  }
  ggplot(n.array,aes(x=p0,y=n))+geom_line() +
    geom_vline(xintercept=p0, color="blue",linetype="longdash") + geom_vline(xintercept=p, color="red",linetype="longdash") +
    labs(title="Sample Sizes for benchmark proportions", x="Observed/Sampled Proportion", y="Sample Size")
}


shinyServer(function(input,output) {

  output$nEst <- renderText(calcN(p=input$p,p0=input$p0,alpha=input$alpha,beta=input$beta))
  output$mEst <- renderText(n.moe(p=input$p,alpha=input$alpha,moe=input$moe))
  output$recommendedN <- renderPrint(cat(min(calcN(p=input$p,p0=input$p0,alpha=input$alpha,beta=input$beta),n.moe(p=input$p,alpha=input$alpha,moe=input$moe))))
  output$sampPlot <- renderPlot(N.plot(p=input$p,p0=input$p0,alpha=input$alpha,beta=input$beta,moe=input$moe))
  })