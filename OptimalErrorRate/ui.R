library(shiny)

shinyUI(fluidPage(
tags$head(includeScript('google_analytics.js')),
  titlePanel("Optimal Error Rate Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      HTML("This tool calculates the optimal Type I error rate (alpha) that minimizes overall error (Type I and II) for different kinds of t-Tests. 
          Optimal alpha is calculated from sample size, effect size, ratio of the costs of making Type I vs. Type II errors, and prior probabilities of the null hypothesis (i.e., no change) being true.
          Click the <strong>Effect Size</strong> tab for help calculating effect size for your data.
           Click the <strong>More Info</strong> tab for background and references."),
      hr(),
      h4("Input parameters"),
      selectInput("type",label="Test type",choices=list("One sample test"="one.sample","Two sample test"="two.sample","Paired test"="paired"),selected="one.sample"),
      numericInput("n1",label="Sample size for group 1",value=10,min=0,step=1),
      conditionalPanel("input.type=='two.sample'",numericInput("n2",label="Sample size for group 2",value=10,min=0,step=1)),
      numericInput("d",label="Effect size",value=1.2,min=0,step=0.01),
      selectInput("tails",label="Tails",choices=list("Single-sided (one-tailed) test"="one.tailed","Two-sided (two-tailed) test"="two.tailed"),selected="one.tailed"),
      numericInput("T1T2cratio",label="Cost ratio for Type I/Type II Errors",value=1,min=0,max=5,step=0.01),
      numericInput("HaHopratio",label="Prior probability ratio for Ho/Ha",value=1,min=0,max=10,step=0.01)
      ), # close sidebarPanel
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Optimal Error Rate",h3("Optimal Error Rate Output"),
              verbatimTextOutput("optabOut"),
              h3("Optimal Error Rate Graph"),
              plotOutput("optabPlot",width="100%",height="400px")),
        tabPanel("Effect Size",h4("Effect Size Explained"),
                 HTML("Effect size is the magnitude of difference between two groups. For a useful statistical test, you should first determine what a meaningful effect size is. 
                    There are many different ways to <a href='http://en.wikipedia.org/wiki/Effect_size' target='_blank'>calculate effect size</a>. The optimal error rate tool uses 
                    the <a href='http://en.wikiversity.org/wiki/Cohen%27s_d' target='_blank'>Cohen's D standardized effect size</a> which is a
                    unitless number that is a difference in means divided by a pooled standard deviation. Depending on the test and options, there are different forumulas for
                    calculating Cohen's D. You can use the calculator below and input the result in the Effect Size box at the left."),
                 hr(),
                 h4("Effect Size Inputs"),
                 selectInput("EStype",label="Select Type",choices=list("Percent Change of a Mean"="pct","Test Against a Threshold"="threshold","Compare Two Samples"="two.sample")),
                 fluidRow(
                   column(6,numericInput("x1",label="Sample Mean",value=10,step=0.01),
                          numericInput("esn1",label="Sample Size",min=0,step=1,value=10),
                          numericInput("s1",label="Sample Variance",value=5,min=0,step=0.01)),
                 column(6,
                        conditionalPanel("input.EStype=='two.sample'",
                            numericInput("x2",label="Sample Mean Group 2",value=20,step=0.01),
                            numericInput("esn2",label="Sample Size Group 2",min=0,step=1,value=10),
                            numericInput("s2",label="Sample Variance Group 2",value=9,min=0,step=0.01)),
                        conditionalPanel("input.EStype!='two.sample'",
                            conditionalPanel("input.EStype=='pct'",
                                             numericInput("pctChg",label="Percent Change",min=0,max=100,step=1,value=25)),
                            conditionalPanel("input.EStype=='threshold'",
                                             numericInput("threshold",label="Threshold Value",step=0.01,value=15)),                            
                            checkboxInput("paired",label="Repeated measures (i.e., paired)?")))
                 ),                          
                 h4("Calculated effect size is: ",textOutput("CohensD",inline=TRUE)),
                 hr(),
                 h4("Helpful Links"),
                 HTML("<ul>
                    <li><a src='http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3444174/' target='_blank'>Using Effect Size-or Why the P Value Is Not Enough, Journal of Graduate Medical Education article</a></li>
                    <li><a src='http://en.wikipedia.org/wiki/Effect_size' target='_blank'>Wikipedia: Effect Size</a></li>
                    <li><a src='http://en.wikiversity.org/wiki/Cohen%27s_d' target='_blank'>Wikiversity: Cohen's D</a></li>
                    </ul>")
                 ),
        tabPanel("Cost Ratio",h4("Cost Ratio Calculator"),
                 p("This is a simple calculator for determining cost ratios to use in the optimal alpha tool. The costs considered here are those related to making a Type I or Type II error.While some error costs are easy to quantiy in monetary terms (direct costs), others are difficult to put a dolar amount to (indirect costs). Examples of indirect costs may include time lost due to appeals and litigation, loss of credibility, damage to partnerships and working relationships. Despite being challenging to quantify, you should make an effort to put a number to them. Alternatively, costs could be specified in relative terms."),
                 hr(),
                 h4("Cost Ratio Inputs"),
                 fluidRow(
                   column(6, h5("Type I Error Costs"),
                          numericInput("IaddMonit",label="Additional Monitoring Costs",min=0,value=15000,step=100),
                          numericInput("IlostUse",label="Costs from Lost Use",min=0,value=10000,step=100),
                          numericInput("IotherDirect",label="Other Direct Costs",min=0,step=100,value=0),
                          numericInput("Iindirect",label="Indirect Costs",value=0,min=0,step=100)),
                   column(6, h5("Type II Error Costs"),
                          numericInput("IIrestoration",label="Additional Monitoring Costs",min=0,value=150000,step=100),
                          numericInput("IIotherDirect",label="Other Direct Costs",min=0,step=100,value=0),
                          numericInput("IIindirect",label="Indirect Costs",value=0,min=0,step=100))
                 ),                          
                 h4("Calculated cost ratio is: ",textOutput("calcCR",inline=TRUE))),
        tabPanel("More Info", p("In the context of natural resource monitoring, failing to detect a change (Type II error) can be more harmful and costly than falsely claiming that a change occurred (Type I error).
                                Rather than using arbitrary error rates (e.g., alpha = 0.05) for statitical analyses and tests which may lead to increased likelihood of making Type II errors, both error rates should be set at levels that minimize the chance of making any type of error.
                                Because Type I and II error rates are related, the optimal error rate is the alpha level (Type I error rate) that minimizes the probability of Type I and Type II errors for given sample and effect sizes
                                (see Mudge et al. 2012a, 2012b). The optimal error rate can be adjusted to account for differential costs of the different types of errors and for expectations 
                                (i.e., prior probabilities) of the likelihood of change occurring (or not occurring)."),
                 p("This tool uses R code provided by Mudge et al. (2012a) to calculate optimal Type I error rates. Specifically, this Shiny tool executes the following function:"),
                 pre("optab(n1,n2,d,type,tails,T1T2cratio,HaHopratio"),
                 p("where n1 and n2 are sample sizes, d is Cohen's effect size, type is the type of t-Test (one sample, two-sample, paired), tails refers to whether the test is for a one-tailed or two-tailed alternative, T1T2cratio = the cost ration of Type I to Type II errors, and HaHopratio is the ratio of prior probabilities. A companion function was written to produce the plot showing how error rates change as Type I error rate goes from zero to one:"),
                 pre("optab.plot(n1,n2,d,type,tails,T1T2cratio,HaHopratio)"),
                 h4("References"),
                 HTML("<ul><li>Mudge, Joseph F., Leanne F. Baker, Christopher B. Edge, and Jeff E. Houlahan. “Setting an Optimal Α That Minimizes Errors in Null Hypothesis Significance Tests.” Edited by Zheng Su. PLoS ONE 7, no. 2 (February 28, 2012): e32734. doi:<a href='http://dx.doi.org/10.1371/journal.pone.0032734' target='_blank'>10.1371/journal.pone.0032734</a>.
</li><li>Mudge, Joseph F., Timothy J. Barrett, Kelly R. Munkittrick, and Jeff E. Houlahan. “Negative Consequences of Using Α = 0.05 for Environmental Monitoring Decisions: A Case Study from a Decade of Canada’s Environmental Effects Monitoring Program.” Environmental Science & Technology 46, no. 17 (September 4, 2012): 9249–55. doi:<a href='http://dx.doi.org/10.1021/es301320n' target='_blank'>10.1021/es301320n</a>.
</li></ul>"))
              )) # Close mainPanel and tabsetPanel
    ) # close sidebarLayout
  
  ))  # close fluidPage and shinyUI