library(shiny)

shinyUI(fluidPage(
#tags$head(includeScript('google_analytics.js')),
  titlePanel("Sampling Sufficiency for Landscape Monitoring Objectives"),
  
  sidebarLayout(
    sidebarPanel(
      HTML('To use this tool, you need to specify for a given indicator: the observed (i.e., estimated) proporiton that meets some benchmark criterion, a landscape management objective as a proportion, the desired <a href="https://en.wikipedia.org/wiki/Type_I_and_type_II_errors#Statistical_test_theory" target="_blank">Type I (false change) and Type II (missed change) error rates</a>, and desired <a href="https://en.wikipedia.org/wiki/Margin_of_error#Basic_concept" target="_blank">margin of error</a>.'),
      hr(),
      h4("Input parameters"),
      numericInput("p",label="Monitoring Objective Proportion (red line)",value=0.25,min=0,max=1,step=0.01),
      numericInput("p0",label="Observed/estimated Proportion (blue line)",value=0.05,min=0,max=1,step=0.01),
      numericInput("alpha",label="Desired Type I Error rate (alpha)",value=0.2,min=0,max=1,step=0.01),
      numericInput("beta",label="Desired Type II Error rate (beta)",value=0.2,min=0,max=1,step=0.01),
      numericInput("moe",label="Desired Margin of Error",value=0.1,min=0,max=1,step=0.01),
      hr(),
      HTML('For more information on selecting error rates, see the <a href="http://shiny.landscapetoolbox.org/OptimalErrorRate/" target="_blank">Optimal Error Rate Calculator</a>.')
      ), # close sidebarPanel
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Sampling Sufficiency",
              p("This tool is intended for evaluating how many sample sites (i.e., plots) are needed to determine if an observed or estimated proporiton of sample sites meets a given management objective (stated as a proportion of the study area). 
                For example, is the estimated proportion of potential sage grouse habitat that is in unsuitable condition in a BLM Field Office less than 20% of total acres? 
                The estimated proportion/area/length from sampling is compared against a specified threshold value for the reporting unit with the idea that meeting or crossing 
                the threshold could result in determining the indicator does not meet a standard or trigger management action."),
              hr(),
              h3("Sample Size Estimates"),
              p("Sample size based on testing difference between proportions: ", textOutput("nEst", inline=TRUE)),
              p("Sample size based on achieving target margin of error: ", textOutput("mEst", inline=TRUE)),
              h4("Recommended sample size is: ",textOutput("recommendedN",inline=TRUE)),
              p("Recommended sample sizes are the smaller of two calculations: 1) the sample size needed to detect a difference between the observed and target proportions, and 2) the sample size required to meet the desired margin of error. 
                In both cases, recommended sample sizes assume that your sampling sites are independent."),
              hr(),
              h3("Sampling Sufficiency Graph"),
              plotOutput("sampPlot",width="100%",height="400px")),
        
        
        tabPanel("More Info", p("This tool performs basic sample size estimations for proprotions. It is appropriate to use this tool when you need to aggregate plot data that has been compared to a benchmark (e.g., a condition has been met or exceeded) and you are interested in what proportion of a larger landscape, administrative unit, or stream system meet this condition."),
                 HTML('<p>For example, plots with perennial grass heights less than 10cm are considered unsuitable for that indicator for Greater Sage-grouse nesting habitat (<a href="http://www.blm.gov/wo/st/en/info/blm-library/publications/blm_publications/tech_refs/SG_HAF.html" target="_blank">Stiver et al. 2015</a>; note that many other indicators factor into determining overall habitat suitability for a site). For management purposes you may have as a management objective that no more than 30% of lands in an unsuitable condition. In this case, each plot would be evaluated for that indicator and determined to be either unsuitable or not. The proportion of unsuitable plots would be compared to the landscape management objective (i.e., 30%). The question this tool addresses is how many sample points do you need to determine if your observed proportion is significantly different than your management objective.</p>'),
                 HTML('<p>This tool takes two approaches to estimating sample sizes for proportions. As the observed proportion in a study area gets close to the stated management objective, the sample size required to determine if those proportions are, in fact, different increases exponentially and without limit. To counteract this, the sample size is capped at a specified <a href="https://en.wikipedia.org/wiki/Margin_of_error#Basic_concept" target="_blank">margin of error</a>. The interpretation of this is that if the difference between your observed proportion and the management objective is less than the margin of error, then you conclude that there is no detectable difference between the two.</p>'),
                 h4('Caution'),
                 p('Results of a sample size estimation tool should be treated with caution and viewed as rough (and conservative) guidelines for how much data is needed to answer a monitoring question. Tools for estimating sample sizes or for judging sampling sufficiency are based on theoretical distributions and are laden with assumptions. It is typically not feasible to test for (or even anticipate) whether or not these assumptions are valid for a given dataset or situation. Additionally, you often must provide values for parameters that you may not know, or only have a rough approximation of. For these reasons a priori sample size estimates for monitoring should be used as a rough starting point and the sampling sufficiency (i.e., power) should be regularly reassessed and additional data collected as needed.'),
                 h4("Additional Reading"),
                 HTML('<ul><li>Steidl, Robert J., J. P. Hayes, and Eric M. Schauber. “Statistical Power Analysis in Wildlife Research.” Journal of Wildlife Management 61 (1997): 270–79.</li><li>Morrison, M. L. “Minireview: On Sample Sizes and Reliable Information.” The Condor 90 (1988): 275–78.</li>
</ul>'))
              )) # Close mainPanel and tabsetPanel
    ) # close sidebarLayout
  
  ))  # close fluidPage and shinyUI