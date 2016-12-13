#Optimal alpha t-test R code version 1.1, updated for compatibility with R version 3.x.
#Authored by Joe Mudge (questions or comments? contact: joe.mudge83@gmail.com).

library(ggplot2)

beta.t.test<-function (n1 = NULL, n2 = NULL, d = NULL, sig.level = 0.05, type = c("two.sample", "one.sample", "paired"),tails = c("two.tailed","one.tailed")) {
  if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > sig.level | sig.level > 1)) 
    stop(sQuote("sig.level"), " must be numeric in [0, 1]")
  if (!is.null(n1) && n1 < 2) 
    stop("number of observations in the first group must be at least 2")
  type <- match.arg(type)
  tails <- match.arg(tails)
  d<-abs(d)
  tsample <- switch(type, one.sample = 1, two.sample = 2, paired = 1)
  tside <- switch(tails, one.tailed = 1, two.tailed = 2)
  if (tside == 1) {
    p.body <- quote({
      nu <- switch(type, one.sample = n1-1, two.sample = n1 + n2 - 2, paired = n1-1)
      pt(qt(sig.level/tside, nu, lower = FALSE), nu, ncp = d * switch(type, one.sample = sqrt(n1), two.sample = (1/sqrt(1/n1 + 1/n2)), paired = sqrt(n1)), lower = FALSE)
    })
  }
  if (tside == 2) {
    p.body <- quote({
      nu <- switch(type, one.sample = n1-1, two.sample = n1 + n2 - 2, paired = n1-1)
      qu <- qt(sig.level/tside, nu, lower = FALSE)
      pt(qu, nu, ncp = d * switch(type, one.sample = sqrt(n1), two.sample = (1/sqrt(1/n1 + 1/n2)), paired = sqrt(n1)), lower = FALSE) + 
        pt(-qu, nu, ncp = d * switch(type, one.sample = sqrt(n1), two.sample = (1/sqrt(1/n1 + 1/n2)), paired = sqrt(n1)), lower = TRUE)
    })
  }
  1-eval(p.body)
}

w.average.error<-function (alpha=NULL,n1=NULL,n2=NULL,d=NULL,T1T2cratio=1,HaHopratio=1,type = c("two.sample", "one.sample", "paired"),tails = c("two.tailed","one.tailed")) 
  ((alpha*T1T2cratio+HaHopratio*(beta.t.test(n1=n1,n2=n2,d=d,sig.level=alpha,type=type,tails=tails))))/(HaHopratio+T1T2cratio)

min.average.error<-function (n1=NULL,n2=NULL,d=NULL,T1T2cratio=1,HaHopratio=1,type = c("two.sample", "one.sample", "paired"),tails = c("two.tailed","one.tailed")) 
  unlist(optimize(w.average.error,c(0,1),tol=0.0000000000001,n1=n1,n2=n2,d=d,T1T2cratio=T1T2cratio,HaHopratio=HaHopratio,type=type,tails=tails))[2]

alpha<-function (n1=NULL,n2=NULL,d=NULL,T1T2cratio=1,HaHopratio=1,type = c("two.sample", "one.sample", "paired"),tails = c("two.tailed","one.tailed")) 
  unlist(optimize(w.average.error,c(0,1),tol=0.000000000001,n1=n1,n2=n2,d=d,T1T2cratio=T1T2cratio,HaHopratio=HaHopratio,type=type,tails=tails))[1]

beta<-function (n1=NULL,n2=NULL,d=NULL,T1T2cratio=1,HaHopratio=1,type = c("two.sample", "one.sample", "paired"),tails = c("two.tailed","one.tailed")) 
  ((T1T2cratio+HaHopratio)*min.average.error(n1=n1,n2=n2,d=d,T1T2cratio=T1T2cratio,HaHopratio=HaHopratio,type=type,tails=tails)-T1T2cratio*alpha(n1=n1,n2=n2,d=d,T1T2cratio=T1T2cratio,HaHopratio=HaHopratio,type=type,tails=tails))/HaHopratio


optab<-function (n1=NULL,n2=NULL,d=NULL,T1T2cratio=1,HaHopratio=1,type = c("two.sample", "one.sample", "paired"),tails = c("two.tailed","one.tailed")) {  
  
  list(
    "test type"=match.arg(type),
    "tails"=match.arg(tails),
    "output"=t(data.frame(
      "sample size 1"=n1,
      "sample size 2"=n2,
      "Cohen's d effect size"=d,
      "Type I/II error cost ratio"=T1T2cratio,
      "Ha/Ho prior probability ratio"=HaHopratio,
      "overall probability of error"=(alpha(n1=n1,n2=n2,d=d,T1T2cratio=T1T2cratio,HaHopratio=HaHopratio,type=type,tails=tails)+HaHopratio*beta(n1=n1,n2=n2,d=d,T1T2cratio=T1T2cratio,HaHopratio=HaHopratio,type=type,tails=tails))/(1+HaHopratio),
      "cost-weighted probability of error"=min.average.error(n1=n1,n2=n2,d=d,T1T2cratio=T1T2cratio,HaHopratio=HaHopratio,type=type,tails=tails), 
      "optimal alpha"=alpha(n1=n1,n2=n2,d=d,T1T2cratio=T1T2cratio,HaHopratio=HaHopratio,type=type,tails=tails),
      "optimal beta"=beta(n1=n1,n2=n2,d=d,T1T2cratio=T1T2cratio,HaHopratio=HaHopratio,type=type,tails=tails),row.names="values"))
  )
  
}

optab.plot <- function(n1=NULL,n2=NULL,d=NULL,a_range=c(0,1),step=0.001,T1T2cratio=1,HaHopratio=1,type = c("two.sample", "one.sample", "paired"),tails = c("two.tailed","one.tailed"),xlim=c(0,0.5),ylim=c(0,0.5)) {  
  rng <- seq(a_range[1],a_range[2],by=step)
  out <- data.frame("alpha"=rng,"w"=numeric(length(rng)))
  for (a in rng) {
    out[out$alpha==a,2]=w.average.error(a,n1,n2,d,T1T2cratio,HaHopratio,type,tails)
  }
  p<-ggplot(data=out,aes(x=alpha,y=w))+geom_line(size=1.5)+coord_cartesian(xlim=xlim,ylim=ylim)+
    xlab("Alpha")+ylab("Average Error Rate (alpha + beta)")+
    #ggtitle("Average error by alpha level")+
    geom_hline(yintercept=min.average.error(n1=n1,n2=n2,d=d,T1T2cratio=T1T2cratio,HaHopratio=HaHopratio,type=type,tails=tails),linetype=2,color="steelblue")+
    geom_vline(xintercept=alpha(n1=n1,n2=n2,d=d,T1T2cratio=T1T2cratio,HaHopratio=HaHopratio,type=type,tails=tails),linetype=2,color="steelblue")
    p+theme(text=element_text(size=18),axis.text.y=element_text(angle=90))
}

effect.size <- function(x1,n1,s1,type,pctChg,x2,n2,s2,paired,threshold=0){
  rho = 0.6
  if(type=="pct"){
    xd <- x1*pctChg/100
    if(paired) {
      sp = sqrt( (n1-1)/n1*s1*2*(1-rho) )
    } else {
      sp <- sqrt( (n1-1)*s1/n1  )
    }
  } else if (type=="two.sample") {
    xd <- abs(x1-x2)
    sp <- sqrt(((n1-1)*s1+(n2-1)*s2)/(n1+n2-2))
  } else { # Threshold
    xd <- abs(x1-threshold)
    if(paired) {
      sp = sqrt( (n1-1)/n1*s1*2*(1-rho) )
    } else {
      sp <- sqrt( (n1-1)*s1/n1  )
    }
  }
  d <- xd/sp
  return(d)
}


#The function used to calculate optimal alphas is: optab(n1=NULL,n2=NULL,d=NULL,T1T2cratio=1,HaHopratio=1,type = c("two.sample", "one.sample", "paired"),tails = c("two.tailed","one.tailed"))
#The arguments 'n1' and 'n2' are the samples sizes of each group (for a one sample test, enter any value >=3 for n2, n2 will be ignored)
#The argument 'd' is the 'Cohen's d' standardized critical effect size. Cohen's d = difference between group means/pooled within group standard deviation
#The argument 'T1T2cratio' is the cost ratio of Type I errors relative to Type II errors. T1T2cratio is set at 1 as a default, making Type I and Type II errors equally serious.
#The argument 'HaHopratio' is the prior probability of the alternate hypothesis relative to the prior probability of the null hypothesis. HaHopratio is set at 1 as a default, to not weight alpha and beta by their prior probabilities (assuming they are unknown).
#The argument 'type' is the type of t-test being undertaken and must be "two.sample", "one.sample" or "paired". If ignored, "two.sample" is the default.
#The argument 'tails'is the number of tails being examined and must be either "two.tailed" or "one.tailed". If ignored, "two.tailed" is the default.
#This code is partially based on code modified from the R package 'pwr'(Champely 2009).


