library(pwr)
library(ggplot2)

mddPct = 0.25
alpha = 0.05
nRange = seq(3,150)
#plotArr = vector("numeric",length(nRange))
results.tab = data.frame(matrix(nrow=1,ncol=9))
names(results.tab)=c("stratum","indicator","mean","stdev","n.actual","coef.var","actualMDD","power","n.desired.mdd")
sim.tab <- data.frame("stratum"="", "indicator"="","n"=0,"pwr"=0)
sim.tab$indicator <- factor(sim.tab$indicator,levels=levels(data$Indicator))

## 1. add sample size line to plots
## 2. 
## 
row = 1
for (strat in levels(data$Site)) {
  for (l in levels(data$Indicator)) {
    data.sub <- data[data$Indicator==l&data$Site==strat,]
    
    #Calculate power stats
    xbar1 <- mean(data.sub$Any.Hit.Avg)
    s1 <- var(data.sub$Any.Hit.Avg)
    if (xbar1==0 & s1==0) break  ## trap for indicators not measured by stratum
    n <- length(data.sub$Any.Hit.Avg)
    cv <- xbar1/sqrt(s1)
    xbar2 = xbar1 + mddPct*xbar1
    sp = sqrt( (n-1)*s1/n  )
    d = (xbar1-xbar2)/sp
    p = pwr.t.test(n=n,d=d,sig.level=alpha,type="one.sample",alternative="two.sided")$power
    desired.n = pwr.t.test(power=0.8,d=d,sig.level=alpha,type="one.sample",alternative="two.sided")$n
    print(paste(l,p,sep=", "))
    
    #Write results to the table
    results.tab = rbind(results.tab,c(strat,l,xbar1,sqrt(s1),n,cv,d,p,desired.n))
    
    # Generate power curve data and write to sim.tab
    for (nSim in nRange) {
      pSim = pwr.t.test(n=nSim,d=d,sig.level=alpha,type="one.sample",alternative="two.sided")
      print(paste(l,nSim,pSim$power,sep=", "))
      sim.tab = rbind(sim.tab,data.frame("stratum"=strat,"indicator"=l,"n"=nSim,"pwr"=pSim$power))
      #plotArr[nSim-2]=pSim$power
    }
  } 
}


results.tab = results.tab[-1,]
sim.tab = sim.tab[-1,]

#Plot the power curve
#plot.frame = data.frame("n"=nRange,"pwr"=plotArr)
g = ggplot(data=sim.tab, aes(x=n,y=pwr,color=indicator))+geom_line()+geom_vline(data=results.tab,aes(xintercept=as.numeric(n),linetype="dashed"))
g+facet_wrap(~stratum)

