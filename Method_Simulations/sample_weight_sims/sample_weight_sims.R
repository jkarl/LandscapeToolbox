# Simulation of sample weighting for combined sampling designs
# First case: Two sample designs: Frame A: occupying the full 500x500 field, and B) the second covering a 100x100 portion of the field. No stratification

library(gstat)
library(raster)
library(spsurvey)

setwd("~/Google Drive/Landscape Toolbox/LandscapeToolbox_GitHub/Method_Simulations/sample_weight_sims")

# Sampling intensity for each frame
numA = 30
numB = 20
n.sim = 100
a=2 # number of weight calculation approaches - for setting up output data frame
field.size = 500
frameA.shp = "field"
frameB.shp = "frameB"

# create structure
xy <- expand.grid(1:field.size, 1:field.size)
names(xy) <- c("x","y")

# define the gstat object (spatial model) for the simulations
g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.25,model="Exp",range=100), nmax=5)

# make a simulation based on the stat object
yy <- predict(g.dummy, newdata=xy, nsim=n.sim)
gridded(yy) = ~x+y

# Read in Frame A and Frame B
frameA <- read.shape(paste(frameA.shp,"shp",sep="."))
designA <- list(None=list(panel=c(Panel=numA),seltype="Equal"))
frameB <- read.shape(paste(frameB.shp,"shp",sep="."))
designB <- list(None=list(panel=c(Panel=numB),seltype="Equal"))


# Iterate over the simulated surfaces
sim.results.1 <- data.frame(i=integer(n.sim),approach=character(n.sim),numA=integer(n.sim),numB=integer(n.sim),samp.mean=double(n.sim),samp.mean.se=double(n.sim),
                          samp.sd=double(n.sim),samp.sd.se=double(n.sim),sim.mean=double(n.sim),sim.sd=double(n.sim),stringsAsFactors=FALSE)
sim.results.2 <- data.frame(i=integer(n.sim),approach=character(n.sim),numA=integer(n.sim),numB=integer(n.sim),samp.mean=double(n.sim),samp.mean.se=double(n.sim),
                            samp.sd=double(n.sim),samp.sd.se=double(n.sim),sim.mean=double(n.sim),sim.sd=double(n.sim),stringsAsFactors=FALSE)
for (i in 1:n.sim) {
  # Create random points for Frames A and B
  sampA <- grts(design=designA,src.frame="sp.object",sp.object=frameA,att.frame=NULL,shapefile=F,type.frame="area")
  sampB <- grts(design=designB,src.frame="sp.object",sp.object=frameB,att.frame=NULL,shapefile=F,type.frame="area")
  
  ## Old way of doing it long hand
  #tmp <- Polygon(matrix(c(0,field.size,field.size,0,0,0,field.size,field.size),ncol=2))
  #poly <- Polygons(list(tmp),"A")
  #frameA <- SpatialPolygons(list(poly))
  #x <- runif(numA,0,field.size)
  #y<-runif(numA,0,field.size)
  #sampA <- data.frame(x=x,y=y)
  #coordinates(sampA)<-~x+y
  
  # Display the scenario, just for fun.
  image(yy[i])
  lines(frameA)
  points(sampA, pch=22)
  lines(frameB)
  points(sampB, pch=24)
  
  # Sample the simulation surface using both point sets
  dataA <- extract(raster(yy[i]),sampA,fun="max",df=T,na.rm=T)
  dataB <- extract(raster(yy[i]),sampB,fun="max",df=T,na.rm=T)

  # Determine simulation surface actual stats
  sim.mean <- cellStats(raster(yy[i]),stat='mean')
  sim.sd <- cellStats(raster(yy[i]),stat='sd')
  
  # Calc weights, produce estimates, and write to output DF
  # Approach 1. Ignore all design info, merge the points and calc equal weights
  approach <- "dumb"
  data.comb <- rbind(dataA,dataB)
  wgt <- rep(field.size^2/nrow(data.comb),nrow(data.comb))
  samp.stat <- total.est(data.comb[,2],wgt,vartype="SRS")
  samp.mean <- samp.stat[2,3]
  samp.mean.se <- samp.stat[2,4]
  samp.sd <- samp.stat[4,3]
  samp.sd.se <- samp.stat[4,4]  
  sim.results.1[i,] <- c(i,approach,numA,numB,samp.mean,samp.mean.se,samp.sd,samp.sd.se,sim.mean,sim.sd)
  
  # Approach 2. Calc new weights for Frame A/B area and Frame A (minus Frame B part)
  approach <- "recalc"
  frame.merged <- union(frameA,frameB)
  sp.A <- SpatialPointsDataFrame(sampA@coords,data=sampA@data)
  sp.B <- SpatialPointsDataFrame(sampB@coords,data=sampB@data)
  samp.comb <- rbind(sp.A,sp.B)
  
  ### Undoubtedly a better way to do this...
  tmp <- over(samp.comb,frame.merged) # membership in the different frames
  wgt.AB <- frame.merged@polygons[[2]]@area/sum(tmp[,3],na.rm=T)
  wgt.Amin <- (frame.merged@polygons[[1]]@area-frame.merged@polygons[[2]]@area)/(nrow(samp.comb)-sum(tmp[,3],na.rm=T))
  tmp["wgt"] <-rep(wgt.Amin,nrow(samp.comb))
  tmp$wgt[tmp$id.2==1] <- wgt.AB
  samp.stat <- total.est(data.comb[,2],tmp$wgt,vartype="SRS")
  samp.mean <- samp.stat[2,3]
  samp.mean.se <- samp.stat[2,4]
  samp.sd <- samp.stat[4,3]
  samp.sd.se <- samp.stat[4,4]  
  sim.results.2[i,] <- c(i,approach,numA,numB,samp.mean,samp.mean.se,samp.sd,samp.sd.se,sim.mean,sim.sd)
  
  # Approach 3. Point weight rescaling
  approach <- "rescaling"
  
  
}

sim.results.1["mean.diff"] <- as.double(sim.results.1$sim.mean) - as.double(sim.results.1$samp.mean)
sim.results.2["mean.diff"] <- as.double(sim.results.2$sim.mean) - as.double(sim.results.2$samp.mean)

plot(sim.results.1$mean.diff,pch=12,ylab="Difference from simulation actual")
points(sim.results.2$mean.diff,pch=16)
abline(h=0)

## plot some extreme examples
par(mfrow=c(1,3))
image(yy[31])
lines(frameA)
lines(frameB)
image(yy[48])
lines(frameA)
lines(frameB)
image(yy[54])
lines(frameA)
lines(frameB)
par(mfrow=c(1,1))

image(yy[100])
lines(frameA)
lines(frameB)
points(sampA, pch=22)
points(sampB, pch=24)

write.csv(sim.results.1,"nested_2frame.dumb_weights.csv")
write.csv(sim.results.2,"nested_2frame.frame_reweighted.csv")


