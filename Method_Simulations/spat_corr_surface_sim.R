# Simulation of sample weighting for combined sampling designs
# First case: Two sample designs: Frame A: occupying the full 500x50 field, and B) the second covering a 100x100 portion of the field. No stratification

library(gstat)
library(raster)
library(spsurvey)

# Sampling intensity for each frame
numA = 30
numB = 20
n.sim = 100
field.size = 500

# create structure
xy <- expand.grid(1:field.size, 1:field.size)
names(xy) <- c("x","y")

# define the gstat object (spatial model) for the simulations
g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.25,model="Exp",range=100), nmax=5)

# make a simulation based on the stat object
yy <- predict(g.dummy, newdata=xy, nsim=n.sim)
gridded(yy) = ~x+y

# Iterate over the simulated surfaces
sim.results <- data.frame()
for (i in 1:n.sim) {
  # Create Frame A and select random points
  tmp <- Polygon(matrix(c(0,field.size,field.size,0,0,0,field.size,field.size),ncol=2))
  poly <- Polygons(list(tmp),"A")
  frameA <- SpatialPolygons(list(poly))
  x <- runif(numA,0,field.size)
  y<-runif(numA,0,field.size)
  sampA <- data.frame(x=x,y=y)
  coordinates(sampA)<-~x+y
  
  # Create Frame B and select random points
  tmp <- Polygon(matrix(c(100,300,300,100,100,100,300,300),ncol=2))
  poly <- Polygons(list(tmp),"B")
  frameB <- SpatialPolygons(list(poly))
  x <- runif(numA,100,300)
  y<-runif(numA,100,300)
  sampB <- data.frame(x=x,y=y)
  coordinates(sampB)<-~x+y
  
  # Display the scenario, just for fun.
  image(yy[i])
  lines(frameA)
  points(sampA, pch=22)
  lines(frameB)
  points(sampB, pch=24)
  
  # Sample the simulation surface using both point sets
  dataA <- extract(raster(yy[i]),sampA,fun="max",df=T,na.rm=T)
  dataB <- extract(raster(yy[i]),sampB,fun="max",df=T,na.rm=T)
  
  # Determine frame intersection and point membership (i.e., A or AB)
  
  # Calc weights
  
  # Produce estimates
  # 1. equal point weights
  samp.comb <- rbind(dataA,dataB)
  wgt <- rep(field.size^2/nrow(samp.comb),nrow(samp.comb))
  samp.stat <- total.est(samp.comb[,2],wgt,vartype="SRS")
  samp.mean <- samp.stat[2,3]
  samp.mean.se <- samp.stat[2,4]
  samp.sd <- samp.stat[4,3]
  samp.sd.se <- samp.stat[4,4]
  
  # Determine simulation surface actual stats
  sim.mean <- cellStats(raster(yy[i]),stat='mean')
  sim.sd <- cellStats(raster(yy[i]),stat='sd')
  
  # Write out the results
  sim.results <- rbind(sim.results,c(i,numA,numB,samp.mean,samp.mean.se,samp.sd,samp.sd.se,sim.mean,sim.sd))
  
}

names(sim.results) <- c("i","numA","numB","samp.mean","samp.mean.se","samp.sd","samp.sd.se","sim.mean","sim.sd")

