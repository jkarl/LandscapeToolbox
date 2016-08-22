# Simulation of different height methods
# 1000x1000 field with randomly selected points in it
# distance from each point calculated and voroni polygons constructed
# random voronoi polygons selected as "shrubs"
# distance from centroid within each shrub normalized and multiplied by a constant selected from a prob distribution
#    to represent heights. Uniform random noise added to the result

# virtual transects laid across each realization and each height method applied.

# Questions to answer with the simulation:
# 1. Does the resulting height information represent the population moments in an unbiased fashion?
#    i.e., what is the indicator that is actually being measured for each method?
# 2. What is the expected difference between the methods in different "shrub" densities and sizes?
# 3. Are there situations where one of the methods breaks down or does not produce unbiased estimates?

library(raster)
library(sp)
library(rgeos)
library(gstat)

setwd("~/Dropbox/manuscripts/2016/HAF-AIM_Height_Comparison/data")

num <- 100 # number of plants to simulate
size <- 25 # average size of plants (in # of cells)
size.var <- 12.5 # variance in size of plants
num.iter <- 100

#Initialize the raster with null values
field <- raster(nrow=1000,ncol=1000,vals=1,crs="+init=epsg:26978",xmn=0,xmx=1000,ymn=0,ymx=1000)

# Start iteration
out.df <- data.frame()
for (j in 1:num.iter) {
  print(paste("Iteration ",j,sep=""))
  #Create some random points
  x <- runif(num,0,1000)
  y<-runif(num,0,1000)
  xy <- data.frame(x=x,y=y)
  coordinates(xy)<-~x+y
  buf.widths <- abs(rnorm(num,mean=size,sd=size.var))+2
  vegBuffers <- gBuffer(xy,width=buf.widths,byid=T)
  
  vegHeights <- stack()
  
  #Iterate through points, buffer by random amount, construct unconditional Gaussian sim within buffer, append to raster stack
  for (i in 1:length(vegBuffers)) {
   print(paste("Creating plant ",i," of ",num," for iteration ",j,sep=""))
    plant <- vegBuffers[i]
    plant.extent <- extent(plant)
    
    ## Create simulation surface
    plant.extent <- extent(plant) ### Get extent of the buffer
    xy <- expand.grid(plant.extent@xmin:plant.extent@xmax, plant.extent@ymin:plant.extent@ymax)
    names(xy) <- c("x","y")
    g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.25,model="Exp",range=plant.extent@xmax-plant.extent@xmin), nmax=10)
    # make simulation based on the stat object
    yy <- predict(g.dummy, newdata=xy, nsim=1)
    gridded(yy) <- ~x+y
    # expand the values to occupy a larger range and shift to avoid negative values
    yy@data <- (yy@data+10)*2 
    # clip the simulation to the buffer
    plant.ht <- mask(raster(yy),plant)
    # append the simulated plant height to the raster stack
    plant.ht <- crop(plant.ht,field) #catch if a buffer extends beyond the field
    plant.ht <- extend(plant.ht,field)
    extent(plant.ht) <- c(0,1000,0,1000)
    vegHeights <- addLayer(vegHeights,plant.ht)
  }
  
  ## Construct a transect of points from which to perform sampling
  x <- seq(5,999,by=20)
  transect <- data.frame(x=x,y=x) # Creates a diagonal transect
  coordinates(transect)<-~x+y
  
  ## Perform AIM sampling at each point
  # buffer the points by 10 (some amount that won't overlap with the neigboring points)
  t.buffer <- gBuffer(transect,width=10,byid=T)
  # Find max veg height in each point buffer
  aim.ht <- extract(vegHeights,t.buffer,fun="max",df=T,na.rm=T)
  aim.ht[aim.ht==-Inf] <- NA
  aim.sum.max <- apply(aim.ht[,-1],1,max,na.rm=T)
  aim.sum.min <- apply(aim.ht[,-1],1,min,na.rm=T) # if there are overlapping plants, this will be different for those overlaps
  aim.sum <- mean(aim.sum.max[aim.sum.max!=-Inf])
  aim.count <-  length(aim.sum.max[aim.sum.max!=-Inf])
  aim.multiples <- sum(rowSums(aim.ht[,-1]>0,na.rm=T)>1)
  paste("Average AIM Height: ",aim.sum,sep="")
  paste("Number of AIM height measurements: ",aim.count,sep="")
  
  ## Now do the HAF height method - Going to be challenging.
  # identify the buffer(s) that intersect a point
  # find the maximum height of that buffer
  # record the max height and also the buffer ID
  # screen out duplicate buffer IDs
  haf.plants <- over(transect,vegBuffers)
  haf.unique <- sort(unique(haf.plants))
  haf.buffers <- vegBuffers[haf.unique]
  if (length(haf.buffers)>0){
    haf.ht <- extract(vegHeights,haf.buffers,fun="max",df=T,na.rm=T)
    haf.ht[haf.ht==-Inf] <- NA
    haf.sum.max <- apply(haf.ht[,-1],1,max,na.rm=T)
    haf.sum <- mean(haf.sum.max[haf.sum.max!=-Inf])
    haf.count <-  length(haf.sum.max[haf.sum.max!=-Inf])
    haf.dups <- sum(data.frame(table(haf.plants,useNA="no"))$Freq>1)  
  } else {
    haf.sum=0
    haf.count=0
    haf.dups=0
  }
  
  paste("Average HAF Height: ",haf.sum,sep="")
  paste("Number of HAF height measurements: ",haf.count,sep="")
  
  ## Finally, compute the stats for the original simulation set for comparison
  veg.ht.max <- extract(vegHeights,vegBuffers,fun="max",df=T,na.rm=T)
  veg.max.sum <- apply(veg.ht.max[,-1],1,max,na.rm=T)
  veg.ht.mean <- extract(vegHeights,vegBuffers,fun="mean",df=T,na.rm=T)
  veg.mean.sum <- apply(veg.ht.mean[,-1],1,max,na.rm=T)
  plot.avg <- mean(veg.mean.sum)
  plot.max <- mean(veg.max.sum)
  paste("Actual Average Plant Height: ",plot.avg,sep="")
  paste("Actual Average Maximum Height: ",plot.max,sep="")

  # Write results to output data frame
  out.df <- rbind(out.df,c(j,num,size,size.var,aim.sum,aim.count,aim.multiples,haf.sum,haf.count,haf.dups,plot.avg,plot.max))
}

names(out.df) <- c("iteration","num.plants","plant.size","plant.size.var","aim.sum","aim.count","aim.multiples","haf.sum","haf.count",
                   "haf.duplicates","plot.average","plot.max")
write.csv(out.df,"aim-haf_compare_d100_medium.csv")