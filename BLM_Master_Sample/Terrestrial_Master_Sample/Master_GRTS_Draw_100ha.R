library(spsurvey)
library(snowfall)

CartProduct = function(CurrentMatrix, NewElement)
{
  if (length(dim(NewElement)) != 0 )
  {
    warning("New vector has more than one dimension.")
    return (NULL)
  } 
  if (length(dim(CurrentMatrix)) == 0)
  {
    CurrentRows = length(CurrentMatrix)
    CurrentMatrix = as.matrix(CurrentMatrix, nrow = CurrentRows, ncol = 1)
  } else {
    CurrentRows = nrow(CurrentMatrix)
  }
  var1 = replicate(length(NewElement), CurrentMatrix, simplify=F)
  var1 = do.call("rbind", var1)
  var2 = rep(NewElement, CurrentRows)
  var2 = matrix(var2[order(var2)], nrow = length(var2), ncol = 1)
  CartProduct = cbind(var1, var2)
  return (CartProduct)
}

randomPt <- function(xCorner,yCorner) {
  return(list(x=xCorner+runif(1)*1000,y=yCorner+runif(1)*1000))
}

wrapper <- function(i) {
  tile <- tiles[i,]  # t[[i]]
  print(paste("Processing tile ",i))
        #design.tile <- list(None=list(panel=c(Panel=n),seltype="Equal",over=0))
        #return(grts(design=design.tile, DesignID="test",src.frame="sp.object",sp.object=tile,type.frame="area",shapefile=FALSE))
        xmin = tile@bbox['x','min']
        xmax = tile@bbox['x','max']
        xrange = xmax-xmin
        ymin = tile@bbox['y','min']
        ymax = tile@bbox['y','max']
        yrange = ymax-ymin
        
        xincrement = xrange/100
        yincrement = yrange/100
        
        xSteps <- seq(from=xmin,to=xmax,by=xincrement)[-1]
        ySteps <- seq(from=ymin,to=ymax,by=yincrement)[-1]
        
        mojo <- CartProduct(xSteps,ySteps)
        test <- mapply(randomPt,mojo[,1],mojo[,2],SIMPLIFY=F)
        test2 <- data.frame(matrix(unlist(test),nrow=length(test),byrow=T))
        return(test2)
}


tiles <- read.shape('/Volumes/DataProducts/USDA/Management Center/Projects/Master_Sample/geodata/100km_tiles.shp')

## Now run tiled GRTS with parallel processing
start <- Sys.time()
sfInit(parallel=TRUE, cpus=2)
#sfInit(parallel=TRUE, cpus=1)
sfLibrary(spsurvey)
sfLibrary(sp)
sfExport('tiles')
sfExport('CartProduct')
sfExport('randomPt')
sfExport('wrapper')

tiles.grts <- sfLapply(1:nrow(tiles),wrapper)
tiled.parallel.grts <- do.call(rbind,tiles.grts)  #this isn't a really efficient way to smash the results back together, but it will work for now...
end <- Sys.time()
print(end-start)
sfStop()

ids <- seq(1:nrow(tiled.parallel.grts))
mdcatty <- rep(0,nrow(tiled.parallel.grts))
mdm <- rep(0,nrow(tiled.parallel.grts))
tiled.parallel.combine <- cbind(ids,tiled.parallel.grts)
names(tiled.parallel.combine) <- c("id","x","y")
names(test)
tiled.parallel.spdf <- SpatialPointsDataFrame(coords=tiled.parallel.grts,data=data.frame("ID"=ids,"mdm"=mdm,"mdcatty"=mdcatty))
test.grts <- grtspts(src.frame="att.frame",ptsframe=test,do.sample=FALSE,startlev=11,maxlev=11)

west13 <- read.shape("/Volumes/DataProducts/USDA/Management Center/Projects/Master_Sample/geodata/Western13States_buffer25k.shp")
proj4string(west13) <- CRS("+init=epsg:5070")
proj4string(tiled.parallel.spdf) <- CRS("+init=epsg:5070")
sel <- over(tiled.parallel.spdf, west13)
west13.points <- tiled.parallel.spdf[!is.na(sel[,1]),]
writeOGR(west13.points, "/Volumes/DataProducts/USDA/Management Center/Projects/Master_Sample/geodata/West13Points.shp", "West13Points", driver="ESRI Shapefile")


end <- Sys.time()
print(end-start)

plot(mojo,col="red",xlim=c(-340000,-320000),ylim=c(260000,280000))
points(test2,col="blue")