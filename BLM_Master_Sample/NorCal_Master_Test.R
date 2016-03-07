library(spsurvey)
#setwd("/Users/jason/Google Drive/BLM_AIM/Master_Sample/Nor_Cal_Test")
setwd("/Volumes/DataProducts/USDA/Management Center/Projects/Master_Sample/Nor_Cal_Test")

##############################################################################
### Run test GRTS designs for BLM lands in NorCal at two point densities
### Norcal area has 1,116,329 ha of BLM land
### at 1pt/10ha = 116,329 points
### at 1pt/100ha = 11,633 points
### Input is dissolved BLM lands polygon from a shapefile
### Output results as shapefiles
##############################################################################

attframe = read.dbf('NorCal_BLM_Dissolve.dbf')
design.100ha <- list(None=list(panel=c(Panel=11633),seltype="Equal",over=0))
test.sample.100ha <- grts(desig=design.100ha, DesignID="test100", src.frame="shapefile", in.shape="NorCal_BLM_Dissolve", att.frame=attframe, shapefile=TRUE, out.shape="test_sample_100ha", type.frame="area")

design.10ha <- list(None=list(panel=c(Panel=116329),seltype="Equal",over=0))
test.sample.10ha <- grts(desig=design.10ha, DesignID="test10", src.frame="shapefile", in.shape="NorCal_BLM_Dissolve", att.frame=attframe, shapefile=TRUE, out.shape="test_sample_10ha", type.frame="area")



# Test of tiling a GRTS design
### !!!! For the parallel processing version to work, working directory needs to be set to a local folder. Won't run over the network. !!! ###
tiles <- read.shape('NorCal_Tiles.shp')

pt_density <- 1/100 # 1 point per 100 ha

## Run the whole NorCal extent at once
start <- Sys.time()
area_m2 <- sum(tiles@data$area_mdm)
area_ha <- area_m2/10000
n <- area_ha * pt_density
design.tile <- list(None=list(panel=c(Panel=n),seltype="Equal",over=0))
singlerun.grts <- grts(design=design.tile, DesignID="test",src.frame="sp.object",sp.object=tiles,type.frame="area")
end <- Sys.time()
print(end-start)

## Now run it with tiling with the sequential processing
start <- Sys.time()
tiles.grts <- list()
for (i in 1:nrow(tiles)) {
  tile <- tiles[i,]
  area_m2 <- tile@data$area_mdm
  area_ha <- area_m2/10000
  n <- area_ha * pt_density
  design.tile <- list(None=list(panel=c(Panel=n),seltype="Equal",over=0))
  tiles.grts[i] <- grts(design=design.tile, DesignID="test",src.frame="sp.object",sp.object=tile,type.frame="area")
}
tiledrun.grts <- do.call(rbind,tiles.grts)  #this isn't a really efficient way to smash the results back together, but it will work for now...
end <- Sys.time()
print(end-start)

## Sequential processing using lapply instead of a for loop
start <- Sys.time()
#tiles.grts <- list()
wrapper = function(i) {
  tile <- tiles[i,]
  area_m2 <- tile@data$area_mdm
  area_ha <- area_m2/10000
  n <- area_ha * pt_density
  design.tile <- list(None=list(panel=c(Panel=n),seltype="Equal",over=0))
  return(grts(design=design.tile, DesignID="test",src.frame="sp.object",sp.object=tile,type.frame="area"))
}
tiles.gets <- lapply(1:nrow(tiles),wrapper)
tiledrun.grts <- do.call(rbind,tiles.grts)  #this isn't a really efficient way to smash the results back together, but it will work for now...
end <- Sys.time()
print(end-start)


## Now run tiled GRTS with parallel processing
start <- Sys.time()
t <- lapply(1:nrow(tiles),function(i){tiles[i,]}) # Convert the tiles into a list of individual tiles for parallel processing
sfInit(parallel=TRUE, cpus=2)
sfLibrary(spsurvey)
sfLibrary(sp)
sfExport('tiles')
sfExport('pt_density')

wrapper <- function(i) {
  tile <- tiles[i,]  # t[[i]]
  area_m2 <- tile@data$area_mdm
  area_ha <- area_m2/10000
  n <- area_ha * pt_density
  print(n)
  design.tile <- list(None=list(panel=c(Panel=n),seltype="Equal",over=0))
  return(grts(design=design.tile, DesignID="test",src.frame="sp.object",sp.object=tile,type.frame="area",shapefile=FALSE))
}
tiles.grts <- sfLapply(1:nrow(tiles),wrapper)
tiled.parallel.grts <- do.call(rbind,tiles.grts)  #this isn't a really efficient way to smash the results back together, but it will work for now...
end <- Sys.time()
print(end-start)
sfStop()

