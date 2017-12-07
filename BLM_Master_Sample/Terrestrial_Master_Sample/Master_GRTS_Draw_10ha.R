"""
File: Master_GRTS_Draw_10ha.R
Author: Jason Karl, USDA-ARS Jornada
Contact: jkarl@nmsu.edu; 575-646-1301
Date: 14 October 2015
Description: Selection of master sample locations at a density of 1 point per 35ha for
              all BLM lands in the 11 western states plus the Dakotas. Points were initially selected
			  at a density of 1 point per 10ha, and then resampled to be about 2 million points overall
			  for the terrestrial master sample due to processing limitations. This yielded a sample of
			  about 1 point per 35 ha. Generation of the points was done within 100kmx100km tiles that
			  were clipped to BLM lands, and the tiled points were merged prior to running a spatially-balanced
			  reordering of the points using GRTS.
Dependencies: 
    R packages:
        spsurvey - GRTS point selection
        rgdal - spatial data outputs
        snowfall - R parallel processing
    Inputs:
        100km_tiles_BLM_only.shp - 100km tiles restricted to BLM lands
"""

####################################################
### Load packages
####################################################
library(spsurvey)
library(snowfall)
library(rgdal)

####################################################
### Define the function that will run GRTS sample for each tile
####################################################
grtsWrapper <- function(i) {
  density = 10  # denominator of density 1 point per X hectare
  tile <- tiles[i,]
  print(paste("Processing tile ",i))
  area <- sum(sapply(slot(tile, "polygons"), slot, "area"))/10000  # Area of BLM lands in the tile in hectares
  sampSize <- ceiling(area / density)
  Equaldsgn <- list(None=list(panel=c(PanelOne=sampSize), seltype="Equal"))  # unweighted selection, no stratification
  Equalsites <- grts(design=Equaldsgn,
                     DesignID=paste("tile",i,sep=""),
                     type.frame="area",
                     src.frame="sp.object",
                     stratum=NULL,
                     sp.object=tile,
                     do.sample=TRUE,
                     startlev=11, maxlev=11,
                     shapefile=TRUE, ### Write out to shapefile in case process bombs.
                     out.shape=paste("grtsTile",i,sep=""))
  print(paste("Selected ",nrow(Equalsites)," points for tile ",i))
  return(Equalsites)
}

####################################################
### Define the working environment and load tiles
####################################################
setwd("/Users/jason/Documents/BLM_AIM/Master_Sample")
tiles <- read.shape('geodata/100km_tiles_BLM_only.shp')

####################################################
## Select sample locations per tile
####################################################
start <- Sys.time()
for (i in 1:nrow(tiles)) {
  tiles.grts <- grtsWrapper(i)
}
end <- Sys.time()
print(end-start)

####################################################
## Append all of the tiles together and write out
####################################################
grts.tile.shps <- list.files(".","*.shp")
grts.points <- read.shape(grts.tile.shps[1])
for (f in grts.tile.shps[-1]) {
  grts.points <- rbind(grts.points,read.shape(f))
}
proj4string(grts.points) <- CRS("+init=epsg:5070")
writeOGR(grts.points, "BLM_GRTS_Draw_10ha.shp", "BLM_GRTS_Draw_10ha", driver="ESRI Shapefile")

####################################################
### Select a subset to total 2 million points and write out
####################################################
prop <- 2000000/6988649 # Proportion of points in each tile in 10ha sample that will sum to 2 million
grts.tile.shps <- list.files(".","*.shp")
grts.points <- read.shape(grts.tile.shps[1])
sel.points <- ceiling(length(grts.points)* prop)
grts.subset <- grts.points[1:sel.points,]
for (f in grts.tile.shps[-1]) {
  paste("processing tile ",f)
  grts.points <- read.shape(f)
  sel.points <- ceiling(length(grts.points)*prop)
  grts.subset <- rbind(grts.subset,grts.points[1:sel.points,])
}
proj4string(grts.subset) <- CRS("+init=epsg:5070")
writeOGR(grts.subset, "BLM_GRTS_Draw_35ha.shp", "BLM_GRTS_Draw_35ha", driver="ESRI Shapefile")

####################################################
## Run the spatially-balanced ordering of the 2m subset
####################################################
start <- Sys.time()
att <- read.dbf("BLM_GRTS_Draw_35ha.dbf")
Equaldsgn <- list(None=list(panel=c(PanelOne=nrow(att)), seltype="Equal"))
Equalsites <- grts(design=Equaldsgn,
                   DesignID="35ha_Master",
                   type.frame="finite",
                   src.frame="shapefile",
                   in.shape="BLM_GRTS_Draw_35ha",
                   att.frame=att,
                   out.shape="BLM_GRTS_Draw_35ha.grts",
                   startlev=11, maxlev=11,
                   shift.grid=FALSE)
end <- Sys.time()
print(end-start)

