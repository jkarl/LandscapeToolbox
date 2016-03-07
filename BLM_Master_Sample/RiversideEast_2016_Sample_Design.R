## Sample Design for Riverside East AIM Project
## Date: 2/16/2016
## Author: Jason Karl
##________________________________________________________________________________
## 5-year sample design for the Riverside East BLM-AIM project.
## Strata: BPS GROUPVEG
## Panels: 5 years
## Sites/panel: target of 100
## Distribution of sample sites determined by area of BpS in the project area. 
## Sample sizes and oversample by stratum and panel calculated outsite R in an Excel spreadsheet
##________________________________________________________________________________

library(spsurvey)
library(rgdal)
library(ggplot2)
library(ggmap)
library(dplyr)
library(rgeos)
library(broom)

setwd("~/Google Drive/BLM_AIM/Master_Sample/Riverside_East")

## Set the environment parameters
project.shpfile <- "Riverside_ProjectArea_2016"
mastersample.shpfile <- "/Users/jason/Google Drive/BLM_AIM/Master_Sample/35ha_GRTS_Draw/BLM_GRTS_Draw_35ha_ordered2.shp"
datapath <- ("//Users/jason/Google Drive/BLM_AIM/Master_Sample")
outpath <- ("//Users/jason/Google Drive/BLM_AIM/Master_Sample/Riverside_East")
mastersample.gdd <- "MasterSamplePoints2015.gdb"

##________________________________________________________________________________
## Option to load the master sample from the file geodatabase (SLOW) and recalc
## the master BPS list, or to load both from an .RData file
##________________________________________________________________________________
## Option 1. Load master sample from file GDB
## Load the BPS codes and group by BPS code
#bps.codes <- read.csv(paste(datapath,"BPS.csv",sep="/"),header=T,stringsAsFactors=FALSE)
#bps.codes$BPS_CODE <- as.character(bps.codes$BPS_CODE)
#bps.codes <- group_by(bps.codes,BPS_CODE)
#bps.codes <- summarize(bps.codes,BPS_NAME=min(BPS_NAME),GROUPNAME=min(GROUPNAME),GROUPVEG=min(GROUPVEG))
#bps.codes[bps.codes=="Barren-Rock/Sand/Clay"]<-"Barren"  ## Recode long name to just 'barren'

## Load master sample
#master.gdb <- paste(datapath,mastersample.gdd,sep="/")
#ogrListLayers(master.gdb)
#master.point.fc <- readOGR(dsn=master.gdb, layer="BLM_Terrestrial_MS_Points_att_update",stringsAsFactors=F)
#master.prj <- proj4string(master.point.fc)
##________________________________________________________________________________
## Option 2. Load master sample and bps codes from .RData
load(paste(datapath,"TerrestrialMasterSample2015.RData",sep="/"),verbose=T)
##________________________________________________________________________________


## Load the project area shapefile
project.shape <- readOGR(paste(project.shpfile,"shp",sep="."),layer=project.shpfile)
project.prj <- proj4string(project.shape)

## extract the master sample points in the project area
## reproject the project area to match the master sample
proj.area <- spTransform(project.shape, master.prj)
riverside.points <- master.point.fc[proj.area,]  # subset the master sample by the project area boundary


## Summarize the data by BPS Codes and add in the BPS names and BPS Group names
riverside.df <- data.frame(riverside.points)
byBPS <- group_by(riverside.df,BPS_BPS_CODE)
cntBPS <- summarize(byBPS,count=n())
riverside.bps <- inner_join(x=cntBPS,y=bps.codes,by=c("BPS_BPS_CODE"="BPS_CODE"))
write.csv(riverside.bps,file="RiversideEastMasterSampleBPS.csv")

## Distribution of points to the strata are assigned outside of R via Excel.
## Need to add the BPS group and group veg names to the points for the stratification
riverside.points <- merge(riverside.points,riverside.bps,by="BPS_BPS_CODE")
riverside.df <- merge(riverside.df,riverside.bps,by="BPS_BPS_CODE")

## Establish the design list
PanelDesign <- list(
  "Shrubland"=list(panel=c(Year1=59, Year2=59, Year3=59, Year4=59, Year5=59), 
    seltype="Equal",
    over=100),
  "Barren"=list(panel=c(Year1=10, Year2=10, Year3=10, Year4=10, Year5=10), 
    seltype="Equal",
    over=50),  
  "Riparian"=list(panel=c(Year1=15, Year2=15, Year3=15, Year4=15, Year5=15), 
    seltype="Equal",
    over=50),
  "Sparse"=list(panel=c(Year1=16, Year2=16, Year3=16, Year4=16, Year5=16), 
    seltype="Equal",
    over=50)
  )



## Do the point selections and output the result
set.seed(20160217)
sample.sites <- grts(design=PanelDesign,
                   DesignID="RiversideEast",
                   type.frame="finite",
                   src.frame="sp.object",
                   stratum="GROUPVEG",
                   sp.object=riverside.points,
                   att.frame=NULL,
                   shapefile=FALSE, ### Don't write the shapefile b/c we want to reproject and write using rGDAL.
                   #prj="master",
                   #out.shape="RiversideEastDesignPoints"
)
sample.sites <- sample.sites[,c(1:9,11)] # Drop the extra fields from the master sample and just keep the ones specific to the draw


## Project the results to Geographic DD NAD83 and output as a shapefile
proj4string(sample.sites) <- master.prj # Assign projection info to the sample sites SPDF
geoDD.prj <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") # define output CRS
sample.sites.geodd <- spTransform(sample.sites,geoDD.prj) # reproject the sample sites
sample.sites.geodd$xcoord <- coordinates(sample.sites.geodd)[,1] # update the X and Y coordinate values to geoDD
sample.sites.geodd$ycoord <- coordinates(sample.sites.geodd)[,2]
writeOGR(sample.sites.geodd, outpath, "RiversideEastSampleDesign",driver="ESRI Shapefile", overwrite_layer=TRUE)


## Create a map of the points
proj.area.geoDD <- spTransform(proj.area,geoDD.prj)
proj.area.dissolve <- tidy(gUnaryUnion(proj.area.geoDD))
map.points<-sample.sites.geodd[sample.sites.geodd$panel!="OverSamp",]
map <- get_map(location=rowMeans(bbox(sample.sites.geodd)),zoom=9)
ggmap(map) + 
  geom_polygon(data=proj.area.dissolve,aes(x=long,y=lat, group=id),color="black",size=0,alpha=0.6) +
  geom_point(data=as.data.frame(map.points), aes(x=xcoord,y=ycoord,colour=stratum)) +
  scale_color_brewer(palette="Accent") + 
  facet_wrap(~panel) +
  labs(title="Riverside East Rotating Panel Sample Design",x="",y="") + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position = c(1, 0), legend.justification = c(1, 0))

