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

setwd("~/Google Drive/BLM_AIM/Master_Sample/Bruneau")

## Set the environment parameters
strata.shpfile <- "ESD_Group20150417"
datapath <- ("//Users/jason/Google Drive/BLM_AIM/Master_Sample")
outpath <- ("//Users/jason/Google Drive/BLM_AIM/Master_Sample/Bruneau")
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
#project.shape <- readOGR(paste(project.shpfile,"shp",sep="."),layer=project.shpfile)
#project.prj <- proj4string(project.shape)

## extract the master sample points in the project area
proj.points <- subset(master.point.fc, ADMU_ADMU_NAME=="Bruneau Field Office")

## Import the strata and intersect/join with the master sample points
strata.shape <- readOGR(paste(strata.shpfile,"shp",sep="."),layer=strata.shpfile,stringsAsFactors=F)
strata.prj <- proj4string(strata.shape) ## Get the projection info for the imported strata
strata <- spTransform(strata.shape, master.prj)  ## reproject the strata to match the master sample
proj.ESD.df <- over(proj.points,strata) ## Intersect the master sample points with the ESD layer and join the attributes.
proj.ESD.df[proj.ESD.df=="Unknown & Aspen"]<-"Unknown and Aspen"
proj_w_ESD <- cbind(proj.points,proj.ESD.df) ## Join the intersected ESD Groups attributes into the master sample points
proj_w_ESD <- proj_w_ESD[!is.na(proj_w_ESD$ESD_Group),] ## Strip out rows without ESD group
coordinates(proj_w_ESD) <- ~xcoord+ycoord ## Rebuild the spatial points data frame
proj4string(proj_w_ESD)<-master.prj

## Distribution of points to the strata are assigned outside of R via Excel.
## Establish the design list
## Strata values
as.data.frame(proj_w_ESD) %>% group_by(ESD_Group) %>% summarize(count=n())
'''
Source: local data frame [10 x 2]

                    ESD_Group count
                       (fctr) (int)
1  Basin Sagebrush Cool Moist   214
2    Big Sagebrush Cool Moist  1758
3      Big Sagebrush Warm Dry  4696
4             Black Sagebrush   501
5    Low Sagebrush Cool Moist  5861
6      Low Sagebrush Warm Dry   288
7                  Meadow Dry    17
8             Salt Desert Mix  2484
9             Unknown & Aspen  1043
'''

PanelDesign <- list(
  "Low Sagebrush Cool Moist"=list(panel=c(Year1=12, Year2=12, Year3=12, Year4=12, Year5=12), 
    seltype="Equal",
    over=20),
  "Salt Desert Mix"=list(panel=c(Year1=7, Year2=7, Year3=7, Year4=7, Year5=7), 
    seltype="Equal",
    over=20),
  "Big Sagebrush Warm Dry"=list(panel=c(Year1=11, Year2=11, Year3=11, Year4=11, Year5=11), 
    seltype="Equal",
    over=20),
  "Big Sagebrush Cool Moist"=list(panel=c(Year1=3, Year2=3, Year3=3, Year4=3, Year5=3), 
    seltype="Equal",
    over=20),
  "Basin Sagebrush Cool Moist"=list(panel=c(Year1=5, Year2=5, Year3=5, Year4=5, Year5=5), 
    seltype="Equal",
    over=20),  
  "Unknown and Aspen"=list(panel=c(Year1=3, Year2=3, Year3=3, Year4=3, Year5=3), 
    seltype="Equal",
    over=20),
  "Black Sagebrush"=list(panel=c(Year1=3, Year2=3, Year3=3, Year4=3, Year5=3), 
    seltype="Equal",
    over=20),
  "Low Sagebrush Warm Dry"=list(panel=c(Year1=3, Year2=3, Year3=3, Year4=3, Year5=3), 
    seltype="Equal",
    over=20),
  "Meadow Dry"=list(panel=c(Year1=3, Year2=3, Year3=3, Year4=3, Year5=3), 
    seltype="Equal",
    over=2)
  )

## Do the point selections and output the result
set.seed(20160308)
sample.sites <- grts(design=PanelDesign,
                   DesignID="Bruneau",
                   type.frame="finite",
                   src.frame="sp.object",
                   stratum="ESD_Group",
                   sp.object=proj_w_ESD,
                   att.frame=NULL,
                   shapefile=FALSE, ### Don't write the shapefile b/c we want to reproject and write using rGDAL.
                   #prj="master",
                   #out.shape="ProjectPoints"
)
sample.sites <- sample.sites[,c(1:9,11)] # Drop the extra fields from the master sample and just keep the ones specific to the draw


## Project the results to Geographic DD NAD83 and output as a shapefile
proj4string(sample.sites) <- master.prj # Assign projection info to the sample sites SPDF
geoDD.prj <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") # define output CRS
sample.sites.geodd <- spTransform(sample.sites,geoDD.prj) # reproject the sample sites
sample.sites.geodd$xcoord <- coordinates(sample.sites.geodd)[,1] # update the X and Y coordinate values to geoDD
sample.sites.geodd$ycoord <- coordinates(sample.sites.geodd)[,2]
writeOGR(sample.sites.geodd, outpath, "BruneauSampleDesign",driver="ESRI Shapefile", overwrite_layer=TRUE)


## Create a map of the points
#proj.area.geoDD <- spTransform(proj.area,geoDD.prj)
#proj.area.dissolve <- tidy(gUnaryUnion(proj.area.geoDD))
map.points<-sample.sites.geodd[sample.sites.geodd$panel!="OverSamp",]
map <- get_map(location=rowMeans(bbox(sample.sites.geodd)),zoom=8)
ggmap(map) + 
  ##geom_polygon(data=proj.area.dissolve,aes(x=long,y=lat, group=id),color="black",size=0,alpha=0.6) +
  geom_point(data=as.data.frame(map.points), aes(x=xcoord,y=ycoord,colour=stratum)) +
  scale_color_brewer(palette="Accent") + 
  facet_wrap(~panel) +
  labs(title="Bruneau Rotating Panel Sample Design",x="",y="") + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position = c(1, 0), legend.justification = c(1, 0))

