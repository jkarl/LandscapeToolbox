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
project.shpfile <- "Ecosites_grouped_in_BFO_on_BLM"
mastersample.shpfile <- "/Users/jason/Google Drive/BLM_AIM/Master_Sample/35ha_GRTS_Draw/BLM_GRTS_Draw_35ha_ordered2.shp"
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


## Import the Bruneau eco site layer and intersect with the master sample points.
project.shape <- readOGR(paste(project.shpfile,"shp",sep="."),layer=project.shpfile,stringsAsFactors=F)
project.prj <- proj4string(project.shape)

## reproject the project area to match the master sample
proj.area <- spTransform(project.shape, master.prj)

## Intersect the master sample points with the ESD layer and join the attributes.
proj.ESD.df <- over(proj.points,proj.area)
proj.BpS_ESD.df <- cbind(proj.points,proj.ESD.df)

## At this point, it's easier to dump to CSV and summarize in Excel.
write.csv(proj.BpS_ESD.df,file="Bruneau_MasterSample_BpS-ESD.csv")

## Summarize the data by BPS Codes and add in the BPS names and BPS Group names
proj.df <- data.frame(proj.points)
byBPS <- group_by(proj.df,BPS_BPS_CODE)
cntBPS <- summarize(byBPS,count=n())
proj.bps <- inner_join(x=cntBPS,y=bps.codes,by=c("BPS_BPS_CODE"="BPS_CODE"))
write.csv(proj.bps,file="SNDOMasterSampleBPS.csv")
