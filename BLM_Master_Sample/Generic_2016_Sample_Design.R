## Sample Design for Generic AIM Project
## Date: 4/20/16
## Author: Nelson Stauffer
##________________________________________________________________________________
## 5-year sample design for the Generic BLM-AIM project.
## Strata: BpS, grouped
## Panels: 5 years
## Sites/panel: 50 points per panel, plus oversample
## Distribution of sample sites determined by proportional area of stratum in the project area
## Sample sizes and oversample by stratum and panel calculated and saved in stratum.info data frame
##________________________________________________________________________________
## This assumes that the workspace already contains the master sample as master.point.fc (a SpatialPointsDataFrame)
## and master.prj (a CRS for the master sample)

## Note: By default, the script only deals with the master sample and the fields available in it
## Code blocks for RASTERS and POLYGON SHAPEFILES can be found following an initial master sample subsetting.

##________________________________________________________________________________
### INITIAL SETUP
## Loading the always-required packages
library(spsurvey)
library(rgdal)
library(ggplot2)
library(ggmap)
library(dplyr)
library(rgeos)
library(broom)

## Set the global variables for the script. A few additional objects/values need to be set under their respective headings
## Just make sure that this if the folder containing the folders specified below
setwd("~")

## Note that the fllowing that the working directory will be inserted before the string, e.g. "C:/Users/username/Documents/" + outpath
## Project path is the folder that all the project-specific data are coming from, assuming there's no folder hierarchy within that location.
## If you have a hierarchical structure going, you're on your own to make this all work for you.
projectpath <- ("Projects/State_Name/Generic_Project")
## Where to write the output to. The script still assumes that this is nested under the working directory as defined above.
outpath <- ("Projects/Sample_Designs/Generic_Project")

## The filename of the point shapefile output. Typically "ProjectName_Dateofcompletion" for tracking purposes
design.output.filename <- "Generic_20160420"

## The name of the project that goes into the project name field in the final output
design.name <- "Generic"
## The seed number for the GRTS function. Typically YEARMONTHDAY of the final draw. Once design is final, DO NOT CHANGE THIS. YOU WILL RUIN EVERYTHING.
grtsseed <- 20160420
##________________________________________________________________________________


##________________________________________________________________________________
### GENERAL HELPFUL STARTING STUFF
## If you need a reference to explore to figure out specifically how you're going to subset the master sample, these
## lines may be helpful.

## dplyr can't wrangle SpatialPointsDataFrames so use this to make a data frame to root around in.
# df.master.point.fc <- master.point.fc %>% as.data.frame()

## These are all the "usual suspect" fields for stratification and subsetting. Weirdly written, but that's because it started
##life as something else. The LUP and ADMU fields are your friends, even if confusingly/cryptically named
# df.master.point.fc[,c(8:9,12:15,26:31,42:44)] %>% colnames() %>% unique()

## This one is great for figuring out what the state called something, e.g. Salt Lake Field Office being "Salt Lake " [sic]
# df.master.point.fc$ADMU_ADMU_NAME[grepl("district", df.master.point.fc$ADMU_ADMU_NAME, ignore.case = T)] %>% unique()
##________________________________________________________________________________


##________________________________________________________________________________
### SUBSETTING THE MASTER SAMPLE
## Extract the master sample points in the project area or, if you're going to restrict later by a raster or polygon shapefile
## just narrow it down as far as you can, e.g. to a state or field office. Note that sometimes issues can show up with improper
## attribution polygons provided by states, so those might show up when a project lead reviews the draft of a design.
proj.points <- subset(master.point.fc, LUPA_E_LUP_NAME=="Generic RMP")
##________________________________________________________________________________


##________________________________________________________________________________
### RASTERS
## If you need to extract from a raster, adapt and uncomment this section
## It assumes that this raster is in the projectpath folder.
# library(raster)
# rastername <- "rasterfilename"
# raster.project <- raster(paste(getwd(), projectpath, rastername, sep = "/"))
# proj.points$rastervalue <- extract(y = proj.points, x = raster.project)

## If this raster defines the sample frame, use the following line
# proj.points <- proj.points[!is.na(proj.points$rastervalue),]
##________________________________________________________________________________

##________________________________________________________________________________
### POLYGON SHAPEFILES
## If you need to extract from a polygon shapefile, adapt and uncomment this section
## It assumes that this shapefile is in the projectpath folder.
# shapefilename <- "polygonfilenamewithoutfileextension"
# polygons.project <- paste(getwd(), projectpath, shapefilename, sep = "/") %>% paste(., "shp", sep = ".") %>% readOGR(.,layer = shapefilename, stringsAsFactors = F) %>% spTransform(master.prj)

## Note that this line is if you haven't already restricted from the master sample! It's recommended that you at least narrow it down to state
# proj.points.polygon <- over(master.point.fc, polygons.project)
## If you've already restricted from the master sample and generated a proj.points, use this line
# proj.points.polygon <- over(proj.points, polygons.project)

## If this defines the strata, use the following line
# proj.points$stratum <- proj.points.polygon$APPROPRIATE_FIELD_NAME

## If this defines the sample frame, use the following line
# proj.points <- proj.points[!is.na(proj.points.polygon$APPROPRIATE_FIELD_NAME),]
##________________________________________________________________________________

##________________________________________________________________________________
### STRATUM ASSIGNMENT VIA LOOKUP TABLE
## Most projects are assigned strata based on grouping values in a field, usually BPS_GROUPNAME
## This can easily be adapted to play nice with a field brought in from a raster or polygon shapefile

## Defining the filename to look for and use
lutname <- "lutfilename.csv"

## Don't have your BPS_GROUPNAME lookup table defined yet? This will write out the .csv so you can populate the STRATUM field
## The APPROX.ACRES field takes the number of master sample points in that BpS group and multiplies it by 85 acres, the density of the master sample
# proj.points %>% as.data.frame() %>% group_by(BPS_GROUPNAME) %>% summarize(APPROX.ACRES = 85*n()) %>% mutate(STRATUM = "") %>%
#   write.csv(., paste(getwd(), projectpath, lutname, sep = "/"))

## This imports a .csv file from the folder projectpath that has a column called STRATUM and one called BPS_GROUPNAME
lut.strata <- read.csv(paste(getwd(), projectpath, lutname, sep = "/"), stringsAsFactors = F)

## This line is for if the lookup table is something like an exported attribute table from a shapefile and has multiple entries per BPS_GROUPNAME
lut.strata <- lut.strata[,c("BPS_GROUPNAME","STRATUM")] %>% unique()
for (n in 1:nrow(lut.strata)){
  proj.points$stratum[proj.points$BPS_GROUPNAME == lut.strata$BPS_GROUPNAME[n]] <- lut.strata$STRATUM[n]
}
##________________________________________________________________________________

##________________________________________________________________________________
### CHECKING STRATA AND PROPORTIONAL DISTRIBUTION OF POINTS
## Weird things can happen and sometimes points STILL don't have strata by this point. Use this to make sure everything's fine then get stratum.info
## to feed into the spreadsheet used to allocate points for the design object.
## If there are any NAs left in the stratum field by the end of this step, GRTS will fail to draw.

## This line will provide all the BpS group names that points still without a stratum fall in
proj.points$BPS_GROUPNAME[is.na(proj.points$stratum)] %>% unique()

## Because we generally trust the "Open Water" assignment, we'll kick it out of the sample frame entirely
#proj.points <- proj.points[proj.points$BPS_BPS_NAME != "Open Water",]

## In case of emergency, break hash. Seriously, though. Only use if the unassigned points should be excluded and you'll accept the sample frame consequences
#proj.points <- proj.points[!is.na(proj.points$stratum),]

## The following assumes identical panels, so the "proportion" column might be used as a starting point, but the base and oversample columns aren't useful
## Set the number of panels
panel_number <- 5
## Set the number of points for a panel. 50 is typical because it represents the average number of points one crew of two to three people can complete in a season
sample_size <- 50
## Set the minimum number of base points that a stratum can have. These are assigned and then remaining points in the sample are allocated proportionally. 3 is standard
min_points <- 3
## Set proportion of oversample to draw beyond the base, e.g. 20 base points with a proprtion of 0.25 will give you 5 oversample points. 0.25 is standard
oversample_proportion <- 0.25
## Set the minimum number of oversample points that a stratum can have. This will override the proportional oversample if that number is too low
min_oversample <- 10

##To get a data frame for use in calculating proportional point allocations
stratum.info <- as.data.frame(proj.points) %>% group_by(stratum) %>% summarize(count=n()) %>%
  mutate(approximate_acres = count * 85) %>%
  mutate(total_master_sample_points = sum(count)) %>%
  mutate(proportion = count/total_master_sample_points,
         single_panel_base = round(min_points + (sample_size - (min_points * length(unique(proj.points$stratum)))) * count / total_master_sample_points)) %>%
  mutate(single_panel_oversample = pmax(min_oversample, oversample_proportion * single_panel_base),
         total_base = panel_number * single_panel_base) %>%
  mutate(total_oversample = panel_number * single_panel_oversample)

stratum.info$final_single_panel_base <- stratum.info$single_panel_base

## Sometimes you want to rejigger the pointcounts a little bit from the proportional allocation. Do that here by uncommenting the line that lets you write a vector in
## Just make sure that your vector contains all the correct final intended base point counts for each panel in the correct order
# stratum.info$final_single_panel_base <- c()

View(stratum.info)
##________________________________________________________________________________

##________________________________________________________________________________
### PANEL DESIGN
## Because the master sample is drawn at a uniform density, the point counts from stratum.info can be as a good-enough stand in for area if Table 2 was never done.
## If you're going to have multiple panels with identical point allocations in each, use the following, which uses stratum.info from above

## Set your panel names here. Year1 through Year5 are the defaults, but the script will accept any number of them. Make sure that the number of panels matches panel_number above
panel_names <- c("Year1", "Year2", "Year3", "Year4", "Year5")

## Don't touch this! It's just initializing the PanelDesign object to write into
PanelDesign <- list()

## For each stratum in stratum.info, this sets up the correctly formatted list in PanelDesign so that grts() can use it
for (n in 1:nrow(stratum.info)){
  names <- c("panel", "seltype", "over")
  stratum_panel_points = c(rep(stratum.info$final_single_panel_base[n], length(panel_names))) %>% setNames(panel_names)
  # If you absolutely have to and know what you're doing, you can change "Equal" but seriously don't even look at it if you don't know why you'd do so.
  stratum_panel <- list(stratum_panel_points, "Equal", stratum.info$total_oversample[n]) %>% setNames(names)
  PanelDesign[[stratum.info$stratum[n]]] <- stratum_panel
}

## Manually defining how many points to put in each stratum in each panel. Oversample is defined per-stratum, not per-panel-per-stratum
## These are typically calculated in an external Excel spreadsheet that makes sure each stratum has a minimum number of points then assigns the remainder
## proportionally by area. Project leads may make some allocation adjustments after that as well.

## Use this if the points in a stratum will vary between panels
# PanelDesign <- list(
#   "Stratum1" = list(panel = c(Year1 = 10, Year2 = 10, Year3 = 10, Year4 = 10, Year5 = 10),
#                     seltype = "Equal",
#                     over = 5),
#   "Stratum2" = list(panel = c(Year1 = 10, Year2 = 10, Year3 = 10, Year4 = 10, Year5 = 10),
#                     seltype = "Equal",
#                     over = 5),
#   "Stratum3" = list(panel = c(Year1 = 10, Year2 = 10, Year3 = 10, Year4 = 10, Year5 = 10),
#                     seltype = "Equal",
#                     over = 5),
#   "Stratum4" = list(panel = c(Year1 = 10, Year2 = 10, Year3 = 10, Year4 = 10, Year5 = 10),
#                     seltype = "Equal",
#                     over = 5)
# )
##________________________________________________________________________________

##________________________________________________________________________________
### DRAWING AND WRITING RESULTS
## Do the point selections and output the result
set.seed(grtsseed)
sample.sites <- grts(design = PanelDesign,
                     DesignID = design.name,
                     type.frame = "finite",
                     src.frame="sp.object",
                     stratum = "stratum",
                     sp.object = proj.points,
                     att.frame = NULL,
                     shapefile = FALSE#, ### Don't write the shapefile b/c we want to reproject and write using rGDAL.
                     #prj="master",
                     #out.shape="ProjectPoints"
)

## Dropping the extra fields from the master sample, just keeping the ones specific to the draw plus the master sample ID
sample.sites <- sample.sites[,c(1:11)]


## Project the results to Geographic DD NAD83 and output as a shapefile
proj4string(sample.sites) <- master.prj # Assign projection info to the sample sites SPDF
geoDD.prj <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") # define output CRS
sample.sites.geodd <- spTransform(sample.sites,geoDD.prj) # reproject the sample sites
sample.sites.geodd$xcoord <- coordinates(sample.sites.geodd)[,1] # update the X and Y coordinate values to geoDD
sample.sites.geodd$ycoord <- coordinates(sample.sites.geodd)[,2]
writeOGR(sample.sites.geodd, paste(getwd(), outpath, sep = "/"), design.output.filename, driver="ESRI Shapefile", overwrite_layer=TRUE)

##________________________________________________________________________________
