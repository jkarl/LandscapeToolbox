#################################################################################
#### VISUALIZING DISTRIBUTIONS AND PROPOSING BENCHMARKS FOR EXISTING DATA
#################################################################################

## So, we want to take TerrADat/AquADat and pull similar plots (probably by non-ecosite covariates)
## then look at the distribution of the indicator in question

## We'd like to have superimposed histograms of the reference distribution and the comparison plots

## Eventually we'll need to be able to filter down to only the "reference" data, however we ID those terrestrially.
## This is a piece of cake with aquatic data because there're two variables in the data frame for it

## Need to incorporate the Type I or Type II errors somewhere along the line?

## Once we get this Shinyed, we're going to need to consider input formats and selection interfaces

## STILL NEED TO IMPORT THE TABLE OF MODEL-DERIVED THRESHOLDS FOR AQUATIC FIGURES

require(ggplot2)
require(dplyr) ## dplyr is love. dplyr is life
require(tidyr)
# require(sp) ## Not required until spatial manipulations are done
# require(xlsx) ## Installed correctly but doesn't seem to be working right now on RStudio Server
# require(rgdal) ## This won't install on the server?

## Just because we need some kind of common projection to work with for mapping stuff. Might as well be the same as the terrestrial master sample?
# master.prj <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

## xlsx isn't working on the RStudio Server, so screw it.
# aquatic.reference.raw <- read.xlsx("/home/nstauffer/jornada_ftp/aquatic_reference_data.xlsx", sheetIndex = 1, rowIndex = 3, stringsAsFactors = F)
aquatic.reference.raw <- read.csv("aquatic_reference_data.csv", stringsAsFactors = F)

## Initialize a list for the fields to use to handle Shiny things
aquatic.fields <- list()
## Write in the metadata column names as a (human-friendly) named vector
aquatic.fields$metadata <- colnames(aquatic.reference.raw)[c(2:11, 20:23)] %>%
  setNames(c("Site.ID", "Duplicate.ID", "Revisit.Overlap.Status", "WSA.Site.ID", "Year", "Julian.Day", "Date.Collected", "Latitude", "Longitude", "State", "Site.CLS", "Location.Name", "Reference.Instream", "Reference.Riparian"))
## Write in the priority indicator column names as a named vector
aquatic.fields$indicators.priority <- colnames(aquatic.reference.raw)[c(33, 65, 34, 37, 38, 32, 63)] %>%
  setNames(c("Pct.Overhead.Cover", "Bank.Overhead.Cover", "Riparian.Veg.Complexity", "Instream.Habitat.Complexity", "Pct.Fines", "Floodplain.Connectivity", "Residual.Pool.Depth"))

## We need these long so that we can assign the type of indicator, either "instream" or "riparian"
## The slice done on aquatic.reference.raw is paring it down to just the important metadata and indicators (already sliced above for the column names above)
aquatic.long <- aquatic.reference.raw[,c(2:11, 20:23, 33, 65, 34, 37, 38, 32, 63)] %>% gather(., key = indicator, value = value, XCDENMID, XCDENBK, XCMG, XFC_NAT, PCT_SAFN, LINCIS_H, RP100)
aquatic.long$indicator.type[aquatic.long$indicator %in% c("XCDENMID", "XCDENBK", "XCMG")] <- "instream"
aquatic.long$indicator.type[aquatic.long$indicator %in% c("XFC_NAT", "PCT_SAFN", "LINCIS_H", "RP100")] <- "riparian"

## Need to add units here
aquatic.long$units[aquatic.long$indicator == aquatic.fields$indicators.priority["Pct.Overhead.Cover"]] <- "percent"
aquatic.long$units[aquatic.long$indicator == aquatic.fields$indicators.priority["Bank.Overhead.Cover"]] <- "percent"
aquatic.long$units[aquatic.long$indicator == aquatic.fields$indicators.priority["Riparian.Veg.Complexity"]] <- "" ## These indicators are unitless
aquatic.long$units[aquatic.long$indicator == aquatic.fields$indicators.priority["Instream.Habitat.Complexity"]] <- ""
aquatic.long$units[aquatic.long$indicator == aquatic.fields$indicators.priority["Pct.Fines"]] <- "percent"
aquatic.long$units[aquatic.long$indicator == aquatic.fields$indicators.priority["Floodplain.Connectivity"]] <- ""
aquatic.long$units[aquatic.long$indicator == aquatic.fields$indicators.priority["Residual.Pool.Depth"]] <- "cm"

## Creating separate data frames for the two classes of indicators to make figure-making work easier later
## The only non-reference sites are those with a value of "T" in the fields corresponding to the classification for in-stream and riparian
aquatic.reference.instream <- aquatic.long[aquatic.long$indicator.type == "instream" & aquatic.long$RST_FSED_AND_RMD_PHAB != "T",]
aquatic.reference.riparian <- aquatic.long[aquatic.long$indicator.type == "riparian" & aquatic.long$RST_FRIP_AND_RMD_PHAB != "T",]

## Plot a histogram
## One histogram of the reference distribution with vertical lines for the model-derived benchmarks (in the case of aquatics)
## Overlay the distribution on the selected sites later, but that implementation has yet to happen
ggplot(data = aquatic.reference.long, aes(x = value)) +
  geom_histogram(binwidth = 10) +
  xlim(0, 300) +
  ylim(0, 500) +
  # geom_vline(VERTICAL LINES FOR THE BENCHMARKS/THRESHOLDS) +
  facet_wrap(~ indicator)

## This is an attempt to generalize the principles so that it can auto-scale the bin widths relative to the range of values
# for (n in seq_along(aquatic.fields$indicators.priority)){
#   ggplot(data = na.exclude(aquatic.reference), aes(x = aquatic.fields$indicators.priority[n])) +
#     geom_histogram(binwidth = (max(aquatic.reference[, aquatic.fields$indicators.priority[n]]) - min(aquatic.reference[, aquatic.fields$indicators.priority[n]]))/20)
# }


# ## We want the column/variable names
# terradat.fields <- terradat %>% colnames() %>% list()
# 
# ## So, I think that I want two different lists of fields:
# ## The sort of metadata (e.g. project name, state, plotID) where it's meaningful to create a unique list of the values in the variable
# ## which means that it becomes a list of lists—a list of named lists containing the unique values for each metadata field
# ## The data that were measured on-the-ground and therefore will generally not be filtered by to narrow down the plots to use
# 
# ## Grabbing just the metadata variable names
# terradat.metadata.names <- terradat.fields[1:"whatever the index of the last metadata field is"]
# ## Creating a list of the lists of unique values for each metadata variable
# terradat.metadata.values <- lapply(terradat.metadata.names, FUN = unique(terradat[,x]))
# ## Renaming the lists for later use
# names(terradat.metadata.values) <- terradat.metadata.names
# 
# ## Grabbing the names of the variables for the indicator values.
# terradat.data.names <- terradat.fields["indices for the fields that contain the indicator values"]
# ## Eventually I want to set up human-friendly naming for presenting to the end user in a Shiny interface
# 
# ## Time to filter down by the metadata options! Just manually set these until there's a Shiny interface
# ## Just comment out the irrelevant ones
# working.df <- terradat %>% as.data.frame() %>% filter(
#   metadatafieldname1 == "whatever value",
#   # metadatafieldname2 == "whatever value",
#   # metadatafieldname3 == "whatever value",
#   # metadatafieldname4 == "whatever value",
#   metadatafieldname5 == "whatever value"
# )
# 
# working.df %>% ggplot(aes(x = "Indicator Field Name")) +
#   geom_histogram() +
#   xlabs("Indicator value") +
#   ylabs("Plot count") +
#   geom
# ggtitle("Distribution of indicator")
