########################################
#### LOADING PACKAGES ####
########################################
library(stringr) ## This package contains specialized functions for manipulating character strings
library(RODBC) ## This package contains functions for reading in databases
library(dplyr) ## This package contains really useful functions for manipulating data frames. It also provides pipes: %>%
library(tidyr) ## This package contains specialized functions for manipulating data frames to easily "tidy" data to format it to use more easily


########################################
#### CONFIGURATION ####
########################################

## A function that does all the summarizing, produces the data frame in R, and writes out the .csv top the file
mongolia.ingestion <- function(data.path, ## The filepath where all the data are stored. The script will look there to find all the other files referenced
                               dima.name, ## The filename of the DIMA being used. This needs to include the file extension, which should be .mdb
                               dima.year, ## The year that the DIMA represents
                               output.path, ## The filepath where the script will save its output
                               output.name, ## The filename to give to the file written at the end of the script
                               species.character, ## The data frame containing the species characteristics
                               ## The data frame containing the plot characteristics
                               plot.character) {
  ########################################
  #### READ IN DIMA DATA ####
  ########################################
  
  ## This part is to read data from the DIMA. The steps are:
  ## Open a channel to the DIMA
  ## Query data from it using SQL and store those data as data frames
  ## Close the channel 
  
  ## Unfortunately, there are two different functions to open a channel. One is for 64-bit R and one is for 32-bit R.
  ## Both will work, but instead of making the user figure out which is correct, the script will check and then switch to the correct one
  ## R.Version() returns a list where each value is a different piece of information about the R installation. The second value one indicates whether it's 64-bit or 32-bit
  if (R.Version()[2] == "x86_64") { ## R.Version()[2] is "x86_64" then it will use the function for 64-bit R to create the channel to the DIMA
    dima.channel <- odbcConnectAccess2007(paste(data.path, dima.name, sep = "/")) ## paste() is combining the filepath and DIMA filename, separated with /, to tell odbcConnectAccess2007() where the DIMA is specifically
  } else if (R.Version()[2] == "i386") { ## R.Version()[2] is "i386" then it will use the function for 32-bit R to create the channel to the DIMA
    dima.channel <- odbcConnectAccess(paste(data.path, dima.name, sep = "/")) ## paste() is combining the filepath and DIMA filename, separated with /, to tell odbcConnectAccess() where the DIMA is specifically
  }
  
  ## Storing the data from DIMA as data frames uses a SQL query to get the particular data we want from the DIMA tables
  ## This SQL query will produce columns for Site, Plot, Line, Point, all canopy layer species, Soil Surface, woody height, woody height species, herbaceous height, and herbaceous height species
  ## Each row will represent one LPI record
  query.lpi <- "SELECT joinSitePlotLine.SiteName, joinSitePlotLine.SiteID, joinSitePlotLine.PlotID, joinSitePlotLine.LineID, tblLPIDetail.PointLoc, tblLPIDetail.TopCanopy, tblLPIDetail.Lower1, tblLPIDetail.Lower2, tblLPIDetail.Lower3, tblLPIDetail.Lower4, tblLPIDetail.Lower5, tblLPIDetail.Lower6, tblLPIDetail.Lower7, tblLPIDetail.SoilSurface, tblLPIDetail.HeightWoody, tblLPIDetail.HeightHerbaceous, tblLPIDetail.SpeciesWoody, tblLPIDetail.SpeciesHerbaceous
FROM joinSitePlotLine INNER JOIN (tblLPIHeader LEFT JOIN tblLPIDetail ON tblLPIHeader.RecKey = tblLPIDetail.RecKey) ON joinSitePlotLine.LineKey = tblLPIHeader.LineKey;"
  query.biomass <- "SELECT joinSitePlotLine.SiteName, joinSitePlotLine.SiteID, joinSitePlotLine.PlotID, joinSitePlotLine.LineID, tblPlantProdDetail.TotalWtHectare
FROM joinSitePlotLine INNER JOIN (tblPlantProdHeader LEFT JOIN tblPlantProdDetail ON tblPlantProdHeader.RecKey = tblPlantProdDetail.RecKey) ON joinSitePlotLine.PlotKey = tblPlantProdHeader.PlotKey
WHERE (((joinSitePlotLine.LineID)='1'));"
  ## This form isn't used in the project, so we can't rely on the values to exist for every site
  # query.speciescount <- "SELECT joinSitePlotLine.SiteName, joinSitePlotLine.SiteID, joinSitePlotLine.PlotID, joinSitePlotLine.LineID, tblSpecRichDetail.SpeciesCount
  # FROM joinSitePlotLine INNER JOIN (tblSpecRichHeader LEFT JOIN tblSpecRichDetail ON tblSpecRichHeader.RecKey = tblSpecRichDetail.RecKey) ON joinSitePlotLine.LineKey = tblSpecRichHeader.LineKey
  # WHERE (((tblSpecRichDetail.SpeciesCount)<>0));"
  
  ## Create a new data frame called data.raw that contains the results of querying the DIMA
  data.lpi.raw <- sqlQuery(channel = dima.channel, ## Telling it what channel to use
                           query = query.lpi, ## What query
                           stringsAsFactors = F) ## Don't treat character strings as factors. Factors are useful for modeling functions, but not for what this script is doing
  data.biomass.raw <- sqlQuery(channel = dima.channel,
                               query = query.biomass,
                               stringsAsFactors = F)
  
  ## Close the channel to the DIMA
  odbcClose(channel = dima.channel)
  
  ## Converting all the site and plot names to all upper-case so that there aren't case mismatches later when the indicators are merged with the plot characteristics
  data.lpi.raw$PlotID <- data.lpi.raw$PlotID %>% str_to_upper()
  data.lpi.raw$SiteID <- data.lpi.raw$SiteID %>% str_to_upper()
  data.biomass.raw$PlotID <- data.biomass.raw$PlotID %>% str_to_upper()
  data.biomass.raw$SiteID <- data.biomass.raw$SiteID %>% str_to_upper()
  
  
  ########################################
  #### DATA MANIPULATION ####
  ########################################

  ## Height and species data should not be kept in the same data frame
  ## Creating a data frame that has all the rows, but only the specified columns
  data.raw.hits <- data.lpi.raw[, c("SiteName", "SiteID", "PlotID", "LineID", "PointLoc", "TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "Lower6", "Lower7", "SoilSurface")]
  data.raw.heights <- data.lpi.raw[, c("SiteName", "SiteID", "PlotID", "LineID", "PointLoc", "HeightWoody", "HeightHerbaceous", "SpeciesWoody", "SpeciesHerbaceous")]
  
  ## In order to work with the data, each record needs to be its own row, so for each LPI measurement there will be a separate row for the top hit, the second hit, the third hit, fourth hit, etc.
  ## Gather will take a wide data set and make it tall by moving each of the listed column names to its own row and creating a new column that contains the value that was in that column
  data.hits.tall <-  gather(data = data.raw.hits, ## Which data set to use
                            key = layer, ## Call the new column "layer" that will contain the name of the column a value originally came from
                            value = value, ## Call the new column "value" where the values from the original columns will be kept
                            TopCanopy, Lower1, Lower2, Lower3, Lower4, Lower5, Lower6, Lower7, SoilSurface ## All the columns to remove from the data and turn into rows
  )
  
  ## The additional species information in species.character can now be added
  data.hits.tall <- merge(x = data.hits.tall, ## Data source x
                          y = species.character, ## Data source y
                          by.x = "value", ## Name of the column to compare from x
                          by.y = "SppCode", ## Name of the column to compare from y
                          all.x = T ## Keep all the rows from data source x even if they don't match with a row from data source y
  )
  
  ## In order to calculate the indicators, the number of LPI records needs to be known because it can't be assumed to be 400. This data frame will be merged with others later
  hitcounts <- data.raw.hits %>% group_by(SiteID, PlotID) %>% summarize(record.count = n())
  
  ## summarize() from the package dplyr can be used to calculate indicators for the plots, but needs to be told which rows should be treated the same
  speciesgroup.data <- group_by(.data = data.hits.tall,
                                SiteID, PlotID, LineID, PointLoc, SpeciesGroupName) ## All rows with the same values in these columns will be treated as part of the same group
  
  ## To make sure that a species group is not counted more than once at a point (e.g. if two different species of sub-shrub were recorded in separate layers)
  speciesgroup.data <- summarize(.data = speciesgroup.data, ## Use the data frame that was just grouped by species group at each LPI pin drop
                                 first = first(value) ## The column called value contains the species code because of the gather() above. This will only take the first, "highest" record at a measurement point
  ) %>% merge(x = ., y = hitcounts, all.x = T)
  
  ## The data need to be grouped by plot to produce plot-level indicators
  speciesgroup.data <- group_by(.data = ungroup(speciesgroup.data), ## Use the ungrouped version of the data
                                SiteID, PlotID, SpeciesGroupName ## Group the data by species group and plot
  )
  
  speciesgroup.indicators <- summarize(.data = speciesgroup.data, ## Use the data frame that was just grouped by species group on plots
                                       pct.cover = 100*n()/first(record.count)) ## Calculate the percent cover by counting the number of number of rows in the group with n(), dividing by the number of LPI measurements on the plot, then multiplying by 100
  
  ## In places where there was no species group assigned, there were NA values. is.na() returns TRUE for all the rows that have NA, but applying ! flips TRUE to FALSE and FALSE to TRUE so !is.na() finds all the rows that are not NA
  speciesgroup.indicators <- speciesgroup.indicators[!is.na(speciesgroup.indicators$SpeciesGroupName),]
  
  ## Make this tall data frame wide with spread()
  speciesgroup.indicators.wide <- spread(data = speciesgroup.indicators, ## Data frame to make wide
                                         key = SpeciesGroupName, ## Column that contains the column names
                                         value = pct.cover, ## Column that contains the values
                                         fill = 0 ## Where there's an NA, fill it with 0
  )
  
  ## Same as above, but for the subcategories
  speciessubcategory.data <- data.hits.tall %>% group_by(SiteID, PlotID, LineID, PointLoc, Subcategory_LifeForm) %>%
    summarize(first = first(value)) %>% merge(x = ., y = hitcounts, all.x = T)
  speciessubcategory.indicators <- speciessubcategory.data %>% ungroup() %>% group_by(SiteID, PlotID, Subcategory_LifeForm) %>%
    summarize(pct.cover = 100*n()/first(record.count))
  speciessubcategory.indicators <- speciessubcategory.indicators[!is.na(speciessubcategory.indicators$Subcategory_LifeForm),]
  speciessubcategory.indicators.wide <- spread(data = speciessubcategory.indicators,
                                         key = Subcategory_LifeForm,
                                         value = pct.cover,
                                         fill = 0)
  
  plant.indicators <- merge(x = speciessubcategory.indicators.wide, y = speciesgroup.indicators.wide, by = c("SiteID", "PlotID"), all.x = T)
  
  ## Calculating the speciescounts
  data.speciescount <- data.hits.tall %>% group_by(SiteID, PlotID) %>% summarize(species.count = length(unique(ScientificName)))
  
  ## Getting bare soil
  ## This makes use of pipes to chain functions together. Eachpipe (%>%) takes the data or output from the function on the left and passes
  ## it to the function on the right. The function on the right will put that data/output into the first argument that it takes according
  ## to the help documentation OR anywhere that there is a . as a placeholder (e.g. you can see below where there's a merge(x = ., y = hitcounts))
  bare.soil.indicator <- data.raw.hits %>% 
    ## filter() will give only the rows that match all the conditions, which here narrow it down to only where there's uncovered bare soil
    filter(TopCanopy == "None" & 
             SoilSurface == "S" &
             !(Lower1 %in% c("L", "WL", "VL")) & ## Returns TRUE for every row where you can't find the value in Lower1 in the vector of "L", "WL", and "VL"
             !(Lower2 %in% c("L", "WL", "VL")) &
             !(Lower3 %in% c("L", "WL", "VL")) &
             !(Lower4 %in% c("L", "WL", "VL")) &
             !(Lower5 %in% c("L", "WL", "VL")) &
             !(Lower6 %in% c("L", "WL", "VL")) &
             !(Lower7 %in% c("L", "WL", "VL"))
    ) %>% merge(x = ., y = hitcounts, all.x = T) %>% ## Merging to add in the information about how many sampling pin drops were done on the plot
    group_by(SiteID, PlotID) %>% ## This groups data.raw.hits by the three named columns
    summarize(pct.bare.soil = 100*n()/first(record.count)) ## Summarizing with the percent bare soil as a column/field
  
  ## Foliar cover
  foliar.cover.indicator <- data.raw.hits %>%
    filter(!is.na(TopCanopy) & TopCanopy != "None") %>% merge(x = ., y = hitcounts, all.x = T) %>%
    group_by(SiteID, PlotID) %>%
    summarize(pct.foliar.cover = 100*n()/first(record.count))
  
  ## Combine all the computed indicators
  ## This chains the merges using pipes. The first merge() is the plot.character data frame and the species group indicators, keeping
  ## all the rows from the plot characteristic data frame regardless of if there were indicators for them.
  data.combined <- merge(x = plot.character, y = plant.indicators, by = c("SiteID", "PlotID"), all.x = T) %>%
    # merge(x = plot.character, y = speciesgroup.indicators.wide, by = c("SiteID", "PlotID"), all.x = T) %>%
    ## Then the result of that merge is merged with the foliar cover indicator data frame, still keeping all the rows, and passed to the next merge()
    merge(x = ., y = foliar.cover.indicator, by = c("SiteID", "PlotID"), all.x = T) %>%
    merge(x = ., y = bare.soil.indicator, by = c("SiteID", "PlotID"), all.x = T) %>%
    merge(x = ., y = data.speciescount, by = c("SiteID", "PlotID"), all.x = T) %>%
    merge(x = ., y = data.biomass.raw, by = c("SiteID", "PlotID"), all.x = T)
  
  ## Manually removing some columns that we don't want/need. When duplicate columns occur in merge(),
  ## it adds .x or .y to the duplicate to indicate which of the data frame sources that duplicate came from
  ## grepl() returns TRUE for each value in the vector that contains the pattern, so this searches for all the column names that contain
  ## ".x" or ".y" and keeps the ones where it didn't find them
  data.combined <- data.combined[,!(grepl(pattern = ".x", x = names(data.combined)) | grepl(pattern = ".y", x = names(data.combined)))]
  
  ## Add in the value from dima.year
  data.combined$Year <- dima.year
  
  ## Write out the results
  write.csv(data.combined, paste(output.path, output.name, sep = "/"))
  
  return(data.combined)
}

########################################
#### VARIABLES ####
########################################

## Read in the species information from a .csv using read.csv()
species.character <- read.csv(file = paste(data.path, "NAMEM_CODEX_241115.csv", sep = "/"), ## paste() is combining the filepath and filename, separated with /
                              stringsAsFactors = F)

## Read in the plot information
plot.character <- read.csv(file = paste(data.path, "plot_character_031126.csv", sep = "/"), ## paste() is combining the filepath and filename, separated with /
                           stringsAsFactors = F) %>% distinct()
## Create SiteID and PlotID columns from the Site/Plot column and convert them to all upper case to prevent problems with case-sensitive merge() later
for (n in 1:nrow(plot.character)) {
  plot.character$SiteID[n] <- str_split(plot.character$Site.Plot, " / ")[[n]][1] %>% str_to_upper()
  plot.character$PlotID[n] <- str_split(plot.character$Site.Plot, " / ")[[n]][2] %>% str_to_upper()
}

data.path <- "C:/Users/nstauffe/Documents/Projects/Mongolia/data"
output.path <- "C:/Users/nstauffe/Documents/Projects/Mongolia"

dima.name.2011 <- "DIMA 4.1a as of All aimag_2011.mdb"
dima.name.2012 <- "DIMA 4.1a as of All aimag_2012.mdb"
dima.name.2013 <- "DIMA 4.1a as of All aimag_2013.mdb"
dima.name.2014 <- "DIMA 4.1a as of All aimag_2014.mdb"
dima.name.2015 <- "DIMA 4.1a as of All aimag_2015.mdb"

output.name.2011 <- "2011_summary.csv"
output.name.2012 <- "2012_summary.csv"
output.name.2013 <- "2013_summary.csv"
output.name.2014 <- "2014_summary.csv"
output.name.2015 <- "2015_summary.csv"

data.2011 <- mongolia.ingestion(data.path, dima.name.2011, dima.year = 2011, output.path, output.name.2011, species.character, plot.character)
data.2012 <- mongolia.ingestion(data.path, dima.name.2012, dima.year = 2012, output.path, output.name.2012, species.character, plot.character)
data.2013 <- mongolia.ingestion(data.path, dima.name.2013, dima.year = 2013, output.path, output.name.2013, species.character, plot.character)
data.2014 <- mongolia.ingestion(data.path, dima.name.2014, dima.year = 2014, output.path, output.name.2014, species.character, plot.character)
data.2015 <- mongolia.ingestion(data.path, dima.name.2015, dima.year = 2015, output.path, output.name.2015, species.character, plot.character)

data.combined <- rbind(data.2011, data.2012, data.2013, data.2014, data.2015)
########################################
#### FIGURE GENERATION ####
########################################
library(ggplot)

## TODO: Boxplots by functional group
## TODO: Line graph of functional groups through the years (maybe facet by the functional group?)
ggplot()


## For later:
## TODO: Variability of functional groups through years
## TODO: PCO plant community composition analysis