#################################################################################
### CALIBRATION CHECKING SCRIPT
### The goal here is to take a DIMA that contains calibration data, grab those data, and serve up information on how the observers compare on the major calibration indicators
### A lot of things are defined as values before they're called so that this can be altered as needed and to add hooks for a Shiny implementation
## Reporting out on the following indicators
## LPI : Foliar Cover, Bare Soil, Litter, Basal Cover, Rock Fragments, Vegetation Heights (by woody/herbaceous and height classes)
## Gap Intercept : Gap counts and proprtion of plot in gaps (by size classes)
#################################################################################

#################################################################################
### BASIC CONFIGURATION #########################################################
#################################################################################
## Getting the packages
require(RODBC)
require(dplyr)

# Filepath to find the DIMA
path.read <- "C:/Users/username/Documents/Projects/"
# DIMA filename
name.dima <- "training__calibration_DIMA_4.1.mdb"
# Filepath to write out the .csv of calibration results
path.write <- "C:/Users/username/Documents/Projects/"
# Filename for the .csv that you want to write out the results in
filename.output <- "calibration_results.csv"
#################################################################################

#################################################################################
### SETTING CALIBRATION TOLERANCES ##############################################
#################################################################################
## In the end, we need everyone to be within tolerances, which is ±5% from the mean for anything reported in percentages and ±2 on the species counts
## I'm still defining them as values in a list in case we want to let people set different tolerances eventually

## Initialize the tolerances list
tolerances <- list()

## LPI tolerances
# These four are the tolerance for (maximum observed percent on plot - minimum observed percent on plot)
tolerances$lpi$foliar.percent.range <- 10
tolerances$lpi$baresoil.percent.range <- 10
tolerances$lpi$basalcover.percent.range <- 10
tolerances$lpi$rockfragments.percent.range <- 10
# These four are maximum tolerance in abs(individual's observed percentage - mean plot percentage)
tolerances$lpi$foliar.percent <- tolerances$lpi$foliar.percent.range/2
tolerances$lpi$baresoil.percent <- tolerances$lpi$baresoil.percent.range/2
tolerances$lpi$basalcover.percent <- tolerances$lpi$basalcover.percent.range/2
tolerances$lpi$rockfragments.percent <- tolerances$lpi$rockfragments.percent.range/2
# Tolerance for (maximum observed count within a height class - minimum observed count within a height class)
tolerances$lpi$heights.count.range <- 4
# Tolerance for abs(individual's observed count within a height class - mean observed count on plot within a height class)
tolerances$lpi$heights.count <- tolerances$lpi$heights.count.range/2
# The breaking points in cm for the height classes. The upper bound is inclusive on each, e.g. <=50, >50 & <=200, >200
tolerances$lpi$heights.breaks <- c(50, 200, 500)



## Gap tolerances
tolerances$gaps$gap.pct.range <- 10
tolerances$gaps$gap.percent <- tolerances$gaps$gap.pct.range/2
## The breaking points in cm for the gap classes. For the first gap class, the check is inclusive, e.g. >= 25 & <=50
## For other gaps, the check is inclusive on the high end, e.g. >50 & <=100
## For the largest gap class, the upper limit is unbound, e.g. >200
tolerances$gaps$gap.breaks <- c(25, 50, 100, 200)
# gap.count is currently unused
tolerances$gaps$gap.count <- 2
#################################################################################


#################################################################################
### PULLING THE DATA FROM A DATABASE ############################################
#################################################################################
## Specify the DIMA filepath
dima.location <- paste(path.read, name.dima, sep = "/")

## Initialize our queries list
queries <- list()

## SQL query for getting a table of all LPI hits by layer with recorder, observer,, heights and species for heights by woody and herbaceous, point location on line,
## point number on line, line, plot, site, and date
queries$lpi <- "SELECT joinSitePlotLine.SiteID, joinSitePlotLine.PlotID, joinSitePlotLine.PlotKey, joinSitePlotLine.LineID, tblLPIHeader.FormDate, tblLPIHeader.Observer, tblLPIHeader.Recorder, tblLPIDetail.PointLoc, tblLPIDetail.PointNbr, tblLPIDetail.TopCanopy, tblLPIDetail.Lower1, tblLPIDetail.Lower2, tblLPIDetail.Lower3, tblLPIDetail.Lower4, tblLPIDetail.SoilSurface, tblLPIDetail.HeightWoody, tblLPIDetail.SpeciesWoody, tblLPIDetail.HeightHerbaceous, tblLPIDetail.SpeciesHerbaceous
FROM joinSitePlotLine INNER JOIN (tblLPIHeader LEFT JOIN tblLPIDetail ON tblLPIHeader.RecKey = tblLPIDetail.RecKey) ON joinSitePlotLine.LineKey = tblLPIHeader.LineKey;"

## SQL query for getting a table of gaps with observer, recorder, line, plot,site, and date
queries$gaps <- "SELECT joinSitePlotLine.SiteID, joinSitePlotLine.PlotID, joinSitePlotLine.PlotKey, joinSitePlotLine.LineID, tblGapHeader.FormDate, tblGapHeader.Observer, tblGapHeader.Recorder, tblGapDetail.Gap, tblGapHeader.LineLengthAmount
FROM joinSitePlotLine INNER JOIN (tblGapHeader INNER JOIN tblGapDetail ON tblGapHeader.RecKey = tblGapDetail.RecKey) ON joinSitePlotLine.LineKey = tblGapHeader.LineKey;"


## Let's get some tables extracted from the specified DIMA!
## Initialize the lists to keep all our data frames in
lpi <- list()
gaps <- list()

## Note that I'm using the function odbcConnectAccess2007() because I have 64-bit R and 64-bit Access installed. If your Access install is 32-bit, use odbcConnectAccess() in 32-bit R
## I also never trust factored fields to work with functions that I want to use, so I avoid them in the first place
## This is just connecting to the database and running the SQL queries then storing the results
lpi$raw <- odbcConnectAccess2007(dima.location) %>% sqlQuery(., queries$lpi, stringsAsFactors = F)
gaps$raw <- odbcConnectAccess2007(dima.location) %>% sqlQuery(., queries$gaps, stringsAsFactors = F)
odbcCloseAll()

## The line length is in meters, but we need cm, so we'll quickly do that
gaps$raw$LineLengthAmount <- gaps$raw$LineLengthAmount*100

## We also want the plot keys to be strings, not numeric values
lpi$raw$PlotKey <- as.character(lpi$raw$PlotKey)
gaps$raw$PlotKey <- as.character(gaps$raw$PlotKey)

## We'll set up some objects we can use to populate options in the Shiny tool that maybe will one day be built
sites.plots <- rbind(gaps$raw[,c("SiteID", "PlotID", "PlotKey")], lpi$raw[,c("SiteID", "PlotID", "PlotKey")]) %>% unique()
observers.all <- c(gaps$raw$Observer, lpi$raw$Observer) %>% unique()

## If you want to see your options for the calibration PlotKey, Plot-, and SiteID, use this
# sites.plots %>% View()

## Where's the calibration data? Specify the SiteID and the PlotID, although all we really need is the key
## This was originally—naively—written to use a combination of the Site- and PlotIDs on a one-at-a-time basis
## We've moved onto a loop that'll look at all the plots in a database using the plotkeys that were found
calibration.SiteID <- "Canyonlands Calibration"
calibration.PlotID <- "Calibration: NWDO"
## Humans struggle with reliably typing out a plot key, so let's just extract it based on the friendlier Site- and PlotIDs
calibration.PlotKey <- sites.plots$PlotKey[sites.plots$SiteID == calibration.SiteID & sites.plots$PlotID == calibration.PlotID]
#################################################################################


#################################################################################
### KICKING OFF THE LOOP THAT'LL ITERATE THROUGH ALL PLOTS ######################
#################################################################################
## For general purposes, we need to make a final data table that has calibration information for every plot
## in the database instead of just one at a time. So, we'll loop through each of the plot keys in turn and
## mash together the results from the data associated to each key.
## Obviously, the conclusion to this needs to come at the end of all the calibration work, so if you comment this
## out make sure you also comment out that section

## This is predicated on the assumption that the database contains only calibration plots. If there's just one calibration
## plot, just make sure it's set up above. If you have multiple calibration plots, let this do its thing and then
## just filter/subset at the end of it all. Your life will be much better for it.

for (i in seq_along(sites.plots$PlotKey)){
  calibration.PlotKey <- sites.plots$PlotKey[i]
  
## I'm aware that this is slow and inelegant, but the script was written assuming a single plot and this is much easier
## than working to rewrite to do calculations without looping and it's not that much data, really
#################################################################################
  
  
  
#################################################################################
### CALIBRATION CHECKING FOR LPI ################################################
#################################################################################
## The indicators being evaluated are Foliar Cover, Bare Soil, Litter, Basal Cover, Rock Fragments, Vegetation Heights (by woody/herbaceous and height classes)
## This is currently set up so that it'll work regardless of how many lines were read

## Create a data frame of just the LPI data from the calibration plot so we can get to work looking at it
## This first line is from the dark times when we didn't use plot keys
# lpi$calibration.raw <- lpi$raw %>% subset(SiteID == calibration.SiteID) %>% subset(PlotID == calibration.PlotID)
lpi$calibration.raw <- lpi$raw %>% subset(PlotKey == calibration.PlotKey)
## Add in some extra variables so we can calculate the indicators relatively painlessly. Normally I wouldn't do this, but summarize() is fighting me and this should make it possible
## First up is to add a 1 to all observations where the top canopy hit isn't a "None" so that we can find the sum to know how many foliar hits there were
lpi$calibration.raw$foliar.cover[lpi$calibration.raw$TopCanopy != "None"] <- 1
## Likewise, do the same sort of thing to all the points where there's a species code at the soil surface, which here is just anywhere where a standard non-vegetative code was not found
surface.codes <- c("S", "LC", "M", "D", "W", "CY", "EL", "R", "GR", "CB", "ST", "BY", "BR")
lpi$calibration.raw$basal.cover[!(lpi$calibration.raw$SoilSurface %in% surface.codes)] <- 1
## And a variable for if the last hit was a rock of some sort that wasn't bedrock because that's not a "rock fragment"
lpi$calibration.raw$rock.fragments[lpi$calibration.raw$SoilSurface %in% surface.codes[8:12]] <- 1
## One for bare ground. Assume it's true and then invalidate it wherever the surface code isn't S or CY, the top code isn't None, or there's anything in Lower1:Lower4. Clunky, but effective
lpi$calibration.raw$bare.soil <- 1
lpi$calibration.raw$bare.soil[!(lpi$calibration.raw$SoilSurface %in% c("S", "CY"))] <- 0
lpi$calibration.raw$bare.soil[lpi$calibration.raw$TopCanopy != "None"] <- 0
lpi$calibration.raw$bare.soil[lpi$calibration.raw$Lower1 != ""] <- 0
lpi$calibration.raw$bare.soil[lpi$calibration.raw$Lower2 != ""] <- 0
lpi$calibration.raw$bare.soil[lpi$calibration.raw$Lower3 != ""] <- 0
lpi$calibration.raw$bare.soil[lpi$calibration.raw$Lower4 != ""] <- 0

## The NAs were a complete nightmare to deal with, so I turned them into -1s. After this next bit I'll turn them back
lpi$calibration.raw$HeightWoody[is.na(lpi$calibration.raw$HeightWoody)] <- -1
lpi$calibration.raw$HeightHerbaceous[is.na(lpi$calibration.raw$HeightHerbaceous)] <- -1

## Adding in the height classes for woody and herbaceous.
for (n in seq_along(tolerances$lpi$heights.breaks)){
  ## For the first loop, we're checking for plants shorter than or equal to the first height break
  if (n == min(seq_along(tolerances$lpi$heights.breaks))){
    # That !is.na() wrapped around the logical statement is because for some reason it was returning a vector of TRUE and NA instead of TRUE and FALSE
    lpi$calibration.raw[lpi$calibration.raw$HeightWoody <= tolerances$lpi$heights.breaks[n] &
                           lpi$calibration.raw$HeightWoody > 0,
                        paste0("woody.0.", tolerances$lpi$heights.breaks[n])] <- 1
    lpi$calibration.raw[lpi$calibration.raw$HeightHerbaceous <= tolerances$lpi$heights.breaks[n] &
                          lpi$calibration.raw$HeightHerbaceous > 0,
                        paste0("herbaceous.0.", tolerances$lpi$heights.breaks[n])] <- 1
  ## The last loop will just look for anything larger than the last height break
  } else if (n == max(seq_along(tolerances$lpi$heights.breaks))){
    lpi$calibration.raw[lpi$calibration.raw$HeightWoody > tolerances$lpi$heights.breaks[n-1] &
                          lpi$calibration.raw$HeightWoody <= tolerances$lpi$heights.breaks[n] &
                          lpi$calibration.raw$HeightWoody > 0,
                        paste0("woody.", tolerances$lpi$heights.breaks[n-1]+1, ".", tolerances$lpi$heights.breaks[n])] <- 1
    lpi$calibration.raw[lpi$calibration.raw$HeightHerbaceous > tolerances$lpi$heights.breaks[n-1] &
                          lpi$calibration.raw$Heightherbaceous <= tolerances$lpi$heights.breaks[n],
                        paste0("herbaceous.", tolerances$lpi$heights.breaks[n-1]+1, ".", tolerances$lpi$heights.breaks[n])] <- 1
    lpi$calibration.raw[lpi$calibration.raw$HeightWoody > tolerances$lpi$heights.breaks[n] &
                          lpi$calibration.raw$HeightWoody > 0,
                        paste0("woody.", tolerances$lpi$heights.breaks[n] + 1)] <- 1
    lpi$calibration.raw[lpi$calibration.raw$HeightHerbaceous > tolerances$lpi$heights.breaks[n],
                        paste0("herbaceous.", tolerances$lpi$heights.breaks[n] + 1)] <- 1
  ## All other loops will find plants greater than the previous height break AND shorter than or equal to the current one
  } else {
    lpi$calibration.raw[lpi$calibration.raw$HeightWoody > tolerances$lpi$heights.breaks[n-1] &
                          lpi$calibration.raw$HeightWoody <= tolerances$lpi$heights.breaks[n] &
                          lpi$calibration.raw$HeightWoody > 0,
                        paste0("woody.", tolerances$lpi$heights.breaks[n-1]+1, ".", tolerances$lpi$heights.breaks[n])] <- 1
    lpi$calibration.raw[lpi$calibration.raw$HeightHerbaceous > tolerances$lpi$heights.breaks[n-1] &
                          lpi$calibration.raw$Heightherbaceous <= tolerances$lpi$heights.breaks[n],
                        paste0("herbaceous.", tolerances$lpi$heights.breaks[n-1]+1, ".", tolerances$lpi$heights.breaks[n])] <- 1
  }
}

## Restoring the NAs so that we can tell that there weren't values there
lpi$calibration.raw$HeightWoody[lpi$calibration.raw$HeightWoody == -1] <- NA
lpi$calibration.raw$HeightHerbaceous[lpi$calibration.raw$HeightHerbaceous == -1] <- NA

## Storing the column names we just generated for future reference
tolerances$lpi$heights.classes <- tail(colnames(lpi$calibration.raw), (length(tolerances$lpi$heights.breaks) + 1)*2)


## Taking those raw data and converting them into the various indicators we want for each observer, specifically: percent total foliar cover, percent bare soil, percent basal cover, percent rock fragments,
## and the woody and herbaceous heights by height classes
lpi$calibration <- lpi$calibration.raw %>% group_by(Observer, SiteID, PlotID, PlotKey) %>%
  summarize(records.lpi = n(),
            foliar.hits = sum(foliar.cover, na.rm = T),
            basal.hits = sum(basal.cover, na.rm = T),
            rock.frag.hits = sum(rock.fragments, na.rm = T),
            bare.soil.hits = sum(bare.soil, na.rm = T))

for (n in seq_along(tolerances$lpi$heights.classes)){
  lpi$calibration <- lpi$calibration.raw %>% group_by(Observer, SiteID, PlotID, PlotKey) %>%
    summarize_(paste0("sum(", tolerances$lpi$heights.classes[n], ", na.rm = T)")) %>%
    merge(y = ., x = lpi$calibration, all = T)
  colnames(lpi$calibration)[length(colnames(lpi$calibration))] <- paste0(tolerances$lpi$heights.classes[n], ".count")
}

## Calculating percentages for the appropriate indicators
lpi$calibration <- lpi$calibration %>% mutate(foliar.cover.pct = 100*(foliar.hits/records.lpi),
                                              basal.cover.pct = 100*(basal.hits/records.lpi),
                                              rock.fragments.pct = 100*(rock.frag.hits/records.lpi),
                                              bare.soil.pct = 100*(bare.soil.hits/records.lpi))

## Now to add in the min and max for each indicator
lpi$calibration <- lpi$calibration %>% mutate(foliar.cover.pct.min = min(foliar.cover.pct),
                                              basal.cover.pct.min = min(basal.cover.pct),
                                              rock.fragments.pct.min = min(rock.fragments.pct),
                                              bare.soil.pct.min = min(bare.soil.pct),
                                              foliar.cover.pct.max = max(foliar.cover.pct),
                                              basal.cover.pct.max = max(basal.cover.pct),
                                              rock.fragments.pct.max = max(rock.fragments.pct),
                                              bare.soil.pct.max = max(bare.soil.pct))

for (n in seq_along(tolerances$lpi$heights.classes)){
  lpi$calibration <- lpi$calibration %>% mutate_(paste0("min(",tolerances$lpi$heights.classes[n],".count)"),
                                                 paste0("max(",tolerances$lpi$heights.classes[n],".count)"),
                                                 paste0("max(",tolerances$lpi$heights.classes[n],".count) - min(",tolerances$lpi$heights.classes[n],".count)"))
  colnames(lpi$calibration)[tail(seq_along(colnames(lpi$calibration)), 3)] <- c(paste0(tolerances$lpi$heights.classes[n], ".count.min"),
                                                                                paste0(tolerances$lpi$heights.classes[n], ".count.max"),
                                                                                paste0(tolerances$lpi$heights.classes[n], ".count.range"))
}


## And now to add the calibrated-or-not logical values!
lpi$calibration$foliar.cover.calibrated <- (lpi$calibration$foliar.cover.pct.max - lpi$calibration$foliar.cover.pct.min) <= tolerances$lpi$foliar.percent.range
lpi$calibration$basal.cover.calibrated <- (lpi$calibration$basal.cover.pct.max - lpi$calibration$basal.cover.pct.min) <= tolerances$lpi$basalcover.percent.range
lpi$calibration$bare.soil.calibrated <- (lpi$calibration$bare.soil.pct.max - lpi$calibration$bare.soil.pct.min) <= tolerances$lpi$baresoil.percent.range
lpi$calibration$rock.fragments.calibrated <- (lpi$calibration$rock.fragments.pct.max - lpi$calibration$rock.fragments.pct.min) <= tolerances$lpi$rockfragments.percent.range

for (n in seq_along(tolerances$lpi$heights.classes)){
  lpi$calibration[,paste0(tolerances$lpi$heights.classes[n], ".calibrated")] <- lpi$calibration[, paste0(tolerances$lpi$heights.classes[n], ".count.range")] <= tolerances$lpi$heights.count.range
}
#################################################################################

#################################################################################
### CALIBRATION CHECKING FOR GAPS ###############################################
#################################################################################
## The indicator being evaluated percentage of line[s] in gaps of of size classes defined by the values in tolerances$gaps$gap.breaks.
## Additionally, the gap count for each size class is included
## This is currently set up so that it'll work regardless of how many lines were read or number of gap classes

## Subset to just the data from the calibration plot
## This used to use a combination of Site- and PlotID, but much more reasonably, if less readably, now uses plot keys
# gaps$calibration.raw <- gaps$raw %>% subset(SiteID == calibration.SiteID) %>% subset(PlotID == calibration.PlotID)
## It also omits all rows with NA values, so just be aware
gaps$calibration.raw <- gaps$raw %>% subset(PlotKey == calibration.PlotKey) %>% na.omit()

## Writing in variables to keep track of what size class these belong to
## These are generalized so that no matter what the gap breaks are or how many size classes they result in, you should get the appropriate
## number of columns with intelligible names
for (n in seq_along(tolerances$gaps$gap.breaks)){
  # This evaluates for the first gap class, which is inclusive of the lower bounding cm value and the upper
  if (n == min(seq_along(tolerances$gaps$gap.breaks))){
    # This is the standard situation for the smallest gap class, but it only applies if there's more than one gap class
    if (length(tolerances$gaps$gap.breaks) > 1){
      # This creates a column with the name of "gap.[lowerbound].[upperbound]" and writes a 1 into it in every row where the size of the gap is both >= the lowest bound and <= the next break
      gaps$calibration.raw[gaps$calibration.raw$Gap >= tolerances$gaps$gap.breaks[n] & gaps$calibration.raw$Gap <= tolerances$gaps$gap.breaks[n+1], paste("gap", tolerances$gaps$gap.breaks[n], tolerances$gaps$gap.breaks[n+1], sep = ".")] <- 1
      # Otherwise, if for some reason there's only one size class, this will handle that situation
    } else {
      gaps$calibration.raw[gaps$calibration.raw$Gap >= tolerances$gaps$gap.breaks[n], paste("gap", tolerances$gaps$gap.breaks[n], sep = ".")] <- 1
    }
    # This evaluates the largest gap class, which has no upper bound on size and is not inclusive on the lower bound
  } else if (n == max(seq_along(tolerances$gaps$gap.breaks))){
    gaps$calibration.raw[gaps$calibration.raw$Gap > tolerances$gaps$gap.breaks[n], paste("gap", tolerances$gaps$gap.breaks[n] + 1, sep = ".")] <- 1
    # This evaluates all other gap classes, which are inclusive only on the upper bound   
  } else {
    gaps$calibration.raw[gaps$calibration.raw$Gap > tolerances$gaps$gap.breaks[n] & gaps$calibration.raw$Gap <= tolerances$gaps$gap.breaks[n+1], paste("gap", tolerances$gaps$gap.breaks[n], tolerances$gaps$gap.breaks[n+1] + 1, sep = ".")] <- 1
  }
}

## Just going to store the resulting gap classes' column names for future reference. They're the only ones with "gap." in them at this point
## Basically every loop after this takes advantage of this vector because it's so useful
tolerances$gaps$gap.classes <- colnames(gaps$calibration.raw)[grep("gap.", colnames(gaps$calibration.raw))]

## Counting gaps and finding their sums. This was the quick-and-dirty solution where I just filtered by the classes and kept merging them into the same calibration data frame
for (n in seq_along(tolerances$gaps$gap.classes)){
  ## As ever, we want to do something different on the first pass because we're creating gaps.calibration here, but with later passes we'll merge
  if (n == min(seq_along(tolerances$gaps$gap.classes))){
    ## Step one is to group the data by an observer on the plot
    gaps$calibration <- gaps$calibration.raw %>% group_by(Observer, SiteID, PlotID, PlotKey) %>%
      ## Then we use filter_() to get only the rows where the currently-being-evaluated gap class was recorded
      ## I guess that filter_() lets us pass strings as arguments whereas filter() doesn't, so we can create a string of "[column name] == 1"
      ## to use as our evaluations statement
      filter_(paste0(tolerances$gaps$gap.classes[n], "==", 1)) %>%
      ## Finish off with summarizing the number of gaps and the sum of the gaps' lengths in that size class, with appropriate but still ambiguously-named columns
      summarize(count = n(), cm.sum = sum(Gap, na.rm = T))
    ## Renaming those columns because doing it inside the summarize() was too hard to implement. Frankly this isn't much easier, but at least it works
    ## The grep() is looking for a column that exactly matches the string provided. "^" indicates that that's the start of the string and "$" is the end
    ## so "^count$" will only return the index of "count" but not "counts" or "account"
    colnames(gaps$calibration)[grep("^count$", colnames(gaps$calibration))] <- paste0(tolerances$gaps$gap.classes[n], ".count")
    colnames(gaps$calibration)[grep("^cm.sum$", colnames(gaps$calibration))] <- paste0(tolerances$gaps$gap.classes[n], ".cm.sum")
    ## And on any subsequent passes, we do the same thing, but merge in so as to not overwrite
  } else {
    gaps$calibration <- gaps$calibration.raw %>% group_by(Observer, SiteID, PlotID, PlotKey) %>%
      filter_(paste0(tolerances$gaps$gap.classes[n], "==", 1)) %>%
      summarize(count = n(), cm.sum = sum(Gap, na.rm = T)) %>%
      ## The only difference from above is that this line merges because gaps$calibration already has data in it
      merge (x = ., y = gaps$calibration, all = T)
    colnames(gaps$calibration)[grep("^count$", colnames(gaps$calibration))] <- paste0(tolerances$gaps$gap.classes[n], ".count")
    colnames(gaps$calibration)[grep("^cm.sum$", colnames(gaps$calibration))] <- paste0(tolerances$gaps$gap.classes[n], ".cm.sum")
  }
}

## Adding in the line lengths on a per-observer basis in case they maybe read different lengths even though they shouldn't
gaps$calibration <- gaps$calibration.raw %>% group_by(Observer, SiteID, PlotID, PlotKey) %>% summarize(length.total.cm = first(LineLengthAmount)) %>% merge (x = ., y = gaps$calibration, all = T)

## We've got NAs, but those are really 0s because this data frame is restricted to observers who completed gap forms, so let's change them
gaps$calibration <- gaps$calibration %>% replace(., is.na(.), 0)

## We need the minimum and maximum gap counts and percent in each gap class
for (n in seq_along(tolerances$gaps$gap.classes)){
  gaps$calibration[,paste0(tolerances$gaps$gap.classes[n], ".pct.max")] <- 100*max(gaps$calibration[, paste0(tolerances$gaps$gap.classes[n], ".cm.sum")]/gaps$calibration$length.total.cm)
  gaps$calibration[,paste0(tolerances$gaps$gap.classes[n], ".pct.min")] <- 100*min(gaps$calibration[, paste0(tolerances$gaps$gap.classes[n], ".cm.sum")]/gaps$calibration$length.total.cm)
  gaps$calibration[,paste0(tolerances$gaps$gap.classes[n], ".count.max")] <- max(gaps$calibration[, paste0(tolerances$gaps$gap.classes[n], ".count")])
  gaps$calibration[,paste0(tolerances$gaps$gap.classes[n], ".count.min")] <- min(gaps$calibration[, paste0(tolerances$gaps$gap.classes[n], ".count")])
}

## Now we'll add the percent of length and the mean percent of length in each gap class and mean count
for (n in seq_along(tolerances$gaps$gap.classes)){
  gaps$calibration <-  gaps$calibration %>% mutate_(paste0("100*(", tolerances$gaps$gap.classes[n], ".cm.sum/length.total.cm)"),
                                                    paste0("100*(mean(", tolerances$gaps$gap.classes[n], ".cm.sum, na.rm = T)/length.total.cm)"),
                                                    paste0(" mean(", tolerances$gaps$gap.classes[n], ".count, na.rm = T)")
  )
  ## So, I can't be bothered to fight naming mutate_() columns anymore. This takes the last three column names in the data frame
  ## using the tail() function to get the indices for them in the vector from colnames() and uses those to rename them with the same paste0()
  ## results that I couldn't get working in the mutate_()
  colnames(gaps$calibration)[tail(seq_along(colnames(gaps$calibration)), 3)] <- c(paste0(tolerances$gaps$gap.classes[n], ".pct"), paste0(tolerances$gaps$gap.classes[n], ".pct.mean"), paste0(tolerances$gaps$gap.classes[n], ".count.mean"))
}

## Final step is to decide if they're calibrated or not.
for (n in seq_along(tolerances$gaps$gap.classes)){
  gaps$calibration[, paste0(tolerances$gaps$gap.classes[n], ".pct.calibrated")] <- (gaps$calibration[, paste0(tolerances$gaps$gap.classes[n], ".pct.max")] - gaps$calibration[, paste0(tolerances$gaps$gap.classes[n], ".pct.min")]) <= tolerances$gaps$gap.pct.range
  gaps$calibration[, paste0(tolerances$gaps$gap.classes[n], ".count.calibrated")] <- (gaps$calibration[, paste0(tolerances$gaps$gap.classes[n], ".count.max")] - gaps$calibration[, paste0(tolerances$gaps$gap.classes[n], ".count.min")]) <= tolerances$gaps$gap.count
}
#################################################################################

#################################################################################
### CONCLUDING THE LOOP THAT'LL ITERATE THROUGH ALL PLOTS #######################
#################################################################################
## And now we wrap up the loop set in motion above. This will result in a single data frame output that contains all the calibration
## information for each observer by plot.

## On the first trip through the loop, it just creates calibration.results
  if (i == 1){
    calibration.results <- merge(lpi$calibration, gaps$calibration, all = T)
    ## On subsequent loops, the calibration.results data frame exists, so the new plot's results are just appended to avoid overwriting
  } else {
    calibration.results <- merge(lpi$calibration, gaps$calibration, all = T) %>% rbind(., calibration.results)
  }
}

#################################################################################


#################################################################################
### COMBINING AND WRITING CALIBRATION RESULTS ###################################
#################################################################################
### COMBINING AND WRITING CALIBRATION RESULTS
## So, now there're two data frames—one for LPI and one for gaps—but we want them combined and written out
## This needs to be uncommented to combine things if you aren't using the whole-database loop!
## calibration.results <- merge(lpi$calibration, gaps$calibration, all = T)
write.csv(calibration.results, paste(path.write, filename.output, sep = "/"))

#################################################################################
