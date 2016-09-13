###############################################
### COMMONLY USED PACKAGES IN AIM R SCRIPTS ###
###############################################

#### DATA WRANGLING ####
install.packages(
  c(
    "dplyr", ## Notably useful for data frame manipulation with group_by(), summarize(), and mutate() and the piping operator %>%. Do you have a moment to hear the good news of our savior, piping?
    "tidyr", ## Lots of useful things, but specifically gather() and spread() for converting wide data frames into long ones and vice versa
    "stringr", ## All about character strings. Great for str_split() in particular, but the whole str_ family of functions are A+
    "RODBC", ## Allows R to pass SQL queries to Access databases and pull out the results
    "XLSX", ## Read in modern Excel workbooks and spreadsheets
    "broom" ## Get stats objects into tidy data frames. Not as common
  )
)

#### SPATIAL DATA ####
install.packages(
  c(
    "sp", ## Functions for manipulating spatial objects. If you want a Spatial _____ Data Frame, this is part of the deal
    "spsurvey", ## Contains plenty, but the most important to AIM is the GRTS function
    "rgeos", ## Additional spatial object functions
    "rgdal", ## Contains the mission critical readOGR() function that we read shapefiles in with
    "raster" ## Everything you didn't know you needed for dealing with rasters
  )
)

#### DATA VISUALIZATION ####
install.packages(
  c(
    "ggplot2", ## The go-to for figure generation. Most R-using scientists AND Nate Silver use it, so you should too
    "ggthemes", ## Quick themes to painlessly apply to figures from ggplot
    "ggmap", ## Mapping support for ggplot
  )
)

#### MISCELLANEOUS PACKAGES ####
install.packages("arcgisbinding") ## Young and finicky, but once you have it all installed (an ordeal) you should be able to read from and write to file geodatabases from R
install.packages("gridExtra") ## Lets you make grid objects that you can place ggplot figures into. May occasionally be preferable to faceting in ggplot, but rarely
install.packages("shiny") ## Required for working with Shiny tools in any form. Can be maddening
install.packages("purrr") ## Really, really useful for writing functions, particularly those that fail gracefully