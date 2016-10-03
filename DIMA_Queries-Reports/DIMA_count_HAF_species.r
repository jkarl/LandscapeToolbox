library(dplyr)
library(readr)
library(RODBC)

DIMA <- "filepath and filename to DIMA"

## This reduces it to forbs only in the process
haf.list <- read_csv("HAF_preferred_species_by_code.csv") %>% subset(GROWTH.HABIT == "FORB")

channel <- odbcConnectAccess(DIMA) ## Assumes 32-bit R and 32-bit Access. Use odbcConnectAccess2007() if both are 64-bit
species.lists <- sqlQuery(channel, "SELECT joinSitePlotLine.SiteID, joinSitePlotLine.PlotKey, joinSitePlotLine.PlotID, tblSpecRichDetail.SpeciesList FROM joinSitePlotLine INNER JOIN (tblSpecRichHeader LEFT JOIN tblSpecRichDetail ON tblSpecRichHeader.RecKey = tblSpecRichDetail.RecKey) ON joinSitePlotLine.LineKey = tblSpecRichHeader.LineKey;")
odbcCloseAll()


## Counting the number of preferred species in the data
# The semicolons are the way that DIMA delimits the species in its own tables
for (n in 1:nrow(species.lists)){
  ## Take the HAF preferred species list, reduce it to a subset where they are also found in the
  # vector species list extracted from the plot, and count the number of rows in that subset. No loop required.
  species.lists$HAF.preferred.count[n] <- subset(haf.list,
                                                 haf.list$CODE %in%
                                                   unlist(strsplit(as.character(species.lists[n,4]),";"))
                                                 ) %>% nrow()
}
