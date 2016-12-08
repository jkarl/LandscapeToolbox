---
title: "R Notebook"
output: html_notebook
---

Notebook to look at the correspondence between plant codes in TerrADat and those in the USDA PLANTS database. Specifically:
1. Which plant codes are used in TerrADat that don't occur in PLANTS? 
  1a. How many are there?
  1b. Do they vary by state/region?
  1c. Are these all made up codes, unknown codes, or retired codes
2. What is the prevalence of unknown species codes by project?
3. Can retired codes be automagically identified and reassigned to legitimate plant codes?


```{r envParams}
library(dplyr)
library(ggplot2)
library(arcgisbinding)
library(markdown)
library(knitr)

work.dir <- "C:\\Users\\jasokarl\\Google Drive\\Landscape Toolbox\\LandscapeToolbox_GitHub\\LandscapeToolbox\\DIMAqc\\DIMAqc"
terradat <- "C:\\Users\\jasokarl\\Google Drive\\BLM_AIM\\geodata\\TerrADatExport_19Jan2016.gdb"
plants.file <- "PLANTSlist_Master_with_attribs.20161116.csv"
```


```{r loadData}
setwd(work.dir)

plants.master <- read.csv(plants.file,header=T)

arc<-arc.check_product()
# load terrestrial AIM featureclass

# load tables with species info and those that are necessary to track back to the plot and project
#    i.e., species richness detail/header, LPI detail/header, line, plot, sites tables
tblPlots<-paste(terradat,"tblPlots",sep="/") %>% arc.open %>% arc.select()
tblLines <- paste(terradat,"tblLines",sep="/") %>% arc.open %>% arc.select()
tblSites <- paste(terradat,"tblSites",sep="/") %>% arc.open %>% arc.select()
tblLPIHeader <- paste(terradat,"tblLPIHeader",sep="/") %>% arc.open %>% arc.select()
tblLPIDetail <- paste(terradat,"tblLPIDetail",sep="/") %>% arc.open %>% arc.select()
tblSpecRichHeader <- paste(terradat,"tblSpecRichHeader",sep="/") %>% arc.open %>% arc.select()
tblSpecRichDetail <- paste(terradat,"tblSpecRichDetail",sep="/") %>% arc.open %>% arc.select()
```

## 1. Which plots have species not in the master species list?
```{r}
species.nomatch <- data.frame()
for (i in 1:nrow(tblSpecRichDetail)) {
  spp.list <- unlist(strsplit(tblSpecRichDetail[i,"SpeciesList"],";"))
  not.in <- !(spp.list %in% plants.master$Accepted.Symbol)
  if (sum(not.in)>0) {
    species.nomatch <- rbind(species.nomatch,data.frame("RecKey"=tblSpecRichDetail[i,"RecKey"],"Code"=spp.list[not.in]))
  }
}
```

Summary of species codes not in PLANTS database by # plots on which they occurred
```{r}
spp.nomatch.sum <- species.nomatch %>% group_by("Code") %>% summarise(n=n())
```


Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).
