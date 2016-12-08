library(xlsx)

setwd("../Downloads")

templateFile <- ("TerrestrialAIM_DataAnalysis_Template.xlsx")

df <- data.frame("id"=seq(1:100),"test2"=rnorm(100,0,1))

file <- system.file("tests",templateFile,package="xlsx")
wb <- loadWorkbook(templateFile)
removeSheet(wb,sheetName="test2")
theSheet <- createSheet(wb,sheetName="test2")
addDataFrame(df,theSheet,row.names=FALSE,col.names=TRUE)
saveWorkbook(wb,"test.xlsx")
