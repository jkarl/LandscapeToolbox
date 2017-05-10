## Assignment of State and RC Class to Mongolian monitoring data

## Parameters
directory <- "C:\\Users\\jasokarl\\Google Drive\\Landscape Toolbox\\LandscapeToolbox_GitHub\\LandscapeToolbox\\Mongolia"
input.file <- "2012_Summary_Excel.csv"
output.file <- "2012_Summary_with_RC.csv"

## Load the data as a CSV file input
rc.data <- read.csv(file=paste(directory,input.file,sep="\\"),header=T,stringsAsFactors=F)

''' Names of rc.data fields
 [1] "ï.."              "YEAR"             "SITEID"           "PLOTID"           "SITE.PLOT"        "RC_INTERP"        "ESG"             
 [8] "STM"              "STATE_INTERP"     "PHASE_INTERP"     "REFERENCENUM"     "REFERENCE"        "ESGEN"            "ACHNA"           
[15] "ANNUAL"           "ARAD"             "ARFRI"            "BARTEM"           "CLEIST"           "CXDU"             "LEYM"            
[22] "ELCHN"            "MEADPF"           "MEADPG"           "NISB"             "PPFF"             "PPGG"             "SMBPG"           
[29] "STBA"             "STGBGL"           "STGR"             "STIPA"            "STKR"             "SRC"              "DRYLAND.CAREX"   
[36] "GOOD.ARTEMISIA"   "HALOPHYTIC.SHRUB" "HIMTNMEADOWSPP"   "MEADOW.CAREX"     "OTHER.ALLIUM"     "OTHER.CAREX"      "SHRUB"           
[43] "SUB.SHRUB"        "TREE"             "XEROPHYTIC.SHRUB" "CXPED"            "POTAC"            "POTBI"            "PCT.FOLIAR.COVER"
[50] "PCT.BARE.SOIL"    "SPECIES.COUNT"    "TOTALWTHECTARE"  
'''

assignRC <- function(data) {
  stm <- data$STM
  state <- 0
  rc<-""
  
  # Catch empty records - i.e., no data collected
  if (is.na(data$ACHNA) & is.na(data$PCT.FOLIAR.COVER)) { # Assume if these are NA, then no data was collected
    return(data.frame("state"=state, "rc"=rc))
  }
  
  ## Call the values from the data table
  ## NA's are converted to Zeros.
  ARFRI <- ifelse(is.na(data$ARFRI),0,data$ARFRI)
  STBA <- ifelse(is.na(data$STBA),0,data$STBA)
  STGBGL <- ifelse(is.na(data$STGBGL),0,data$STGBGL)
  STIPA <- ifelse(is.na(data$STIPA),0,data$STIPA)
  STGR <- ifelse(is.na(data$STGR),0,data$STGR)
  STKR <- ifelse(is.na(data$STKR),0,data$STKR)
  PPGG <- ifelse(is.na(data$PPGG),0,data$PPGG)
  PPFF <- ifelse(is.na(data$PPFF),0,data$PPFF)
  SMBPG <- ifelse(is.na(data$SMBPG),0,data$SMBPG)
  CLEIST <- ifelse(is.na(data$CLEIST),0,data$CLEIST)
  ELCHN <- ifelse(is.na(data$ELCHN),0,data$ELCHN)
  ACHNA <- ifelse(is.na(data$ACHNA),0,data$ACHNA)
  LEYM <- ifelse(is.na(data$LEYM),0,data$LEYM)
  CXDU <- ifelse(is.na(data$CXDU),0,data$CXDU)
  MEADPF <- ifelse(is.na(data$MEADPF),0,data$MEADPF)
  MEADPG <- ifelse(is.na(data$MEADPG),0,data$MEADPG)
  BARTEM <- ifelse(is.na(data$BARTEM),0,data$BARTEM)
  BARE <- ifelse(is.na(data$PCT.BARE.SOIL),0,data$PCT.BARE.SOIL)
  ARAD <- ifelse(is.na(data$ARAD),0,data$ARAD)
  ANNUAL <- ifelse(is.na(data$ANNUAL),0,data$ANNUAL)
  NISB <- ifelse(is.na(data$NISB),0,data$NISB)
  src <- ifelse(is.na(data$SRC),0,data$SRC)
  
  #calc intermediate indicators
  allStipa <- STBA + STGBGL + STGR + STIPA + STKR
  grass.1 <- allStipa + PPGG + SMBPG
  grass.2 <- CLEIST + ELCHN + PPGG + SMBPG
  grass.3 <- ACHNA + CLEIST + LEYM + STGBGL + STKR
  grass.4 <- CLEIST + ELCHN + PPGG + SMBPG + STGBGL + STKR
  grass.5 <- ACHNA + CLEIST + LEYM + PPGG + STGBGL + STKR
  grass.6 <- CLEIST + PPGG + SMBPG + STGBGL + STKR
  
  
  # Assign the state classes
  switch(stm,
        "1"={
          if((grass.1>=20) & (ARFRI<30)) {
            state<-1
            rc<-"I"
          } else if(src>=3) {
            state<-3
            rc<-"IV"
          } else {
            state<-2
            rc<-"III"
          }
        },
        "2"={
          if((grass.1>=20) & (ARFRI<30)) {
            state<-1
            rc<-"I"
          } else if(BARTEM>=30){
            state<-3
            rc<-"IV"
          } else {
            state<-2
            rc<-"III"
          }
        },
        "3"={
          if((STBA >= 30) || ((STBA >= 15) & (CXDU + ELCHN < 30))) {
            state<-1
            rc<-"I"
          } else {
            state<-2
            rc<-"III"
          }
        },
        "4"={
          if((MEADPF >= 10) || (MEADPG >= 10)) {
            state<-1
            rc<-"I"
          } else {
            state<-2
            rc<-"II"
          }
        },
        "5"={
          if(PPFF+MEADPF>=25) {
            state<-1
            rc<-"I"
          } else {
            state<-2
            rc<-"II"
          }
        },
        "6"={
          if(STGR>=20) {
            state<-1
            rc<-"I"
          } else if((BARTEM>=15) & ((CXDU>=20) || (BARE>=25))) {
            state<-3
            rc<-"III"
          } else {
            state<-2
            rc<-"II"
          }
        },
        "7"={
          if ((STKR>=25) || (SMBPG>=25)) {
            state<-1
            rc<-"I"
          } else if((ARFRI>=25) & ((PPGG<5) || (STKR<5) || (SMBPG<5))) {
            state<-3
            rc<-"IV"
          } else {
            state<-2
            rc<-"III"
          }
        },
        "8"={
          if(STKR>=20) {
            state<-1
            rc<-"I"
          } else if((CXDU>=40) & (grass.2>=25)) {
            state<- 2
            rc<-"II"
          } else if(grass.2>=10) {
            state<-3
            rc<-"III"
          } else if(ARAD>=30) {
            state<-5
            rc<-"V"
          } else {
            state<-4
            rc<-"IV"
          }
        },
        "9"={
          if(STKR>=15) {
            state<-1
            rc<-"I"
          } else if(CXDU>=30) {
            state<-4
            rc<-"IV"
          } else if(grass.2>=15) {
            state<-2
            rc<-"II"
          } else {
            state<-3
            rc<-"III"
          }
        },
        "10"={
          if((grass.3>CXDU+ARFRI) & (grass.3>BARTEM+ANNUAL)) {
            state<-1
            rc<-"I"
          } else if(((CXDU+ARFRI)>(BARTEM+ANNUAL)) & ((CXDU+ARFRI)>grass.3)) {
            state<-2
            rc<-"II"
          } else if(src>=3) {
            state<-4
            rc<-"IV"
          } else {
            state<-3
            rc<-"III"
          }
        },
        "11"={
          if(grass.4>=25) {
            state<-1
            rc<-"I"
          } else if(ARFRI>=15) {
            state<-2
            rc<-"III"
          } else {
            state<-3
            rc<-"IV"
          }
        },
        "12"={
          if(grass.4>=15) {
            state<-1
            rc<-"I"
          } else if(grass.4>=3) {
            state<-2
            rc<-"II"
          } else if(BARTEM>=10) {
            state<-4
            rc<-"IV"
          } else {
            state<-3
            rc<-"III"
          }
        },
        "13"={
          if((grass.5>(CXDU+ARFRI)) & (grass.5>(BARTEM+ANNUAL))) {
            state<-1
            rc<-"I"
          } else if(((CXDU+ARFRI)>(BARTEM+ANNUAL)) & ((CXDU+ARFRI)>grass.5)) {
            state<-2
            rc<-"II"
          } else if(src>=3) {
            state<-4
            rc<-"IV"
          } else {
            state<-3
            rc<-"III"
          }
        },
        "14"={
          if(STGBGL >=10) {
            state<-1
            rc<-"I"
          } else if(STGBGL >=3) {
            state<-2
            rc<-"III"
          } else {
            state<-3
            rc<-"IV"
          }
        },
        "15"={
          if(STGBGL>=10) {
            state<-1
            rc<-"I"
          } else {
            state<-2
            rc<-"II"
          }
        },
        "16"={
          if((STGBGL>=3) & (grass.2<10)) {
            state<-1
            rc<-"I"
          } else if(CLEIST>=10) {
            state<-2
            rc<-"III"
          } else {
            state<-3
            rc<-"IV"
          }
        },
        "17"={
          if(src>=3) {
            state<-2
            rc<-"II"
          } else {
            state<-1
            rc<-"I"
          }
        },
        "18"={
          state<-1
          rc<-"I"
          },
        "19"={
          state<-1
          rc<-"I"},
        "20"={
          if(NISB<10) {
            state <- 1
            rc<-"I"
          } else if ((NISB>=10) & (src >= 3)) {
            state <- 2
            rc<-"I"
          } else {
            state <- 0
            rc<-""
          }
        },
        "21"={
          state<-0
          rc<-""},
        "22"={
          if (src >= 3) {
            state <- 2
            rc<-"I"
          } else {
            state <- 1
            rc<-"I"
          }
        },
        "23"={
          if(grass.6 > PPFF) {
            state <- 1
            rc<-"I"
          } else {
            state <- 2
            rc<-"I"
          }
        },
        "24"={
          if(grass.6 > PPFF) {
              state <- 1 
              rc<-"I"
            } else {
              state <- 2
              rc<-"I"
            }
        },
        "25"={
          state<-1
          rc<-"I"}
        )
  return(data.frame("state"=state, "rc"=rc))
}


## Make the assignments to the data
rc.data$RC.Class <- ""
rc.data$state.Class <- ""
for(i in 1:nrow(rc.data)) {
  d <- rc.data[i,]
  vals <- assignRC(d)
  rc.data$state.Class[i] <- as.character(vals[1,1]) 
  rc.data$RC.Class[i] <- as.character(vals[1,2])
  print(paste("State for row ",i," is ",vals[,1]))
}


## Write the output file
write.csv(rc.data,paste(directory,output.file,sep="\\"))
