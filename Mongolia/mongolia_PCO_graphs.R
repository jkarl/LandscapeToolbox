## Ordinations of Mongolia Data

library(vegan)
library(dplyr)
library(ggplot2)
library(reshape)
library(gridExtra)
library(ellipse)

## Load the data
infile <- "C:\\Users\\jasokarl\\Google Drive\\Landscape Toolbox\\LandscapeToolbox_GitHub\\LandscapeToolbox\\Mongolia\\2012-2015_Merged_last_20170109.csv"
rc.data <- read.csv(file=infile,header=T,stringsAsFactors=F)
rc.data[grep("Multiple*",rc.data$RC.Symbol.2),"RC.Symbol.2"] <- "Multiple"

vegClean <- function(rc.veg) {
  #rc.veg <- df[,c(15:34,36:43)]
  ## remove rows with all NA
  t <- apply(rc.veg,1,function(x) !all(is.na(x)))
  rc.veg.trim <- rc.veg[t,]
  # replace remaining NAs with Zero and remove all other blank rows
  rc.veg.trim[is.na(rc.veg.trim)] <- 0
  t2 <- apply(rc.veg.trim,1,function(x) sum(x)>0)
  rc.veg.trim <- rc.veg.trim[t2,]
  return(rc.veg.trim)  
}

calcEllipse <- function(df) {
  df_ell <- data.frame()
  for(g in unique(df$RC.Symbol.2)){
    df_ell <- rbind(df_ell, cbind(as.data.frame(with(df[df$RC.Symbol.2==g,], ellipse(cor(Dim1, Dim2), 
                                                                                     scale=c(sd(Dim1),sd(Dim2)),
                                                                                     level=0.9,
                                                                                     centre=c(mean(Dim1),mean(Dim2))))),group=g))
  }
  return(df_ell)
}

## Calc the intermediate grass indicators
rc.data$allStipa <- rc.data$STBA + rc.data$STGBGL + rc.data$STGR + rc.data$STIPA + rc.data$STKR
rc.data$grass.1 <- rc.data$allStipa + rc.data$PPGG + rc.data$SMBPG
rc.data$grass.2 <- rc.data$CLEIST + rc.data$ELCHN + rc.data$PPGG + rc.data$SMBPG
rc.data$grass.3 <- rc.data$ACHNA + rc.data$CLEIST + rc.data$LEYM + rc.data$STGBGL + rc.data$STKR
rc.data$grass.4 <- rc.data$CLEIST + rc.data$ELCHN + rc.data$PPGG + rc.data$SMBPG + rc.data$STGBGL + rc.data$STKR
rc.data$grass.5 <- rc.data$ACHNA + rc.data$CLEIST + rc.data$LEYM + rc.data$PPGG + rc.data$STGBGL + rc.data$STKR
rc.data$grass.6 <- rc.data$CLEIST + rc.data$PPGG + rc.data$SMBPG + rc.data$STGBGL + rc.data$STKR


## Plot of RC Class 1 stability by defining indicators
rc.data.1 <- rc.data[rc.data$STM==1,c(15:34,36:43)]
#rc.data.2 <- rc.data[rc.data$STM==2,c("ARFRI","BARTEM","CXDU","PPFF","SMBPG","STKR","SUB.SHRUB","POTAC","POTBI")]
rc.veg <- vegClean(rc.data.1)
# compute distance matrix
rc.veg.dist <- vegdist(rc.veg,"bray",na.rm=TRUE)
rc.veg.pco <- cmdscale(rc.veg.dist,k=3,eig=TRUE)
cor(rc.veg,scores(rc.veg.pco))
eigs <- eigenvals(rc.veg.pco)
(eigs/sum(eigs))[1:3]
scrs <- scores(rc.veg.pco)
rc.data.pco <- merge(rc.data,scrs,by="row.names")

rc.data.stm <- rc.data.pco[rc.data.pco$RC.Symbol!="Insufficient Data",]
# Extract rows for plots stable in RC Class I or III and build confidence ellipses
df_ell <- calcEllipse(rc.data.stm[rc.data.stm$RC.Symbol.2=="I" | rc.data.stm$RC.Symbol.2=="III",c("Dim1","Dim2","RC.Symbol.2")])

#p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.23 <- ggplot(data=rc.data.stm,aes(x=Dim2,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.13 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#grid.arrange(p.12,p.23,p.13,ncol=3)
p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+facet_wrap(~RC.Symbol.2,scales="fixed")+
  geom_line(aes(group=SITE.PLOT,colour=RC.Symbol.2), arrow=arrow(angle=30,length=unit(2, "mm"),type="closed"))+
  geom_path(data=df_ell, aes(x=x, y=y,colour=group), size=1, linetype=2) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
  ggtitle("Plots for STM 1 by PCO Dimensions 1 and 2")
p.12




## Plot of RC Class 2 stability by defining indicators
rc.data.2 <- rc.data[rc.data$STM==2,c(15:34,36:43)]
#rc.data.2 <- rc.data[rc.data$STM==2,c("ARFRI","BARTEM","CXDU","PPFF","SMBPG","STKR","SUB.SHRUB","POTAC","POTBI")]
rc.veg <- vegClean(rc.data.2)
# compute distance matrix
rc.veg.dist <- vegdist(rc.veg,"bray",na.rm=TRUE)
rc.veg.pco <- cmdscale(rc.veg.dist,k=3,eig=TRUE)
cor(rc.veg,scores(rc.veg.pco))
eigs <- eigenvals(rc.veg.pco)
(eigs/sum(eigs))[1:3]
scrs <- scores(rc.veg.pco)
rc.data.pco <- merge(rc.data,scrs,by="row.names")

rc.data.stm <- rc.data.pco[rc.data.pco$RC.Symbol.2!="Insufficient Data" & (rc.data.pco$RC.Symbol.2 %in% c("I","III","I-III","III-I","Multiple")),]
# Extract rows for plots stable in RC Class I or III and build confidence ellipses
df_ell <- calcEllipse(rc.data.stm[rc.data.stm$RC.Symbol.2=="I" | rc.data.stm$RC.Symbol.2=="III",c("Dim1","Dim2","RC.Symbol.2")])

#p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.23 <- ggplot(data=rc.data.stm,aes(x=Dim2,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.13 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#grid.arrange(p.12,p.23,p.13,ncol=3)
p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+facet_wrap(~RC.Symbol.2,scales="fixed")+
  geom_line(aes(group=SITE.PLOT,colour=RC.Symbol.2), arrow=arrow(angle=30,length=unit(2, "mm"),type="closed"))+
  geom_path(data=df_ell, aes(x=x, y=y,colour=group), size=1, linetype=2) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
  ggtitle("Plots for STM 2 by PCO Dimensions 1 and 2")
p.12


## RC 3
rc.data.3 <- rc.data[rc.data$STM==3,c(15:34,36:43)]
#rc.data.2 <- rc.data[rc.data$STM==2,c("ARFRI","BARTEM","CXDU","PPFF","SMBPG","STKR","SUB.SHRUB","POTAC","POTBI")]
rc.veg <- vegClean(rc.data.3)
# compute distance matrix
rc.veg.dist <- vegdist(rc.veg,"bray",na.rm=TRUE)
rc.veg.pco <- cmdscale(rc.veg.dist,k=3,eig=TRUE)
cor(rc.veg,scores(rc.veg.pco))
eigs <- eigenvals(rc.veg.pco)
(eigs/sum(eigs))[1:3]
scrs <- scores(rc.veg.pco)
rc.data.pco <- merge(rc.data,scrs,by="row.names")

#rc.data.stm <- rc.data.pco[rc.data.pco$RC.Symbol.2!="Insufficient Data",]
rc.data.stm <- rc.data.pco[rc.data.pco$RC.Symbol.2!="Insufficient Data" & (rc.data.pco$RC.Symbol.2 %in% c("I","III","I-III","III-I","Multiple")),]
# Extract rows for plots stable in RC Class I or III and build confidence ellipses
df_ell <- calcEllipse(rc.data.stm[rc.data.stm$RC.Symbol.2=="I" | rc.data.stm$RC.Symbol.2=="III",c("Dim1","Dim2","RC.Symbol.2")])

#p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.23 <- ggplot(data=rc.data.stm,aes(x=Dim2,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.13 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#grid.arrange(p.12,p.23,p.13,ncol=3)
p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+facet_wrap(~RC.Symbol.2,scales="fixed")+
  geom_line(aes(group=SITE.PLOT,colour=RC.Symbol.2), arrow=arrow(angle=30,length=unit(2, "mm"),type="closed"))+
  geom_path(data=df_ell, aes(x=x, y=y,colour=group), size=1, linetype=2) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
  ggtitle("Plots for STM 3 by PCO Dimensions 1 and 2")
p.12

## Plot of RC Class stability by defining indicators
#
rc.data.6 <- rc.data[rc.data$STM==6,c(15:34,36:43)]
#rc.data.2 <- rc.data[rc.data$STM==2,c("ARFRI","BARTEM","CXDU","PPFF","SMBPG","STKR","SUB.SHRUB","POTAC","POTBI")]
rc.veg <- vegClean(rc.data.6)
# compute distance matrix
rc.veg.dist <- vegdist(rc.veg,"bray",na.rm=TRUE)
rc.veg.pco <- cmdscale(rc.veg.dist,k=3,eig=TRUE)
cor(rc.veg,scores(rc.veg.pco))
eigs <- eigenvals(rc.veg.pco)
(eigs/sum(eigs))[1:3]
scrs <- scores(rc.veg.pco)
rc.data.pco <- merge(rc.data,scrs,by="row.names")

rc.data.stm <- rc.data.pco[rc.data.pco$RC.Symbol.2!="Insufficient Data" & rc.data.pco$RC.Symbol.2!="I-III",]
# Extract rows for plots stable in RC Class I or III and build confidence ellipses
df_ell <- calcEllipse(rc.data.stm[rc.data.stm$RC.Symbol.2=="I" | rc.data.stm$RC.Symbol.2=="II",c("Dim1","Dim2","RC.Symbol.2")])

#p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.23 <- ggplot(data=rc.data.stm,aes(x=Dim2,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.13 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#grid.arrange(p.12,p.23,p.13,ncol=3)
p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+facet_wrap(~RC.Symbol.2,scales="fixed")+
  geom_line(aes(group=SITE.PLOT,colour=RC.Symbol.2), arrow=arrow(angle=30,length=unit(2, "mm"),type="closed"))+
  geom_path(data=df_ell, aes(x=x, y=y,colour=group), size=1, linetype=2) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
  ggtitle("Plots for STM 6 by PCO Dimensions 1 and 2")
p.12


## RC 8
rc.data.8 <- rc.data[rc.data$STM==8,c(15:34,36:43)]
#rc.data.2 <- rc.data[rc.data$STM==2,c("ARFRI","BARTEM","CXDU","PPFF","SMBPG","STKR","SUB.SHRUB","POTAC","POTBI")]
rc.veg <- vegClean(rc.data.8)
# compute distance matrix
rc.veg.dist <- vegdist(rc.veg,"bray",na.rm=TRUE)
rc.veg.pco <- cmdscale(rc.veg.dist,k=3,eig=TRUE)
#rc.veg.2.nmds <- metaMDS(rc.veg.2,dist="bray",k=3,trymax=25)
cor(rc.veg,scores(rc.veg.pco))
eigs <- eigenvals(rc.veg.pco)
(eigs/sum(eigs))[1:3]
scrs <- scores(rc.veg.pco)
rc.data.pco <- merge(rc.data,scrs,by="row.names")

#rc.data.stm <- rc.data.pco[rc.data.pco$RC.Symbol.2!="Insufficient Data",]
rc.data.stm <- rc.data.pco[rc.data.pco$RC.Symbol.2!="Insufficient Data" & (rc.data.pco$RC.Symbol.2 %in% c("I","III","I-III","III-I","Multiple")),]
# Extract rows for plots stable in RC Class I or III and build confidence ellipses
df_ell <- calcEllipse(rc.data.stm[rc.data.stm$RC.Symbol.2=="I" | rc.data.stm$RC.Symbol.2=="III",c("Dim1","Dim2","RC.Symbol.2")])

#p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.23 <- ggplot(data=rc.data.stm,aes(x=Dim2,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.13 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#grid.arrange(p.12,p.23,p.13,ncol=3)
p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+facet_wrap(~RC.Symbol.2,scales="fixed")+
  geom_line(aes(group=SITE.PLOT,colour=RC.Symbol.2), arrow=arrow(angle=30,length=unit(2, "mm"),type="closed"))+
  geom_path(data=df_ell, aes(x=x, y=y,colour=group), size=1, linetype=2) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
  ggtitle("Plots for STM 8 by PCO Dimensions 1 and 2")
p.12


## Plot of RC Class stability by defining indicators
#
rc.data.12 <- rc.data[rc.data$STM==12,c(15:34,36:43)]
#rc.data.2 <- rc.data[rc.data$STM==2,c("ARFRI","BARTEM","CXDU","PPFF","SMBPG","STKR","SUB.SHRUB","POTAC","POTBI")]
rc.veg <- vegClean(rc.data.12)
# compute distance matrix
rc.veg.dist <- vegdist(rc.veg,"bray",na.rm=TRUE)
rc.veg.pco <- cmdscale(rc.veg.dist,k=3,eig=TRUE)
#rc.veg.2.nmds <- metaMDS(rc.veg.2,dist="bray",k=3,trymax=25)
cor(rc.veg,scores(rc.veg.pco))
eigs <- eigenvals(rc.veg.pco)
(eigs/sum(eigs))[1:3]
scrs <- scores(rc.veg.pco)
rc.data.pco <- merge(rc.data,scrs,by="row.names")

#rc.data.stm <- rc.data.pco[rc.data.pco$RC.Symbol.2!="Insufficient Data",]
rc.data.stm <- rc.data.pco[rc.data.pco$RC.Symbol.2!="Insufficient Data" & (rc.data.pco$RC.Symbol.2 %in% c("I","II","I-II","II-I","I-III","III-I","II-III","Multiple")),]
# Extract rows for plots stable in RC Class I or III and build confidence ellipses
df_ell <- calcEllipse(rc.data.stm[rc.data.stm$RC.Symbol.2=="I" | rc.data.stm$RC.Symbol.2=="II",c("Dim1","Dim2","RC.Symbol.2")])

#p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.23 <- ggplot(data=rc.data.stm,aes(x=Dim2,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.13 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#grid.arrange(p.12,p.23,p.13,ncol=3)
p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+facet_wrap(~RC.Symbol.2,scales="fixed")+
  geom_line(aes(group=SITE.PLOT,colour=RC.Symbol.2), arrow=arrow(angle=30,length=unit(2, "mm"),type="closed"))+
  geom_path(data=df_ell, aes(x=x, y=y,colour=group), size=1, linetype=2) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
  ggtitle("Plots for STM 12 by PCO Dimensions 1 and 2")
p.12

## Plot of RC Class stability by defining indicators
#
rc.data.15 <- rc.data[rc.data$STM==15,c(15:34,36:43)]
#rc.data.2 <- rc.data[rc.data$STM==2,c("ARFRI","BARTEM","CXDU","PPFF","SMBPG","STKR","SUB.SHRUB","POTAC","POTBI")]
rc.veg <- vegClean(rc.data.15)
# compute distance matrix
rc.veg.dist <- vegdist(rc.veg,"bray",na.rm=TRUE)
rc.veg.pco <- cmdscale(rc.veg.dist,k=3,eig=TRUE)
#rc.veg.2.nmds <- metaMDS(rc.veg.2,dist="bray",k=3,trymax=25)
cor(rc.veg,scores(rc.veg.pco))
eigs <- eigenvals(rc.veg.pco)
(eigs/sum(eigs))[1:3]
scrs <- scores(rc.veg.pco)
rc.data.pco <- merge(rc.data,scrs,by="row.names")

rc.data.stm <- rc.data.pco[rc.data.pco$RC.Symbol.2!="Insufficient Data"&rc.data.pco$RC.Symbol.2!="I-III",]
#rc.data.stm <- rc.data.pco[rc.data.pco$RC.Symbol.2!="Insufficient Data" & (rc.data.pco$RC.Symbol.2 %in% c("I","III","I-III","III-I","Multiple")),]
# Extract rows for plots stable in RC Class I or III and build confidence ellipses
df_ell <- calcEllipse(rc.data.stm[rc.data.stm$RC.Symbol.2=="I" | rc.data.stm$RC.Symbol.2=="II",c("Dim1","Dim2","RC.Symbol.2")])

#p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.23 <- ggplot(data=rc.data.stm,aes(x=Dim2,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.13 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#grid.arrange(p.12,p.23,p.13,ncol=3)
p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+facet_wrap(~RC.Symbol.2,scales="fixed")+
  geom_line(aes(group=SITE.PLOT,colour=RC.Symbol.2), arrow=arrow(angle=30,length=unit(2, "mm"),type="closed"))+
  geom_path(data=df_ell, aes(x=x, y=y,colour=group), size=1, linetype=2) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
  ggtitle("Plots for STM 15 by PCO Dimensions 1 and 2")
p.12

## Plot of RC Class stability by defining indicators
#
rc.data.16 <- rc.data[rc.data$STM==16,c(15:34,36:43)]
#rc.data.2 <- rc.data[rc.data$STM==2,c("ARFRI","BARTEM","CXDU","PPFF","SMBPG","STKR","SUB.SHRUB","POTAC","POTBI")]
rc.veg <- vegClean(rc.data.16)
# compute distance matrix
rc.veg.dist <- vegdist(rc.veg,"bray",na.rm=TRUE)
rc.veg.pco <- cmdscale(rc.veg.dist,k=3,eig=TRUE)
#rc.veg.2.nmds <- metaMDS(rc.veg.2,dist="bray",k=3,trymax=25)
cor(rc.veg,scores(rc.veg.pco))
eigs <- eigenvals(rc.veg.pco)
(eigs/sum(eigs))[1:3]
scrs <- scores(rc.veg.pco)
rc.data.pco <- merge(rc.data,scrs,by="row.names")

rc.data.stm <- rc.data.pco[rc.data.pco$RC.Symbol!="Insufficient Data",]
# Extract rows for plots stable in RC Class I or III and build confidence ellipses
df_ell <- calcEllipse(rc.data.stm[rc.data.stm$RC.Symbol.2=="I" | rc.data.stm$RC.Symbol.2=="IV",c("Dim1","Dim2","RC.Symbol.2")])

#p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.23 <- ggplot(data=rc.data.stm,aes(x=Dim2,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#p.13 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim3))+geom_point(aes(colour=RC.Symbol.2))+
#  ggtitle("Plots for STM 3 by PCO Dimensions")
#grid.arrange(p.12,p.23,p.13,ncol=3)
p.12 <- ggplot(data=rc.data.stm,aes(x=Dim1,y=Dim2))+geom_point(aes(colour=RC.Symbol.2))+facet_wrap(~RC.Symbol.2,scales="fixed")+
  geom_line(aes(group=SITE.PLOT,colour=RC.Symbol.2), arrow=arrow(angle=30,length=unit(2, "mm"),type="closed"))+
  geom_path(data=df_ell, aes(x=x, y=y,colour=group), size=1, linetype=2) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
  ggtitle("Plots for STM 16 by PCO Dimensions 1 and 2")
p.12
