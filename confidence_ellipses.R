library(ggplot2)
library(dplyr)

# Test data frame with fields for the two variables and fields for the confidence interval width for each variable
df <- data.frame("plot"=c("a","b","c"),"v1"=c(1.23,3.1,2.45),"v1ci"=c(0.2,0.1,0.2),"v2"=c(2.12,2.34,4.6),"v2ci"=c(1,1.5,0.75), stringsAsFactors = F)

### Function for calculating a simple, non-rotated ellipse given a center coordinate and axes in the X and Y direction
simple.ellipse <- function(centerX,centerY,xdim,ydim) {
  out <- data.frame()
  for(t in seq(0,2*pi,by=(2*pi)/360)) {
    # get the radius at each angle
    r = (xdim*ydim)/sqrt((ydim*cos(t))^2+(xdim*sin(t))^2)
    #calc the offset from the radius
    x <- centerX + r*cos(t)
    y <- centerY + r*sin(t)
    # Compile the results
    out <- rbind(out,data.frame("x"=x,"y"=y))
  }
  return(out)
}

### Calculate the confidence ellipses for a data frame
ci.ellipses <- function(df,id="plot",v.1="v1",v.2="v2",v1.ci="v1ci",v2.ci="v2ci"){
  # create an empty data frame for the results
  df.ellipses <- data.frame(stringsAsFactors = F)
  for (i in 1:nrow(df)) {  # Iterate over the rows in the input data frame
    #get a row
    df.i <- df[i,]
    #calc the simple ellipse for that row
    e <- simple.ellipse(select(df.i,one_of(v.1)), select(df.i,one_of(v.2)), select(df.i,one_of(v1.ci)), select(df.i,one_of(v2.ci)))
    #assign the row ID - necessary for the plotting
    e$ID <- df.i[i][1,1]
    #fix the names
    names(e)<-c("x","y","ID")
    #compile the results
    df.ellipses <- rbind(df.ellipses,e)
  }
  return(df.ellipses)
}

test.ellipses <- ci.ellipses(df,"plot","v1","v2","v1ci","v2ci")

ggplot(data=df,aes(x=v1,y=v2))+geom_point()+geom_polygon(data=test.ellipses,aes(x=x,y=y,group=ID),alpha=0.3)

