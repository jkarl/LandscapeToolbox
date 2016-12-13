# for invoking grts
stratumdesign<- function(ns, points, oversamp) {
 
  designlist<-list(panel=c(PanelOne=points), seltype="Equal", over=oversamp)
  return(designlist)
}