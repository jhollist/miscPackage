#' define an area on a spatial plot and extract selected polygon or 
#' point features
#' 
#' This function utilizes `sp::over` to select features that overlay 
#' on an input polygon.  As no method is defined for SpatialLines, 
#' only Polygons and Points may be selected.
#' 
#' @param in_spatial The sp object from which to extract features
#' @param over_poly A polygon that specifies the features to extract.  
#'                  used in non-interactive mode.  NOT YET IMPLEMENTED
#' @param plot_result Boolean to plot the resulting features and selection 
#'                    poly.
#' @import rgdal, sp                   
#' @export
#' @examples
#'  x <- c(0.5, 0.5, 1.2, 1.5)
#'  y <- c(1.5, 0.5, 0.5, 0.5)
#'  xy <- cbind(x,y)
#'  dimnames(xy)[[1]] <- c("a", "b", "c", "d")
#'  pts <- SpatialPoints(xy)
#'  xpol <- c(0,1,1,0,0)
#'  ypol <- c(0,0,1,1,0)
#'  pol <- SpatialPolygons(list(
#'  Polygons(list(Polygon(cbind(xpol-1.05,ypol))), ID="x1"),
#'  Polygons(list(Polygon(cbind(xpol,ypol))), ID="x2"),
#'  Polygons(list(Polygon(cbind(xpol,ypol-1.05))), ID="x3"),
#'  Polygons(list(Polygon(cbind(xpol+1.05,ypol))), ID="x4"),
#'  Polygons(list(Polygon(cbind(xpol+.4, ypol+.1))), ID="x5")
#'  ))
#'  extract_spatial(pts,T)
#'  extract_spatial(pol,T)

extract_spatial<-function(in_spatial,over_poly=NULL,plot_result=FALSE){
  plot(in_spatial)
  message("Select at least 3 points to define the extent")
  coords<-locator() %>% 
    data.frame()
  poly<-rbind(coords,coords[1,]) %>% 
    Polygon() %>% list() %>% 
    Polygons(ID=1) %>% list() %>% 
    SpatialPolygons(proj4string=CRS(proj4string(in_spatial)))
  idx<-over(poly,in_spatial,returnList=TRUE) %>% unlist()
  if(plot_result){
    plot(in_spatial[idx,])
    plot(poly,add=T)
  }
  return(in_spatial[idx,])
}


