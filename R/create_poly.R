#' Interactively Create Polygon from active graphics device
#' @param p4s proj.4 string for the new poly.  Should be taken from data showing in current graphics device 
#' @noRD
create_poly <- function(p4s=NULL){
  coords <- data.frame(locator()) 
  poly <- rbind(coords, coords[1, ])
  poly <- list(Polygon(poly)) 
  poly <- SpatialPolygons(list(Polygons(poly, ID = 1)), proj4string = CRS(p4s))
  return(poly)
}