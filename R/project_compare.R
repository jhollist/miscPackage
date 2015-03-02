#' Compare projections
#' 
#' Different projections represent the distortions resulting from projection 
#' spatial data in different ways.  This results in variation of basic 
#' measurements that result solely from the projection chosen.  This function
#' compares the resultant area and/or length measurements that result from 
#' different projections.
#' 
#' @param insp An Spatial* object from the \code{sp} package.  Input CRS must
#'             be defined 
#' @param ...  spTransform arguments
#' 
#' @export
project_compare<-function(insp,...){
  #Need to test for CRS on insp
  
  #Check units on
  insp2<-sp::spTransform(insp,...)
  myList<-list(Original=list(proj=proj4string(insp),
                             area=rgeos::gArea(insp),
                             perim=rgeos::gLength(insp)),
               Projected=list(proj=proj4string(insp2),
                              area=rgeos::gArea(insp2),
                              perim=rgeos::gLength(insp2)))
  return(myList)
}