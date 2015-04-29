#' sp bbox to poly
#' @param sp
#' @export
bbox_to_sp<-function(sp){
	bbox <- bbox(sp)
	x <- c(bbox[1,1], bbox[1,1], bbox[1,2], bbox[1,2], bbox[1,1])
	y <- c(bbox[2,1], bbox[2,2], bbox[2,2], bbox[2,1], bbox[2,1])
	p <- Polygon(cbind(x, y))
	ps <- Polygons(list(p), "p1")
	sp <- SpatialPolygons(list(ps), 1L, proj4string = CRS(proj4string(sp)))
	return(sp)
}