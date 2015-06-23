#' Get a NLCD from USGS LANDFIRE program
#' 
#' Uses the USGS LANDFIRE NLCD/USGS_EDC_LandCover_NLCD (ImageServer) REST API.  Not sure
#' which year this is returning...
#' 
#' @param bbx a bounding box from \code{sp}
#' @param p4s a proj4string of projection to request image in.
#' @return raster with NLCD colortable
#' @examples
#' \dontrun{
#' data(lake,package="quickmapr")
#' x_nlcd<-get_nlcd(bbox(buffer),proj4string(buffer))
#' }
#' #@keywords internal
#' @export
get_nlcd <- function(bbx=NULL,p4s=NULL){
  server_url<-"http://landfire.cr.usgs.gov/arcgis/rest/services/NLCD/USGS_EDC_LandCover_NLCD/ImageServer/exportImage?"
  xdiff<-abs(bbx[1,1]-bbx[1,2])
  ydiff<-abs(bbx[2,1]-bbx[2,2])
  width<-round(xdiff/30)
  height<-round(ydiff/30)
  bbx_url<-paste("bbox=",bbx[1,1],",",bbx[2,1],",",bbx[1,2],",",bbx[2,2],sep="") 
  format_url<-"&format=tiff"
  pixel_url<-"&pixelType=U8&noDataInterpretation=esriNoDataMatchAny&interpolation=+RSP_BilinearInterpolation"
  file_url<-"&f=image"
  bbx_sr_url<-paste("&bboxSR={'wkt':'",rgdal::showWKT(p4s),"'}",sep="")
  image_sr_url<-paste("&imageSR={'wkt':'",rgdal::showWKT(p4s),"'}",sep="")
  size_url<-paste("&size=",width,",",height,sep="")
  request_url<-paste0(server_url,bbx_url,bbx_sr_url,image_sr_url,size_url,format_url,pixel_url,file_url)
  tmp<-tempfile()
  download.file(request_url,tmp,quiet=TRUE)
  img<-raster(rgdal::readGDAL(tmp,silent=TRUE,p4s=p4s))
  ct<-system.file("extdata/nlcd_colortable.txt",package="miscPackage")
  ct<-readLines(ct)
  img@legend@colortable<-ct
  file.remove(tmp)
  return(img)
}