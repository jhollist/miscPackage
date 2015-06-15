#' function to download all available shapefile files from a URL
#' @import RCurl
#' @export
#' @examples
#' \dontrun{
#' download_shp("ftp://ftp.granit.sr.unh.edu/pub/GRANIT_Data/Vector_Data/Administrative_and_Political_Boundaries/d-nhsenatedists/2012","NHSenateDists2012")
#' }
download_shp<-function(shape_url,layer,outfolder=".")
{
  #written by: jw hollister
  #Oct 10, 2012
  
  #updated: June 15, 2015
  
  #set-up/clean-up variables
  if(length(grep("/$",shape_url))==0)
  {
    shape_url<-paste(shape_url,"/",sep="")
  }
  #creates vector of all possible shapefile extensions
  shapefile_ext<-c(".shp",".shx",".dbf",".prj",".sbn",".sbx",
                   ".shp.xml",".fbn",".fbx",".ain",".aih",".ixs",
                   ".mxs",".atx",".cpg")
  
  #Check which shapefile files exist
  if(require(RCurl))
  {
    xurl<-getURL(shape_url)
    xlogic<-NULL
    for(i in paste(layer,shapefile_ext,sep=""))
    {
      xlogic<-c(xlogic,grepl(i,xurl))
    }
    
    #Set-up list of shapefiles to download
    shapefiles<-paste(shape_url,layer,shapefile_ext,sep="")[xlogic]
    #Set-up output file names
    outfiles<-paste(outfolder,"/",layer,shapefile_ext,sep="")[xlogic]   
  }
  #Download all shapefiles
  if(sum(xlogic)>0)
  {
    for(i in 1:length(shapefiles))
    {
      x<-suppressWarnings(httr::GET(shapefiles[i],
                                    httr::write_disk(outfiles[i],
                                                     overwrite = TRUE)))
      dwnld_file <- strsplit(shapefiles[i],"/")[[1]]
      dwnld_file <- dwnld_file[length(dwnld_file)]
      print(paste0("Downloaded ", dwnld_file, " to ", outfiles[i],"."))
    }
  } else
  {
    stop("An Error has occured with the input URL
         or name of shapefile")
  }
  }
