#' function to download all available shapefile files from a URL
#' @import RCurl
#' @export
#' @examples
#' \dontrun{
#' download_shp('ftp://ftp.granit.sr.unh.edu/pub/GRANIT_Data/Vector_Data/Administrative_and_Political_Boundaries/d-nhsenatedists/2012','NHSenateDists2012')
#' download_shp('http://jwhollister.com/iale_open_science/files','LocPt')
#' }
download_shp <- function(shape_url, layer, outfolder = ".") {
    if (length(grep("/$", shape_url)) == 0) {
        shape_url <- paste(shape_url, "/", sep = "")
    }
    
    shapefile_ext <- c(".shp", ".shx", ".dbf", ".prj", ".sbn", ".sbx", ".shp.xml", ".fbn", ".fbx", ".ain", 
        ".aih", ".ixs", ".mxs", ".atx", ".cpg")
    
    xlogic <- NULL
    if (substr(shape_url, 1, 3) == "ftp") {
        xurl <- RCurl::getURL(shape_url)
        for (i in paste(layer, shapefile_ext, sep = "")) {
            xlogic <- c(xlogic, grepl(i, xurl))
        }
    } else if (substr(shape_url, 1, 4) == "http") {
        for (i in paste(shape_url, layer, shapefile_ext, sep = "")) {
            xlogic <- c(xlogic, httr::HEAD(i)$status == 200)
        }
    }
    
    
    shapefiles <- paste(shape_url, layer, shapefile_ext, sep = "")[xlogic]
    outfiles <- paste(outfolder, "/", layer, shapefile_ext, sep = "")[xlogic]
    
    if (sum(xlogic) > 0) {
        for (i in 1:length(shapefiles)) {
            x <- suppressWarnings(httr::GET(shapefiles[i], httr::write_disk(outfiles[i], overwrite = TRUE)))
            
            dwnld_file <- strsplit(shapefiles[i], "/")[[1]]
            dwnld_file <- dwnld_file[length(dwnld_file)]
            
            print(paste0("Downloaded ", dwnld_file, " to ", outfiles[i], "."))
        }
    } else {
        stop("An Error has occured with the input URL or \n              name of shapefile")
    }
} 
