#' Get RStudio Mirror logs
#' 
#' This function downloads the RStudio CRAN mirror logs for a date range to a 
#' specified folder.  Returns a data frame for all downloaded files
#' 
#' @param date_range
#' @param folder
#' @param missing NOT YET IMPLEMENTED
#' @export
#' @examples
#' x<-get_rstudiolog(c(Sys.Date()-5,Sys.Date()-1),folder="rstudio_logs",missing=TRUE)
get_rstudiolog<-function(date_range=c(Sys.Date()-2,Sys.Date()-1),folder=".", 
                         missing=TRUE){
  
  # Code modified from http://cran-logs.rstudio.com/
  all_days <- unique(seq(date_range[1], date_range[2], by = 'day'))
  year <- as.POSIXlt(all_days)$year + 1900
  files<- paste0( all_days, '.csv.gz')
  urls <- paste0('http://cran-logs.rstudio.com/', year, '/', files)
  
  # Checks for existence of folder, creates if doesn't exist
  if(!file.exists(folder)){
    dir.create(folder)
  }
  
  #Only get files missing from folder
  if(missing){
    current_files<-gsub(".gz","",list.files(folder))
    all_files<-gsub(".gz","",files)
    miss<-!all_files%in%current_files
    files<-files[miss]
    urls<-urls[miss]
    if(length(urls)==0){
      warning("All requested logs have already been downloaded.")
    }
  }
  
  # Download and unzip each log
  if(length(urls)>0){
    for(i in 1:length(urls)){
      filename<-paste0(folder,"/",files[i])
      download.file(urls[i],filename)
      R.utils::gunzip(filename,overwrite=TRUE)
    }
  }
  
  # rbind_all
  xdf<-dplyr::rbind_all(lapply(list.files(folder,full.names=TRUE), function(x) read.csv(x, stringsAsFactors = FALSE)))
  return(xdf)
}
