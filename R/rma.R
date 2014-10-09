#' rma
#' 
#' This function cleans up the current environment.  Simply a shortcut for rm(list=ls()).
#' 
#' @export
#' 
rma<-function(){
  if(readline("Delete all objects in memory? 1 to continue, any other key to cancel: ")=="1"){
    rm(list=ls())
  }
}