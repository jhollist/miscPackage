#' sys_perform
#' 
#' This function returns the time it takes to run the following: a big matrix
#' multiplication, a loop without pre-allocated mem, a loop with 
#' pre-allocated memory, and writing a csv to file.  All but the file write 
#' can be tried on mulitple cores.  As I am not a computer scientist, I have
#' no idea if these test are meaningful.  Just things I do a lot and wanted 
#' to see if they work faster on different machines.
#' 
#' @param n the number of times to run the test
#' @param cores the number of cores to test it on
#' 
#' @export
sys_perform<-function(n=10, cores=1){
  for(i in 1:n){
    
    #Matrix Multiply Timining
    mm<-vector("numeric",n)
    mm[i]<-system.time(matrix(rnorm(1000000),1000)%*%matrix(rnorm(1000000),1000))[3]
    
    #Unallocated Loop Timing
    l1<-vector("numeric",n)
    x<-vector()
    l1[i]<-system.time(
      for(i in 1:50000)
      {x[i]<-mean(rnorm(10))}
    )[3]
    
    #Allocated Loop Timing
    l2<-vector("numeric",n)
    x<-vector("numeric",50000)
    l2[i]<-system.time(
      for(i in 1:50000)
      {x[i]<-mean(rnorm(10))}
    )[3]
    
    #Write csv timing
    wr<-vector("numeric",n)
    wr[i]<-system.time(write.csv(data.frame(rnorm(10000),rnorm(10000)),"test.csv"))[3]
    file.remove("test.csv")
  }
  
  return(list(params=c(n=n,cores=cores),
              matrix.multiply=mean(mm),
              unallocated.loop=mean(l1),
              allocated.loop=mean(l2),
              write.csv=mean(wr)))
  
  
}   