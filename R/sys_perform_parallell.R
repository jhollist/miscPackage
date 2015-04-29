#' sys_perform
#' 
#' This function returns the time it takes to run the following: a big matrix
#' multiplication, a loop without pre-allocated mem, a loop with 
#' pre-allocated memory, and writing a csv to file. As I am not a computer 
#' scientist, I have no idea if these test are meaningful.  Just things I do a 
#' lot and wanted to see if they work faster on different machines. 
#' 
#' @param n numeric value indicating the number of times to run the test.  Default is 10
#' @param file Character string for file (and path) of output file to write results to
#' 
#' @examples
#' \dontrun{
#' #Following example will write to a shared file on ORD_public
#' #Linux
#' #You will need sudo rights and mount ORD_Public, like:
#' # $sudo mkdir /mnt/ord_public
#' # $sudo mount //V2626UMCEC501.aa.ad.epa.gov/ORD_Public$ -o username=aa\\YOURUSERNAME,
#' #            sec=ntlmsspi,uid=YOURUSERNAME /mnt/ord_public
#' 
#' #Linux Example then is
#' sys_perform(file='/mnt/ord_public/jhollist/R/epa_r_sys_performance.csv')
#' 
#' #Windows
#' 
#' #No need to map, the following should work
#' sys_perform(file='//aa.ad.epa.gov/ORD/ORD/DATA/Public/jhollist/R/epa_r_sys_performance.csv')
#' }
#' @export
#' @import snowfall
#' @import snow
sys_perform_parallel <- function(n = 10, file = NULL, num_core = 1) {
  t1<-proc.time()  
  # validate number of cores
  if (num_core > parallel::detectCores()) {
    return(stop(paste("Your system only has", parallel::detectCores(), 
                      "cores available")))
  }
  
  # Set up single core/sequential
  if (num_core == 1) {
    sfInit(parallel = FALSE)
  }
  
  # Set up multiple cores/parallel
  if (num_core > 1) {
    sfInit(parallel = TRUE, cpus = num_core)
    sfLibrary(miscPackage)
  }
  
  # sys_perform Function to pass to sfLapply
  sf_sys_perform <- function(x) {
    out_sys_perform <- sys_perform(1)
    return(out_sys_perform)
  }
  
  # Run it
  result <- sfLapply(1:n, sf_sys_perform)
  
  # Summarize Results
  result <- data.frame(do.call("rbind",result))
  time <- proc.time()-t1
  result <- with(result,
                 list(User.Name = unique(unlist(User.Name)),
                      R.Version = unique(unlist(R.version)),
                      Machine.Name = unique(unlist(Machine.Name)),
                      OS = unique(unlist(OS)),
                      Memory.GB = unique(unlist(Memory.GB)),
                      Drive = unique(unlist(Drive)),
                      Number.Runs = n,
                      Number.Cores = num_core,
                      matrix.multiply = (sum(unlist(matrix.multiply))/num_core)/n,
                      unallocated.loop = (sum(unlist(unallocated.loop))/num_core)/n,
                      allocated.loop = (sum(unlist(allocated.loop))/num_core)/n,
                      write.csv = (sum(unlist(write.csv))/num_core)/n,
                      avg.time = (sum(unlist(avg.time))/num_core)/n,
                      total.time = time[[3]],
                      date = unlist(date)[1]
                      ))
  # Kill cluster
  sfStop()
  
  return(result)
  
}