#' Speed test for read and write
#' 
#' This function records I/O times for the current working directory
#' 
#' @param n Number of times to repeat the read/write operations
#' @param delete_file Delete the random csv generated for this test.  
#' @export
#' @examples 
#' sys_perform_io(5)                     
sys_perform_io <- function(n = 10, delete_file = TRUE){
  df <- data.frame(rnorm(500),rnorm(500),rnorm(500),rnorm(500),rnorm(500),
             rnorm(500),rnorm(500),rnorm(500),rnorm(500),rnorm(500),rnorm(500),
             rnorm(500),rnorm(500),rnorm(500),rnorm(500),rnorm(500),rnorm(500),
             rnorm(500),rnorm(500),rnorm(500),rnorm(500),rnorm(500),rnorm(500),
             rnorm(500),rnorm(500),rnorm(500),rnorm(500),rnorm(500),rnorm(500))
  tmp_file <- tempfile(tmpdir = getwd(), fileext = ".csv")
  
  # write
  write_time <- system.time({
    for(i in seq_along(1:n)){
      write.csv(df,tmp_file)
    }
  })
  
  # read
  read_time <- system.time({
    for(i in seq_along(1:n)){
      df <- read.csv(tmp_file)
    }
  })
  
  if(delete_file){
    unlink(tmp_file)
  }
  
  list(write_time = write_time["elapsed"]/n,
       write_time_total = write_time["elapsed"],
       read_time = read_time["elapsed"]/n,
       read_time_total = read_time["elapsed"],
       total_time = write_time["elapsed"] + read_time["elapsed"],
       n = n)
}