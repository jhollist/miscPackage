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
#' 
sys_perform <- function(n = 10, cores = 1, file = NULL) {
    if(parallelly::availableCores("multicore")>1L){
      future::multicore(workers = cores)
    } else {
      future::multisession(workers = cores)
    }
    t1 <- proc.time()
    timings <- future.apply::future_lapply(1:n, function(x){ 
        
        # Matrix Multiply Timining
        mm <- system.time(matrix(rnorm(1e+06), 1000) %*% matrix(rnorm(1e+06), 1000))[3]
        
        # Unallocated Loop Timing
        x <- vector()
        l1 <- system.time(for (j in 1:100000) {
            x[j] <- mean(rnorm(100))
        })[3]
        
        # Allocated Loop Timing
        x <- vector("numeric", 100000)
        l2 <- system.time(for (j in 1:100000) {
            x[j] <- mean(rnorm(100))
        })[3]
        
        # Write csv timing
        tf <- tempfile()
        wr <- system.time(write.csv(data.frame(rnorm(1e+05), rnorm(1e+05)), tf))[3]
        rd <- system.time(read.csv(tf))[3]
        file.remove(tf)
        
        out <- tibble::tibble(matrix_multiply = mean(mm), 
                              unallocated_loop = mean(l1), 
                              allocated_loop = mean(l2), 
                              wrt_csv = mean(wr),
                              rd_csv = mean(rd))
        out
    }, future.seed = NULL)
    if (Sys.info()["sysname"] == "Linux") {
        mem <- strsplit(system("cat /proc/meminfo", T)[1], " ")[[1]]
        mem <- round(as.numeric(mem[(length(mem) - 1)])/1048576)
    } else if (Sys.info()["sysname"] == "Windows") {
        mem <- system("wmic OS get TotalVisibleMemorySize /Value", T)
        mem <- mem[grep("TotalVisibleMemorySize", mem)]
        mem <- round(as.numeric(gsub("[a-zA-Z=\r]", "", mem))/1048576)
    }
    time <- proc.time() - t1
    out <- timings |>
      dplyr::bind_rows() |>
      dplyr::summarise(matrix_multiply = mean(matrix_multiply),
                       unallocated_loop = mean(unallocated_loop),
                       allocated_loop = mean(allocated_loop),
                       wrt_csv = mean(wrt_csv),
                       rd_csv = mean(rd_csv)) |>
      dplyr::mutate(avg_time = time[[3]]/n, 
                    total_time = time[[3]],
                    user_name = Sys.info()["user"], 
                    r_version = R.version$version.string, 
                    machine_name = Sys.info()["nodename"], 
                    os = paste(Sys.info()["sysname"], Sys.info()["release"]), 
                    memory_gb = mem, 
                    drive = getwd(), 
                    number_runs = n,
                    date = lubridate::now(),
                    parallel = cores)
        if (is.null(file) == FALSE) {
        if (file.exists(file)) {
            readr::write_csv(out, file, append = TRUE)
        } else {
            readr::write_csv(out, file)
        }
    }
    future::sequential()
    out
} 
