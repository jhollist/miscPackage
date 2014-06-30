#' sys_perform
#' 
#' This function returns the time it takes to run the following: a big matrix
#' multiplication, a loop without pre-allocated mem, a loop with 
#' pre-allocated memory, and writing a csv to file. As I am not a computer 
#' scientist, I have no idea if these test are meaningful.  Just things I do a 
#' lot and wanted to see if they work faster on different machines. 
#' 
#' @param n numeric value indicating the number of times to run the test.  Default is 10
#' @param addTo for those on EPA network a boolean indicating wether you want 
#'        the results added to shared csv file. Default is FALSE
#' @param file Character string for file (and path) of output file to write results to
#' 
#' @export
sys_perform <- function(n = 10, addTo = FALSE, file=NULL) {
    mm <- vector("numeric", n)
    l1 <- vector("numeric", n)
    l2 <- vector("numeric", n)
    wr <- vector("numeric", n)
    for (i in 1:n) {
        
        # Matrix Multiply Timining
        mm[i] <- system.time(matrix(rnorm(1e+06), 1000) %*% matrix(rnorm(1e+06), 1000))[3]
        
        # Unallocated Loop Timing
        x <- vector()
        l1[i] <- system.time(for (j in 1:50000) {
            x[j] <- mean(rnorm(10))
        })[3]
        
        # Allocated Loop Timing
        x <- vector("numeric", 50000)
        l2[i] <- system.time(for (j in 1:50000) {
            x[j] <- mean(rnorm(10))
        })[3]
        
        # Write csv timing
        tf<-tempfile()
        wr[i] <- system.time(write.csv(data.frame(rnorm(1e+05), rnorm(1e+05)), tf))[3]
        file.remove(tf)
    }
    if (Sys.info()["sysname"] == "Linux") {
        mem <- strsplit(system("cat /proc/meminfo", T)[1], " ")[[1]]
        mem <- round(as.numeric(mem[(length(mem) - 1)])/1048576)
    } else if (Sys.info()["sysname"] == "Windows") {
        mem <- system("wmic OS get TotalVisibleMemorySize /Value", T)
        mem <- mem[grep("TotalVisibleMemorySize", mem)]
        mem <- round(as.numeric(gsub("[a-zA-Z=\r]", "", mem))/1048576)
    }
    out<-list(User.Name = Sys.info()["user"], R.version = R.version$version.string, Machine.Name = Sys.info()["nodename"], 
              OS = paste(Sys.info()["sysname"], Sys.info()["release"]), Memory.GB = mem, Drive = getwd(), Number.Runs = n, matrix.multiply = mean(mm), 
              unallocated.loop = mean(l1), allocated.loop = mean(l2), write.csv = mean(wr), total.time = sum(mean(mm), mean(l1), mean(l2), 
                                                                                                             mean(wr)))
    if(addTo){
      if(file.exists(file)){
        write.table(out,file,row.names=FALSE,col.names=FALSE,append=TRUE,sep=",")
      } else {
        write.csv(out,file,row.names=FALSE)
      } 
    }
    return(out)
} 
