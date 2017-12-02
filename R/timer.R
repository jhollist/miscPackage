#' timer function
#' a timer
#' @param secs
#' @param beep
#' @export

timer <- function(secs, beep = 3) {
  t1 <- Sys.time()
  elapsed <- 0
  while(elapsed < secs){
    elapsed <- as.numeric(round(difftime(Sys.time(), t1, units = "secs"), 0))
    Sys.sleep(1)
    if (elapsed %% 5 == 0){
      message(paste("Time Elapsed:", elapsed, "secs."), appendLF = TRUE)
    }
  }
  return(beepr::beep(beep))
}
