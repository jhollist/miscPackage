#' Find called packages in a folder
#' 
#' This function searches a folder recursively for packages called by `library()`,
#' `require()`, or `::` that are within R, Rmd, or qmd files.  It returns a 
#' vector of all unique package calls and optionally writes out a "packages.R" file.
#' @export

find_packages_folder <- function(folder, packages_r = TRUE){
  files <- list.files(folder, full.names = TRUE, recursive = TRUE)
  files <- files[stringr::str_detect(files, ".Rmd$|.R$|.qmd$")]
  pkgs <- lapply(files, find_packages)
  pkgs <- unique(unlist(pkgs))
  pkgs <- sort(pkgs)
  if(packages_r){
    pkg_r <- file("packages.R", "w")
    for(i in pkgs){
      cat(paste0("library(", i, ")"), file = pkg_r, append=TRUE, sep = "\n")
    }
    close(pkg_r)
  }
  pkgs
}
