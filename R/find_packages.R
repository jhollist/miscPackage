#' Find all packages mentioned in a file
#'  
#' looks for all istances of "libray(package_name)" or "package_name::function".
#' Returns a unique listing of the mentioned packages.
#' 
#' @param file a file to look for packages in
#' @export
#' @examples 
#' find_packages("lessons/01_rstudio.Rmd")
find_packages <- function(file){
  
  lines <- readr::read_lines(file)
  lines_library <- stringr::str_extract(lines, 
                                        'library\\(\\"*[:alnum:]+\\"*\\)')
  lines_library <- lines_library[!is.na(lines_library)]
  lines_library <- stringr::str_remove(lines_library, "library\\(")
  lines_library <- stringr::str_remove(lines_library, "\\)")
  lines_library <- stringr::str_remove(lines_library, '\\"')
  lines_library <- stringr::str_remove(lines_library, '\\"')
  lines_colon <- stringr::str_extract(lines, 
                                      '[:alnum:]+\\:\\:[:alnum:]+')
  lines_colon <- lines_colon[!is.na(lines_colon)]
  lines_colon <- stringr::str_replace(lines_colon, "\\:\\:[:alnum:]+", "")
  unique(c(lines_library, lines_colon))
}