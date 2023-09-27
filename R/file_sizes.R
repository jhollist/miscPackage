#' Return tibble with file sizes
#' 
#' This function returns file sizes for all files in a directory
#' 
#' @param path path to return file sizes
#' @param ...
#' @export
#' @examples 
#' file_sizes()                     
file_sizes <- function(path = ".", ...){
  dir_sz <- fs::dir_info(path = path, recurse = TRUE, all = TRUE, type = "file", 
                         fail = FALSE)
  dir_sz <- dplyr::arrange(dir_sz, dplyr::desc(size))
  dplyr::select(dir_sz, path, size)
}