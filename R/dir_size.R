#' Return tibble with directory size
#' 
#' This function calculates total size for a specified directory.
#' 
#' @param path path to return directory size
#' @param ...
#' @export
#' @examples 
#' dir_size()                     
dir_size <- function(path = ".", ...){
  #path <- fs::path(normalizePath("../high_res_cyano"))
  dir_sz <- fs::dir_info(path = path, recurse = TRUE, all = TRUE,fail = FALSE)
  #dir_sizes <- dplyr::mutate(dir_sizes, directory = stringr::str_sub(path, 
  #                                             end = 
  #                                               stringr::str_locate(path, "/")[,2]-1))
  #dir_sizes <- dplyr::select(dir_sizes, directory, size)
  #dir_sizes <- dplyr::group_by(dir_sizes, directory)
  #dir_sizes <- dplyr::summarise(dir_sizes, size = sum(size))
  tibble::tibble(path = path, size = sum(dir_sz$size))
  
}

#' Return tibble with directory sizes
#' 
#' This function calculates total sizes for a vector of directories.
#' 
#' @param paths paths for which to return directory sizes
#' @param ...
#' @export
#' @examples 
#' dir_size()      
dir_sizes <- function(paths, ...){
  dir_szs <- fs::dir_map(paths, fun = dir_size, type = "directory")
  dplyr::bind_rows(dir_szs)
}