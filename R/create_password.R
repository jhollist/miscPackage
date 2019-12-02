#' Create a new password
#' 
#' This function creates a new strong passord
#' @param length
#' @export
create_password <- function(length = 10){
  special <- c("!","@","#","$","%","^","&","*")
  low <- sample(letters, 1)
  up <- sample(LETTERS, 1)
  num <- sample(0:9, 1)
  spec <- sample(special, 1)
  rest <- sample(c(letters, LETTERS, 0:9, special),
                 length - 4, replace = TRUE)
  paste0(c(rest, low, up, num, spec), collapse = "")
  
}
