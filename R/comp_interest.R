#' Compound Interest
#' 
#' @param principal
#' @param apr
#' @param years
#' @param compound
#' @param recurr
#' @export
comp_interest <- function(principal, apr, years, compound, recurr = 0){
  if(recurr == 0){
    total <- principal * (1 + (apr/compound))^(compound*years)
  } else if(recurr > 0){
    total <- principal * (1 + (apr/compound))^(compound*years) +
      (recurr * (((1 + (apr/compound))^(compound*years)-1)/(apr/compound)))
  }
  total
}