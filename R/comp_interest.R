#' Compound Interest
#' 
#' Formula to calculate compound interest with or without recurring payments
#' @param principal starting ammound
#' @param apr annual percentage rate (as a decimal)
#' @param years number of years to compound
#' @param compound frequency at which interest is compounded
#' @param recurr recurring payments made at same frequency as compounding
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