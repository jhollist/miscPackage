#' Calc ww points for booze
#' @export
ww_booze <- function(abv, oz) {
    d <- read.csv(system.file("extdata", "boozepoints.csv", package = "miscPackage"))
    pt_mod <- lm(ww_pts/oz ~ abv, data = d)
    new_d <- data.frame(abv = abv, oz = oz)
    return(round(predict(pt_mod, new_d) * oz))
} 
