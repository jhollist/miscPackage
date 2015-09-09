#' Byte Converter
#' 
#' Simple wrapper to object size and format. 
#' 
#' @param obj object to return size for.  Can also be a numeric (assumes bytes).
#' @param units units to convert to.
#' @param num boolean to switch converting an actual value as opposed to `object
#'            size` object (the default)
#' @export
#' 
byter <- function(obj, units = c("b", "Kb", "Mb", "Gb", "B", "KB", "MB", "GB", "auto"), num = FALSE, 
    ...) {
    units <- match.arg(units)
    if (num) {
        class(obj) <- "object_size"
        return(format(obj, units, ...))
    } else {
        return(format(object.size(obj), units, ...))
    }
} 
