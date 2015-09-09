#' Read Google Scholar Profile and save citation counts to a file
#' 
#' This will read in google scholar Profile.  If the file already exists (i.e. this 
#' has been run before), then a new column is added with the number of new citations
#' @export
#' @examples
#' \dontrun{
#' update_pubs('Fn9BjfIAAAAJ','~/projects/google_scholar.csv')}
update_pubs <- function(id, scholar_csv) {
    if (file.exists(scholar_csv)) {
        y <- scholar::get_publications(id)[, 1:6]
        x <- read.csv(scholar_csv)
        new_cites <- y$cites - x$total_cites
        asof <- format(Sys.Date(), "%b_%d_%Y")
        if (!asof %in% names(x)) {
            assign(asof, new_cites)
            x <- data.frame(x, get(asof))
            names(x)[ncol(x)] <- asof
            write.csv(x, scholar_csv, row.names = FALSE)
        }
    } else {
        x <- scholar::get_publications(id)[, 1:6]
        x <- x[, c(1:4, 6, 5)]
        names(x)[6] <- "total_cites"
        asof <- format(Sys.Date(), "%b_%d_%Y")
        assign(asof, x$total_cites)
        x <- data.frame(x, get(asof))
        names(x)[ncol(x)] <- asof
        write.csv(x, scholar_csv, row.names = FALSE)
    }
} 
