#' Counting Words in R
#' 
#' Function to count words in a text file or a character vector.
#' 
#' @param txtfile path and filename of a file to count the words
#' @examples 
#' my_file <- system.file('extdata/test.Rmd',package='miscPackage')
#' word_count(my_file)
#' @export
word_count <- function(txtfile) {
    con <- file(txtfile, "r", blocking = FALSE)
    x <- readLines(con)
    
    # Remove YAML front matter on Rmd
    yaml_lines <- grep("---", x)
    if (length(yaml_lines) > 0) {
        if (length(yaml_lines) != 2) {
            stop("YAML header might be goofy.")
        }
        x <- x[-seq(yaml_lines[1], yaml_lines[2])]
    }
    
    # Remove Code chunks
    code_chunk_lines <- grep("```", x)
    if (length(code_chunk_lines > 0)) {
        if (length(code_chunk_lines)%%2 != 0) {
            stop("Mismatched code chunks")
        }
        idx <- NULL
        for (i in seq(2, length(code_chunk_lines), 2)) {
            idx <- c(idx, code_chunk_lines[i - 1]:code_chunk_lines[i])
        }
        x <- x[-idx]
    }
    
    # Count Words
    wrds <- 0
    for (line in x) {
        # Removes non character and splits
        split_line <- strsplit(gsub("[^[:alnum:] ]", "", line), " +")[[1]]
        # Removes empty string
        split_line <- split_line[split_line != ""]
        wrds <- wrds + length(split_line)
    }
    close(con)
    return(wrds)
} 
