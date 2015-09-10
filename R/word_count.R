#' Counting Words in R
#' 
#' Function to count words in an Rmd file.  YAML headers and Code Chunks are ignored and 
#' optional lines to skip can be included.
#' 
#' @param txtfile    path and filename of a file to count the words
#' @param skip_lines a numeric vector indicating lines of the file to skip.  
#'                   These lines are removed prior to removing YAML and code chunks.
#'                   Default is NULL.
#'                  
#' @examples 
#' my_file <- system.file('extdata/test.Rmd',package='miscPackage')
#' word_count(my_file)
#' @export
word_count <- function(txtfile,skip_lines = NULL) {
    con <- file(txtfile, "r", blocking = FALSE)
    x <- readLines(con)
    
    #Remove skip_lines
    if(!is.null(skip_lines)){
      x<-x[-skip_lines]
    }
    
    
    # Remove YAML front matter on Rmd
    yaml_lines <- grep("---", x)
    if (length(yaml_lines) > 0) {
        if (length(yaml_lines) != 2) {
            warning("YAML header might be goofy. No YAML removed")
        } else {
          x <- x[-seq(yaml_lines[1], yaml_lines[2])]
        }
    }
    
    # Remove Code chunks
    code_chunk_lines <- grep("```", x)
    if (length(code_chunk_lines > 0)) {
        if (length(code_chunk_lines)%%2 != 0) {
            warning("Mismatched code chunks")
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
