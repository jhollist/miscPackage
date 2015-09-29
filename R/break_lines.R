#' Break lines of an input file
#' 
#' Function to break lines at a specified width.  Finds nearest white space to 
#' 80 characters and breaks the line there.
#' 
#' @param txtfile    Path and filename of a file to count the words
#' @param outfile    Output file to save 
#' @param width      integer indicating width at which to set line break.  
#'                   Defaults to 80 characters.
#' @param yaml       Logical indicating whether or not YAML should be included.
#'                   Not yet implemented.
#' @param chunks     Logical indicating whether or not code chunks should be
#'                   included.  Not yet implemented.
#'                  
#' @examples 
#' my_file <- system.file('extdata/test.Rmd',package='miscPackage')
#' word_count(my_file)
#' #@export
word_count <- function(txtfile,outfile,width=80,yaml=FALSE,chunks=FALSE,
                       skip_lines = NULL) {
  con <- file(txtfile, "r", blocking = FALSE)
  x <- readLines(con)
  
  if(yaml){
    x<-break_yaml(x)
  }
  
  if(chunks){
    x<-break_chunks(x)
  }
  
  x<-break_text(x)
  outcon<-file(outfile,'w')
  writeLines(x)
  close(outcon,con)
}
    
#' #@keywords internal
#' add breaks to text
break_text<-function(char_vec,width){
  yaml_lines <- grep("---", char_vec)
  chunk_lines <- grep("```", char_vec)
  skip_lines <- seq(yaml_lines[1],yaml_lines[2])
  if (length(chunk_lines > 0)) {
    if (length(chunk_lines)%%2 != 0) {
      stop("Mismatched code chunks")
    }
    for (i in seq(2, length(chunk_lines), 2)) {
      skip_lines <- c(skip_lines, chunk_lines[i - 1]:chunk_lines[i])
    }
  }
  x<-as.list(char_vec)
  for(i in (1:length(x))[-skip_lines]){
    x[[i]]
  }
  inline_lines <- grep("`", char_vec)
  

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
