#' Counting Words in R
#' 
#' Function to count words in a text file or a character vector.
#' 
#' @param txt   character that either contains either a vector to count or path 
#'              and filename of a file to count the words
#' @param file  boolean to indicate if txt is a file or not (probably better 
#'              way to implement, but this was quick).
#' @export
word_count<-function(txt,file=FALSE){
  if(file){
    con<-file(txt, "r", blocking=FALSE)
    x<-readLines(con)
    #Remove YAML front matter on Rmd
    if(length(grep("---",x))>0){x<-x[-seq(1,max(grep("---",x)))]}
    wrds<-0
    for(line in x){
      #Removes non character and splits
      split_line<-strsplit(gsub("[^[:alnum:] ]", "", line), " +")[[1]]
      #Removes empty string
      split_line<-split_line[split_line!=""]
      wrds<-wrds+length(split_line)
    }
  } else {
    split_line<-strsplit(gsub("[^[:alnum:] ]", "", txt), " +")[[1]]
    split_line<-split_line[split_line!=""]
    wrds<-length(split_line)
  }
  return(wrds)
}