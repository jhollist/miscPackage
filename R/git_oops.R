#' Git oops...
#'
#' This function uses the git filter-branch method of removing an accidentally 
#' commited BIG file from the git history so that you may get back to happily
#' commiting your little files!  It assumes you want to keep the big file, but 
#' no longer track it.  As such it does not remove the file and adds it to 
#' .gitignore.
#' 
#' @param big_file
#' @param keep_file
#' @param ignore_file
#' @export
git_oops <- function(big_file, keep_file = TRUE, ignore_file = TRUE){
  if(keep_file){
    tmpfile <- tempfile()
    file.copy(big_file, tmpfile)
  }
  if(ignore_file){
    system(paste0("echo ", big_file, " >> .gitignore"))
  }
  browser()
  git_filter_branch <- paste0("git filter-branch --force --index-filter \
  'git rm -r --cached --ignore-unmatch ", big_file, "' \
  --prune-empty --tag-name-filter cat -- --all")
  
  system(git_filter_branch)
  system("git add .gitignore && commit -m 'removing big file'")
  message("A forced push is likely needed. Perhaps 'git push origin master --force' will do the trick.")
  
  if(keep_file){file.copy(tmpfile, big_file)}
}   
