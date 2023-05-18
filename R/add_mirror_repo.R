#' Sets up a secondary git remote to push to
#' 
#' This function looks at a current git repository, and sets up tow push 
#' remotes, as origin, to push to.  First push remote is the original origin and
#' second is a new git repo to push to.  The new repo serves as a mirror of the
#' original and is set up as a bare repo it is a local file.
#' 
#' @param mirror_repo The repository that will serve as the mirror.  Maybe a 
#'                    GitHub repo URL or a local file path.  If a local file 
#'                    path, it must not already exist.  Do not use the .git 
#'                    extension
#' @param current_repo The repo you wish to mirror.  Default is to use the current
#'                     folder.  It must be a git repository.
#' @export
#' @examples 
#' mirror <- normalizePath("C:\\Users\\JHollist\\OneDrive - Environmental Protection Agency (EPA)\\projects\\miscPackage", mustWork = FALSE)
#' add_mirror_repo(mirror)                     
add_mirror_repo <- function(mirror_repo = one_drive_mirror(), current_repo = ".",
                            overwrite_remotes = FALSE){
  
  if(!grepl("https://", mirror_repo)){
    if(dir.exists(mirror_repo)){stop(paste("The mirror_repo,", mirror_repo, 
                                           "already exists.  Choose a new name."))}
  }
  
  if(!is_git()){
    stop(paste("The current_repo,", current_repo, "is not a git repo."))
  }
  
  system(paste0("git clone --bare ", current_repo, ' "', mirror_repo, '"'))
  
  current_remotes <- usethis::git_remotes()$origin
  if(is.null(current_remotes)){
    stop("There are no current remotes set for this repo.  Please set a legit remote.")
  }
  
  system(paste0('git remote set-url --add --push origin "', current_remotes, '"'))
  system(paste0('git remote set-url --add --push origin "', mirror_repo, '"'))
}

#' Set up path for a OneDrive remote
#' 
#' Defaults on this are for jeff
#' 
#' @keywords internal
one_drive_mirror <- function(repo_name = basename(getwd()), jeff = TRUE){
  od_path <- normalizePath("~")
  od_path <- gsub("(Profile\\\\Documents)", "", od_path)
  if(jeff){
    return(paste0(od_path, "projects\\", repo_name))
  } else {
    return(paste0(od_path, repo_name))
  }
}

#' Is this a git repo
#' @export
is_git <- function(repo = "."){
  gitty <- system(paste0("git -C ", repo, " rev-parse"), ignore.stdout = TRUE, 
                  ignore.stderr = TRUE)
  gitty == 0
}  