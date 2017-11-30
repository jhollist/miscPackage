#' Generate a bibliography of packages
#'
#' This function creates an output bibliography from an input character vector
#' of package names.  The primary use case for this function is when working
#' with other authors and you need to provide a reference list of packages used.
#'
#' @param pkgs A character vector of packages
#' @param csl A csl file used to format the output bibliography.  Good place to
#'   look for these is \url{https://github.com/citation-style-language/styles}
#' @param keep_intermediate A logical indicating if you would like to keep the
#'   intermediate \code{.bib} and \code{.md} files.  Most useful if you would
#'   like to get an output \code{.bib} or need to troubleshoot.  Default is
#'   FALSE.
#' @param ... Used to pass arguments to \link[rmarkdown]{render}.  Most useful
#'   for changing output format (e.g. \code{output_format = "word_document"}) or
#'   output file name.
#' @export
#' @examples
#' download.file("https://raw.githubusercontent.com/citation-style-language/styles/master/plos.csl",
#'                 destfile = "plos.csl")
#' bibify_pkgs(c("dplyr","readr"),"plos.csl", output_format = "word_document",
#'             output_file = "my_package_bib.docx")
bibify_pkgs <- function(pkgs, csl, keep_intermediate = FALSE, ...){
  tmpbib <- paste0(tempfile(tmpdir = "."), ".bib")
  con <- file(tmpbib, "w+", encoding = "UTF-8")
  for(i in pkgs){
    ref <- toBibtex(citation(i))
    #adds an id to bib entry
    ref[1] <- gsub("\\{,", paste0("{", i, ","), ref[1])
    #Edit title to maintain capitalization and adds version number.
    ref[2] <- gsub("\\{", "{{", ref[2])
    ref[2] <- gsub("\\}", paste0("(v ",packageVersion(i),")}}"), ref[2])
    writeLines(ref, con)
  }
  close(con)
  
  tmpmd <- "references.md"
  con <- file(tmpmd, "w+", encoding = "UTF-8")
  lines <- paste0("---\n",
                  "bibliography: ", tmpbib, "\n",
                  "csl: ", csl, "\n",
                  "nocite: |\n",
                  "  @*\n",
                  "---")
  writeLines(lines, con)
  close(con)
  
  rmarkdown::render(tmpmd, ...)
  if(!keep_intermediate){
    del <- file.remove(c(tmpbib, tmpmd))
  }
}
