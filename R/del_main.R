#' Delete files created by knitr and markdown when something goes wrong
#'
#' \code{del_main} is a function that deletes output files created for
#' debugging errors when rendering the paper using knitr and rmarkdown.
#' This will also delete _main.tex and _main.pdf. Function should only be
#' used after something has gone wrong with compiling the paper.
#'
#'
#' @param directory Character vector with path to where the error files are
#' stored.
#'
#' @return Deletes the files, nothing is returned
#'
#' @export

del_main <- function(directory = NULL) {

  if (is.null(directory)) {
    directory <- "."
  }

  fs::file_delete(
    fs::dir_ls(regexp = "_main",
               recurse = TRUE)
  )
}
