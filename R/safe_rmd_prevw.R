#' Preview chapter from other location than project folder
#'
#' \code{safe_rmd_prevw} allows the user to preview a book chapter
#' rendered with \code{bookdown} when the chapter .Rmd file is located in
#' a sub-folder away from the project file. It creates a temporary environment
#' and executes the \code{bookdown::preview_chapter()} within this temporary
#' working directory. Furthermore, it stores the output pdf in the same
#' directory.
#' Robust function: FALSE
#'
#' @param folder Path to where .Rmd files are located within project
#' @param chapter Name (with .Rmd extension) of chapter to render
#'
#' @return PDF
#'
#' @importFrom  xfun in_dir
#' @importFrom  bookdown preview_chapter
#'
#' @export
safe_rmd_prevw <- function(chapter = NULL, folder = NULL) {

  if (is.null(folder)) {
    message("No folder passed, defaulting to manuscript")
    folder <- "manuscript"
  }

  if (is.null(chapter)) {
    stop("No chapter passed")
  }

  if (!fs::file_exists(fs::path(folder, chapter, ext = "Rmd"))) {
    error_msg <- glue::glue("No .Rmd file named: crayon::red({chapter}
                            in directory: crayon::yellow({folder})")
    stop(error_msg)
  }

  xfun::in_dir(folder,
               bookdown::preview_chapter(chapter,
                                         output_dir = ".")
  )
}
