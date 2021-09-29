#' Add script specific folders to pipeline
#'
#' \code{addto_pipeline} creates a folder inside the top-level pipeline folder,
#' ensuring an easy association between .R files and the output from them.
#' A path variable to the folder is also assigned to the global environment.
#'
#' A top-level folder named pipeline is created if it does not exist already.
#'
#' Robust function: TRUE
#'
#' @param name Character, name of script without .R extension
#' @param ... Additional arguments
#'
#' @return Creates folder in project structure and assigns global path to
#'         script sub folder. Returns a message if successful.
#' @export
#'
#' @examples
#' \dontrun{
#' addto_pipeline(name = "my_script")
#' }

addto_pipeline <- function(name = NULL, ...) {

  if (!fs::dir_exists(fs::path("pipeline"))) {
    fs::dir_create("pipeline")
  }

  if (is.null(name) | name == "") {
    stop("Please specify name")
  }

  pipeline <- fs::path("pipeline", name)

  if (!fs::dir_exists(pipeline)) {
    fs::dir_create(pipeline)
  }

  assign(paste0("path_", pipeline), pipeline, envir = parent.frame())

  message("Script folder created and path added to environment")

}
