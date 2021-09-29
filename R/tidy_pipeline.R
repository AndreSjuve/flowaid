#' Tidy pipeline project folder
#'
#' \code{tidy_pipeline()} moves pipeline folders that have
#' names without a matching counterpart in the R/ project folder.
#'
#' @param rpath  Path to project folder with R scripts
#' @param path_pipeline Path to pipeline
#'
#' @return Message when folders are moved
#' @export

tidy_pipeline <- function(rpath = NULL, path_pipeline = NULL) {

  if (is.null(rpath))         rpath         <- fs::path("R")
  if (is.null(path_pipeline)) path_pipeline <- fs::path("pipeline")

  active_scripts <-
    fs::path_ext_remove(
      fs::path_file(
        fs::dir_ls(path = rpath,
                   type = "file")
        )
      )

  active_pipelines <-
    fs::path_file(
      fs::dir_ls(path = path_pipeline,
                 regexp = "_archive",
                 invert = T))

  move_pipelines <-
    active_pipelines[!which(active_pipelines %in% active_scripts)]

  if (length(move_pipelines) == 0) {
    message("There are no inactive folders to move")
  } else {

    old_location <-
      fs::path(
        "pipeline",
        move_pipelines
      )

    new_location <-
      fs::path(
        "pipeline/_archive",
        move_pipelines
      )

    fs::file_move(old_location, new_location)

    msg <-
      move_pipelines %>%
      paste0(.data, " \n", collapse = " ") %>%
      paste0("The following folders were moved to the archives: \n ", .data)

    message(msg)

  }

}

