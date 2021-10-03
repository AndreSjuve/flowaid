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

  assign(paste0("path_", name), pipeline, envir = parent.frame())

  message("Script folder created and path added to environment")

}

#' Delete empty pipeline folders
#'
#' \code{clean_pipeline()} differs from \code{tidy_pipeline()} in that the
#' latter *moves* all pipeline folders without a matching .R file to the
#' *_archive* sub-folder, while \code{clean_pipeline()} deletes all pipeline
#' folders that are empty. Such folders typically exist if a script does not
#' result in any output.
#'
#' *Not very elegant, repeating code, should be rewritten*
#'
#' @param dir_path Directory path
#'
#' @importFrom rlang .data
#'
#' @return Message on which, if any, folders were deleted.
#' @export

clean_pipeline <- function(dir_path = NULL) {

  if (is.null(dir_path)) {
    # There should only be one folder with the name pipeline in project
    # directory
    dir_path <- fs::dir_ls(".", regexp = "pipeline", type = "dir")
  }

  empty_sub_folders <-
    fs::dir_info(dir_path, recurse = TRUE) %>%
    dplyr::select(.data$path, .data$type, .data$size) %>%
    dplyr::filter(.data$type == "directory") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$path) %>%
    dplyr::mutate(n_files = length(fs::dir_ls(.data$path))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$n_files == 0)

  if (nrow(empty_sub_folders) == 0) {
    message("No empty folders found in directory")
  } else {

    msg <-
      paste0(nrow(empty_sub_folders), " sub folders deleted")

    message(msg)

    fs::dir_delete(empty_sub_folders$path)


    empty_head_folders <-
      fs::dir_info(dir_path, recurse = TRUE) %>%
      dplyr::select(c(.data$path, .data$type, .data$size)) %>%
      dplyr::filter(.data$type == "directory") %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$path) %>%
      dplyr::mutate(n_files = length(fs::dir_ls(.data$path))) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$n_files == 0)


    msg <-
      paste0(nrow(empty_head_folders), " head folders deleted")

    message(msg)

    fs::dir_delete(empty_head_folders$path)

  }

}

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
