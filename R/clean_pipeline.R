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
