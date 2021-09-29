#' Save paper draft
#'
#' \code{save_version} saves a copy of the manuscript
#' with a custom note appended to the filename by the variable \code{note}.
#' If no note is given the filename is extended by a version number.
#'
#' Used in scripts: {}
#' Robust function: FALSE
#'
#' @param folder Directory to look for manuscript
#'
#' @param title Paper title, if NULL puts project name in front
#'
#' @param note Character to append to filename
#'
#' @return Returns the new file path invisibly
#'
#' @export
#'
#' @importFrom fs file_exists file_copy


save_version <- function(folder = NULL, title = NULL, note = NULL) {

  if (is.null(folder)) {
    tidymess("No folder supplied, using default of
             manuscript", type = "message")

    folder <- "manuscript"
  }

  file_to_save <-
    fs::path(folder, "_main", ext = "pdf")

  if (!fs::file_exists(file_to_save)) {

    msg <- paste0("No new version of the paper was found in ",
                  folder,
                  "so nothing to save.
                  Check that it hasn't ended up somewhere else.
                  Returning NULL")

    tidymess(msg, type = "message")

    return(NULL)
  }

  if (is.null(note)) {

    draft_version <-
      fs::dir_ls(folder, regexp = "draft_\\d+_\\w+") %>%
      length() + 1

    note <- paste0("v", draft_version)

  }

  if (is.null(title)) {
    title <-
      stringr::str_remove(
        fs::path_ext_remove(
          fs::dir_ls(
            ".",
            type = "file",
            regexp = "\\.Rproj")
        ),
        "project_")

  }

  draft_name <-
    Sys.Date() %>%
    as.character() %>%
    paste0("-",
           substr(.data, 9,10),
           substr(.data, 6,7),
           substr(.data, 3,4)) %>%
    stringr::str_split(.data, "-") %>%
    unlist() %>%
    .data[4] %>%
    paste(title, .data, note, sep = "_")

  new_path <-
    fs::path(folder, draft_name, ext = "pdf")

  fs::file_copy(file_to_save, new_path, overwrite = TRUE)

  return(invisible(new_path))
}
