#' Extract file path from folder
#'
#' \code{get_file} is a function that takes the name of a file and returns
#' the path to the file, if found.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param name Character vector of the name of the file to look for
#'
#' @param ext The file extension *without* a period (.)
#'
#' @param file_num Numeric. In case more than one file matches the \code{name}
#' argument, one can specify which of the files to return. Defaults to one.
#'
#' @return Returns the file path, if the file is not found it returns an error.
#'         If more than one file matches the name, a list of all matches is
#'         also printed to console to facilitate picking the right file_num
#'
#' @export

get_file <- function(name, ext = "fst", file_num = 1) {

  if (missing(name)) stop("File name missing", call. = F)

  path <- fs::path_filter(
    fs::dir_ls(
      type    = "file",
      recurse = T,
      glob    = paste0("*.", ext)
      ),
    regexp = name)

  lpath <- length(path)

  if (lpath == 0) stop("File not found", call. = F)

  if (lpath > 1) {

    out_path <- path[file_num]

    cat(
      glue::glue_col(
        "{red Warning:}
    More than one file matched the name {name}.
    Returning match nr {file_num}: {out_path}"), sep = "\n")

    cat(
      crayon::bold(
        crayon::silver("All matches:")
      ),
      crayon::cyan(path),
      sep = "\n")

    out_path

  } else {
    path
  }
}
