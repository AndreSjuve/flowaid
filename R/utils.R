
# find_file() ------------------------------------------------------------------

#' Extract file path from folder
#'
#' \code{find_file} is a function that takes the name of a file and returns
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

find_file <- function(name, ext = "fst", file_num = 1) {

  if (missing(name)) stop("File name missing", call. = F)

  path <- fs::path_filter(
    fs::dir_ls(
      type    = "file",
      recurse = TRUE,
      glob    = paste0("*.", ext)
    ),
    regexp = name)

  lpath <- length(path)

  if (lpath == 0) stop("File not found", call. = F)

  if (lpath > 1) {

    out_path <- path[file_num]

    msg <- paste0(
      "Oops: More than one file matched ",
      name,
      ". Returning match nr ",
      file_num,
      ": ",
      out_path, "\n"
      )

    usethis::ui_info(msg)

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


# tidymess() -------------------------------------------------------------------

#' Long user messages in a tidy way
#'
#' \code{tidymess} let's the programmer write longer and more informative
#' messages to the user by ensuring that the message is printed in a tidy way
#' to the console. One can specify which type of message to give: error,
#' warning or message.
#'
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param ... Message to user
#'
#' @param type Character, either "error", "warning" or "message"
#'
#' @param prefix Prefix before message
#'
#' @param initial Initial, ""
#'
#' @return The input message either in form of an error, warning or message.
#'  If the input type does not match either, an error is thrown.
#'
#' @export

tidymess <- function(..., type = "warning", prefix = " ", initial = ""){

  type <- match.arg(type, c("error", "warning", "message"))

  if (type == "error") {
    stop(strwrap(..., prefix = prefix, initial = initial), call. = F)
  } else if (type == "warning") {
    warning(strwrap(..., prefix = prefix, initial = initial), call. = F)
  } else{
    message(strwrap(..., prefix = prefix, initial = initial), call. = F)
  }
}
