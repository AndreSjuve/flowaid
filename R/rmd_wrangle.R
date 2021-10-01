
# del_main() -------------------------------------------------------------------

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

# kill_sumatra() ---------------------------------------------------------------

#' Kill the SumatraPDF task on windows
#'
#' \code{kill_sumatra} is small function that kills the
#' SumatraPDF reader program
#'
#' @param exe Character vector of length one with the name of process to kill -
#'   see windows task manager for the names
#'
#' @return Message if process was killed or not
#'
#' @export

kill_sumatra <- function(exe = NULL) {

  user <- Sys.info()[6]

  if (user == "s12600") {
    exe <- "SUMATR~1.EXE"
  } else if (user == "DrDre") {
    exe <- "SUMATR~1.EXE"
  } else {
    exe <- exe
  }

  if (is.null(exe)) {
    warning("No process supplied, returning NULL", call. = F)
    return(NULL)
  }

  task_to_kill <-
    paste0("taskkill /f /im ", exe)

  system(task_to_kill)
}


# open_paper() -----------------------------------------------------------------

#' Open a pdf
#'
#' \code{open_paper} opens a pdf file to view in pdf reader program
#' If the output from rendering the paper with RMarkdown is located somewhere
#' other than project directory, the pdf will not open automatically.
#'
#' @param filep Character vector with path to file
#'
#' @param pdf_viewer Path to .exe file for pdf program to open PDF with
#'
#' @return Opens pdf
#'
#' @export

open_paper <- function(filep = NULL, pdf_viewer = NULL) {

  if (is.null(filep)) {
    filep <-
      fs::dir_ls(type    = "file",
                 regexp  = "_main.pdf",
                 recurse = TRUE)[1]
  }

  user <- Sys.info()[6]

  if (user == "s12600") {
    pdf_viewer <-
      "C:/Program Files/SumatraPDF/SumatraPDF.exe"
  } else if (user == "DrDre") {
    pdf_viewer <-
      "C:/Program Files/SumatraPDF/SumatraPDF.exe"
  } else {
    pdf_viewer <-
      pdf_viewer
  }

  fs::file_show(path    = filep,
                browser = pdf_viewer)
}


# safe_rmd_prevw() -------------------------------------------------------------

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


# save_version() ---------------------------------------------------------------

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
