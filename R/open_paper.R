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
