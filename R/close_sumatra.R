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
