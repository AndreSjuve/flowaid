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
