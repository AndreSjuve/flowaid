% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmd_wrangle.R
\name{kill_sumatra}
\alias{kill_sumatra}
\title{Kill the SumatraPDF task on windows}
\usage{
kill_sumatra(exe = NULL)
}
\arguments{
\item{exe}{Character vector of length one with the name of process to kill -
see windows task manager for the names}
}
\value{
Message if process was killed or not
}
\description{
\code{kill_sumatra} is small function that kills the
SumatraPDF reader program
}
