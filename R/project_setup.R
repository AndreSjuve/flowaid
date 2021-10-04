
# create_project_folders() -------------------------

#' Create folders for different parts of academic project
#'
#' \code{create_project_folders()} is a function that within the project folder
#' creates a set of sub-folders. Currently: data, pipeline, R, manuscript,
#' output and lit. Within each of these it also creates an _archive folder.
#' In the manuscript folder a sub-folder called latex is also created to hold
#' .tex files to include in markdown YAML.
#'
#' @param proj_folder Path to .Rproj folder
#' @param ... Other arguments
#'
#' @return Message if folders are created
#'
#' @export

create_project_folders <- function(proj_folder = NULL, ...) {

  if (is.null(proj_folder)) {
    usethis::ui_stop("Cannot create folders unless a project directory
                     is given")

  }

  folders <-
    c("data", "pipeline", "R", "manuscript", "output", "lit")
  fs::dir_create(proj_folder, folders)
  fs::dir_create(fs::path(proj_folder,
                          folders, "_archive"))
  fs::dir_create(fs::path(proj_folder,
                          folders[which(folders == "manuscript")],
                          "latex"))

  usethis::ui_done("Project folders created")

}


# start_project() -------------------------

#' Initiate R project with defaults
#'
#' \code{start_project()} is a function that does three things:
#' 1) Relies on \code{usethis::create_project()} to setup a new R project.
#' 2) Calls on \code{create_project_folders()} to populate project with
#' relevant folders.
#' 3) Writes a setup script using \code{write_rsetup()}
#'
#' @param dir Path to where project folder should go
#' @param proj_name Character, name of project
#' @param ... Other arguments
#'
#' @return Message if successful
#'
#' @export

start_project <- function(dir = NULL, proj_name = NULL, ...) {

  if (is.null(dir)) {

    dir <- getwd()

    wrn <- paste0("No project directory supplied, defaulting to ",
                  dir)

    usethis::ui_warn(wrn)
  }

  if (is.null(proj_name)) {
    usethis::ui_stop("No project name supplied")
  }

  proj_folder <- fs::path(dir, proj_name)

  # Step 1: Create the project folder

  usethis::create_project(path    = proj_folder,
                          rstudio = TRUE,
                          open    = TRUE)


  usethis::ui_done("Project created")

  # Step 2: Populate the project with relevant folders

  create_project_folders(proj_folder = proj_folder)

  # Step 3: Write ~setup.R file

  write_rsetup(proj_folder = proj_folder, packages = ...)

  # Step 4: Write _render.R file

  write_rrender(proj_folder = proj_folder)
}


#' Write setup script to R/ folder in project directory
#'
#' @param proj_folder Path to .Rproj folder
#' @param packages Character vector of packages to include
#' @param ... Other arguments
#'
#' @return Writes an R file to R folder and gives message if successful
#' @export

write_rsetup <- function(proj_folder = NULL, packages = NULL, ...) {

  if (is.null(proj_folder)) {
    usethis::ui_stop("Can't create setup file without project directory")
  }


  if (is.null(packages)) {

    default_pkgs <-
      c('tidyverse','fs', 'rio',
        'lfe', 'lubridate',
        'magrittr', 'flowaid', 'regaid', 'texaid', 'drdre')

    msg <-
      paste0(
        "No packages specified, defaulting to ",
        paste0(default_pkgs, collapse = ",\n"))

    usethis::ui_oops(msg)

    pkgs <-
      paste0("'",default_pkgs, "'") %>%
      paste0(collapse = ", ")

  } else {

    pkgs <-
      paste0("'",packages, "'") %>%
      paste0(collapse = ", ")

  }


  setup_header <-
    "
  #_______________________________________________________________________________
  # Setup
  # Andre W. Sjuve, andre.sjuve@nhh.no
  # Norwegian School of Economics
  # Created: '24 September, 2021'

  # Description: Loads packages and project specific helper functions
  #_______________________________________________________________________________

  # Packages  ----------- \n
    \n"


  setup_pkgs <-
    paste0(" invisible(
    lapply(c(",pkgs,
    "), \n function(x) {suppressMessages(library(x,  character.only = TRUE))}))
    \n"
    )

  setup_fn_header <- "\n # Functions -----------\n"

  lines <-
    paste0(setup_header, setup_pkgs, setup_fn_header)


  out_path <- fs::path(proj_folder, "R", "~setup", ext = "R")

  writeLines(lines, con = out_path)

  usethis::ui_done("~setup.R written")

}

# write_rrender() --------------------------------------------------------------

#' Write R script that renders the project paper
#'
#' \code{write_rrender()} creates an .R file and saves it in R/ folder of the
#' current project. It contains code to render the .Rmd file index.Rmd that
#' should be located in the subfolder manuscript.
#'
#' @param proj_folder Path .Rproj folder
#' @param ... Other arguments
#'
#' @return Message when file is written
#' @export

write_rrender <- function(proj_folder = NULL, ...) {

  if (is.null(proj_folder)) {
    usethis::ui_stop("Can't create setup file without project directory")
  }

  lines <-
    "
#_______________________________________________________________________________
# Render paper
# André Wattø Sjuve, andre.sjuve@nhh.no
# Norwegian School of Economics
# Created: '04 oktober, 2021'

# Description: This script renders the project paper
#_______________________________________________________________________________

index_dir <- fs::dir_ls(regexp = 'manuscript', type = 'directory')

xfun::in_dir(index_dir,
             bookdown::render_book(
               input         = 'index.Rmd',
               output_format = 'bookdown::pdf_book',
               output_dir    = '.')
)

#_______________________________________________________________________________
# Copyright Andre W. Sjuve 2021 ----
  "

  out_path <- fs::path(proj_folder, "R", "_render", ext = "R")

  writeLines(lines, con = out_path)

  usethis::ui_done("_render.R written")


}



