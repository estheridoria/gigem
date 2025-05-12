#' Setup Environment and Define ExperimentData Class (Internal)
#'
#' This internal function checks if the `ExperimentData` class is already defined.
#' If not, it defines the class to be used for storing experimental data in downstream functions.
#'
#' @return NULL
#' @keywords internal
.onLoad <- function(libname, pkgname) {

  if (!methods::isClass("ExperimentData")) {
  methods::setClass(
    Class = "ExperimentData",
    slots = list(
      Batch = "character",
      monitorlist = "list",
      genotypelist = "list",
      loadinginfo = "data.table"
    )
  )
  }

}
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#
#   # Get the list of R files in the directory
#   r_files <- list.files(getwd(), pattern = "\\.R$", full.names = TRUE)
#   # Source each R file
#   for (r_file in r_files) {
#     source(r_file) # should I take this out? and reformat it to remove source()?
#   }
