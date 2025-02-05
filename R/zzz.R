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
