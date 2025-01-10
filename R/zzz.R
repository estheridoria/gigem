#' Setup Environment and Define ExperimentData Class (Internal)
#'
#' This internal function checks if the `ExperimentData` class is already defined.
#' If not, it defines the class to be used for storing experimental data in downstream functions.
#'
#' @return NULL
#' @keywords internal
.onLoad <- function(libname, pkgname) {

  if (!methods::isClass("ExperimentData")) {
  # Define the ExperimentData class to store information
    if (!methods::isClass("data.table")) {
      methods::setClass("data.table", contains = "data.frame")
    }
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


# parent_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(parent_dir)
#
# source("setStatus.R")
# source("runOneBatch.R")
# source("concatenate.R")
# source("writeLoading.R")
# source("activityAndSleep.R")
# source("aliveVsDead.R")
# source("manualDeadRemoval.R")
# source("cleanSummary.R")
# source("genotypePlots.R")
# source("statsSummary.R")
# source("normSummary.R")
# source("generateSE.R")
# source("summarySE.R")
# source("concatenate.R")
# source("corMat.R")
# source("kmeansCluster.R")
# source("normDisplay.R")
# source("plotPreferences.R")
# source("processDays.R")
# source("runAllBatches.R")
# source("concatGenotypePlots.R")
#
#
#

