#' Generate Combined Genotype Plots (Internal)
#'
#' Creates combined plots for each unique combination of `Light`, `Environment`, and `Genotype` in the dataset.
#' Generates overlay sleep plots and sleep duration plots, saving each combination as a PDF file.
#'
#' @param combined_sleepdata A `data.table` summarized accross all batches containing curated sleep data with columns such as `id` and `asleep`.
#' @param combined_sleepmeta A `data.table` summarized accross all batches containing the metadata associated with combined_sleepdata.
#' @param summary_dt_final A `data.table` containing summary statistics with columns including `Light`, `Environment`,
#'   `Genotype`, `Treatment`, `Sex` and various sleep metrics.
#' @param font A character string variable determining the font style of the produced plots.
#'
#' @details
#' The function iterates through all unique combinations of `Light`, `Environment`, `Treatment`, `Sex`, and `Genotype`.
#' For each combination:
#' - An overlay sleep plot is created using `ggetho`.
#' - Multiple sleep duration plots (e.g., total sleep, daytime sleep, nighttime sleep) are generated
#'   using a helper function, `create_sleeptime_plot`.
#' - Plots are combined into a single figure using `cowplot::plot_grid`.
#' - The combined figure is saved as a PDF file, with the filename reflecting the combination of `Light`,
#'   `Environment`, and `Genotype`.
#'
#' @return None. Plots are saved as PDF files.
#' @keywords internal

concatGenotypePlots <- function(combined_sleepdata, combined_sleepmeta, summary_dt_final, font) {
  data.table::set(summary_dt_final, j = "Light", value = gsub("\"", "", summary_dt_final$Light))

  #link metadata and behavr data
  data.table::setkey(combined_sleepdata, id)
  data.table::setkey(combined_sleepmeta, id)
  dt_curated_final <- behavr::behavr(combined_sleepdata, combined_sleepmeta)

  ExperimentData <- new("ExperimentData",
                        Batch = "MultiBatch",
                        monitorlist = list(),
                        genotypelist = list(),
                        loadinginfo = data.table::data.table())

  genotypePlots(ExperimentData, dt_curated_final, summary_dt_final, font)
}

