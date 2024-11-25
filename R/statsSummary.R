#' Generate Summary Statistics (Internal)
#'
#' This function calculates summary statistics for specified groups using the `GenerateSE` function and writes
#' the results to a CSV file. The summary includes the raw data processed for each group based on the provided
#' normalization factor.
#'
#' @param ExperimentData An S4 object containing experimental data, including a `Batch` attribute.
#' @param dt A data.table containing raw summary statistics to be processed.
#' @param groups A character vector specifying the groups for which to calculate summary statistics.
#' @param norm_factor A character string indicating the factor used for normalization.
#'
#' @return A data.table containing the summary statistics, which is also saved as a CSV file.
#' @keywords internal
statsSummary <- function(ExperimentData, dt, groups, norm_factor) {

  # Generate summary statistics for specified groups
  summary <- generateSE(dt,
                        groups,
                        ExperimentData@Batch,
                        norm = FALSE)

  # Write the summary data table to a CSV file
  data.table::fwrite(summary, paste0("stat_", ExperimentData@Batch, ".csv"))

  return(summary)
}
