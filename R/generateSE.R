#' Generate Summary Statistics of Normalized Values (Internal)
#'
#' Computes summary statistics (mean, standard deviation, standard error, and confidence interval) for normalized values across specified groups. Merges these statistics into a single summary table.
#'
#' @param dt A `data.table` containing the data for analysis.
#' @param groups A character vector of group names for which summary statistics are calculated.
#' @param Batch A character string representing the batch name for identification.
#' @param norm Logical; if `TRUE`, calculates statistics for normalized values. Default is `FALSE`.
#'
#' @return A `data.table` containing summary statistics (mean, sd, se, ci) for each group.
#' @keywords internal
generateSE <- function(dt, groups, Batch, norm = FALSE) {

  # Initialize a flag to track whether to add the N column
  N_col <- FALSE

  # Loop through each group to compute summary statistics
  for (group in groups) {
    # Compute summary statistics for the current group
    summary_group <- summarySE(dt,
                                measurevar = paste0(group),
                                groupvars = c("sex", "genotype", "treatment", "environment", "light"))

    # Merge summary statistics into a common table
    if (N_col) {
      summary_norm_common <- merge(summary_norm_common, summary_group[, c(1:5, 7:10)],
                                   by = c("sex", "genotype", "treatment", "environment", "light"))
    } else {
      summary_norm_common <- summary_group
      N_col <- TRUE
    }
  }

  return(summary_norm_common)
}
