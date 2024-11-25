#' Generate Normalized Statistics Summary (Internal)
#'
#' Produces a summary data table with normalized statistics for each genotype and writes it as a CSV file.
#' Combines the normalized statistics columns for the specified `groups` with a subset of other columns,
#' keeping key summary data.
#'
#' @param ExperimentData An S4 object containing experimental data, including a `Batch` attribute.
#' @param readin_summary_dt_final A data.table with summary statistics for all genotypes, to be normalized.
#' @param groups A character vector specifying the groups to include in the normalization.
#' @param normalized_factor A data.table with the normalization factor for each genotype.
#' @param controltreat A character string specifying the control treatment for normalizing.
#' @param controlgeno A character string specifying the control genotype for normalizing.
#'
#' @return A data.table with selected columns from the normalized summary, also written as a CSV file.
#' @export
#'
#' @keywords internal
#'
#' @details
#' This function generates a normalized summary of statistics for each genotype by normalizing the values
#' in the specified `groups` using the provided `normalized_factor`. It combines the normalized values for
#' the specified groups with other key summary columns, retaining the first 12 columns and appending the
#' last columns corresponding to the normalized groups. The resulting data table is then written to a CSV file.
#' Additionally, the function runs statistical analyses on the normalized data and writes the results to a
#' separate CSV file.
normSummary <- function(ExperimentData, readin_summary_dt_final, groups, normalized_factor, controlgeno, controltreat) {

  # Generate a list of data tables, one for each group, with normalized values
  DT.list <- generateNorms(ExperimentData,
                           readin_summary_dt_final,
                           normalized_factor,
                           groups,
                           apply_genotype_filter = FALSE,
                           controlgeno,
                           controltreat)
  # Combine tables by keeping the first table entirely and only the last column from each subsequent table
  norm_keep <- do.call(cbind, c(DT.list[1],
                                lapply(DT.list[2:length(DT.list)],
                                       function(x) x[, .SD, .SDcols = ncol(x)]
                                )
  )
  )

  # Select the initial 10 columns and the last columns corresponding to each norm group
  last_cols <- (ncol(norm_keep) - length(groups) + 1):ncol(norm_keep)
  norm_keep <- norm_keep[, c(1:12, last_cols), with = FALSE]
  norm_keep$batch_name <- ExperimentData@Batch

  # Write the normalized summary data table to a CSV file
  data.table::fwrite(norm_keep, paste("norm_summary_", ExperimentData@Batch, ".csv", sep = ""))

  norm_groups <- paste0("norm_", groups)
  # Run statistical analyses for the normalized data
  summary_norm <- generateSE(norm_keep, norm_groups, ExperimentData@Batch, norm=TRUE)
  data.table::fwrite(summary_norm,paste("stat_norm_",ExperimentData@Batch,".csv",sep = ""))

  return(norm_keep)
}
