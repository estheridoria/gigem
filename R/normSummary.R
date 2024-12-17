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
normSummary <- function(ExperimentData, readin_summary_dt_final, groups,
                        normalized_factor, controlgeno, controltreat,
                        controllight, controlenviro) {

  # Generate a data table with only the metadata
  norm_keep <- data.table::data.table(readin_summary_dt_final[,1:13])
  norm_keep$batch_name <- ExperimentData@Batch

  for (group in groups) {

    # Calculate normalization factor for use in generating normalized values (everything is normalized to the controls)
    a <- normalized_factor[
      genotype == controlgeno &
        light == controllight &
        environment == controlenviro &
        treatment == controltreat, ..group]

    factor <- as.numeric(a[[1]])

    # Calculate normalized values for the group and assign to new column
    new_col_name <- paste0("norm_", group)
    norm_keep[,new_col_name] <- readin_summary_dt_final[,get(group)] / factor
  }

  # Write the normalized summary data table to a CSV file
  data.table::fwrite(norm_keep, paste("norm_summary_", ExperimentData@Batch, ".csv", sep = ""))

  norm_groups <- paste0("norm_", groups)
  # Run statistical analyses for the normalized data
  summary_norm <- generateSE(dt = norm_keep, groups = norm_groups, Batch = ExperimentData@Batch, norm=TRUE)
  data.table::fwrite(summary_norm,paste("stat_norm_",ExperimentData@Batch,".csv",sep = ""))

  return(norm_keep)
}
