#' Generate Normalized Statistics Summary Specifically of Sleep Loss (Internal)
#'
#' Produces a summary data table with normalized statistics for each Genotype and writes it as a CSV file.
#' Combines the normalized statistics columns for the specified `groups` with a subset of other columns,
#' keeping key summary data.
#'
#' @param ExperimentData An S4 object containing experimental data, including a `Batch` attribute.
#' @param readin_summary_dt_final A data.table with summary statistics for all Genotypes, to be normalized.
#' @param groups A character vector specifying the groups to include in the normalization.
#' @param normalized_factor A data.table with the normalization factor for each Genotype.
#' @param control A character string specifying the control for normalizing.
#'
#' @return None. Save A data.table with selected columns from the relative summary written as a CSV file.
#'
#' @keywords internal
#'
#' @details
#' This function generates a normalized summary of statistics for each Genotype by normalizing the values
#' in the specified `groups` using the provided `normalized_factor`. It combines the normalized values for
#' the specified groups with other key summary columns, retaining the first 12 columns and appending the
#' last columns corresponding to the normalized groups. The resulting data table is then written to a CSV file.
#' Additionally, the function runs statistical analyses on the normalized data and writes the results to a
#' separate CSV file.
relSummary <- function(ExperimentData, readin_summary_dt_final, groups,
                        normalized_factor, control) {
  # define meta variables
  columns_to_consider <- c("Sex", "Genotype", "Temperature", #"Treatment",
                           "Environment", "Light")
  # Specify the columns you want to keep in rel_dat
  keep_cols <- c("Sex", "Genotype", "Temperature", "Treatment",
                 "Environment", "Light", "Batch", groups)


  # Identify the control column by matching the control string
  control_col <- columns_to_consider[
    sapply(columns_to_consider, function(col) any(grepl(control, readin_summary_dt_final[[col]])))
  ]
  # Get all columns *except* the control column
  cols<- columns_to_consider[columns_to_consider != control_col]
  # Get each unique combination of meta variables excluding the control column
  condition_combinations <- unique(readin_summary_dt_final[, ..cols])


  # For each combination, calculate relative values
  rel_results <- condition_combinations[, {

    current_vals <- .SD[1]  # Single row as list

    # Get control data (same meta variables, control value for control_col)
    cont_dat <- normalized_factor[
      Reduce(`&`, lapply(columns_to_consider, function(col) {
        if (col == control_col) {
          normalized_factor[[col]] == control
        } else {
          normalized_factor[[col]] == current_vals[[col]]
        }
      }))
    ]
    cont_dat[Treatment == "Grp"]


    # Get experimental (raw) data
    raw_dat <- readin_summary_dt_final[
      Reduce(`&`, lapply(cols, function(col) {
        readin_summary_dt_final[[col]] == current_vals[[col]]
      })),
      .SD,
      .SDcols = keep_cols
    ]

    # Make a copy of only those columns from raw_dat
    rel_dat <- data.table::copy(raw_dat)

    # Perform the relative calculation only on the 'groups' subset
    # This will generate new columns like "sleep_rel", "activity_rel", etc.

    for (col in groups) {
      rel_dat[, (col) := raw_dat[[col]] / cont_dat[[col]][1]]
    }

    rel_dat
  }, by = seq_len(nrow(condition_combinations))]


  data.table::fwrite(rel_results, paste("relative_summary_", ExperimentData@Batch, ".csv", sep = ""))

  # summary_norm <- generateSE(dt = norm_keep, groups = norm_groups, Batch = ExperimentData@Batch, norm=TRUE)
  # data.table::fwrite(summary_norm,paste("stat_norm_",ExperimentData@Batch,".csv",sep = ""))
}
