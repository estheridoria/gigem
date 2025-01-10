#' Generate Normalized Statistics Summary Specifically of Sleep Loss (Internal)
#'
#' Produces a summary data table with normalized statistics for each genotype and writes it as a CSV file.
#' Combines the normalized statistics columns for the specified `groups` with a subset of other columns,
#' keeping key summary data.
#'
#' @param ExperimentData An S4 object containing experimental data, including a `Batch` attribute.
#' @param readin_summary_dt_final A data.table with summary statistics for all genotypes, to be normalized.
#' @param groups A character vector specifying the groups to include in the normalization.
#' @param normalized_factor A data.table with the normalization factor for each genotype.
#' @param control A character string specifying the control for normalizing.
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
                        normalized_factor, control) {
  # Ensure necessary variables
  control_col <- readin_summary_dt_final[, 8:13]
  control_col <- names(control_col)[
    sapply(control_col, function(column) any(grepl(control, column)))
  ]
  telist <- unique(readin_summary_dt_final$temperature)
  llist <- unique(readin_summary_dt_final$light)
  elist <- unique(readin_summary_dt_final$environment)
  glist <- unique(readin_summary_dt_final$genotype)
  slist <- unique(readin_summary_dt_final$sex)

  grp_data <- normalized_factor[grepl("Grp", treatment)]
  iso_data <- normalized_factor[grepl("Iso", treatment)]

  # Combine all combinations of light, environment, genotype, and sex into a data.table
  combinations <- data.table::CJ(temperature = telist, light = llist, environment = elist, genotype = glist, sex = slist)

  # This will hold the results
  result <- data.table::data.table()

  # Loop over groups only
  for (group in groups) {
    # Calculate top and bottom using the combined data.table
    for (i in 1:nrow(combinations)) {
      te <- combinations$temperature[i]
      l <- combinations$light[i]
      e <- combinations$environment[i]
      g <- combinations$genotype[i]
      s <- combinations$sex[i]

      # Calculate group and isolation for this combination
      a <- grp_data[temperature == te & light == l & environment == e & genotype == g, get(group)]
      b <- iso_data[temperature == te & light == l & environment == e & genotype == g, get(group)]
      top <- (a-b) / a

      valid_rows <- normalized_factor[
        ((control_col == "temperature" & temperature == control) | (control_col != "temperature" & temperature == te)) &
        ((control_col == "light" & light == control) | (control_col != "light" & light == l)) &
          ((control_col == "environment" & environment == control) | (control_col != "environment" & environment == e)) &
          ((control_col == "genotype" & genotype == control) | (control_col != "genotype" & genotype == g)) &
          ((control_col == "sex" & sex == control) | (control_col != "sex" & sex == s))
      ]

      # Filter for "Grp" and "Iso" within the valid rows
      c <- valid_rows[grepl("Grp", treatment), get(group)]
      d <- valid_rows[grepl("Iso", treatment), get(group)]
      bottom <- (c-d) / c

      # Update the result table with normalized values
      new_col_name <- paste0("norm_", group)
      data.table::set(normalized_factor,
                      i = which(normalized_factor$genotype == g & normalized_factor$environment == e &
                                  normalized_factor$light == l & normalized_factor$sex == s),
                      j = new_col_name,
                      value = top / bottom)
    }
  }
  # Write the normalized summary data table to a CSV file
normalized_factor[, light := paste0('"', light, '"')]
  data.table::fwrite(normalized_factor, paste("norm_summary_", ExperimentData@Batch, ".csv", sep = ""))

  # norm_groups <- paste0("norm_", groups)
  # # Run statistical analyses for the normalized data
  # summary_norm <- generateSE(dt = norm_keep, groups = norm_groups, Batch = ExperimentData@Batch, norm=TRUE)
  # data.table::fwrite(summary_norm,paste("stat_norm_",ExperimentData@Batch,".csv",sep = ""))
  return(normalized_factor)
}
