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
#' @return A data.table with selected columns from the normalized summary, also written as a CSV file.
#' @export
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
normSummary <- function(ExperimentData, readin_summary_dt_final, groups,
                        normalized_factor, control) {
  # Ensure necessary variables
  control_col <- readin_summary_dt_final[, 8:13]
  control_col <- names(control_col)[
    sapply(control_col, function(column) any(grepl(control, column)))
  ]
  telist <- unique(readin_summary_dt_final$Temperature)
  llist <- unique(readin_summary_dt_final$Light)
  elist <- unique(readin_summary_dt_final$Environment)
  glist <- unique(readin_summary_dt_final$Genotype)
  slist <- unique(readin_summary_dt_final$Sex)

  grp_data <- normalized_factor[grepl("Grp", Treatment)]
  iso_data <- normalized_factor[grepl("Iso", Treatment)]

  # Combine all combinations of Light, Environment, Genotype, and Sex into a data.table
  combinations <- data.table::CJ(Temperature = telist, Light = llist, Environment = elist, Genotype = glist, Sex = slist)

  # This will hold the results
  result <- data.table::data.table()

  # Loop over groups only
  for (group in groups) {
    # Calculate top and bottom using the combined data.table
    for (i in 1:nrow(combinations)) {
      te <- combinations$Temperature[i]
      l <- combinations$Light[i]
      e <- combinations$Environment[i]
      g <- combinations$Genotype[i]
      s <- combinations$Sex[i]

      # Calculate group and isolation for this combination
      a <- grp_data[Temperature == te & Light == l & Environment == e & Genotype == g, get(group)]
      b <- iso_data[Temperature == te & Light == l & Environment == e & Genotype == g, get(group)]
      top <- (a-b) / a

      valid_rows <- normalized_factor[
        ((control_col == "Temperature" & Temperature == control) | (control_col != "Temperature" & Temperature == te)) &
        ((control_col == "Light" & Light == control) | (control_col != "Light" & Light == l)) &
          ((control_col == "Environment" & Environment == control) | (control_col != "Environment" & Environment == e)) &
          ((control_col == "Genotype" & Genotype == control) | (control_col != "Genotype" & Genotype == g)) &
          ((control_col == "Sex" & Sex == control) | (control_col != "Sex" & Sex == s))
      ]

      # Filter for "Grp" and "Iso" within the valid rows
      c <- valid_rows[grepl("Grp", Treatment), get(group)]
      d <- valid_rows[grepl("Iso", Treatment), get(group)]
      bottom <- (c-d) / c

      # Update the result table with normalized values
      new_col_name <- paste0("norm_", group)
      data.table::set(normalized_factor,
                      i = which(normalized_factor$Genotype == g & normalized_factor$Environment == e &
                                  normalized_factor$Light == l & normalized_factor$Sex == s),
                      j = new_col_name,
                      value = top / bottom)
    }
  }
  # Write the normalized summary data table to a CSV file
normalized_factor[, Light := paste0('"', Light, '"')]
  data.table::fwrite(normalized_factor, paste("norm_summary_", ExperimentData@Batch, ".csv", sep = ""))

  # norm_groups <- paste0("norm_", groups)
  # # Run statistical analyses for the normalized data
  # summary_norm <- generateSE(dt = norm_keep, groups = norm_groups, Batch = ExperimentData@Batch, norm=TRUE)
  # data.table::fwrite(summary_norm,paste("stat_norm_",ExperimentData@Batch,".csv",sep = ""))
  return(normalized_factor)
}
