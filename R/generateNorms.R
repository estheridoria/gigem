#' Generate Normalized Values (Internal)
#'
#' Computes normalized values for specified groups using a normalization factor and optional genotype filtering.
#'
#' @param ExperimentData An S4 object containing experiment metadata, including a `genotypelist` slot.
#' @param readin_summary_dt_final A `data.table` with summary data to normalize.
#' @param normalized_factor A `data.table` containing normalization factors for each group and genotype.
#' @param groups A character vector of group names for normalization.
#' @param geno A character string specifying the genotype for filtering, if applicable.
#' @param apply_genotype_filter Logical; if `TRUE`, applies filtering based on the `geno` parameter.
#' @param controltreat A character string specifying the control treatment for normalizing.
#' @param controlgeno A character string specifying the control genotype for normalizing.

#'
#' @return A list of `data.table` objects containing the normalized values for each group.
#' @keywords internal
generateNorms <- function(ExperimentData, readin_summary_dt_final, normalized_factor, groups, apply_genotype_filter, controlgeno, controltreat) {
  DT.list <- list()

  for (group in groups) {
    keep <- data.table::data.table()  # Initialize an empty data.table to store results

    for (i in ExperimentData@genotypelist) {

      # Subset data by genotype
      genotype_subset <- readin_summary_dt_final[genotype == i]

      # Calculate normalization factor for the current group and genotype
      if (apply_genotype_filter) {
        a <- normalized_factor[grepl(controltreat, treatment) & genotype == controlgeno, ..group]
      } else {
        a <- normalized_factor[grepl(controltreat, treatment) & genotype == i, ..group]
      }
      
      
      
      factor <- as.numeric(a[[1]])

      # Calculate normalized values for the group and assign to new column
      new_col_name <- paste0("norm_", group)
      #genotype_subset[, (new_col_name) := data.table::.SD[[group]] / factor] replaced with the line below

      data.table::set(genotype_subset, j = new_col_name, value = genotype_subset[[group]] / factor)

      keep <- rbind(keep, genotype_subset)
    }
    DT.list <- append(DT.list, list(keep))
  }

  return(DT.list)
}

