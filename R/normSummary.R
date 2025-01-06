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
                        normalized_factor, control) {
#   # determine the column control is in
#   control_col <- readin_summary_dt_final[,8:13]
#   control_col <- names(control_col)[
#   sapply(control_col, function(column) any(grepl(control, column)))
# ]
# llist <- unique(readin_summary_dt_final$light)
# elist <- unique(readin_summary_dt_final$environment)
# glist <- unique(readin_summary_dt_final$genotype)
#
# grp_data <- normalized_factor[grepl("Grp", treatment)]
# iso_data <- normalized_factor[grepl("Iso", treatment)]
#
# # Create a single data.table for control conditions
# # control_conditions <- normalized_factor[
# #   (control_col == "light" & light == control) |
# #     (control_col != "light" & light %in% llist) &
# #     (control_col == "environment" & environment == control) |
# #     (control_col != "environment" & environment %in% elist) &
# #     (control_col == "genotype" & genotype == control) |
# #     (control_col != "genotype" & genotype %in% glist),
# #   .(genotype, environment, light, sex)  # Keep only necessary columns for join
# # ]
#
# for (g in glist){
#   for (e in elist) {
#     for (l in llist) {
#       for (group in groups) {
#         # calculate the group and isolation
#         a<- grp_data[light == l & environment == e & genotype == g, get(group)]
#         b <- iso_data[light == l & environment == e & genotype == g, get(group)]
#             top <- (b-a)/a
#
#             # c <- normalized_factor[
#             #   ((control_col == "light" & light == control) |
#             #      control_col != "light" & light == l) &
#             #     ((control_col == "environment" & environment == control) |
#             #        control_col != "environment" & environment == e) &
#             #     grepl("Grp", treatment) &
#             #     ((control_col == "genotype" & genotype == control) |
#             #        control_col != "genotype" & genotype == g),
#             #   get(group)
#             # ]
#             #
#             # # Calculate d
#             # d <- normalized_factor[
#             #   ((control_col == "light" & light == control) |
#             #      control_col != "light" & light == l) &
#             #     ((control_col == "environment" & environment == control) |
#             #        control_col != "environment" & environment == e) &
#             #     grepl("Iso", treatment) &
#             #     ((control_col == "genotype" & genotype == control) |
#             #        control_col != "genotype" & genotype == g),
#             #   get(group)
#             # ]
#             valid_rows <- normalized_factor[
#               ((control_col == "light" & light == control) | (control_col != "light" & light == l)) &
#                 ((control_col == "environment" & environment == control) | (control_col != "environment" & environment == e)) &
#                 ((control_col == "genotype" & genotype == control) | (control_col != "genotype" & genotype == g))
#             ]
#
#             # Filter for "Grp" and "Iso" within the valid rows
#             c <- valid_rows[grepl("Grp", treatment), get(group)]
#             d <- valid_rows[grepl("Iso", treatment), get(group)]
#             bottom <- (d-c)/c
#
#             new_col_name <- paste0("norm_", group)
#             normalized_factor[genotype == g & environment == e &
#                                 light == l, new_col_name] <- top/bottom
#             }
#           }
#         }
#       }

  # Ensure the required variables are defined
  control_col <- readin_summary_dt_final[, 8:13]
  control_col <- names(control_col)[
    sapply(control_col, function(column) any(grepl(control, column)))
  ]

  llist <- unique(readin_summary_dt_final$light)
  elist <- unique(readin_summary_dt_final$environment)
  glist <- unique(readin_summary_dt_final$genotype)

  grp_data <- normalized_factor[grepl("Grp", treatment)]
  iso_data <- normalized_factor[grepl("Iso", treatment)]

  # Vectorized calculation for normalization
  normalized_factor <- normalized_factor[, {
    # Calculate the group and isolation differences
    a <- grp_data[.SD, on = .(light, environment, genotype), get(group)]
    b <- iso_data[.SD, on = .(light, environment, genotype), get(group)]
    top <- (b - a) / a

    # Filter valid rows
    valid_rows <- .SD[(
      ((control_col == "light" & light == control) | (control_col != "light" & light)) &
        ((control_col == "environment" & environment == control) | (control_col != "environment" & environment)) &
        ((control_col == "genotype" & genotype == control) | (control_col != "genotype" & genotype))
    )]

    # Calculate the bottom value
    c <- valid_rows[grepl("Grp", treatment), get(group)]
    d <- valid_rows[grepl("Iso", treatment), get(group)]
    bottom <- (d - c) / c

    # Assign normalized value
    new_col_name <- paste0("norm_", group)
    .(new_col_name = top / bottom)
  }, by = .(genotype, environment, light, group)]

  # Write the normalized summary data table to a CSV file
normalized_factor[, light := paste0('"', light, '"')]
  data.table::fwrite(normalized_factor, paste("norm_summary_", ExperimentData@Batch, ".csv", sep = ""))

  # norm_groups <- paste0("norm_", groups)
  # # Run statistical analyses for the normalized data
  # summary_norm <- generateSE(dt = norm_keep, groups = norm_groups, Batch = ExperimentData@Batch, norm=TRUE)
  # data.table::fwrite(summary_norm,paste("stat_norm_",ExperimentData@Batch,".csv",sep = ""))
  return(normalized_factor)
}
