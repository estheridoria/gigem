#' Generate Summary Statistics (Internal)
#'
#' This function calculates summary statistics for specified groups using the `summarySE` function and writes
#' the results to a CSV file.
#'
#' @param ExperimentData An S4 object containing experimental data, including a `Batch` attribute.
#' @param dt A data.table containing raw summary statistics to be processed.
#' @param groups A character vector specifying the groups for which to calculate summary statistics.
#' @param norm_factor A character string indicating the factor used for normalization.
#'
#' @return A data.table containing the summary statistics, which is also saved as a CSV file.
#' @keywords internal
statsSummary <- function(ExperimentData, dt, groups, norm_factor) {

  # Define all grouping variables consistently
  group_vars <- c("Sex", "Genotype", "Temperature", "Treatment","Environment","Light", "Batch")

  # Initialize a flag to track the first pass
  n_col <- FALSE
  summary_norm_common <- data.table::data.table() # Initialize as an empty data.table

  # Loop through each group to compute summary statistics
  for (group in groups) {
    # Compute summary statistics for the current group using the dynamically-named summarySE
    # length2 <- function(x, na.rm = FALSE) {
    #   if (FALSE) sum(!is.na(x)) else length(x)
    # }

    # 1. Calculate basic statistics
    # Use plyr::ddply, which is fine, but may return a data.frame or tibble.
    datac <- plyr::ddply(dt, group_vars, .drop = TRUE, .fun = function(xx, col) {
      c(n = length(xx[[col]]),
        mean = mean(xx[[col]], na.rm = F),
        sd = sd(xx[[col]], na.rm = F))
    }, group)

    # 2. Add standard error and confidence interval
    datac$se <- datac$sd / sqrt(datac$n)  # Standard error

    # Critical value (t-score)
    ciMult <- qt(0.95 / 2 + .5, datac$n - 1)
    datac$ci <- datac$se * ciMult

    # 3. Ensure data.table class and perform renaming
    # Set to data.table if it isn't already (important for efficiency/merging in statsSummary)
    data.table::setDT(datac)

    # Dynamic Renaming
    old_names <- c("mean", "sd", "se", "ci")
    new_names <- c(paste(group, "mean", sep = "_"),
                   paste(group, "sd", sep = "_"),
                   paste(group, "se", sep = "_"),
                   paste(group, "ci", sep = "_"))

    # Use data.table::setnames for efficient, in-place renaming
    data.table::setnames(datac, old_names, new_names, skip_absent = TRUE)

    # Merge summary statistics into a common table
    if (n_col) {
      # Merge on all group variables plus n (n should be the same across all metrics for a given group)
      summary_norm_common <- merge(
        summary_norm_common,
        datac,
        by = c(group_vars, "n"), # The dynamically-named columns (e.g., Sleep_Time_L_mean) will be added
        all = TRUE # Ensure all groups are kept even if a metric is NA for some
      )
    } else {
      # First pass: Set the common table and update flag
      summary_norm_common <- datac
      n_col <- TRUE
    }
  }

  # Write the summary data table to a CSV file
  data.table::fwrite(summary_norm_common, paste0("stat_summary_", ExperimentData@Batch, ".csv"))

  # Return the generated data table
  return(summary_norm_common)
}
