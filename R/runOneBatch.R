#' Run the Complete Batch Analysis (Internal)
#'
#' This function processes data for a single batch by performing various steps including data cleaning,
#' generating activity and sleep plots, trimming dead animals, calculating summary statistics, and
#' generating normalized statistics.
#'
#' @param info A data.table containing experimental information (e.g., monitor, genotype, etc.).
#' @param divisions A list of time divisions for the analysis.
#' @param num_days The number of days to consider for the analysis.
#' @param pref A vector of preferences for generating specific plots (e.g., whether to generate concatenated plots).
#' @param controltreat A character string specifying the control treatment.
#' @param controlgeno A character string specifying the control genotype.
#'
#' @return This function does not return a value but performs a series of steps to process the data,
#' generate plots, and calculate statistics.
#'
#' @details
#' This function executes the following steps:
#' 1. Initializes an `ExperimentData` object containing metadata for the batch.
#' 2. Writes the loading metadata to a CSV file.
#' 3. Generates activity and sleep plots based on the input data.
#' 4. Trims out animals identified as dead using the `aliveVsDead` and `manualDeadRemoval` functions.
#' 5. Creates a final summary of the bout data and latency statistics.
#' 6. Optionally, generates concatenated genotype plots.
#' 7. Computes summary statistics and normalized statistics for sleep time for all groups.
#' 8. Writes relevant output files, including the final summary and normalized statistics.
#'
#' @keywords internal
runOneBatch <- function(info, divisions, num_days, pref, controlgeno, controltreat,
                        controllight, controlenviro, font) {

  # Create an object that contains all of your inputs
  ExperimentData <- new("ExperimentData",
                              Batch = Title,
                              monitorlist = as.list(unique(info$monitor)),
                              genotypelist = as.list(unique(info$genotype)),
                              loadinginfo = info)

  # Store this as a CSV file, used to curate summary tables
  loading_metadata <- writeLoading(ExperimentData)

  # Create activity plots and sleep plots of each monitor at time t
  dt_activity <- activityAndSleep(ExperimentData, loading_metadata, pref)

  # Create activity plots before and after removing "dead", providing list of IDs removed from first trimming
  dt_curated <- aliveVsDead(ExperimentData, dt_activity)

  # Further removal and trimming of animals that died before specified time, providing list of IDs removed
  dt_final <- manualDeadRemoval(ExperimentData, dt_curated, num_days, divisions, pref, font)

  # Write bout length pdf, and calculate bout and latency stats
  dt_finalSummary <- cleanSummary(ExperimentData, dt_final, num_days, loading_metadata, divisions, pref, font)

  if (pref[5] == 1){
  # Generate concatenated plots
  genotypePlots(dt_final, dt_finalSummary, font)
  }

  # Define input column names for normalized statistics
  groups <- c("sleep_time_all",
              "sleep_time_l",
              "sleep_time_d",
              "n_bouts_L",
              "mean_bout_length_L",
              "n_bouts_D",
              "mean_bout_length_D")

  # Calculate the normalization factor for statistics
  norm_factor <- dt_finalSummary[, lapply(.SD, mean),
                                 by = .(genotype, treatment,light,environment),
                                 .SDcols = groups]

  # Summary of statistics for sleep time for all groups
  stat_summary <- statsSummary(ExperimentData, dt_finalSummary, groups, norm_factor)

  # Calculate normalized statistics of sleep time for all groups
  norm_summary <- normSummary(ExperimentData, readin_summary_dt_final = dt_finalSummary, groups,
                              norm_factor, controlgeno, controltreat,
                              controllight, controlenviro)
}
