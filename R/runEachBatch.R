#' Run the Each Batch Analysis (Internal)
#'
#' This function processes data for each single batch by performing various steps including data cleaning,
#' generating activity and sleep plots, trimming dead animals, calculating summary statistics, and
#' generating normalized statistics.
#'
#' @param control A character string specifying the control.
#' @param num_days A numerical value specifying the number of days to be used in analysis.
#' @param oneBatch A character string of the Batch folder to be analyzed.
#' @param font A string variable determining the font style of the produced plots.
#' @param pref A vector of preferences for generating specific plots (e.g., whether to generate concatenated plots).
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
#' @keywords Internal
runEachBatch <- function(control, num_days, oneBatch, font, pref) {

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
  dt_finalSummary <- cleanSummary(ExperimentData, dt = dt_final, num_days, loadinginfo_linked =loading_metadata, divisions, pref, font)

  if (pref[6] == 1){
    # Generate concatenated plots
    genotypePlots(ExperimentData, dt_final, dt_finalSummary, control, font)
  }

  # Define input column names for normalized statistics
  groups <- c("Sleep_Time_All",
              "Sleep_Time_L",
              "Sleep_Time_D",
              "n_Bouts_L",
              "mean_Bout_Length_L",
              "n_Bouts_D",
              "mean_Bout_Length_D")

  # Calculate the normalization factor for statistics
  norm_factor <- dt_finalSummary[, lapply(.SD, mean),
                                 by = .(Sex, Genotype, Temperature, Treatment,Environment,Light, Batch),
                                 .SDcols = groups]

  # Summary of statistics for sleep time for all groups
  stat_summary <- statsSummary(ExperimentData, dt_finalSummary, groups, norm_factor)

  # if (any(dt_finalSummary[,Treatment] == "Grp") & any(dt_finalSummary[,Treatment ] == "Iso")){
  #   # Calculate normalized sleep loss statistics for all groups
  #   norm_summary <- normSummary(ExperimentData, dt_finalSummary, groups,
  #                               norm_factor,control)
  # }
}
