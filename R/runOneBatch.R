#' Run the Complete Batch Analysis (Export)
#'
#' This function processes data for a single batch by performing various steps including data cleaning,
#' generating activity and sleep plots, trimming dead animals, calculating summary statistics, and
#' generating normalized statistics.
#'
#' @param info A data.table containing experimental information (e.g., monitor, genotype, etc.).
#' @param divisions A list of time divisions for the analysis.
#' @param num_days The number of days to consider for the analysis.
#' @param pref A vector of preferences for generating specific plots (e.g., whether to generate concatenated plots).
#' @param control A character string specifying the control.
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
#' @keywords export
runOneBatch <- function(info = NULL, divisions, num_days, pref = NULL, control, font = "plain") {

    if (!(font %in% c("plain", "bold", "italic","bold.italic"))){
    stop("'font' must be 'plain', 'bold', 'italic', or 'bold.italic'")
    }

  if (is.null(pref)){
    #warnings
    #ask user which plots they want
    pref <- plotPreferences("one")


    #add more warnings copying runAllBatches

    # Save the current working directory
    original_wd <- getwd()
    # Change to the target directory
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))

    # Get the list of R files in the directory
    r_files <- list.files(dir, pattern = "\\.R$", full.names = TRUE)

    # Source each R file (run info)
    for (r_file in r_files) {
      source(r_file) # should I take this out? and reformat it to remove source()?
    }

  }

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

  if (pref[5] == 1){
  # Generate concatenated plots
  genotypePlots(dt_final, dt_finalSummary, font)
  }

  # Define input column names for normalized statistics
  groups <- c("sleep_time_All",
              "sleep_time_L",
              "sleep_time_D",
              "n_bouts_L",
              "mean_bout_length_L",
              "n_bouts_D",
              "mean_bout_length_D")

  # Calculate the normalization factor for statistics
  norm_factor <- dt_finalSummary[, lapply(.SD, mean),
                                 by = .(sex, genotype, temperature, treatment,environment,light, Batch),
                                 .SDcols = groups]

  # Summary of statistics for sleep time for all groups
  stat_summary <- statsSummary(ExperimentData, dt_finalSummary, groups, norm_factor)

if (any(dt_finalSummary[,treatment] == "Grp") & any(dt_finalSummary[,treatment ] != "Iso")){
  # Calculate normalized sleep loss statistics for all groups
  norm_summary <- normSummary(ExperimentData, dt_finalSummary, groups,
                              norm_factor,control)
}

  if(is.null(pref)){
    setwd(original_wd)
  }

}
