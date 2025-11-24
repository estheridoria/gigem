#' Run Complete Single Batch Analysis Workflow
#'
#' This function processes data for each single batch by performing various steps including data cleaning,
#' generating activity and sleep plots, trimming dead animals, calculating summary statistics, and
#' generating normalized statistics.
#'
#' @param numDays A numerical value specifying the number of days to be used in analysis.
#' @param oneBatch A character string of the Batch folder to be analyzed.
#' @param font A string variable determining the font style of the produced plots.
#' @param pref A numeric vector of plot preferences (e.g., pref[1] controls actograms, pref[6] controls combined plots).
#' @param divisions A character vector (length 3) containing the names of the meta-variables used for plot overlay and facetting (e.g., c("Treatment", "Genotype", "Environment")).
#' @param pValues A TRUE/FALSE vector for if combined plots will display p values for 2-condition overlays.
#' @param incodeinfo A data.table or data.frame containing the batch metadata (e.g., Sex, Genotype, Monitor ID) loaded from the "Main" R file.
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
runEachBatch <- function(numDays, oneBatch, font, pref, divisions, pValues, incodeinfo) {

  # Create an object that contains all of your inputs
  ExperimentData <- new("ExperimentData",
                        Batch = unique(incodeinfo$Batch)[1],
                        monitorlist = as.list(unique(incodeinfo$monitor)),
                        genotypelist = as.list(unique(incodeinfo$genotype)),
                        loadinginfo = incodeinfo)

  # Store this as a CSV file, used to curate summary tables
  data.table::fwrite(ExperimentData@loadinginfo, paste("loadinginfo_",ExperimentData@Batch,".csv",sep = ""))
  # Link metadata
  loadinginfo_linked <- damr::link_dam_metadata(ExperimentData@loadinginfo, result_dir = getwd())

  # Create activity plots and sleep plots of each monitor at time t
  dt_activity <- activityAndSleep(ExperimentData, loadinginfo_linked, pref)

  # Create activity plots before and after removing "dead", providing list of IDs removed from first trimming
  dt_curated <- aliveVsDead(ExperimentData, dt_activity)

  # Warning: Custom function to stop on specific warning if columns are not aligned with each batch's main files
  combine_with_warning_check <- function(ExperimentData, dt_curated, numDays, divisions, pref, font) {
    tryCatch({

      # Further removal and trimming of animals that died before specified time, providing list of IDs removed
      result <- manualDeadRemoval(ExperimentData, dt_curated, numDays, divisions, pref, font)

    }, error = function(e) {
      # Check if the error contains the specific message
      if (grepl("Faceting variables must have at least one value", conditionMessage(e))) {
        msg<- paste0("\n",
        "1) the start/stop date(s) within the 'Main' file is/are incorrect OR\n",
        "2) 'numDays' is too many days")
        stop(simpleError(msg))
      }
    })
    return(result)
  }
  dt_final <- combine_with_warning_check(ExperimentData, dt_curated, numDays, divisions, pref, font)

  batchMeta <- behavr::meta(dt_final)

  # Write bout length pdf, and calculate bout and latency stats
  dt_finalSummary <- cleanSummary(ExperimentData, dt = dt_final, batchMeta, numDays, loadinginfo_linked, divisions, pref, font)

  if (pref[6] == 1){
    # Generate concatenated plots
    combinedPlots(ExperimentData, dt_curated_final = dt_final, summary_dt_final = dt_finalSummary, font, divisions, pValues)
  } # dt_curated_final <- dt_final; summary_dt_final <- dt_finalSummary

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

}
