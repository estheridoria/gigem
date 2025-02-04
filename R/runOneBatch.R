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
#' @param font A string variable determining the font style of the produced plots.
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
#' @keywords export
runOneBatch <- function(control, oneBatch, font = "plain", pref = NULL) {

#warn thy user
    if (!(font %in% c("plain", "bold", "italic","bold.italic"))){
    stop("'font' must be 'plain', 'bold', 'italic', or 'bold.italic'")
    }
    if(any(!(divisions[1:6] %in% c("Sex", "Genotype", "Temperature", "Treatment","Environment","Light")))){
      stop("'divisions' entries must be from the parameter list: 'Sex', 'Genotype', 'Temperature', 'Treatment', 'Environment', or 'Light'")
    }
    if (missing(control)){
      stop("'control' must be specified")
    }

# conditionally run set up for singlet batches
                  if (is.null(pref)){
                    #add more warnings copying runAllBatches

                    pref<- plotPreferences("one")

                    # Get the list of all sub directories
                    all_dirs <- list.dirs(getwd(), full.names = FALSE, recursive = FALSE)
                    if (length(grep(oneBatch, all_dirs)) != 1){
                      stop("The 'oneBatch' specified is not a subdirectory inside the current working directory. Please make sure your current directory is correct.")
                    }
                    #
                    # #setwd & check that 'Batch' is valid
                    # singlet <- TRUE
                   }
original_wd <- getwd()

# set the stage
  # Change to the target directory
  setwd(paste0(original_wd, "/", oneBatch))

  # Get the list of R files in the directory
  r_files <- list.files(getwd(), pattern = "\\.R$", full.names = TRUE)

  # Source each R file (run info)
  for (r_file in r_files) {
    source(r_file)
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

  if (pref[6] == 1){
  # Generate concatenated plots
  genotypePlots(ExperimentData, dt_final, dt_finalSummary, font)
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

if (any(dt_finalSummary[,Treatment] == "Grp") & any(dt_finalSummary[,Treatment ] == "Iso")){
  # Calculate normalized sleep loss statistics for all groups
  norm_summary <- normSummary(ExperimentData, dt_finalSummary, groups,
                              norm_factor,control)
}
  # if(exists("singlet") & singlet == TRUE) {
    setwd(original_wd)
    # }
}
