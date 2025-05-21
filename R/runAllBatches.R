#' Run All Batches' Analyses
#'
#' This function processes all batch directories by sourcing the necessary R files, running
#' the analysis for each batch, and combining the summary results into a single CSV file for all batches.
#' It iterates through directories matching the batch pattern, executes relevant R files, and combines
#' both normalized and general summary statistics from each batch into a final report.
#'
#' @param control A character string specifying the control condition for normalization (ex. Canton S Vs to SIP-L1-1).
#' @param num_days A numerical value specifying the number of days to be used in analysis.
#' @param overlayVar A character string specifying which variable to overlay and color plots by Default is "Treatment".
#' @param rowVar A character string specifying which variable to facet rows in plots by. Default is "Genotype".
#' @param columnVar A character string specifying which variable to facet columns in plots by. Default is "Environment".
#' @param font A string variable determining the font style of the produced plots.
#'
#' @return This function does not return a value, but generates and saves two CSV files:
#'         \code{all_batches_norm_summary.csv} and \code{all_batches_summary.csv}.
#' @importFrom behavr rejoin xmv hours days meta
#' @importFrom cluster silhouette clusGap
#' @importFrom cowplot plot_grid
#' @importFrom damr load_dam link_dam_metadata
#' @importFrom data.table fwrite set setDT
#' @importFrom dplyr group_by summarise case_when mutate_all filter select pull rename mutate all_of
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom ggcorrplot cor_pmat ggcorrplot
#' @importFrom ggetho ggetho stat_bar_tile_etho scale_x_days stat_ld_annotations stat_pop_etho
#' @importFrom ggplot2 aes annotate coord_cartesian element_rect facet_grid geom_errorbar geom_point geom_smooth geom_text geom_violin ggplot ggsave ggtitle guide_legend labs margin mean_cl_boot scale_color_manual scale_color_viridis_c scale_fill_manual scale_fill_viridis_d scale_shape_manual scale_x_discrete scale_y_continuous stat_summary theme vars ylim
#' @importFrom ggprism theme_prism
#' @importFrom grDevices dev.off pdf
#' @importFrom grid gpar
#' @importFrom gridExtra grid.arrange
#' @import Hmisc
#' @importFrom magrittr `%>%`
#' @importFrom methods isClass setClass new
#' @importFrom plyr ddply
#' @importFrom reshape2 melt
#' @importFrom rlang "!!" sym is_string
#' @importFrom scales alpha percent pretty_breaks
#' @importFrom sleepr bout_analysis curate_dead_animals
#' @importFrom stats kmeans cor dist p.adjust qt sd t.test
#' @importFrom utils menu read.csv write.csv
#' @export
#'
#' @details
#' This function performs the following steps for each batch directory:
#' 1. It iterates over each batch directory matching the batch pattern "Batch...".
#' 2. For each batch directory, it sources all R files and runs the analysis via the `runOneBatch` function.
#' 3. It concatenates the normalized and general summary statistics for each batch into separate CSV files.
#' 4. The final CSV files, \code{all_batches_norm_summary.csv} and \code{all_batches_summary.csv},
#'    are saved in the parent directory, containing combined results for all batches.
runAllBatches <- function(control, num_days,
                          overlayVar = c("Treatment", "Sex", "Genotype", "Temperature", "Environment", "Light"),
                          rowVar = c("Genotype", "Sex", "Temperature", "Treatment", "Environment", "Light"),
                          columnVar = c("Environment", "Sex", "Genotype", "Temperature", "Treatment", "Light"),
                          font = c("plain", "bold", "italic", "bold.italic")) {
  # Warnings/Errors-------------------------------------------------------------
  if (missing(control)){
    stop("'control' must be specified")
  }
  if (missing(num_days) || !is.numeric(num_days)){
    stop("'num_days' must be specified as a whole number.")
  }
  if(length(unique(c(overlayVar, rowVar, columnVar))) < 3){  # divisions for fascetting plots
    stop("'overlayVar', rowVar, and columnVar cannot contain the same variable names.")
    }
  divisions<- character()
  divisions[1]<- match.arg(overlayVar)
  divisions[2]<- match.arg(rowVar)
  divisions[3]<- match.arg(columnVar)
  #divisions<- c(overlayVar, rowVar, columnVar)
  font<- match.arg(font)

  # Save the current working directory
  original_wd <- getwd()
  # Get the list of all sub directories
  all_dirs <- list.dirs(original_wd, full.names = TRUE, recursive = FALSE)
  # Function to iterate through Meta.r files
  run_r_files_in_dir <- function(dir) {
    setwd(dir) # Change to the target directory
    r_files <- list.files(dir, pattern = "^Main[0-9_a-zA-Z]*\\.R$", full.names = TRUE) # Get the list of R files in the directory
    for (r_file in r_files) {
      source(r_file) # Source each R file
    }
  }
  # Filter directories that match the "Batch" pattern.
  batch_dirs <- grep("Batch[0-9_a-zA-Z]*", all_dirs, value = TRUE)
  # Warning
  if(length(batch_dirs) ==0){
    stop("The folder(s) inside the parent directory containing each Batch's data is either not present or is not formatted correctly. Please add or rename the folder and R file within to follow the format: 'Batch' followed by any combination of letters, numbers and/or underscores.")
  }

  # Warning: Iterate over each Main file and concatenate to make sure everything is formatted correctly
  all_tables <- list()
  for (batch_dir in batch_dirs) {
    run_r_files_in_dir(batch_dir)
    all_tables[[length(all_tables) + 1]] <- info
  }
  # Warning: Custom function to stop on specific warning if columns are not aligned with each batch's main files
  combine_with_warning_check <- function(dt_list) {
    withCallingHandlers({
      # Attempt to combine the data.tables
      combined <- data.table::rbindlist(dt_list, fill = TRUE, use.names = TRUE)

    }, warning = function(w) {
      # Check if the warning contains the specific message
      if (grepl("Item .* has .* rows but longest item has .*; recycled with remainder", conditionMessage(w))) {
        stop("Error: One or more variables in one or more of your
             Main.R files has a different number of conditions than the other variables.
             Please ensure each variable has an equal number of conditions within
             the respective Main.R file(s).")
      }
      if (grepl("Column .* of item .* is missing", conditionMessage(w))) {
        stop("Error: One or more variables is missing from some of your data tables.
             Please ensure all variables are present and named correctly in all Main.R files.")
        }
    })
    for (i in seq_along(all_tables)) {
      current_table <- all_tables[[i]]
      condition1_found <- FALSE
      for (col in names(current_dt)) {
        if (any(current_dt[[col]] == "condition1")) {
          condition1_found <- TRUE
          break # Once found in any column, no need to check further in this table
        }
      }
      if(condition1_found == FALSE){
        stop("'control' must be a condition within each batch.")
      }
    }
    return()
  }

  # Set the stage---------------------------------------------------------------

  # Save the current working directory
  setwd(original_wd)

  # Ask user which plots they want
  pref <- plotPreferences("all")

  # Analyze each batch
  for (oneBatch in batch_dirs){
    run_r_files_in_dir(oneBatch)
    runEachBatch(control, num_days, oneBatch, font, pref, divisions)
  }

  # Restore the original working directory
  setwd(original_wd)

# Concatenate files from batches------------------------------------------------

  # Summaries: relative, stat, summary, & possibly sleep, meta
  concatList<- c("^stat_Batch[0-9_a-zA-Z]*\\.csv$", "^summary_Batch[0-9_a-zA-Z]*\\.csv$") #, "ks.results_L.csv$", "^relative_summary_Batch[0-9_a-zA-Z]*\\.csv$"
  concatNames<- c("all_batches_stat.csv", "all_batches_summary.csv") #, "all_batches_ks.result_L.csv", "all_batches_relative_summary.csv"

  if (pref[7] == 1){ # concatenated sleepdata & metadata --> genotypePlots
  concatList <- c(concatList, "^sleepdata_Batch[0-9_a-zA-Z]*\\.csv$", "^sleepmeta_Batch[0-9_a-zA-Z]*\\.csv$")
  concatNames <- c(concatNames, "all_sleepdata.csv", "all_sleepmeta.csv")
  }

  combined_data_list <- list()

  for (i in seq_along(concatList)){

    all_tables <- list()

    # Iterate over each batch directory and read the concat-specified files
    for (batch_dir in batch_dirs) {

      all_tables <- concatenate(batch_dir, all_tables, concatList[i])
    }

    # Concatenate all data frames into one large data frame
    combined_data <- do.call(rbind, all_tables[1:9])

    if(i == 3){
      summary_dt_final<-combined_data
    }
    if(i == 4){
      combined_sleepdata <-combined_data
    }
    if(i == 5){
      combined_sleepmeta<-combined_data
    }

    # Save the combined data frame to a CSV file in the parent directory
    output_file <- file.path(original_wd, concatNames[i])
    data.table::fwrite(combined_data, output_file, row.names = FALSE)
  }

  setwd(original_wd)

  if(pref[7] ==1){
  concatGenotypePlots(combined_sleepdata, combined_sleepmeta, summary_dt_final, control, font)
  }

}
