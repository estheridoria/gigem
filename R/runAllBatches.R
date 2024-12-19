#' Run All Batches Analysis
#'
#' This function processes all batch directories by sourcing the necessary R files, running
#' the analysis for each batch, and combining the summary results into a single CSV file for all batches.
#' It iterates through directories matching the batch pattern, executes relevant R files, and combines
#' both normalized and general summary statistics from each batch into a final report.
#'
#' @param parent_dir The parent directory where all batch directories are located.
#' @param info A data.table containing experimental information.
#' @param divisions A list of time divisions for the analysis.
#' @param num_days The number of days to consider for the analysis.
#' @param control A character string specifying the control treatment.
#' @param controltreat A character string specifying the control treatment.
#' @param controlgeno A character string specifying the control genotype.
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
#' @importFrom ggplot2 theme facet_grid aes margin mean_cl_boot vars scale_y_continuous element_rect ggplot geom_errorbar geom_point scale_fill_viridis_d ggtitle scale_x_discrete geom_text ggsave scale_color_manual scale_fill_manual labs stat_summary geom_violin ylim geom_smooth scale_shape_manual scale_color_viridis_c coord_cartesian
#' @importFrom ggprism theme_prism
#' @importFrom methods isClass setClass
#' @importFrom plyr ddply
#' @importFrom reshape2 melt
#' @importFrom rlang "!!" sym is_string
#' @importFrom scales percent alpha
#' @importFrom sleepr curate_dead_animals bout_analysis
#' @importFrom stats kmeans
#' @export
#'
#' @details
#' This function performs the following steps for each batch directory:
#' 1. It iterates over each batch directory matching the batch pattern "Batch[0-9]+_[0-9]+".
#' 2. For each batch directory, it sources all R files and runs the analysis via the `runOneBatch` function.
#' 3. It concatenates the normalized and general summary statistics for each batch into separate CSV files.
#' 4. The final CSV files, \code{all_batches_norm_summary.csv} and \code{all_batches_summary.csv},
#'    are saved in the parent directory, containing combined results for all batches.
runAllBatches <- function(controlgeno = NULL, controltreat = NULL,
                          controllight = NULL, controlenviro = NULL, font = "plain") {
  if (is.null(controlgeno) & is.null(controltreat) & is.null(controllight) &is.null(controlenviro)){
    stop("At least one control measure is necessary. Please specify a controlgeno, controltreat, controllight and/or controlenviro.")
  }

  #ask user which plots they want
  pref <- plotPreferences()

  # Get the list of all sub directories
  all_dirs <- list.dirs(parent_dir, full.names = TRUE, recursive = FALSE)

  # Save the current working directory
  original_wd <- getwd()

  run_r_files_in_dir <- function(dir) {

    # Change to the target directory
    setwd(dir)

    # Get the list of R files in the directory
    r_files <- list.files(dir, pattern = "\\.R$", full.names = TRUE)

    # Source each R file
    for (r_file in r_files) {
      source(r_file) # should I take this out? and reformat it to remove source()?
    }
  }

  # Filter directories that match the "Batch" pattern.
  batch_dirs <- grep("Batch[0-9_a-zA-Z]*", all_dirs, value = TRUE)

  if(length(batch_dirs) ==0)
    stop("The folder(s) inside the parent directory containing each Batch's data is either not present or is not formatted correctly. Please add or rename the folder and R file within to follow the format: 'Batch' followed by any combination of letters, numbers and/or underscores.")

  # Iterate over each batch directory and run the R files
  for (batch_dir in batch_dirs){
    run_r_files_in_dir(batch_dir)
    runOneBatch(info, divisions, num_days, pref, controlgeno, controltreat,
                controllight, controlenviro, font)
  }

  # Restore the original working directory
  setwd(original_wd)

  # Concatenate all summaries and combine them into one
  # save in this parent (current) directory.

  all_tables <- list()

  # Iterate over each batch directory and read the summary files
  for (batch_dir in batch_dirs) {
    all_tables <- concatenate(batch_dir, all_tables, "^norm_summary_Batch[0-9_a-zA-Z]*\\.csv$")
  }

  # Concatenate all data frames into one large data frame
  combined_data <- do.call(rbind, all_tables)

  # Save the combined data frame to a CSV file in the parent directory
  output_file <- file.path(parent_dir, "all_batches_norm_summary.csv")
  data.table::fwrite(combined_data, output_file, row.names = FALSE)

  all_tables <- list()

  # Iterate over each batch directory and read the summary files
  for (batch_dir in batch_dirs) {
    all_tables <- concatenate(batch_dir, all_tables, "^summary_Batch[0-9_a-zA-Z]*\\.csv$")
  }

  # Concatenate all data frames into one large data frame
  combined_data <- do.call(rbind, all_tables)
  data.table::setDT(combined_data)

  # Save the combined data frame to a CSV file in the parent directory
  output_file <- file.path(parent_dir, "all_batches_summary.csv")
  data.table::fwrite(combined_data, output_file, row.names = FALSE)

if (pref[7] == 1){
  #concatenate all behavr data tables
  all_tables <- list()

  # Iterate over each batch directory and read the summary files
  for (batch_dir in batch_dirs) {
    all_tables <- concatenate(batch_dir, all_tables, "^sleepdata_Batch[0-9_a-zA-Z]*\\.csv$")
    }

  # Concatenate all data frames into one large data frame
  combined_sleepdata <- do.call(rbind, all_tables)
  data.table::setDT(combined_sleepdata)

  # Save the combined data frame to a CSV file in the parent directory
  output_file <- file.path(parent_dir, "all_sleepdata.csv")
  data.table::fwrite(combined_sleepdata, output_file, row.names = FALSE)


  #concatenate all behavr meta data
  all_tables <- list()

  # Iterate over each batch directory and read the summary files
  for (batch_dir in batch_dirs) {
    all_tables <- concatenate(batch_dir, all_tables, "^sleepmeta_Batch[0-9_a-zA-Z]*\\.csv$")
    }

  # Concatenate all data frames into one large data frame
  combined_sleepmeta <- do.call(rbind, all_tables)
  data.table::setDT(combined_sleepmeta)

  # Save the combined data frame to a CSV file in the parent directory
  output_file <- file.path(parent_dir, "all_sleepmeta.csv")
  data.table::fwrite(combined_sleepmeta, output_file, row.names = FALSE)

  concatGenotypePlots(combined_sleepdata, combined_sleepmeta, summary_dt_final = combined_data, font)
  }
}
