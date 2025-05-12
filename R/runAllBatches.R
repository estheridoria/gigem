#' Run All Batches' Analyses
#'
#' This function processes all batch directories by sourcing the necessary R files, running
#' the analysis for each batch, and combining the summary results into a single CSV file for all batches.
#' It iterates through directories matching the batch pattern, executes relevant R files, and combines
#' both normalized and general summary statistics from each batch into a final report.
#'
#' @param control A character string specifying the control condition for normalization (ex. Canton S Vs to SIP-L1-1).
#' @param num_days A numerical value specifying the number of days to be used in analysis.
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
runAllBatches <- function(control, num_days, font = "plain") {
  # Warnings

  tryCatch({
  #divisions warnings/setting


  if(!exists("divisions", envir = .GlobalEnv)){
    # print("Please select the fascetting divisions for each plot type from the following variables: 'Sex', 'Genotype', 'Temperature', 'Treatment', 'Environment', or 'Light'.")
    # d1 <- readline(prompt = "Enter the variable for the Sleep plots, overlay and color: ")
    # d2 <- readline(prompt = "Enter the variable for the Sleep plots, rows: ")
    # d3 <- readline(prompt = "Enter the variable for the Sleep plots, columns: ")
    # d4 <- readline(prompt = "Enter the variable for the Point plot, overlay and color: ")
    # d5 <- readline(prompt = "Enter the variable for the Point plot, rows: ")
    # d6 <- readline(prompt = "Enter the variable for the Point plot, columns: ")
    # divisions<- c(d1,d2,d3,d4,d5,d6)
    # }
    d1 <- menu(c('Sex', 'Genotype', 'Temperature', 'Treatment', 'Environment','Light'),
               title="Please select the variable
             for determining the plots' OVERLAY and COLOR: ")
    d2 <- menu(c('Sex', 'Genotype', 'Temperature', 'Treatment', 'Environment','Light'),
               title="Please select the variable
             for determining the plots' COLUMNS:")
    d3 <- menu(c('Sex', 'Genotype', 'Temperature', 'Treatment', 'Environment','Light'),
               title="Please select the variable
             for determining the plots' ROWS:")
    divisions<- c(d1,d2,d3)

    labels <- c("Sex", "Genotype", "Temperature", "Treatment", "Environment", "Light")
    # Map the numeric values to the corresponding labels
    divisions <- sapply(divisions, function(x) labels[x])
  }
  if(any(!(divisions[1:3] %in% c("Sex", "Genotype", "Temperature", "Treatment","Environment","Light")))){
    stop("'divisions' entries must be from the variable list: 'Sex', 'Genotype', 'Temperature', 'Treatment', 'Environment', or 'Light'")
  }

  if (!(font %in% c("plain", "bold", "italic","bold.italic"))){
    stop("'font' must be 'plain', 'bold', 'italic', or 'bold.italic'")
  }
  if (missing(control)){
    stop("'control' must be specified")
  }
  if (missing(num_days) || !is.numeric(num_days)){
    stop("'num_days' must be specified as a whole number.")
    }

  #ask user which plots they want
  pref <- plotPreferences("all")

  # Save the current working directory
  original_wd <- getwd()

  # Get the list of all sub directories
  all_dirs <- list.dirs(original_wd, full.names = TRUE, recursive = FALSE)

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

  if(length(batch_dirs) ==0){
    stop("The folder(s) inside the parent directory containing each Batch's data is either not present or is not formatted correctly. Please add or rename the folder and R file within to follow the format: 'Batch' followed by any combination of letters, numbers and/or underscores.")
  }

  # Iterate over each Main file and concatenate to make sure everything is formatted correctly
  all_tables <- list()
  for (batch_dir in batch_dirs) {
    run_r_files_in_dir(batch_dir)
    all_tables[[length(all_tables) + 1]] <- info
  }
    # Custom function to stop on specific warning if columns are not aligned with each batch's main files
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
  # if(!any(unique(combined[,get(divisions[1])]) == control)){
  #   stop("'control' must be a condition within the divisions[1] variable for all batches.")
  # } ## doesn't check for each row and column separation.

  return()

}

setwd(original_wd)

each_dir <- list.dirs(original_wd, full.names = FALSE, recursive = FALSE)

  # Iterate over each batch directory and run the R files to run each batch
  for (oneBatch in each_dir){
    thisBatch <- grep(oneBatch, batch_dirs, value = TRUE)
    run_r_files_in_dir(thisBatch)
    runEachBatch(control, num_days, oneBatch, font, pref, divisions)
  }

#-------------------------------------------------------------------------------
  # Restore the original working directory
  setwd(original_wd)

  # Concatenate all summaries and combine them into one
  # save in this parent (current) directory.

# relative summary concatenate
  all_tables <- list()

  # Iterate over each batch directory and read the summary files
  for (batch_dir in batch_dirs) {
    all_tables <- concatenate(batch_dir, all_tables, "^relative_summary_Batch[0-9_a-zA-Z]*\\.csv$")
  }

  # Concatenate all data frames into one large data frame
  combined_data <- do.call(rbind, all_tables[1:9])

  # Save the combined data frame to a CSV file in the parent directory
  output_file <- file.path(original_wd, "all_batches_relative_summary.csv")
  data.table::fwrite(combined_data, output_file, row.names = FALSE)

# stat concatenate
all_tables <- list()

# Iterate over each batch directory and read the summary files
for (batch_dir in batch_dirs) {
  all_tables <- concatenate(batch_dir, all_tables, "^stat_Batch[0-9_a-zA-Z]*\\.csv$")
}

# Concatenate all data frames into one large data frame
combined_data <- do.call(rbind, all_tables)
data.table::setDT(combined_data)

# Save the combined data frame to a CSV file in the parent directory
output_file <- file.path(original_wd, "all_batches_stat.csv")
data.table::fwrite(combined_data, output_file, row.names = FALSE)

# regular summary concatenate
  all_tables <- list()

  # Iterate over each batch directory and read the summary files
  for (batch_dir in batch_dirs) {
    all_tables <- concatenate(batch_dir, all_tables, "^summary_Batch[0-9_a-zA-Z]*\\.csv$")
  }

  # Concatenate all data frames into one large data frame
  combined_data <- do.call(rbind, all_tables)
  data.table::setDT(combined_data)

  # Save the combined data frame to a CSV file in the parent directory
  output_file <- file.path(original_wd, "all_batches_summary.csv")
  data.table::fwrite(combined_data, output_file, row.names = FALSE)
# # regular ksresults concatenate
#   all_tables <- list()
#   # Iterate over each batch directory and read the summary files
#   for (batch_dir in batch_dirs) {
#     all_tables <- concatenate(batch_dir, all_tables, "ks.results_L.csv$")
#     }
#   # Concatenate all data frames into one large data frame
#   combined_data <- do.call(rbind, all_tables)
#   data.table::setDT(combined_data)
#   # Save the combined data frame to a CSV file in the parent directory
#   output_file <- file.path(original_wd, "all_batches_ks.result_L.csv")
#   data.table::fwrite(combined_data, output_file, row.names = FALSE)
#   # regular ksresults concatenate
#   all_tables <- list()
#   # Iterate over each batch directory and read the summary files
#   for (batch_dir in batch_dirs) {
#     all_tables <- concatenate(batch_dir, all_tables, "ks.results_D.csv$")
#   }
#   # Concatenate all data frames into one large data frame
#   combined_data <- do.call(rbind, all_tables)
#   data.table::setDT(combined_data)
#   # Save the combined data frame to a CSV file in the parent directory
#   output_file <- file.path(original_wd, "all_batches_ks.result_D.csv")
#   data.table::fwrite(combined_data, output_file, row.names = FALSE)
#   
  

# concatenated sleepdata & metadata --> genotypePlots
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
  output_file <- file.path(original_wd, "all_sleepdata.csv")
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
  output_file <- file.path(original_wd, "all_sleepmeta.csv")
  data.table::fwrite(combined_sleepmeta, output_file, row.names = FALSE)
  concatGenotypePlots(combined_sleepdata, combined_sleepmeta, combined_data, control, font, divisions)
}
  setwd(original_wd)

  }, error = function(e) {
    message("An error occurred: ", e$message)
    message("Working directory is: ", getwd())})

}
