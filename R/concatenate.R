#' Concatenate Summary Tables (Internal)
#'
#' An internal helper function that reads summary CSV files matching a specified pattern from a directory and appends them to a list of tables.
#'
#' @param dir A character string specifying the directory containing the summary files.
#' @param all_tables A list to store and update with the loaded data tables.
#' @param patternname A character string pattern to match the summary CSV files.
#'
#' @return An updated list of data tables with the newly loaded data appended.
#' @keywords internal
concatenate <- function(dir, all_tables, patternname) {
  # Get the list of summary files matching the pattern
  summary_file <- list.files(dir, pattern = patternname, full.names = TRUE)

  # Read each summary CSV file and store in the list
  data <- read.csv(summary_file)
  all_tables[[length(all_tables) + 1]] <- data

  return(all_tables)
}
