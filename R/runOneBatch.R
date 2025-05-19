#' Run the Complete Single Batch Analysis
#'
#' This function processes data for a single batch by performing various steps including data cleaning,
#' generating activity and sleep plots, trimming dead animals, calculating summary statistics, and
#' generating normalized statistics.
#'
#' @param oneBatch A character string of the Batch folder to be analyzed.
#' @param control A character string specifying the control condition for normalization (ex. Canton S Vs to SIP-L1-1).
#' @param num_days A numerical value specifying the number of days to be used in analysis.
#' @param overlayVar A character string specifying which variable to overlay and color plots by Default is "Treatment".
#' @param rowVar A character string specifying which variable to facet rows in plots by. Default is "Genotype".
#' @param columnVar A character string specifying which variable to facet columns in plots by. Default is "Environment".
#' @param font A string variable determining the font style of the produced plots.
#' @export
#'
#' @return This function does not return a value but performs a series of steps to process the data,
#' generate plots, and calculate statistics.
#'
#'
#' @keywords export
runOneBatch <- function(oneBatch, control, num_days, 
                        overlayVar = c("Treatment", "Sex", "Genotype", "Temperature", "Environment", "Light"), 
                        rowVar = c("Genotype", "Sex", "Temperature", "Treatment", "Environment", "Light"), 
                        columnVar = c("Environment", "Sex", "Genotype", "Temperature", "Treatment", "Light"), 
                        font = c("plain", "bold", "italic", "bold.italic")) {
  # Warnings/Errors-------------------------------------------------------------
  if (missing(oneBatch)){
    stop("'oneBatch' must be specified")
  }
  all_dirs <- list.dirs(getwd(), full.names = FALSE, recursive = FALSE)
  if (length(grep(oneBatch, all_dirs)) != 1){
    stop("The 'oneBatch' specified is not a subdirectory inside the current working directory. Please make sure your current directory is correct.")
  }
  if (missing(control)){
    stop("'control' must be specified")
  }
  if (missing(num_days) || !is.numeric(num_days)){
    stop("'num_days' must be specified as a whole number.")
  }
  if(length(unique(c(overlayVar, rowVar, columnVar))) < 3){  # divisions for fascetting plots
    stop("'overlayVar', rowVar, and columnVar cannot contain the same variable names.")
  }
  divisions<- list()
  divisions[1]<- match.arg(overlayVar)
  divisions[2]<- match.arg(rowVar)
  divisions[3]<- match.arg(columnVar)
  font<- match.arg(font)
  

  # Set the stage---------------------------------------------------------------
  
  # Save the current working directory
  original_wd <- getwd() 

  # Change to the target directory
  setwd(paste0(original_wd, "/", oneBatch))

  # Get the list of R files in the directory
  r_files <- list.files(getwd(), pattern = "\\.R$", full.names = TRUE)

  # Source each R file (run info)
  for (r_file in r_files) {
    source(r_file)
  }
  
  # Ask user which plots they want
  pref<- plotPreferences("one")
  
  # Analyze the batch
  runEachBatch(control, num_days, oneBatch, font, pref, divisions)

  setwd(original_wd)

}
