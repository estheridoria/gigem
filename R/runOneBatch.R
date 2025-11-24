#' Run the Complete Single Batch Analysis
#'
#' This function processes data for a single batch by performing various steps including data cleaning,
#' generating activity and sleep plots, trimming dead animals, calculating summary statistics, and
#' generating normalized statistics.
#'
#' @param oneBatch A character string of the Batch folder to be analyzed.
#' @param numDays A numerical value specifying the number of days to be used in analysis.
#' @param overlayVar A character string specifying which variable to overlay and color plots by Default is "Treatment".
#' @param rowVar A character string specifying which variable to facet rows in plots by. Default is "Genotype".
#' @param columnVar A character string specifying which variable to facet columns in plots by. Default is "Environment".
#' @param plotSelection A character string specifying if the user wants all possible plots, no optional plots, or select specific plot outputs.
#' @param font A string variable determining the font style of the produced plots.
#' @param pValues A TRUE/FALSE vector for if combined plots will display p values for 2-condition overlays.
#' @export
#'
#' @return This function does not return a value but performs a series of steps to process the data,
#' generate plots, and calculate statistics.
#'
#'
#' @keywords export
runOneBatch <- function(oneBatch, numDays = 2,
                        overlayVar = "Treatment",
                        rowVar = "Genotype",
                        columnVar = "Environment",
                        plotSelection = "All",
                        font = "plain",
                        pValues = FALSE) {
  # Warnings/Errors-------------------------------------------------------------
  if (missing(oneBatch)){
    stop("'oneBatch' must be specified")
  }
  all_dirs <- list.dirs(getwd(), full.names = FALSE, recursive = FALSE)
  if (length(grep(oneBatch, all_dirs)) != 1){
    stop("The 'oneBatch' specified is not a subdirectory inside the current working directory. Please make sure your current directory is correct.")
  }
  if (!is.numeric(numDays)){
    stop("'numDays' must be specified as a whole number.")
  }
  if(length(unique(c(overlayVar, rowVar, columnVar))) < 3){  # divisions for fascetting plots
    stop("'overlayVar', rowVar, and columnVar cannot contain the same variable names.")
  }
  divisions<- character()
  divisions[1]<- match.arg(overlayVar, c("Treatment", "Sex", "Genotype", "Temperature", "Environment", "Light"))
  divisions[2]<- match.arg(rowVar, c("Genotype", "Sex", "Temperature", "Treatment", "Environment", "Light"))
  divisions[3]<- match.arg(columnVar, c("Environment", "Sex", "Genotype", "Temperature", "Treatment", "Light"))
  #divisions<- c(overlayVar, rowVar, columnVar)
  plotSelection <- match.arg(plotSelection, c("All", "None", "Select"))
  font <- match.arg(font, c("plain", "bold", "italic", "bold.italic"))
  if(!is.logical(pValues)){
    stop("'pValues' must be either 'TRUE' or 'FALSE'")
  }
  # Set the stage---------------------------------------------------------------

  # Save the current working directory
  original_wd <- getwd()

  # Change to the target directory
  setwd(paste0(original_wd, "/", oneBatch))

  # Get the list of R files in the directory
  r_files <- list.files(getwd(), pattern = "^Main[0-9_a-zA-Z]*\\.R$", full.names = TRUE)

  # Store all 'info' objects in a list, in case multiple R files exist
  all_info_list <- list()

  # Execute each R file within a temporary local environment
  for (r_file in r_files) {
    # 1. Create a new, temporary environment for execution
    temp_env <- new.env()

    # 2. Execute the script within that environment
    source(r_file, local = TRUE)
    incodeinfo<-info
  }


  # add "monitor" to the info file
  incodeinfo[["monitor"]]<- paste0("M", gsub("\\D", "", incodeinfo$file))
  #change any NA values to "NA" to prevent errors
  incodeinfo[is.na(incodeinfo)] <- "NA"
  incodeinfo[,6:11]<- lapply(incodeinfo[,6:11], as.character)

  if(plotSelection == "All"){
    # Ask user which plots they want
    pref <- c(1,1,1,1,1,1,1)
  }
  if(plotSelection == "None"){
    # Ask user which plots they want
    pref <- c(2,2,2,2,2,2,2)
  }
  if(plotSelection == "Select"){
    # Ask user which plots they want
    pref <- plotPreferences("one")
  }

  # Analyze the batch
  runEachBatch(numDays, oneBatch, font, pref, divisions, pValues, incodeinfo)

  setwd(original_wd)

}
