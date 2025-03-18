#' Run the Complete Single Batch Analysis (Export)
#'
#' This function processes data for a single batch by performing various steps including data cleaning,
#' generating activity and sleep plots, trimming dead animals, calculating summary statistics, and
#' generating normalized statistics.
#'
#' @param oneBatch A character string of the Batch folder to be analyzed.
#' @param control A character string specifying the control.
#' @param num_days A numerical value specifying the number of days to be used in analysis.
#' @param font A string variable determining the font style of the produced plots.
#' @export
#'
#' @return This function does not return a value but performs a series of steps to process the data,
#' generate plots, and calculate statistics.
#'
#'
#' @keywords export
runOneBatch <- function(oneBatch, control, num_days, font = "plain") {

#warn thy user

  #divisions warnings/setting
  if(!exists("divisions", envir = .GlobalEnv)){
    print("Please select the fascetting divisions for each plot type from the following parameters: 'Sex', 'Genotype', 'Temperature', 'Treatment', 'Environment', or 'Light'.")
    d1 <- readline(prompt = "Enter the parameter for the Sleep plots, overlay and color: ")
    d2 <- readline(prompt = "Enter the parameter for the Sleep plots, rows: ")
    d3 <- readline(prompt = "Enter the parameter for the Sleep plots, columns: ")
    d4 <- readline(prompt = "Enter the parameter for the Point plot, overlay and color: ")
    d5 <- readline(prompt = "Enter the parameter for the Point plot, rows: ")
    d6 <- readline(prompt = "Enter the parameter for the Point plot, columns: ")
    divisions<- c(d1,d2,d3,d4,d5,d6)
  }
    if(any(!(divisions[1:6] %in% c("Sex", "Genotype", "Temperature", "Treatment","Environment","Light")))){
      stop("'divisions' entries must be from the parameter list: 'Sex', 'Genotype', 'Temperature', 'Treatment', 'Environment', or 'Light'")
    }

  if (missing(oneBatch)){
    stop("'oneBatch' must be specified")
  }

    if (missing(control)){
      stop("'control' must be specified")
    }

  if (missing(num_days) || !is.numeric(num_days)){
    stop("'num_days' must be specified as a whole number.")
  }

  if (!(font %in% c("plain", "bold", "italic","bold.italic"))){
    stop("'font' must be 'plain', 'bold', 'italic', or 'bold.italic'")
  }

  #ask user which plots they want
  pref<- plotPreferences("one")

  # Get the list of all sub directories
  all_dirs <- list.dirs(getwd(), full.names = FALSE, recursive = FALSE)
  if (length(grep(oneBatch, all_dirs)) != 1){
    stop("The 'oneBatch' specified is not a subdirectory inside the current working directory. Please make sure your current directory is correct.")
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

  if(!any(unique(info[,get(divisions[1])]) == control)){
    stop("'control' must be a condition within the divisions[1] variable.")
  } ## doesn't check for each row and column separation.

    runEachBatch(control, num_days, oneBatch, font, pref)

  setwd(original_wd)
}
