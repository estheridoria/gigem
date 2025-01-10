#' Visualize Changes in Sleep Between
#'
#' This function generates bar plots to visualize the changes in sleep across different genotypes and treatments.
#' It reads a CSV file containing normalized sleep data, calculates the changes in sleep, and then creates plots
#' for each of the sleep-related metrics (`TotalSleepChange`, `DayTimeSleepChange`, `NightTimeSleepChange`).
#' Two plots are made showing the normalized sleeploss for the treatment of interest (`treat`).
#' One plot contains only the `control` entry in `x` while the other plot contains every entry except `control`.
#' The plots are saved as PDFs labeled, "NormalizedSleepLoss...".
#'
#' @param x The name of a parameter in "all_batches_norm_summary.csv" to be used as the x-axis in the plot.
#' @param treat A string specifying a treatment to subset the data by.
#' @param temp A string specifying a temperature condition to subset the data by.
#' @param enviro A string specifying a environment condition to subset the data by.
#' @param lights A string specifying a light condition to subset the data by.
#' @param geno A string specifying a genotype condition to subset the data by.
#' @param font A character string determining the font style of the produced plots. ("plain", "bold", "italic", or "bold.italic")
#'
#'
#' @return None. Saves the plots as PDF files labeled `NormalizedSleepLoss...`
#' @export
#'
#' @keywords internal
normDisplay <- function(x, treat = NULL, temp = NULL, enviro = NULL, lights = NULL, geno = NULL, font = "plain") {
  if (!file.exists("all_batches_norm_summary.csv")) {
    stop("'all_batches_norm_summary.csv' is not found in the current directory. Please run 'runAllBatches' before attempting to run 'kmeansCluster'. If you have already run it, ")
  }
  # Validate that x is provided and is a valid string.
  if(missing(x) || !rlang::is_string(x) || !(x %in% c("temperature", "sex", "treatment", "genotype", "environment", "light"))){
    stop("x is missing or invalid")
  }
  if (!(font %in% c("plain", "bold", "italic","bold.italic"))){
    stop("'font' must be 'plain', 'bold', 'italic', or 'bold.italic'")
  }


  # Read the normalized data from CSV file
  dataset <- read.csv("all_batches_norm_summary.csv")
  dataset$light <- gsub("\"", "", dataset$light)
  titlee <- c("")
  if(!is.null(treat)){
    dataset <- dataset[dataset$treatment == treat,]

    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'treat' specified is not included in the data within the 'treatment' parameter")
    }
    titlee <- trimws(paste(titlee, treat))
  }
  if(!is.null(temp)){
    dataset <- dataset[dataset$temperature == temp,]
    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'temp' specified is not included in the data within the 'temperature' parameter")
    }
    titlee <- trimws(paste(titlee, temp))
  }
  if(!is.null(enviro)){
  dataset <- dataset[dataset$environment == enviro,]
  # warning if condition is invalid
  if (nrow(dataset) == 0) {
    stop("The 'enviro' specified is not included in the data within the 'environment' parameter")
  }
  titlee <- trimws(paste(titlee, enviro))
  }
  if(!is.null(lights)){
    dataset <- dataset[dataset$light == lights,]
    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'lights' specified is not included in the data within the 'light' parameter")
    }
    titlee <- trimws(paste(titlee, lights))
  }
  if(!is.null(geno)){
    dataset <- dataset[dataset$genotype == geno,]
    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'geno' specified is not included in the data within the 'genotype' parameter")
    }
    titlee <- trimws(paste(titlee, geno))
    }

  dataset <- data.table::setDT(dataset)

  # Calculate the change in sleep
  dataset_delta <- dplyr::mutate(dataset, TotalSleepChange = norm_sleep_time_all)
  dataset_delta <- dplyr::mutate(dataset_delta, DayTimeSleepChange = norm_sleep_time_l)
  dataset_delta <- dplyr::mutate(dataset_delta, NightTimeSleepChange = norm_sleep_time_d)

  # Filter data based on treatment and x
  # dataset_delta_Iso_treat <- dplyr::filter(dataset_delta, treatment == treat)

  # Calculate averages for each x
  average_lookup <- dplyr::summarise(
    dplyr::group_by(dataset_delta, !!rlang::sym(x)),
    TotalSleepChange = mean(TotalSleepChange),
    DayTimeSleepChange = mean(DayTimeSleepChange),
    NightTimeSleepChange = mean(NightTimeSleepChange))
  average_lookup <- average_lookup[order(average_lookup$TotalSleepChange), ]  # sort

  # Reorder x based on the average changes in sleep
  dataset_delta[[x]] <- factor(dataset_delta[[x]], levels = average_lookup[[x]])

  # List of sleep-related metrics to plot
  names <- c("TotalSleepChange", "DayTimeSleepChange", "NightTimeSleepChange")

  # Generate and save plots for each sleep metric
  for (i in 1:length(names)){

    # Create a function to iterate through each plot
    plot_fun <- function(plot_data, title, x, font) {
      p <-ggplot2::ggplot(plot_data, ggplot2::aes (x = get(x), y = get(names[i]))) +
        #ggplot2::coord_cartesian(ylim = c(-5,5)) +
        ggplot2::stat_summary(fun = "mean", geom = "bar", width = .85, fill="grey50") +
        ggplot2::labs(title = paste(title, names[i]),
                      x = "",
                      y = "Change in Sleep") +
        ggprism::theme_prism(base_fontface = font) +
        ggplot2::theme(
          title = ggplot2::element_text(size = 16),
          axis.text.x = ggplot2::element_text(hjust = 1, vjust = .5, angle = 90, size = 9),
          axis.text.y = ggplot2::element_text(size = 9),
          axis.title.y = ggplot2::element_text(size = 16),
          legend.position="none")
      return(p)
    }

    # Plot the 2 conditions as well as the controls
    p1 <- plot_fun(plot_data = dataset_delta, title = titlee, x = x, font = font)

titlee <- gsub(" ", "", titlee)
    ggplot2::ggsave(paste0("NormalizedSleepLoss_", titlee, "_", names[i],".pdf"),
                    p1, width = (5), height = (4))
    }
}
