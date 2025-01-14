#' Visualize Changes in Sleep Between
#'
#' This function generates bar plots to visualize the changes in sleep across different Genotypes and Treatments.
#' It reads a CSV file containing normalized sleep data, calculates the changes in sleep, and then creates plots
#' for each of the sleep-related metrics (`TotalSleepChange`, `DayTimeSleepChange`, `NightTimeSleepChange`).
#' Two plots are made showing the normalized sleeploss for the Treatment of interest (`treat`).
#' One plot contains only the `control` entry in `x` while the other plot contains every entry except `control`.
#' The plots are saved as PDFs labeled, "NormalizedSleepLoss...".
#'
#' @param x The name of a parameter in "all_batches_norm_summary.csv" to be used as the x-axis in the plot.
#' @param treat A string specifying a Treatment to subset the data by.
#' @param temp A string specifying a Temperature condition to subset the data by.
#' @param enviro A string specifying a Environment condition to subset the data by.
#' @param Lights A string specifying a Light condition to subset the data by.
#' @param geno A string specifying a Genotype condition to subset the data by.
#' @param font A character string determining the font style of the produced plots. ("plain", "bold", "italic", or "bold.italic")
#'
#'
#' @return None. Saves the plots as PDF files labeled `NormalizedSleepLoss...`
#' @export
#'
#' @keywords internal
normDisplay <- function(x, treat = NULL, temp = NULL, enviro = NULL, Lights = NULL, geno = NULL, font = "plain") {
  if (!file.exists("all_batches_norm_summary.csv")) {
    stop("'all_batches_norm_summary.csv' is not found in the current directory. Please run 'runAllBatches' before attempting to run 'kmeansCluster'. If you have already run it, ")
  }
  # Validate that x is provided and is a valid string.
  if(missing(x) || !rlang::is_string(x) || !(x %in% c("Temperature", "Sex", "Treatment", "Genotype", "Environment", "Light"))){
    stop("x is missing or invalid")
  }
  if (!(font %in% c("plain", "bold", "italic","bold.italic"))){
    stop("'font' must be 'plain', 'bold', 'italic', or 'bold.italic'")
  }


  # Read the normalized data from CSV file
  dataset <- read.csv("all_batches_norm_summary.csv")
  dataset$Light <- gsub("\"", "", dataset$Light)
  titlee <- c("")
  if(!is.null(treat)){
    dataset <- dataset[dataset$Treatment == treat,]

    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'treat' specified is not included in the data within the 'Treatment' parameter")
    }
    titlee <- trimws(paste(titlee, treat))
  }
  if(!is.null(temp)){
    dataset <- dataset[dataset$Temperature == temp,]
    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'temp' specified is not included in the data within the 'Temperature' parameter")
    }
    titlee <- trimws(paste(titlee, temp))
  }
  if(!is.null(enviro)){
  dataset <- dataset[dataset$Environment == enviro,]
  # warning if condition is invalid
  if (nrow(dataset) == 0) {
    stop("The 'enviro' specified is not included in the data within the 'Environment' parameter")
  }
  titlee <- trimws(paste(titlee, enviro))
  }
  if(!is.null(Lights)){
    dataset <- dataset[dataset$Light == Lights,]
    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'Lights' specified is not included in the data within the 'Light' parameter")
    }
    titlee <- trimws(paste(titlee, Lights))
  }
  if(!is.null(geno)){
    dataset <- dataset[dataset$Genotype == geno,]
    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'geno' specified is not included in the data within the 'Genotype' parameter")
    }
    titlee <- trimws(paste(titlee, geno))
    }

  dataset <- data.table::setDT(dataset)

  # Calculate the change in sleep
  dataset_delta <- dplyr::mutate(dataset, TotalSleepChange = norm_Sleep_Time_All)
  dataset_delta <- dplyr::mutate(dataset_delta, DayTimeSleepChange = norm_Sleep_Time_L)
  dataset_delta <- dplyr::mutate(dataset_delta, NightTimeSleepChange = norm_Sleep_Time_D)
  #dataset_delta[, 22:24] <- dataset_delta[, 22:24] * -1

  # Filter data based on Treatment and x
  # dataset_delta_Iso_treat <- dplyr::filter(dataset_delta, Treatment == treat)

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
