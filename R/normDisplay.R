#' Visualize Changes in Sleep Between Genotypes and Treatments
#'
#' This function generates bar plots to visualize the changes in sleep across different genotypes and treatments.
#' It reads a CSV file containing normalized sleep data, calculates the changes in sleep, and then creates plots
#' for each of the sleep-related metrics (`TotalSleepChange`, `DayTimeSleepChange`, `NightTimeSleepChange`).
#' Two plots are made showing the normalized sleeploss for the treatment of interest (`treat`).
#' One plot contains only the `Control` entry in `x` while the other plot contains every entry except `Control`.
#' The plots are saved as PDFs labeled, "NormalizedSleepLoss...".
#'
#' @param treat A string specifying a treatment group.
#' @param treat2 A string specifying a treatment group (optional).
#' @param x The name of a column (variable) in "all_batches_norm_summary.csv" that will be used for comparison in the data.
#' @param Control A string specifying the control group within the variable `x`.
#'
#' @return None. Saves the plots as PDF files labeled 'NormalizedSleepLoss...'.
#' @export
#'
#' @keywords internal
normDisplay <- function(treat = "Iso", treat2 = NULL, enviro = NULL, lights = NULL, geno = NULL, x, Control, font = "plain") {
  # Read the normalized data from CSV file
  dataset <- read.csv("all_batches_norm_summary.csv")
  dataset$light <- gsub("\"", "", dataset$light)
  titlee <- treat
  if(!is.null(enviro)){
  dataset <- dataset[dataset$environment == enviro,]
  titlee <- trimws(paste(titlee, enviro))
  }
  if(!is.null(lights)){
    dataset <- dataset[dataset$light == lights,]
    titlee <- trimws(paste(titlee, lights))
  }
  if(!is.null(geno)){
    dataset <- dataset[dataset$genotype == geno,]
    titlee <- trimws(paste(titlee, geno))
    }

  dataset <- data.table::setDT(dataset)

  # Calculate the change in sleep
  dataset_delta <- dplyr::mutate(dataset, TotalSleepChange = norm_sleep_time_all-1)
  dataset_delta <- dplyr::mutate(dataset_delta, DayTimeSleepChange = norm_sleep_time_l-1)
  dataset_delta <- dplyr::mutate(dataset_delta, NightTimeSleepChange = norm_sleep_time_d-1)

  # Filter data based on treatment and x
  dataset_delta_Iso_treat <- dplyr::filter(dataset_delta, treatment == treat)# & get(x) != Control)
  # dataset_delta_Iso_treat_Control <- dplyr::filter(dataset_delta, treatment == treat & get(x) == Control)

  # Calculate averages for each x
  average_lookup <- dplyr::summarise(
    dplyr::group_by(dataset_delta_Iso_treat, !!rlang::sym(x)),
    TotalSleepChange = mean(TotalSleepChange),
    DayTimeSleepChange = mean(DayTimeSleepChange),
    NightTimeSleepChange = mean(NightTimeSleepChange))
  average_lookup <- average_lookup[order(average_lookup$TotalSleepChange), ]  # sort

  # Reorder x based on the average changes in sleep
  dataset_delta_Iso_treat[[x]] <- factor(dataset_delta_Iso_treat[[x]], levels = average_lookup[[x]])

  # Add the second treatment group's calculations if applicable
  if (!is.null(treat2)){
    # Filter data based on treatment and x
    dataset_delta_Iso_treat2 <- dplyr::filter(dataset_delta, treatment == treat2)# & get(x) != Control)
    # dataset_delta_Iso_treat2_Control <- dplyr::filter(dataset_delta, treatment == treat2 & get(x) == Control)

    # Reorder x based on the average changes in sleep
    dataset_delta_Iso_treat2[[x]]<- factor(dataset_delta_Iso_treat2[[x]], levels = average_lookup[[x]])
  }

  # List of sleep-related metrics to plot
  names <- c("TotalSleepChange", "DayTimeSleepChange", "NightTimeSleepChange")

  # # Set the number of batch names for adjusting plot width
  # if (length(unique(dataset$Batch)) > 4) {
  #   q <- length(unique(dataset$Batch))
  # } else {
  #   q <- 5
  # }

  # Generate and save plots for each sleep metric
  for (i in 1:length(names)){

    # Create a function to iterate through each plot
    plot_fun <- function(plot_data, title, xdef, font) {
      p <-ggplot2::ggplot(plot_data, ggplot2::aes (x = get(xdef), y = get(names[i]))) +
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
    p1 <- plot_fun(plot_data = dataset_delta_Iso_treat, title = titlee, xdef = x, font = font)
    # p2<- plot_fun(dataset_delta_Iso_treat_Control, paste(treat, Control), "Batch", font = font)

    if (!is.null(treat2)){
    p3 <- plot_fun(dataset_delta_Iso_treat2, treat2, x, font = font)
    # p4 <- plot_fun(dataset_delta_Iso_treat2_Control, paste(treat2, Control), "Batch", font = font)
    }


    # Combine and save the plots as a PDF
    if(!is.null(treat2)){
    combined_plot <- cowplot::plot_grid(p1, p3, ncol = 2, align = "h", axis = "b")
    }else{
      combined_plot <- cowplot::plot_grid(p1, ncol = 1, align = "h", axis = "b")
      }
titlee <- gsub(" ", "", titlee)
    ggplot2::ggsave(paste0("NormalizedSleepLoss_", titlee, "_", names[i],".pdf"),
                    combined_plot, width = (5), height = (4))
    }
}
