#' Visualize Changes in Sleep Between Genotypes and Treatments
#'
#' This function generates bar plots to visualize the changes in sleep across different genotypes and treatments.
#' It reads a CSV file containing normalized sleep data, calculates the changes in sleep, and then creates plots
#' for each of the sleep-related metrics (`TotalSleepChange`, `DayTimeSleepChange`, `NightTimeSleepChange`).
#' The plots compare the changes between two treatments (C1 and C2) for each genotype, and also include plots
#' for wild type (WT) genotypes within each treatment group. The resulting plots are saved as PDF files.
#'
#' @param C1 A string specifying the first treatment group.
#' @param C2 A string specifying the second treatment group.
#' @param WT A string specifying the wild type genotype to be used for comparison.
#' @param column_name The name of a column in the normalized summary CSV that will be used for comparison in the data.
#'
#' @return None. Saves the plots as PDF files.
#' @export
#'
#' @details
#' The function processes normalized sleep data from a CSV file ("all_batches_norm_summary.csv") and calculates the
#' change in sleep for each treatment group (`C1`, `C2`) and genotype, including wild type (WT). The changes in sleep
#' are calculated for three metrics: `TotalSleepChange`, `DayTimeSleepChange`, and `NightTimeSleepChange`.
#'
#' For each sleep metric, the function generates bar plots comparing the changes in sleep between the two treatments
#' (C1 and C2) for each genotype, as well as for wild type genotypes within each treatment group. The plots are saved as
#' PDF files. The number of batch names is used to adjust the width of the plots, and each plot is formatted to show the
#' mean change in sleep with customized themes and labels.
#'
#' @keywords internal
normDisplay <- function(C1, C2, column_name, WT) {
  # Read the normalized data from CSV file
  dataset <- read.csv("all_batches_norm_summary.csv")
  dataset <- data.table::setDT(dataset)

  # Calculate the change in sleep
  dataset_delta <- dplyr::mutate(dataset, TotalSleepChange = norm_sleep_time_all - 1)
  dataset_delta <- dplyr::mutate(dataset_delta, DayTimeSleepChange = norm_sleep_time_l - 1)
  dataset_delta <- dplyr::mutate(dataset_delta, NightTimeSleepChange = norm_sleep_time_d - 1)

  # Filter data based on treatment and column_name
  dataset_delta_Iso_C1 <- dplyr::filter(dataset_delta, treatment == C1 & get(column_name) != WT)
  dataset_delta_Iso_C2 <- dplyr::filter(dataset_delta, treatment == C2 & get(column_name) != WT)
  dataset_delta_Iso_C1_WT <- dplyr::filter(dataset_delta, treatment == C1 & get(column_name) == WT)
  dataset_delta_Iso_C2_WT <- dplyr::filter(dataset_delta, treatment == C2 & get(column_name) == WT)

  # Calculate averages for each column_name
  average_lookup <- dplyr::summarise(
    dplyr::group_by(dataset_delta_Iso_C2, !!rlang::sym(column_name)),
    TotalSleepChange = mean(TotalSleepChange),
    DayTimeSleepChange = mean(DayTimeSleepChange),
    NightTimeSleepChange = mean(NightTimeSleepChange))
  average_lookup <- average_lookup[order(average_lookup$TotalSleepChange), ]  # sort

  # Reorder column_names based on the average changes in sleep
  dataset_delta_Iso_C2[[column_name]]<- factor(dataset_delta_Iso_C2[[column_name]], levels = average_lookup[[column_name]])
  dataset_delta_Iso_C1[[column_name]] <- factor(dataset_delta_Iso_C1[[column_name]], levels = average_lookup[[column_name]])

  # List of sleep-related metrics to plot
  names <- c("TotalSleepChange", "DayTimeSleepChange", "NightTimeSleepChange")

  # Set the number of batch names for adjusting plot width
  if (length(unique(dataset$batch_name)) > 4) {
    q <- length(unique(dataset$batch_name))
  } else {
    q <- 5
  }

  # Generate and save plots for each sleep metric
  for (i in 1:length(names)){

    # Create a function to iterate through each plot
    plot_fun <- function(plot_data, title, xdef, xtitle = NULL) {
      p <-ggplot2::ggplot(plot_data, ggplot2::aes (x = get(xdef), y = get(names[i]))) +
        ggplot2::coord_cartesian(ylim = c(-.7, 0.2)) +
        ggplot2::stat_summary(fun = "mean", geom = "bar", width = .9, fill="grey40") +
        ggplot2::labs(title = paste(title, names[i]),
                      x = xtitle,
                      y = "Change in Sleep") +
        ggprism::theme_prism(base_fontface = "plain", base_line_size = 0.7) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, vjust = 1, angle = 45),
            legend.position="none")
      return(p)
    }

    # Plot the 2 conditions as well as the wildtypes
    p1 <- plot_fun(dataset_delta_Iso_C2, C2, column_name, column_name)
    p2 <- plot_fun(dataset_delta_Iso_C1, C1, column_name, column_name)
    p3 <- plot_fun(dataset_delta_Iso_C2_WT, paste(C1, WT), column_name, "Wild-type")
    p4<- plot_fun(dataset_delta_Iso_C1_WT, paste(C2, WT), column_name, "Wild-type")


    # Combine and save the plots as a PDF
    combined_plot <- cowplot::plot_grid(p2,p4,p1,p3, ncol = 2, align = "h", axis = "b")
    ggplot2::ggsave(paste0("NormalizedSleepLoss_", names[i],".pdf"),
                    combined_plot, width = (10), height = (8))
    }
}
