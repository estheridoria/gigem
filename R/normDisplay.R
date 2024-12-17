#' Visualize Changes in Sleep Between Genotypes and Treatments
#'
#' This function generates bar plots to visualize the changes in sleep across different genotypes and treatments.
#' It reads a CSV file containing normalized sleep data, calculates the changes in sleep, and then creates plots
#' for each of the sleep-related metrics (`TotalSleepChange`, `DayTimeSleepChange`, `NightTimeSleepChange`).
#' Two plots are made showing the normalized sleeploss for the treatment of interest (`treat`).
#' One plot contains only the `Control` entry in `column_name` while the other plot contains every entry except `Control`.
#' The plots are saved as PDFs labeled, "NormalizedSleepLoss...".
#'
#' @param treat A string specifying a treatment group.
#' @param treat2 A string specifying a treatment group (optional).
#' @param column_name The name of a column (variable) in "all_batches_norm_summary.csv" that will be used for comparison in the data.
#' @param Control A string specifying the control group within the variable `column_name`.
#'
#' @return None. Saves the plots as PDF files labeled 'NormalizedSleepLoss...'.
#' @export
#'
#' @keywords internal
normDisplay <- function(treat, treat2 = NULL, column_name, Control, font = "plain") {
  # Read the normalized data from CSV file
  dataset <- read.csv("all_batches_norm_summary.csv")
  dataset <- data.table::setDT(dataset)

  # Calculate the change in sleep
  dataset_delta <- dplyr::mutate(dataset, TotalSleepChange = norm_sleep_time_all - 1)
  dataset_delta <- dplyr::mutate(dataset_delta, DayTimeSleepChange = norm_sleep_time_l - 1)
  dataset_delta <- dplyr::mutate(dataset_delta, NightTimeSleepChange = norm_sleep_time_d - 1)

  # Filter data based on treatment and column_name
  dataset_delta_Iso_treat <- dplyr::filter(dataset_delta, treatment == treat & get(column_name) != Control)
  dataset_delta_Iso_treat_Control <- dplyr::filter(dataset_delta, treatment == treat & get(column_name) == Control)

  # Calculate averages for each column_name
  average_lookup <- dplyr::summarise(
    dplyr::group_by(dataset_delta_Iso_treat, !!rlang::sym(column_name)),
    TotalSleepChange = mean(TotalSleepChange),
    DayTimeSleepChange = mean(DayTimeSleepChange),
    NightTimeSleepChange = mean(NightTimeSleepChange))
  average_lookup <- average_lookup[order(average_lookup$TotalSleepChange), ]  # sort

  # Reorder column_names based on the average changes in sleep
  dataset_delta_Iso_treat[[column_name]] <- factor(dataset_delta_Iso_treat[[column_name]], levels = average_lookup[[column_name]])

  # Add the second treatment group's calculations if applicable
  if (treat2 != "NULL"){
    # Filter data based on treatment and column_name
    dataset_delta_Iso_treat2 <- dplyr::filter(dataset_delta, treatment == treat2 & get(column_name) != Control)
    dataset_delta_Iso_treat2_Control <- dplyr::filter(dataset_delta, treatment == treat2 & get(column_name) == Control)

    # Reorder column_names based on the average changes in sleep
    dataset_delta_Iso_treat2[[column_name]]<- factor(dataset_delta_Iso_treat2[[column_name]], levels = average_lookup[[column_name]])
  }

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
    plot_fun <- function(plot_data, title, xdef, font) {
      p <-ggplot2::ggplot(plot_data, ggplot2::aes (x = get(xdef), y = get(names[i]))) +
        ggplot2::coord_cartesian(ylim = c(-.7, 0.2)) +
        ggplot2::stat_summary(fun = "mean", geom = "bar", width = .85, fill="grey50") +
        ggplot2::labs(title = paste(title, names[i]),
                      x = "",
                      y = "Change in Sleep") +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggprism::theme_prism(base_fontface = font) +
        ggplot2::theme(
          title = ggplot2::element_text(size = 16),
          axis.text.x = ggplot2::element_text(hjust = 1, vjust = 1, angle = 90, size = 9),
          axis.text.y = ggplot2::element_text(size = 9),
          axis.title.y = ggplot2::element_text(size = 16),
          legend.position="none")
      return(p)
    }

    # Plot the 2 conditions as well as the controls
    p1 <- plot_fun(dataset_delta_Iso_treat, treat, column_name, font = font)
    p2<- plot_fun(dataset_delta_Iso_treat_Control, paste(treat, Control), "batch_name", font = font)

    if (treat2 != "NULL"){
    p3 <- plot_fun(dataset_delta_Iso_treat2, treat2, column_name, font = font)
    p4 <- plot_fun(dataset_delta_Iso_treat2_Control, paste(treat2, Control), "batch_name", font = font)
    }


    # Combine and save the plots as a PDF
    if(treat2 != "NULL"){
    combined_plot <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2, align = "h", axis = "b")}
    else{combined_plot <- cowplot::plot_grid(p1, p2, ncol = 2, align = "h", axis = "b")}

    ggplot2::ggsave(paste0("NormalizedSleepLoss_", treat, "_", names[i],".pdf"),
                    combined_plot, width = (10), height = (8))
    }
}
