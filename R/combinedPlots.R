#' Generate Combined Plots (Internal)
#'
#' Creates combined plots for each unique combination of `Light`, `Environment`, `Treatment`, `Sex`, and `Genotype` in the dataset.
#' Generates overlay sleep plots and sleep duration plots, saving each combination as a PDF file.
#'
#' @param ExperimentData An S4 object containing experiment metadata.
#' @param dt_curated_final A `data.table` containing curated sleep data with columns such as `id` and `asleep`.
#' @param summary_dt_final A `data.table` containing summary statistics with columns including `Light`, `Environment`,
#'   `Genotype`, `Treatment`, `Sex` and various sleep metrics.
#' @param font A character string variable determining the font style of the produced plots.
#' @param divisions A list of grouping columns used for facetting plots.
#' @param pValues A TRUE/FALSE vector for if combined plots will display p values for 2-condition overlays.
#'
#' @details
#' The function iterates through all unique combinations of `Light`, `Environment`, `Treatment`, `Sex`, and `Genotype`.
#' For each combination:
#' - An overlay sleep plot is created using `ggetho`.
#' - Multiple sleep duration plots (e.g., total sleep, daytime sleep, nighttime sleep) are generated
#'   using a helper function, `create_sleeptime_plot`.
#' - Plots are combined into a single figure using `cowplot::plot_grid`.
#' - The combined figure is saved as a PDF file, with the filename reflecting the combination of `Light`,
#'   `Environment`, and `Genotype`.
#'
#' The function dynamically adjusts plot widths based on the number of unique possibilities of the first divisions entry (a parameter).
#'
#' @return None. Plots are saved as PDF files.
#' @keywords internal

combinedPlots <- function(ExperimentData, dt_curated_final, summary_dt_final, font, divisions, pValues) {
  # Dynamically exclude columns where the value is equal to divisions[1] &
  columns_to_consider <- c("Sex", "Genotype", "Temperature", "Treatment", "Environment", "Light")
  # Exclude the first division and get unique combinations
  cols <- columns_to_consider[columns_to_consider != divisions[1]]
  condition_combinations <- unique(summary_dt_final[, ..cols])

  condition_combinations[, p1title := trimws(paste0(
    if (divisions[1] != "Genotype" && length(unique(Genotype)) > 1) paste0(Genotype, " ") else "",
    if (divisions[1] != "Light" && length(unique(Light)) > 1) paste0(Light, " ") else "",
    if (divisions[1] != "Treatment" && length(unique(Treatment)) > 1) paste0(Treatment, " ") else "",
    if (divisions[1] != "Temperature" && length(unique(Temperature)) > 1) paste0(Temperature, " ") else "",
    if (divisions[1] != "Sex" && length(unique(Sex)) > 1) paste0(Sex, " ") else "",
    if (divisions[1] != "Environment" && length(unique(Environment)) > 1) paste0(Environment, " ") else ""
  ))]

  # find out if there is a control for each combination of conditions
  all_conditions <- unique(summary_dt_final[,.SD,.SDcols = columns_to_consider])

    p_values <- data.table::data.table()

  # Function to create sleep duration plots
  create_sleeptime_plot <- function(plot_data, yParam, Yname, limits, geom, font, p_value) {
    pointplot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[divisions[1]]], y = .data[[yParam]]))
    if(geom == "bar"){
      pointplot <- pointplot +
        ggplot2::stat_summary(fun = "mean", geom = geom, width = .5, fill="grey90")}

    if(geom == "violin"){
      pointplot <- pointplot +
        ggplot2::geom_violin(fill="grey90")}

    # Add beeswarm plot (for both bar and violin)
    pointplot <- pointplot +
      ggbeeswarm::geom_beeswarm(ggplot2::aes(fill = .data[[divisions[1]]], color = .data[[divisions[1]]]),
                                dodge.width = 0.9, shape = 21, cex = 3.5) +
      ggplot2::scale_color_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .7)) +
      ggplot2::scale_fill_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .6)) +
      ggplot2::geom_errorbar(stat = "summary", fun.data = ggplot2::mean_cl_boot, width = 0.2, color = "black") +
      ggplot2::geom_point(size = 1.5, stat = "summary", fun = mean, shape = 3, color = "black") +
      ggplot2::scale_y_continuous(name = Yname) +
      ggplot2::coord_cartesian(ylim = c(0,limits))+
      ggplot2::scale_x_discrete(name = NULL)+
      ggprism::theme_prism(base_fontface = font)  +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20),
                     axis.text.x = ggplot2::element_text(size = 16, angle = 45, vjust = 1, hjust= 1),
                     axis.text.y = ggplot2::element_text(size = 16),
                     legend.position = "none")
    if (p_value == "No") {
      invisible()
    }else{
      # Define thresholds and corresponding labels
      thresholds <- c(0.0001, 0.001, 0.01, 0.05, 0.07)
      labels <- c("****", "***", "**", "*", " ")

      # Find the corresponding label directly using logical comparisons
      p_label <- ifelse(p_value < thresholds[1], labels[1],
                 ifelse(p_value < thresholds[2], labels[2],
                 ifelse(p_value < thresholds[3], labels[3],
                 ifelse(p_value < thresholds[4], labels[4],
                 ifelse(p_value < thresholds[5], labels[5],
                        "")))))

      # If a label is assigned, annotate the plot
      if (p_label !="") {
        pointplot <- pointplot +
          ggplot2::annotate("text", x = 1.5, y = (limits),
                            label = paste("p =", round(p_value, 4)), size = 4.5,
                            color = "black", fontface = font) +
          ggplot2::annotate("text", x = 1.5, y = (limits- (limits / 13)),
                            label = p_label, size = 5, color = "black", fontface = font) +
          ggplot2::geom_segment( mapping = NULL, x = 1, xend = 2,
                                 y = (limits -(limits/20)- (limits / 20)),
                                 yend = (limits - (limits / 20)- (limits / 20)),
                                color = "black", linewidth = 1)
       }
      }
    return(pointplot)
  }

  yParams<- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L", "n_Bouts_D", "mean_Bout_Length_L", "mean_Bout_Length_D")

  # Apply the logic for subsetting and plotting using data.table------------
  condition_combinations[, {
    plot_subdata2 <- summary_dt_final[
      Reduce(`&`, lapply(c("Light", "Environment", "Genotype", "Treatment", "Temperature", "Sex"), function(col) {
        if (divisions[1] == col) {
          return(TRUE)  # Skip this condition if divisions[1] matches the column name
        } else {return(get(col) == .SD[[col]])  # Compare the column if it doesn't match divisions[1]
          }}))]

    # Curate data for plotting
    plot_subdata <- dt_curated_final[id %in% plot_subdata2$id]
    u <- length(unique(plot_subdata2[[divisions[1]]]))

    # Count samples per group
    group_counts <- dplyr::count(plot_subdata2, by = get(divisions[1]))
    group_counts <- dplyr::mutate(group_counts, label = paste0(by, " (n=", n, ")"))

    label_map <- stats::setNames(group_counts$label, group_counts$by)

        # Create overlay sleep plot--------------
          p1 <- ggetho::ggetho(plot_subdata, ggplot2::aes(x = t, y = asleep, colour = .data[[divisions[1]]]), time_wrap = behavr::hours(24)) +
            ggetho::stat_pop_etho(show.legend = T) +
            ggetho::stat_ld_annotations() +
            ggplot2::scale_color_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6"), labels = label_map) +
            ggplot2::scale_fill_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6"), labels = label_map) +
            ggplot2::labs(title = p1title, y= "Sleep (%)") +
            ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent)+
            ggprism::theme_prism(base_fontface = font) +
            ggplot2::theme(title = ggplot2::element_text(size = 22),
                           axis.title.x = ggplot2::element_text(size = 20),
                           axis.title.y = ggplot2::element_text(size = 20),
                           axis.text.x = ggplot2::element_text(size = 16),
                           axis.text.y = ggplot2::element_text(size = 16),
                           legend.text = ggplot2::element_text(size = 16, face = font))
            if(length(unique(plot_subdata2[[divisions[1]]])) <= 2){
              p1 <- p1 + ggplot2::theme(legend.position = c(0.75,0.15)) ###continued warning about legend position inside
              addedspace <- 6
            } else {
                addedspace <- 8
              }

    if(pValues){

    # find out if there is a control for each combination of conditions
    if (length(unique(plot_subdata2[[divisions[1]]])) == 2) {
      controlee <- unique(plot_subdata2[[divisions[1]]])[1]

      # Perform t-test if the 'divisions[1]'  has 'control'
      all_conditions <- unique(plot_subdata2[,.SD,.SDcols = columns_to_consider])
      t.test_y_vars <- setdiff(unique(plot_subdata2[[divisions[1]]]), controlee) # should be same as above line

      #set up for p_values & run the t.test
      p_value <- matrix(, nrow = length(t.test_y_vars), ncol = length(yParams)+2)
      p_vals <- list()

      for(j in seq_along(yParams)){
        for (i in seq_along(t.test_y_vars)){
          t_test_result <- t.test(plot_subdata2[get(divisions[1]) == controlee, get(yParams[j])], # needs to be generalized
                                  plot_subdata2[get(divisions[1]) == t.test_y_vars[i], get(yParams[j])])
          p_value[i,j] <- t_test_result$p.value
        }
      }
    }else {
      p_value <- matrix("No", nrow = 1, ncol = length(yParams))#+2)
    }
    }else {
      p_value <- matrix("No", nrow = 1, ncol = length(yParams))#+2)
    }

      p1title <- gsub(" ", "_", p1title)
      p1title <- gsub(":", ".", p1title)
      p_values[, (p1title) := list(list(p_value))]

        # Generate sleep duration plots
        p2 <- create_sleeptime_plot(plot_subdata2, yParams[1], "Total Sleep (min)", 1500, "bar", font, p_value[,1])
        p3 <- create_sleeptime_plot(plot_subdata2, yParams[2], "Daytime Sleep (min)", 1000, "bar", font, p_value[,2])
        p4 <- create_sleeptime_plot(plot_subdata2, yParams[3], "Nighttime Sleep (min)", 1000, "bar", font, p_value[,3])
        p5 <- create_sleeptime_plot(plot_subdata2, yParams[4], "# Daytime Sleep Bouts", ceiling(max(plot_subdata2[,get(yParams[4])])/50)*50, "violin", font, p_value[,4])
        p6 <- create_sleeptime_plot(plot_subdata2, yParams[5], "# Nighttime Sleep Bouts", ceiling(max(plot_subdata2[,get(yParams[5])])/50)*50, "violin", font, p_value[,5])
        p7 <- create_sleeptime_plot(plot_subdata2, yParams[6], "Daytime Bout Length", ceiling(max(plot_subdata2[,get(yParams[6])])/50)*50, "violin", font, p_value[,6])
        p8 <- create_sleeptime_plot(plot_subdata2, yParams[7], "Nighttime Bout Length", ceiling(max(plot_subdata2[,get(yParams[7])])/50)*50, "violin", font, p_value[,7])

        # Combine plots
        rel_width <- 1 + (u / 2) + ((u - 1) * 0.1)
suppressWarnings(
          combined_plot <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 8, align = "h", axis = "tb",
                                              rel_widths = c(addedspace, rep(rel_width, 7)))
)
total_width <- addedspace + 7 * rel_width

p1titlee <- gsub(" ", "_", p1title)
p1titlee <- gsub(":", ".", p1titlee)
        # Save combined plot
        ggplot2::ggsave(paste0("CombinedPlots", p1titlee, ExperimentData@Batch, ".pdf"),
                        combined_plot, width = total_width, height = 4)
}, by = 1:nrow(condition_combinations)]

  #-------------
  if(pValues){
  pvdf <- data.frame(matrix(unlist(p_values), nrow = ncol(p_values), byrow = TRUE))
  colnames(pvdf) <- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L", "n_Bouts_D", "mean_Bout_Length_L", "mean_Bout_Length_D")
  rownames(pvdf)<- names(p_values)
  write.csv(pvdf, paste0("pValues_", ExperimentData@Batch, ".csv"))
  }
}
