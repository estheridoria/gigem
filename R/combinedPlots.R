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

    # Call the helper to get the p1 plot object (using NULL for dimensions/facets)
    p1_results <- render_sleep_profile_plot(
      plot_data = plot_subdata,
      divisions = divisions,
      batchMeta = plot_subdata2, # Using plot_subdata2 as a stand-in for meta
      numb_days = 1, # Set to 1 because combinedPlots is always 24h wrapped
      font = font,
      overlay_mode = TRUE,
      wrap_time = behavr::hours(24),
      p1title = p1title
    )

    p1 <- p1_results$plot

    # Recalculate addedspace and apply specific legend theme *AFTER* plot generation
    if(length(unique(plot_subdata2[[divisions[1]]])) <= 2){
      # Note: Using p1_results$plot here since p1 is a ggplot object
      p1 <- p1 + ggplot2::theme(legend.position = c(0.75,0.15))
      addedspace <- 6
    } else {
      addedspace <- 8
    }

    # --- T-Test and P-Value Calculation ---
    u <- length(unique(plot_subdata2[[divisions[1]]]))

    if (pValues && u == 2) {
      # Only proceed if pValues is TRUE and exactly 2 unique groups exist
      controlee <- unique(plot_subdata2[[divisions[1]]])[1]
      t.test_y_vars <- unique(plot_subdata2[[divisions[1]]])
      t.test_y_vars <- t.test_y_vars[t.test_y_vars != controlee]

      p_value <- matrix(, nrow = 1, ncol = length(yParams))

      # Run the t.test for all 7 metrics against the control
      for(j in seq_along(yParams)){
        t_test_result <- t.test(plot_subdata2[get(divisions[1]) == controlee, get(yParams[j])],
                                plot_subdata2[get(divisions[1]) == t.test_y_vars[1], get(yParams[j])])
        p_value[1, j] <- t_test_result$p.value
      }
    } else {
      # Set to "No" if pValues is FALSE or if there are not exactly 2 groups (u != 2)
      p_value <- matrix("No", nrow = 1, ncol = length(yParams))
    }

      p1title <- gsub(" ", "_", p1title)
      p1title <- gsub(":", ".", p1title)
      p_values[, (p1title) := list(list(p_value))]

      # Generate sleep duration plots
      # Note: p_value is either a 1x7 matrix of p-values or a 1x7 matrix of "No".
      p2 <- create_sleeptime_plot(plot_subdata2, yParams[1], "Total Sleep (min)", divisions, 1500, "bar", font, p_value[1, 1], is_faceted = FALSE)
      p3 <- create_sleeptime_plot(plot_subdata2, yParams[2], "Daytime Sleep (min)", divisions, 1000, "bar", font, p_value[1, 2], is_faceted = FALSE)
      p4 <- create_sleeptime_plot(plot_subdata2, yParams[3], "Nighttime Sleep (min)", divisions, 1000, "bar", font, p_value[1, 3], is_faceted = FALSE)

      # Bout Counts (using dynamic ceiling for Y-limit)
      p5 <- create_sleeptime_plot(plot_subdata2, yParams[4], "# Daytime Sleep Bouts", divisions, ceiling(max(plot_subdata2[,get(yParams[4])], na.rm = TRUE)/50)*50, "violin", font, p_value[1, 4], is_faceted = FALSE)
      p6 <- create_sleeptime_plot(plot_subdata2, yParams[5], "# Nighttime Sleep Bouts", divisions, ceiling(max(plot_subdata2[,get(yParams[5])], na.rm = TRUE)/50)*50, "violin", font, p_value[1, 5], is_faceted = FALSE)

      # Bout Lengths (using dynamic ceiling for Y-limit)
      p7 <- create_sleeptime_plot(plot_subdata2, yParams[6], "Daytime Bout Length", divisions, ceiling(max(plot_subdata2[,get(yParams[6])], na.rm = TRUE)/50)*50, "violin", font, p_value[1, 6], is_faceted = FALSE)
      p8 <- create_sleeptime_plot(plot_subdata2, yParams[7], "Nighttime Bout Length", divisions, ceiling(max(plot_subdata2[,get(yParams[7])], na.rm = TRUE)/50)*50, "violin", font, p_value[1, 7], is_faceted = FALSE)

      # Combine plots
        rel_width <- 1 + (u / 2) + ((u - 1) * 0.1)
suppressWarnings(
          combined_plot <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 8, align = "h", axis = "tb",
                                              rel_widths = c(addedspace, rep(rel_width, 7)))
)
total_width <- addedspace + 7 * rel_width

p1titlee <- gsub(" ", "_", p1title)
p1titlee <- gsub(":", ".", p1titlee)
p1titlee <- gsub("/", ".", p1titlee)

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
