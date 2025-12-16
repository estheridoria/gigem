#' Generate Clean Summary Statistics (Internal)
#'
#' This internal helper function calculates and summarizes sleep metrics, performs bout analysis, and saves related plots.
#'
#' @param ExperimentData An S4 object containing experiment metadata.
#'   Must include `Batch` (identifier for the batch being processed).
#' @param dt A `behavr` table containing sleep and activity data.
#' @param numDays Number of days for displaying and calculating average metrics.
#' @param loadinginfo_linked A data object linked with `behavr` metadata. Used to load Drosophila Activity Monitor (DAM) data.
#' @param divisions A list of grouping columns used for facetting plots.
#' @param pref A preference vector to control plot generation.
#' @param font A string variable determining the font style of the produced plots.
#'
#' @return A summarized `data.table` (`summary_<Batch>.csv`) containing sleep metrics and bout data for each ID.
#'
#' @keywords internal
cleanSummary <- function(ExperimentData, dt_final, batchMeta, numDays, loadinginfo_linked, divisions, pref, font) {

  # # Add linked information and prepare data
  # dt_final <- behavr::behavr(dt_final, loadinginfo_linked)

  # day night sleep calculation using modulo operation,
  dt_final[, phase := ifelse(t %% behavr::hours(24) < behavr::hours(12), "L", "D")]

  # Calculate overall sleep metrics and phase-based sleep data
  summary_dt_final <- behavr::rejoin(dt_final[, .(
    Sleep_Fraction_All = mean(asleep),
    Sleep_Time_All = 1440 * mean(asleep),
    Sleep_Fraction_L = mean(asleep[phase == "L"]),
    Sleep_Time_L = 720 * mean(asleep[phase == "L"]),
    Sleep_Fraction_D = mean(asleep[phase == "D"]),
    Sleep_Time_D = 720 * mean(asleep[phase == "D"])
  ), by = id])

  # Remove the 'file_info' column
  summary_dt_final <- summary_dt_final[, file_info := NULL]

  # Perform bout analysis for sleep architecture
  bout_dt <- sleepr::bout_analysis(asleep, dt_final)[asleep == TRUE, -"asleep"]

  # plotting
  if(pref[4] == 1){
  # Plot bout duration by time of day
  pdf(paste0(ExperimentData@Batch, '_Overlaid_Sleep_Bout_Profiles.pdf'),
      width = 5*length(unique(batchMeta[[divisions[3]]]))+2,
      height = 3*length(unique(batchMeta[[divisions[2]]]))+2)

      plot<- ggetho::ggetho(bout_dt, ggplot2::aes(x = t, y = duration / 60, colour = .data[[divisions[1]]]), time_wrap = behavr::hours(24)) +
        ggetho::stat_pop_etho() +
        ggetho::stat_ld_annotations() +
        ggplot2::scale_color_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .7)) +
        ggplot2::scale_fill_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .6)) +
        ggprism::theme_prism(base_fontface = font) +
        ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(divisions[2])),
                            cols = ggplot2::vars(!!rlang::sym(divisions[3]))) +
        ggplot2::scale_y_continuous(name = "Sleep Bout Length (min)") +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20),
                       axis.title.y = ggplot2::element_text(size = 20),
                       axis.text.x = ggplot2::element_text(size = 16),
                       axis.text.y = ggplot2::element_text(size = 16),
                       strip.text = ggplot2::element_text(size = 20),
                       legend.text = ggplot2::element_text(size = 16, face = font),
                       legend.position = "right")
      suppressWarnings(print(plot))
  dev.off()
}
  # Process daily bout length and latency by Light/dark phase
  summary_dt_final <- processDays(numDays, bout_dt, dt, summary_dt_final)

  # Calculate bout lengths during Light (L) and dark (D) phases, filtering by duration
  bout_dt_min <- sleepr::bout_analysis(asleep, dt_final)[, .(
    id, duration = duration / 60, t = t / 60,
    phase = ifelse(t %% behavr::hours(24) < behavr::hours(12), "L", "D")
  )][duration >= 5]

  # Summarize bout lengths for Light and dark phases
  summary_bout_L <- bout_dt_min[phase == "L", .(
    n_Bouts_L = .N / numDays,
    mean_Bout_Length_L = mean(duration)
  ), by = id]

  summary_bout_D <- bout_dt_min[phase == "D", .(
    n_Bouts_D = .N / numDays,
    mean_Bout_Length_D = mean(duration)
  ), by = id]

  # Merge bout summary data into final summary table
  summary_dt_final <- merge(summary_dt_final, summary_bout_L, by = "id")
  summary_dt_final <- merge(summary_dt_final, summary_bout_D, by = "id")

  # LightCol <- summary_dt_final[,Light]
  # summary_dt_final[, Light := paste0('"', Light, '"')]

  #summary_dt_final<- data.table::data.table(summary_dt_final[,1:13],
    # Batch = ExperimentData@Batch, summary_dt_final[,14:ncol(summary_dt_final)])
  # add batch column

  # move Batch to After monitor
  monitor_index <- match("monitor", names(summary_dt_final))
  new_order <- c(names(summary_dt_final)[1:monitor_index], 
                 "Batch", 
                 names(summary_dt_final)[(monitor_index + 1):length(names(summary_dt_final))])
  new_order <- new_order[new_order != "Batch" | duplicated(new_order)]
  data.table::setcolorder(summary_dt_final, neworder = new_order)

  # Now write to the file
  data.table::fwrite(
    summary_dt_final,
    paste0("summary_", ExperimentData@Batch, ".csv"),
    quote = TRUE
  )
  if(pref[5] ==1){
    #Generate sleep time and bout plots for Light and dark phases
      # Define plot parameters in a list of lists for easy iteration
      plot_params <- list(
        list(yParam = "Sleep_Time_All", Yname = "Total Sleep (min)", limits = 1500, geom = "bar"),
        list(yParam = "Sleep_Time_L", Yname = "Daytime Sleep (min)", limits = 1000, geom = "bar"),
        list(yParam = "Sleep_Time_D", Yname = "Nighttime Sleep (min)", limits = 1000, geom = "bar"),
        list(yParam = "n_Bouts_L", Yname = "# Daytime Sleep Bouts", limits = 80, geom = "violin"),
        list(yParam = "n_Bouts_D", Yname = "# Nighttime Sleep Bouts", limits = 80, geom = "violin"),
        list(yParam = "mean_Bout_Length_L", Yname = "Daytime Bout Length", limits = NA, geom = "violin"),
        list(yParam = "mean_Bout_Length_D", Yname = "Nighttime Bout Length", limits = NA, geom = "violin")
      )

      # Calculate dynamic size components once
      # Assumes batchMeta and divisions are available in the scope of cleanSummary
      plot_width <- prod(sapply(divisions[c(1,3)], function(col) length(unique(batchMeta[[col]])))) * 1.5 + 2
      plot_height <- length(unique(batchMeta[[divisions[2]]])) * 3.7 + 2

      # Loop through all metrics to generate and save each plot individually
      for (p in plot_params) {

        current_limits <- p$limits
        
        # Check if we need to calculate a dynamic ceiling limit
        if (is.na(current_limits)) {
          # Calculate the dynamic ceiling limit based on the current yParam
          max_val <- max(summary_dt_final[, get(p$yParam)], na.rm = TRUE)
          current_limits <- ceiling(max_val / 50) * 50
        }
        
        # 1. Generate the ggplot object using the unified function
        sleeptime_plot <- create_sleeptime_plot(
          plot_data = summary_dt_final,
          yParam = p$yParam,
          Yname = p$Yname,
          divisions = divisions,
          limits = current_limits,
          geom = p$geom,
          font = font,
          p_value = NULL,           # No P-value needed for batch summary plots
          is_faceted = TRUE         # Set to TRUE to enable facetting
        )

        # 2. Define a unique filename using the yParam
        filename <- paste0(ExperimentData@Batch, '_', p$yParam, '.pdf')

        # 3. Save the plot with dynamic dimensions
        # Suppress warnings often caused by coord_cartesian when used with ggsave
        suppressWarnings(
          ggplot2::ggsave(
            filename = filename,
            plot = sleeptime_plot,
            width = plot_width,
            height = plot_height
          )
        )
      }
  }

  # Return the final summary table
  return(summary_dt_final)
}
