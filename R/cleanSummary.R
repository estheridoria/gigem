#' Generate Clean Summary Statistics (Internal)
#'
#' This internal helper function calculates and summarizes sleep metrics, performs bout analysis, and saves related plots.
#'
#' @param ExperimentData An S4 object with experiment metadata, including a `Batch` identifier for saved filenames.
#' @param dt A `behavr` table containing sleep and activity data.
#' @param num_days Number of days for calculating average metrics.
#' @param loadinginfo_linked Linked metadata for the behavioral data.
#' @param divisions A list of grouping columns used for facet plots.
#' @param pref A preference vector to control plot generation.
#'
#' @return A summarized `data.table` containing sleep metrics and bout data for each ID.
#' @details
#' - Calculates sleep fractions and durations for overall, light, and dark phases.
#' - Performs bout analysis for sleep architecture.
#' - Generates and saves plots of sleep and bout metrics by grouping factors.
#' - Outputs a CSV file named `summary_<Batch>.csv` with summarized data.
#'
#' @keywords internal
cleanSummary <- function(ExperimentData, dt, num_days, loadinginfo_linked, divisions, pref) {

  # # Add linked information and prepare data
  # dt <- behavr::behavr(dt, loadinginfo_linked)

  # day night sleep calculation using modulo operation,
  dt[, phase := ifelse(t %% behavr::hours(24) < behavr::hours(12), "L", "D")]

  # Calculate overall sleep metrics and phase-based sleep data
  summary_dt_final <- behavr::rejoin(dt[, .(
    sleep_fraction = mean(asleep),
    sleep_fraction_all = mean(asleep),
    sleep_time_all = 1440 * mean(asleep),
    sleep_fraction_l = mean(asleep[phase == "L"]),
    sleep_time_l = 720 * mean(asleep[phase == "L"]),
    sleep_fraction_d = mean(asleep[phase == "D"]),
    sleep_time_d = 720 * mean(asleep[phase == "D"])
  ), by = id])

  # Remove the 'file_info' column
  summary_dt_final <- summary_dt_final[, file_info := NULL]

  # Perform bout analysis for sleep architecture
  bout_dt <- sleepr::bout_analysis(asleep, dt)[asleep == TRUE, -"asleep"]

  if(pref[6] == 1){
  # Plot bout duration by time of day
  pdf(paste0(ExperimentData@Batch, '_population_sleep_bout_wrap', '.pdf'),
      width = 5*length(unique(info[[divisions[3]]]))+2,
      height = 3*length(unique(info[[divisions[2]]]))+2)
  print(
    ggetho::ggetho(bout_dt, ggplot2::aes(y = duration / 60, colour = treatment), time_wrap = behavr::hours(24)) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
                     strip.background = ggplot2::element_rect(fill="white"),
                     plot.margin = ggplot2::margin(1,1,1,1,"inches"))+
      ggetho::stat_pop_etho() +
      ggetho::stat_ld_annotations() +
      ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(divisions[2])),
                          cols = ggplot2::vars(!!rlang::sym(divisions[3]))) +
      ggplot2::scale_y_continuous(name = "Bout length (min)")
  )
  dev.off()
}
  # Process daily bout length and latency by light/dark phase
  summary_dt_final <- processDays(num_days, bout_dt, summary_dt_final)

  # Calculate bout lengths during light (L) and dark (D) phases, filtering by duration
  bout_dt_min <- sleepr::bout_analysis(asleep, dt)[, .(
    id, duration = duration / 60, t = t / 60,
    phase = ifelse(t %% behavr::hours(24) < behavr::hours(12), "L", "D")
  )][duration >= 5]

  # Summarize bout lengths for light and dark phases
  summary_bout_L <- bout_dt_min[phase == "L", .(
    n_bouts_L = .N / num_days,
    mean_bout_length_L = mean(duration)
  ), by = id]

  summary_bout_D <- bout_dt_min[phase == "D", .(
    n_bouts_D = .N / num_days,
    mean_bout_length_D = mean(duration)
  ), by = id]

  # Merge bout summary data into final summary table
  summary_dt_final <- merge(summary_dt_final, summary_bout_L, by = "id")
  summary_dt_final <- merge(summary_dt_final, summary_bout_D, by = "id")

  summary_dt_final <- summary_dt_final[, c(-2,-3)]

  # Save summary data to CSV
  data.table::fwrite(summary_dt_final, paste("summary_", ExperimentData@Batch, ".csv", sep = ""))

  if(pref[4] ==1){
  # Helper function to create sleep plots for specified metrics
  create_sleeptime_plot <- function(filename, plot_data, yParam, Yname, divisions) {
    pdf(filename, width = (prod(sapply(divisions[c(4,6)], function(col) length(unique(info[[col]]))))*1.5+2), ## swapped 6 and 5 between this line and the next
        height = length(unique(info[[divisions[5]]]))*3.7 +2)
    sleeptime_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[divisions[4]]],
                                                     y = .data[[yParam]]))+
      ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(divisions[5])),
                          cols = ggplot2::vars(!!rlang::sym(divisions[6])))+
      ggplot2::geom_errorbar(stat = "summary", fun.data = ggplot2::mean_cl_boot, width = 0.2,
                             color = "black") +
      ggplot2::geom_point(size = 4, stat = "summary", fun = mean, shape = 3,
                          color = "black") +
      ggbeeswarm::geom_beeswarm(ggplot2::aes(fill = .data[[divisions[1]]]), color = "black",
                                dodge.width = 0.9, shape = 21, cex = 3.5, alpha = 2/3) +
      ggplot2::scale_fill_viridis_d() +
      ggprism::theme_prism(base_fontface = "plain", base_line_size = 0.7)+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0, hjust=1),
                     plot.margin = ggplot2::margin(1,1,1,1,"inches"))+
      ggplot2::ggtitle(yParam) +                                                          #this was added by me
      ggplot2::scale_x_discrete(name = " ")+
      ggplot2::scale_y_continuous(name = Yname)
    print(sleeptime_plot)
    dev.off()
  }

  # Generate sleep time and bout plots for light and dark phases
  create_sleeptime_plot(paste0(ExperimentData@Batch, '_sleeptime_all.pdf'), summary_dt_final, "sleep_time_all", "Time sleeping (min)", divisions)
  create_sleeptime_plot(paste0(ExperimentData@Batch, '_sleeptime_l.pdf'), summary_dt_final, "sleep_time_l", "Time sleeping (min)", divisions)
  create_sleeptime_plot(paste0(ExperimentData@Batch, '_sleeptime_d.pdf'), summary_dt_final, "sleep_time_d", "Time sleeping (min)", divisions)
  create_sleeptime_plot(paste0(ExperimentData@Batch, '_n_bouts_L.pdf'), summary_dt_final, "n_bouts_L", "Number of sleep bouts", divisions)
  create_sleeptime_plot(paste0(ExperimentData@Batch, '_n_bouts_D.pdf'), summary_dt_final, "n_bouts_D", "Number of sleep bouts", divisions)

}
  # Return the final summary table
  return(summary_dt_final)
}
