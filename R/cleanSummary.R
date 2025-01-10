#' Generate Clean Summary Statistics (Internal)
#'
#' This internal helper function calculates and summarizes sleep metrics, performs bout analysis, and saves related plots.
#'
#' @param ExperimentData An S4 object containing experiment metadata.
#'   Must include `Batch` (identifier for the batch being processed).
#' @param dt A `behavr` table containing sleep and activity data.
#' @param num_days Number of days for displaying and calculating average metrics.
#' @param loadinginfo_linked Linked metadata for the behavioral data.
#' @param divisions A list of grouping columns used for facetting plots.
#' @param pref A preference vector to control plot generation.
#' @param font A string variable determining the font style of the produced plots.
#'
#' @return A summarized `data.table` (`summary_<Batch>.csv`) containing sleep metrics and bout data for each ID.
#'
#' @keywords internal
cleanSummary <- function(ExperimentData, dt, num_days, loadinginfo_linked, divisions, pref, font) {

  # # Add linked information and prepare data
  # dt <- behavr::behavr(dt, loadinginfo_linked)

  # day night sleep calculation using modulo operation,
  dt[, phase := ifelse(t %% behavr::hours(24) < behavr::hours(12), "L", "D")]

  # Calculate overall sleep metrics and phase-based sleep data
  summary_dt_final <- behavr::rejoin(dt[, .(
    sleep_fraction = mean(asleep),
    sleep_fraction_All = mean(asleep),
    sleep_time_all = 1440 * mean(asleep),
    sleep_fraction_L = mean(asleep[phase == "L"]),
    sleep_time_L = 720 * mean(asleep[phase == "L"]),
    sleep_fraction_D = mean(asleep[phase == "D"]),
    sleep_time_D = 720 * mean(asleep[phase == "D"])
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
    suppressWarnings(print(
      ggetho::ggetho(bout_dt, ggplot2::aes(y = duration / 60, colour = .data[[divisions[1]]]), time_wrap = behavr::hours(24)) +
      ggetho::stat_pop_etho() +
      ggetho::stat_ld_annotations() +
      ggplot2::scale_color_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +
      ggplot2::scale_fill_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +
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
  )
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

  lightCol <- summary_dt_final[,light]
  summary_dt_final[, light := paste0('"', light, '"')]

  summary_dt_final<- data.table::data.table(summary_dt_final[,1:13], Batch = ExperimentData@Batch, summary_dt_final[,14:ncol(summary_dt_final)])

  # Now write to the file
  data.table::fwrite(
    summary_dt_final,
    paste0("summary_", ExperimentData@Batch, ".csv"),
    quote = TRUE
  )
  summary_dt_final[, light := lightCol]

  if(pref[4] ==1){
  # Helper function to create sleep plots for specified metrics
  create_sleeptime_plot <- function(plot_data, yParam,Yname, divisions, limits, geom) {
    pdf(paste0(ExperimentData@Batch, '_', yParam, '.pdf'), width = (prod(sapply(divisions[c(4,6)], function(col) length(unique(info[[col]]))))*1.5+2), ## swapped 6 and 5 between this line and the next
        height = length(unique(info[[divisions[5]]]))*3.7 +2)
    sleeptime_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[divisions[4]]],
                                                     y = .data[[yParam]]))+
      ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(divisions[5])),
                          cols = ggplot2::vars(!!rlang::sym(divisions[6])))
      if(geom == "bar"){
        sleeptime_plot <- sleeptime_plot +
          ggplot2::stat_summary(fun = "mean", geom = geom, width = .5, fill="grey90")}
        if(geom == "violin"){
      sleeptime_plot <- sleeptime_plot +
        ggplot2::geom_violin(fill="grey90")}
    sleeptime_plot <- sleeptime_plot +
      ggbeeswarm::geom_beeswarm(ggplot2::aes(fill = .data[[divisions[1]]], color = .data[[divisions[1]]]),
                                dodge.width = 0.9, shape = 21, cex = 3.5) +
      ggplot2::scale_color_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6"), alpha = .7)) +
      ggplot2::scale_fill_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6"), alpha = .6)) +
      ggplot2::geom_errorbar(stat = "summary", fun.data = ggplot2::mean_cl_boot, width = 0.2,
                             color = "black") +
      ggplot2::geom_point(size = 1.5, stat = "summary", fun = mean, shape = 3,
                          color = "black") +
      ggprism::theme_prism(base_fontface = font)  +
      ggplot2::scale_y_continuous(name = Yname, limits = c(0,limits)) +
      ggplot2::scale_x_discrete(name = NULL)+
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20),
                       axis.text.x = ggplot2::element_text(size = 16, angle = 45, vjust = 1, hjust= 1),
                       axis.text.y = ggplot2::element_text(size = 16),
                       strip.text = ggplot2::element_text(size = 20),
                       legend.text = ggplot2::element_text(size = 16, face = font),
                       legend.position = "right")
    print(sleeptime_plot)
    dev.off()
  }

  # Generate sleep time and bout plots for light and dark phases
  create_sleeptime_plot(summary_dt_final, "sleep_time_All", "Total Sleep (min)", divisions, 1500, "bar")
  create_sleeptime_plot(summary_dt_final, "sleep_time_L", "Daytime Sleep (min)", divisions, 1000, "bar")
  create_sleeptime_plot(summary_dt_final, "sleep_time_D", "Nighttime Sleep (min)", divisions, 1000, "bar")
  create_sleeptime_plot(summary_dt_final, "n_bouts_L", "Daytime Sleep Bouts", divisions, 80, "violin")
  create_sleeptime_plot(summary_dt_final, "n_bouts_D", "Nighttime Sleep Bouts", divisions, 80, "violin")
}
  # Return the final summary table
  return(summary_dt_final)
}
