#' Manually Remove IDs of Animals Detected as Dead (Internal)
#'
#' Removes animals detected as dead before a specified final time point, trims the data to a defined time range,
#' and generates population and overlay plots of sleep data.
#'
#' @param ExperimentData An S4 object containing experiment metadata, including `Batch` for output filenames.
#' @param dt A `data.table` containing experimental results, including columns `id` and `t` (time).
#' @param numDays Numeric, the minimum lifespan (in days) required for an animal to be retained.
#' @param divisions A list of grouping columns used for facetting plots.
#' @param pref A numeric vector specifying whether to generate population plots (`pref[2] == 1`) and overlay plots (`pref[3] == 1`).
#' @param font A character string variable determining the font style of the produced plots. ("plain", "bold", "italic", or "bold.italic")
#'
#' @details
#' Population plots are saved with the suffix `_population_sleep.pdf` and overlay plots with `_sleep_overlay.pdf`.
#'
#' @return A `data.table` of the curated data (`dt`) limited to animals meeting or exceeding the lifespan threshold.
#' @keywords internal
manualDeadRemoval <- function(ExperimentData, dt, numDays, divisions, pref, font) {
  # # debug
  # dt<- dt_curated

  # Remove animals dying too early
  lifespan_dt <- dt[, .(lifespan = max(t)), by=id]
  valid_ids <- lifespan_dt[lifespan >= behavr::days(numDays), id] ## previously it was stated > not >=

  # Apply the filter and ensure it stays a behavr object
  filtered_meta <- behavr::meta(dt)[id %in% valid_ids]
  dt_curated_2 <- dt[id %in% valid_ids]
  dt_curated_2 <- behavr::behavr(dt_curated_2, filtered_meta)

  # Identify and save IDs being removed
  removed_ids <- setdiff(dt[, id, meta = TRUE], dt_curated_2[, id, meta = TRUE]) # replaced with the line below
  curated_2_list <- data.table::data.table(removed_ids)
  data.table::fwrite(curated_2_list, paste0("removed_list2_", ExperimentData@Batch, ".csv"))

  # Trim data to the desired time range
  dt_curated_final <- dt_curated_2[dt_curated_2$t >= behavr::days(0) &
                                     dt_curated_2$t <= behavr::days(numDays)]

  #save behavr and metadata into csv files (with Light quoted) for concatGenotypePlots
  if (pref[7] == 1) {
  data.table::fwrite(dt_curated_final, paste0("sleepdata_",ExperimentData@Batch, ".csv"))
  meta_data <- behavr::meta(dt_curated_final)
  meta_data <- meta_data[, !sapply(meta_data, is.list), with = FALSE]
  meta_data[, Light := paste0('"', Light, '"')]
  data.table::fwrite(meta_data,
                     paste0("sleepmeta_",ExperimentData@Batch, ".csv"),
                     quote = TRUE)
  }


  if(pref[2]==1){
  # Generate population plots
  create_population_plot <- function(filename, plot_data, divisions, numb_days = numDays, wrap_time = NULL) {
    pdf(filename,
        # width = 5*numb_days+2,
        # height = 3*prod(sapply(divisions[1:3], function(col) length(unique(info[[col]]))))+2)
        width = 5*numb_days*length(unique(info[[divisions[3]]]))+2,
        height = 3*length(unique(info[[divisions[2]]]))*length(unique(info[[divisions[1]]]))+2)
    pop_sleep_plot <- ggetho::ggetho(plot_data, ggplot2::aes(x=t, y = asleep, colour = .data[[divisions[1]]]),
                                     time_wrap = wrap_time) +
      ggetho::stat_pop_etho() +
      ggetho::stat_ld_annotations(ggplot2::aes(x=t), inherit.aes = FALSE) +
      ggplot2::scale_color_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +
      ggplot2::scale_fill_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +

      ggprism::theme_prism(base_fontface = font) +
      ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(divisions[1]),
                                               !!rlang::sym(divisions[2])),
                          cols = ggplot2::vars(!!rlang::sym(divisions[3]))) + #added
      ggplot2::labs(y = "Sleep (%)") +
      ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20),
                     axis.title.y = ggplot2::element_text(size = 20),
                     axis.text.x = ggplot2::element_text(size = 16),
                     axis.text.y = ggplot2::element_text(size = 16),
                     legend.text = ggplot2::element_text(size = 16, face = font),
                     strip.text = ggplot2::element_text(size = 20)) +
      ggetho::stat_ld_annotations()
    print(pop_sleep_plot)
    dev.off()
  }

  suppressWarnings(
    create_population_plot(paste0(ExperimentData@Batch, '_Population_Sleep_Profiles.pdf'),
                           dt_curated_final, divisions))
  suppressWarnings(
    create_population_plot(paste0(ExperimentData@Batch, '_Population_Sleep_Profiles_Wrap.pdf'),
                            dt_curated_final, divisions, numb_days = 1, wrap_time = behavr::hours(24)))

}

  if(pref[3]==1){
  # Generate overlay plots
  create_overlay_plot <- function(filename, plot_data, divisions, numb_days = numDays, wrap_time = NULL) {
    pdf(filename,
        width = 5*numb_days*length(unique(info[[divisions[3]]]))+2,
        height = 3*length(unique(info[[divisions[2]]]))+2)
    pop_sleep_plot <- ggetho::ggetho(plot_data, ggplot2::aes(x = t, y = asleep, colour = .data[[divisions[1]]]), time_wrap = wrap_time) +
      ggetho::stat_pop_etho() +
      ggetho::stat_ld_annotations() +
      ggplot2::scale_color_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +
      ggplot2::scale_fill_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +
      ggprism::theme_prism(base_fontface = font, base_line_size = 0.7) +
      ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(divisions[2])),
                          cols = ggplot2::vars(!!rlang::sym(divisions[3])))+
      ggplot2::labs(y = "Sleep (%)") +
      ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20),
                     axis.title.y = ggplot2::element_text(size = 20),
                     axis.text.x = ggplot2::element_text(size = 16),
                     axis.text.y = ggplot2::element_text(size = 16),
                     legend.text = ggplot2::element_text(size = 16, face = font),
                     strip.text = ggplot2::element_text(size = 20))
    print(pop_sleep_plot)
    dev.off()
  }
  suppressWarnings(
    create_overlay_plot(paste0(ExperimentData@Batch, '_Overlaid_Sleep_Profile.pdf'),
                      dt_curated_final, divisions, numb_days = numDays))
  suppressWarnings(
    create_overlay_plot(paste0(ExperimentData@Batch, '_Overlaid_Sleep_Profile_Wrap.pdf'),
                      dt_curated_final, divisions, numb_days = 1, wrap_time = behavr::hours(24)))

}

  return(dt_curated_final)
  }
