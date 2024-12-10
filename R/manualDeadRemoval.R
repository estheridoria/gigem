#' Manually Remove IDs of Animals Detected as Dead (Internal)
#'
#' Removes animals detected as dead before a specified final time point, trims the data to a defined time range,
#' and generates population and overlay plots of sleep data.
#'
#' @param ExperimentData An S4 object containing experiment metadata, including `Batch` for output filenames.
#' @param dt A `data.table` containing experimental results, including columns `id` and `t` (time).
#' @param num_days Numeric, the minimum lifespan (in days) required for an animal to be retained.
#' @param divisions A character vector defining grouping variables for plot facets.
#' @param pref A numeric vector specifying whether to generate population plots (`pref[2] == 1`) and overlay plots (`pref[3] == 1`).
#'
#' @details
#' This function removes animals that are detected as dead before the specified lifespan threshold (`num_days`).
#' It filters the data to retain only animals with a lifespan greater than or equal to `num_days`, generates population
#' plots and overlay plots of sleep data based on user preferences (`pref`), and saves the results as PDF files.
#'
#' Population plots are saved with the suffix `_population_sleep.pdf` and overlay plots with `_sleep_overlay.pdf`.
#'
#' @return A `data.table` of the curated data (`dt`) limited to animals meeting the lifespan threshold.
#' @keywords internal
manualDeadRemoval <- function(ExperimentData, dt, num_days, divisions, pref) {

  # Remove animals dying too early
  lifespan_dt <- dt[, .(lifespan = max(t)), by=id]
  valid_ids <- lifespan_dt[lifespan >= behavr::days(num_days), id] ## previously it was stated > not >=

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
                                     dt_curated_2$t <= behavr::days(num_days)]


  if(pref[2]==1){
  # Generate population plots
  create_population_plot <- function(filename, plot_data, divisions, numb_days = num_days, wrap_time = NULL) {
    pdf(filename, width = 5*numb_days+2,
        height = 3*prod(sapply(divisions[1:3], function(col) length(unique(info[[col]]))))+2)
    pop_sleep_plot <- ggetho::ggetho(plot_data, ggplot2::aes(y = asleep, colour = .data[[divisions[1]]]), time_wrap = wrap_time) +
      ggetho::stat_pop_etho() +
      ggetho::stat_ld_annotations() +
      ggplot2::scale_color_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +
      ggplot2::scale_fill_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +

      ggprism::theme_prism(base_fontface = "plain", base_line_size = 0.7) +
      ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(divisions[1]),
                                               !!rlang::sym(divisions[2]),
                                               !!rlang::sym(divisions[3])))+
      ggplot2::labs(y = "Percentage of flies sleeping") +
      ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent)
    print(pop_sleep_plot)
    dev.off()
  }

  suppressWarnings(
  create_population_plot(paste0(ExperimentData@Batch, '_population_sleep.pdf'), dt_curated_final, divisions))
  suppressWarnings(
  create_population_plot(paste0(ExperimentData@Batch, '_population_sleep_wrap.pdf'), dt_curated_final, divisions, numb_days = 1, wrap_time = behavr::hours(24)))

}

  if(pref[3]==1){
  # Generate overlay plots
  create_overlay_plot <- function(filename, plot_data, divisions, numb_days = num_days, wrap_time = NULL) { ##might work without this "divisions"
    pdf(filename,width = 5*numb_days*length(unique(info[[divisions[3]]]))+2,
        height = 3*length(unique(info[[divisions[2]]]))+2)
    pop_sleep_plot <- ggetho::ggetho(plot_data, ggplot2::aes(y = asleep, colour = .data[[divisions[1]]]), time_wrap = wrap_time) +
      ggetho::stat_pop_etho() +
      ggetho::stat_ld_annotations()+
      ggplot2::scale_color_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +
      ggplot2::scale_fill_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +
      ggprism::theme_prism(base_fontface = "plain", base_line_size = 0.7) +
      ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(divisions[2])),
                          cols = ggplot2::vars(!!rlang::sym(divisions[3])))+
      ggplot2::labs(y = "Percentage of flies sleeping") +
      ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent)
    print(pop_sleep_plot)
    dev.off()
  }
  suppressWarnings(
  create_overlay_plot(paste0(ExperimentData@Batch, '_sleep_overlay.pdf'), dt_curated_final, divisions, numb_days = num_days))
  suppressWarnings(
  create_overlay_plot(paste0(ExperimentData@Batch, '_sleep_wrap_overlay.pdf'), dt_curated_final, divisions, numb_days = 1, wrap_time = behavr::hours(24)))

}

  return(dt_curated_final)
  }
