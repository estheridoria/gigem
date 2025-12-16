#' Helper to Render Population or Overlay Sleep Plot (Returns ggplot)
#'
#' Generates the ggplot object and returns it along with sizing variables.
#'
#' @param plot_data A behavr table containing the final curated time-series data.
#' @param divisions A character vector (length >= 3) specifying grouping columns.
#' @param batchMeta A data.table containing metadata for dynamic sizing.
#' @param numb_days Numeric, the number of days represented in the x-axis.
#' @param font A character string for the font face.
#' @param overlay_mode Logical. TRUE for condensed (overlay) mode. Defaults to FALSE.
#' @param wrap_time Time duration to wrap the x-axis. Defaults to behavr::hours(24).
#' @param p1title A string for the plot title (optional, used in combinedPlots).
#' @return A list containing the ggplot object (plot), dynamic width (plot_width), and height (plot_height).
render_sleep_profile_plot <- function(plot_data, divisions, batchMeta, numb_days, font,
                                      overlay_mode = FALSE, wrap_time = behavr::hours(24), p1title = NULL) {

  if (is.null(wrap_time)){
    wrap_time<- behavr::days(numb_days)
  }
  # 1. Define width and height
  plot_width <- 5 * numb_days * length(unique(batchMeta[[divisions[3]]])) + 2

  if (overlay_mode) {
    row_fascets <- ggplot2::vars(!!rlang::sym(divisions[2]))
    plot_height <- 4 * length(unique(batchMeta[[divisions[2]]]))
    } else {
    row_fascets <- ggplot2::vars(!!rlang::sym(divisions[1]), !!rlang::sym(divisions[2]))
    plot_height <- 4 * length(unique(batchMeta[[divisions[2]]])) * length(unique(batchMeta[[divisions[1]]]))
}

  # --- Plot Generation ---
    pop_sleep_plot <- ggetho::ggetho(plot_data, ggplot2::aes(x = t, y = asleep, colour = .data[[divisions[1]]]), time_wrap = wrap_time) +
    ggetho::stat_pop_etho() +
    ggetho::stat_ld_annotations() +
    ggplot2::scale_color_manual(values = scales::alpha(c("#0000FF", "#FF0000",
                                "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .7)) +
    ggplot2::scale_fill_manual(values = scales::alpha(c("#0000FF", "#FF0000",
                               "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .6)) +
    # Facet
      ggplot2::facet_grid(rows = row_fascets, cols = ggplot2::vars(!!rlang::sym(divisions[3])))+
      # Only combinedPlots has a p1title. If overlay_mode is for combinedPlots, use labs for title and theme to position legend
      ggplot2::labs(title = p1title) +
    # Aesthetics
    #ggplot2::labs(y = "Sleep (%)") +
    ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent) +
    ggprism::theme_prism(base_fontface = font, base_line_size = 0.7) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20),
                   axis.title.y = ggplot2::element_text(size = 20),
                   axis.text.x = ggplot2::element_text(size = 16),
                   axis.text.y = ggplot2::element_text(size = 16),
                   legend.text = ggplot2::element_text(size = 16, face = font),
                   strip.text = if (overlay_mode) {
                     ggplot2::element_blank()
                   } else {
                     ggplot2::element_text(size = 20)
                   }

  return(list(plot = pop_sleep_plot, plot_width = plot_width, plot_height = plot_height))
}
