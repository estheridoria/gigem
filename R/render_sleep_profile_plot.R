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

  # 1. Define Facet Structure and Height based on Mode
  group_col <- divisions[1]

  if (overlay_mode) {
    # Overlay mode: divisions[2] in the rows (for general use) or NULL (for combinedPlots)
    # For combinedPlots, we ignore divisions[2] and [3] facets, and only use the aesthetic.
    facet_rows <- NULL
    plot_height <- 4 # A fixed small height is appropriate when not using facets for combination
  } else {
    # Population mode: divisions[1] and divisions[2] in the rows
    facet_rows <- ggplot2::vars(!!rlang::sym(group_col), !!rlang::sym(divisions[2]))
    plot_height <- 3 * length(unique(batchMeta[[divisions[2]]])) * length(unique(batchMeta[[group_col]])) + 2
  }

  # Calculate Plot Width
  plot_width <- 5 * numb_days * length(unique(batchMeta[[divisions[3]]])) + 2

  # --- Plot Generation ---
  pop_sleep_plot <- ggetho::ggetho(plot_data, ggplot2::aes(x = t, y = asleep, colour = .data[[group_col]]),
                                   time_wrap = wrap_time) +
    ggetho::stat_pop_etho() +
    ggetho::stat_ld_annotations() +
    ggplot2::scale_color_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +
    ggplot2::scale_fill_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +
    # Facet (only if not in overlay_mode, or if using a simple row facet)
    # Note: If overlay_mode is TRUE, the 'facet_rows' logic above must handle the simple overlay case.
    { if (!is.null(facet_rows)) {
      ggplot2::facet_grid(rows = facet_rows, cols = ggplot2::vars(!!rlang::sym(divisions[3])))
    } else {
      # If overlay_mode is for combinedPlots, use labs for title and theme to position legend
      ggplot2::labs(title = p1title)
    }
    } +

    # Aesthetics
    ggplot2::labs(y = "Sleep (%)") +
    ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent) +
    ggprism::theme_prism(base_fontface = font, base_line_size = 0.7) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20),
                   axis.title.y = ggplot2::element_text(size = 20),
                   axis.text.x = ggplot2::element_text(size = 16),
                   axis.text.y = ggplot2::element_text(size = 16),
                   legend.text = ggplot2::element_text(size = 16, face = font),
                   strip.text = ggplot2::element_text(size = 20))

  return(list(plot = pop_sleep_plot, plot_width = plot_width, plot_height = plot_height))
}
