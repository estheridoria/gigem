#' Helper to Create and Save Population or Overlay Sleep Plot (Internal)
#'
#' This flexible internal function generates either a fully-faceted population plot 
#' or a condensed overlay plot, saving the result as a PDF.
#'
#' @param filename A character string specifying the full path and filename for the output PDF.
#' @param batchMeta A data.table containing the metadata (behavr::meta(dt)) 
#'   of the curated data, used for dynamic plot sizing.
#' @param plot_data A \code{behavr} table containing the final curated time-series data.
#' @param divisions A character vector (length >= 3) specifying the grouping columns
#'   for colour, row facet, and column facet aesthetics, respectively.
#' @param numb_days Numeric, the number of days represented in the x-axis, used for width calculation.
#' @param font A character string for the font face (e.g., "plain", "bold").
#' @param overlay_mode Logical. If TRUE, the plot uses the condensed (overlay) facetting structure 
#'   (omitting the colour variable from the row facets). Defaults to FALSE (population mode).
#' @param wrap_time Time duration to wrap the x-axis (e.g., behavr::hours(24)). Defaults to \code{NULL}.
#'
#' @return \code{NULL}. Saves a PDF plot file.
#' @keywords internal
wrapper_sleep_profile_plot <- function(filename, batchMeta, plot_data, divisions, numb_days, font, overlay_mode = FALSE, wrap_time = NULL) {
  
  # Call the new helper function to get the plot object and sizing
  plot_result <- render_sleep_profile_plot(
    plot_data = plot_data, 
    divisions = divisions, 
    batchMeta = batchMeta, 
    numb_days = numb_days, 
    font = font, 
    overlay_mode = overlay_mode, 
    wrap_time = wrap_time
  )
  
  # Use the returned dynamic sizing variables
  pdf(filename, width = plot_result$plot_width, height = plot_result$plot_height)
  
  print(plot_result$plot)
  dev.off()
}