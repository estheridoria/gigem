#' Helper to Create and Save a Tile-Based Sleep Actogram (Internal)
#'
#' This internal function is used for Quality Control (QC) within the data
#' curation process. It generates a density tile plot of sleep data for
#' visualizing the activity pattern of all animals in a dataset.
#'
#' @param data A \code{behavr} table containing animal activity and sleep data,
#'   including the columns \code{t}, \code{id}, and \code{asleep}.
#' @param filename A character string specifying the full path and filename
#'   for the output plot. The plot is saved as a PDF file.
#'
#' @return \code{NULL}. The function's main purpose is the side-effect of
#'   saving a PDF plot file.
#'
#' @seealso \code{\link[sleepr]{curate_dead_animals}}, \code{\link{aliveVsDead}}
#' @keywords internal
create_sleep_plot <- function(data, filename) {
  pdf(filename)
  sleep_plot <- ggetho::ggetho(data, ggplot2::aes(z = asleep)) +
    ggetho::stat_ld_annotations(ypos = "top") +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
                   strip.background = ggplot2::element_rect(fill="white"),
                   axis.text.y = ggplot2::element_text(size = 2))+
    ggetho::stat_tile_etho()
  print(sleep_plot)
  dev.off()
}
