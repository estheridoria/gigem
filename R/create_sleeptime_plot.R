#' Create Sleep Time and Bout Summary Plot (Unified)
#'
#' Generates a plot for a specific sleep metric, optionally faceted by grouping
#' variables. Includes mean, CIs, individual data points, and optional P-value
#' annotation for two-group comparisons.
#'
#' @param plot_data A data.table subset containing the summary metrics.
#' @param yParam A string: the name of the column to plot on the Y-axis.
#' @param Yname A string: the descriptive label for the Y-axis.
#' @param divisions A list of 3 grouping columns for facetting (e.g., divisions[1] is the X-axis group).
#' @param limits The upper limit for the Y-axis (for coord_cartesian).
#' @param geom A string: either "bar" (for geom_bar/stat_summary) or "violin" (for geom_violin).
#' @param font A string: the base font family to use for the plot.
#' @param p_value A numeric p-value to display, or NULL/non-numeric to skip annotation (default: NULL).
#' @param is_faceted Logical: TRUE if facetting should be applied (e.g., for batch summaries). Default is FALSE.
#'
#' @return A ggplot object.
#' @export
create_sleeptime_plot <- function(plot_data, yParam, Yname, divisions, limits, geom, font, p_value = NULL, is_faceted = FALSE) {

  pointplot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[divisions[1]]], y = .data[[yParam]]))

  # 1. CONDITIONAL FACETTING (Used in batch summaries, not combinedPlots)
  if(is_faceted) {
    pointplot <- pointplot +
      ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(divisions[2])),
                          cols = ggplot2::vars(!!rlang::sym(divisions[3])))
  }

  # 2. Add background geometry
  if(geom == "bar"){
    pointplot <- pointplot +
      ggplot2::stat_summary(fun = "mean", geom = geom, width = .5, fill="grey90")}
  if(geom == "violin"){
    pointplot <- pointplot +
      ggplot2::geom_violin(fill="grey90")}

  # 3. Add individual data points and stats (using the wider color palette)
  pointplot <- pointplot +
    ggbeeswarm::geom_beeswarm(ggplot2::aes(fill = .data[[divisions[1]]], color = .data[[divisions[1]]]),
                              dodge.width = 0.5, shape = 21, cex = 3.5) +
    ggplot2::scale_color_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .7)) +
    ggplot2::scale_fill_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .6)) +
    ggplot2::geom_errorbar(stat = "summary", fun.data = ggplot2::mean_cl_boot, width = 0.2, color = "black") +
    ggplot2::geom_point(size = 1.5, stat = "summary", fun = mean, shape = 3, color = "black") +
    ggplot2::scale_y_continuous(name = Yname) +
    ggplot2::coord_cartesian(ylim = c(0,limits))+
    ggplot2::scale_x_discrete(name = NULL)+
    ggprism::theme_prism(base_fontface = font) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20),
                   axis.text.x = ggplot2::element_text(size = 16, angle = 45, vjust = 1, hjust= 1),
                   axis.text.y = ggplot2::element_text(size = 16))

  # 4. CONDITIONAL P-VALUE ANNOTATION (Used only in combinedPlots)
  if (!is.null(p_value) && is.numeric(p_value) && p_value != "No") {
    thresholds <- c(0.0001, 0.001, 0.01, 0.05, 0.07)
    labels <- c("****", "***", "**", "*", " ")

    p_label <- ifelse(p_value < thresholds[1], labels[1],
                      ifelse(p_value < thresholds[2], labels[2],
                             ifelse(p_value < thresholds[3], labels[3],
                                    ifelse(p_value < thresholds[4], labels[4],
                                           ifelse(p_value < thresholds[5], labels[5],
                                                  "")))))

    if (p_label !="") {
      pointplot <- pointplot +
        ggplot2::annotate("text", x = 1.5, y = (limits), label = paste("p =", round(p_value, 4)),
                          size = 4.5, color = "black", fontface = font) +
        ggplot2::annotate("text", x = 1.5, y = (limits - (limits / 13)), label = p_label,
                          size = 5, color = "black", fontface = font) +
        ggplot2::geom_segment( mapping = NULL, x = 1, xend = 2,
                               y = (limits -(limits/20)- (limits / 20)),
                               yend = (limits - (limits / 20)- (limits / 20)),
                               color = "black", linewidth = 1)
    }
  }
  if(!is_faceted){
    pointplot <- pointplot + ggplot2::theme(legend.position = "none")
  }
  return(pointplot)
}
