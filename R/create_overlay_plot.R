create_overlay_plot <- function(filename, batchMeta, plot_data, divisions, numb_days = numDays, wrap_time = NULL) {
  pdf(filename,
      width = 5*numb_days*length(unique(batchMeta[[divisions[3]]]))+2,
      height = 3*length(unique(batchMeta[[divisions[2]]]))+2)
  pop_sleep_plot <- ggetho::ggetho(plot_data, ggplot2::aes(x = t, y = asleep, colour = .data[[divisions[1]]]), time_wrap = wrap_time) +
    ggetho::stat_pop_etho() +
    ggetho::stat_ld_annotations() +
    ggplot2::scale_color_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +
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