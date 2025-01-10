#' Generate Combined Genotype Plots (Internal)
#'
#' Creates combined plots for each unique combination of `light`, `environment`, and `genotype` in the dataset.
#' Generates overlay sleep plots and sleep duration plots, saving each combination as a PDF file.
#'
#' @param combined_sleepdata A `data.table` summarized accross all batches containing curated sleep data with columns such as `id` and `asleep`.
#' @param combined_sleepmeta A `data.table` summarized accross all batches containing the metadata associated with combined_sleepdata.
#' @param summary_dt_final A `data.table` containing summary statistics with columns including `light`, `environment`,
#'   `genotype`, `treatment`, `sex` and various sleep metrics.
#' @param font A character string variable determining the font style of the produced plots.
#'
#' @details
#' The function iterates through all unique combinations of `light`, `environment`, `treatment`, `sex`, and `genotype`.
#' For each combination:
#' - An overlay sleep plot is created using `ggetho`.
#' - Multiple sleep duration plots (e.g., total sleep, daytime sleep, nighttime sleep) are generated
#'   using a helper function, `create_sleeptime_plot`.
#' - Plots are combined into a single figure using `cowplot::plot_grid`.
#' - The combined figure is saved as a PDF file, with the filename reflecting the combination of `light`,
#'   `environment`, and `genotype`.
#'
#' @return None. Plots are saved as PDF files.
#' @keywords internal

concatGenotypePlots <- function(combined_sleepdata, combined_sleepmeta, summary_dt_final, font) {

  summary_dt_final <- summary_dt_final[, light := gsub("\"", "", light)]


  #link metadata and behavr data
  data.table::setkey(combined_sleepdata, id)
  data.table::setkey(combined_sleepmeta, id)
  dt_curated_final <- behavr::behavr(combined_sleepdata, combined_sleepmeta)

  # Get unique values for light, environment, and genotype
  llist <- unique(summary_dt_final$light)
  elist <- unique(summary_dt_final$environment)
  glist <- unique(summary_dt_final$genotype)
  tlist <- unique(summary_dt_final$treatment)
  slist <- unique(summary_dt_final$sex)

  # Create a data.table for the combinations of all conditions
  condition_combinations <- expand.grid(
    light = if (divisions[1] != "light") llist else NA,
    environment = if (divisions[1] != "environment") elist else NA,
    genotype = if (divisions[1] != "genotype") glist else NA,
    treatment = if (divisions[1] != "treatment") tlist else NA,
    sex = if (divisions[1] != "sex") slist else NA,
    stringsAsFactors = FALSE
  )

  # Apply the logic for subsetting and plotting using data.table
  condition_combinations_dt <- data.table::data.table(condition_combinations)
  result <- condition_combinations_dt[, {
    # Subset data based on the current combination of conditions
    sub_data <- summary_dt_final[
      (divisions[1] == "light" | light == .SD$light) &
        (divisions[1] == "environment" | environment == .SD$environment) &
        (divisions[1] == "genotype" | genotype == .SD$genotype) &
        (divisions[1] == "treatment" | treatment == .SD$treatment) &
        (divisions[1] == "sex" | sex == .SD$sex),
    ]

    # Curate data for plotting
    plot_subdata <- dt_curated_final[id %in% sub_data$id]
    plot_subdata2 <- summary_dt_final[id %in% sub_data$id]

    p1title <- trimws(paste(
      if (divisions[1] != "sex" && !is.na(sex) && sex != "NA") {sex},
      if (divisions[1] != "genotype" && !is.na(genotype) && genotype != "NA") {genotype},
      if (divisions[1] != "light" && !is.na(light) && light != "NA") {light},
      if (divisions[1] != "treatment" && !is.na(treatment) && treatment != "NA") {treatment},
      if (divisions[1] != "environment" && !is.na(environment) && environment != "NA") {environment}
    ))

          # Create overlay sleep plot
          p1 <- ggetho::ggetho(plot_subdata, ggplot2::aes(y = asleep, colour = .data[[divisions[1]]]), time_wrap = behavr::hours(24)) +
            ggetho::stat_pop_etho(show.legend = T) +
            ggetho::stat_ld_annotations() +
            ggplot2::scale_color_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +
            ggplot2::scale_fill_manual(values = c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6")) +
            ggplot2::labs(title = p1title, y= "% Flies Sleeping") +
            ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent)+
            ggprism::theme_prism(base_fontface = font) +
            ggplot2::theme(title = ggplot2::element_text(size = 22),
                           axis.title.x = ggplot2::element_text(size = 20),
                           axis.title.y = ggplot2::element_text(size = 20),
                           axis.text.x = ggplot2::element_text(size = 16),
                           axis.text.y = ggplot2::element_text(size = 16),
                           legend.text = ggplot2::element_text(size = 16, face = font))
          if(length(unique(plot_subdata2[[divisions[1]]])) <= 2){
            p1 <- p1 + ggplot2::theme(legend.position.inside = c(0.8,0.15))}

          # Function to create sleep duration plots
          create_sleeptime_plot <- function(plot_data, yParam, Yname, limits, geom, font) {
            pointplot<- ggplot2::ggplot(plot_data,
                                        ggplot2::aes(x = .data[[divisions[1]]], y = .data[[yParam]]))
            if(geom == "bar"){
              pointplot <- pointplot +
                ggplot2::stat_summary(fun = "mean", geom = geom, width = .5, fill="grey90")}
            if(geom == "violin"){
              pointplot <- pointplot +
                ggplot2::geom_violin(fill="grey90")}
            pointplot <- pointplot +
              ggbeeswarm::geom_beeswarm(ggplot2::aes(fill = .data[[divisions[1]]], color = .data[[divisions[1]]]),
                                        dodge.width = 0.9, shape = 21, cex = 3.5) +
              ggplot2::scale_color_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6"), alpha = .7)) +
              ggplot2::scale_fill_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#FFA500","#ADD8E6"), alpha = .6)) +
              ggplot2::geom_errorbar(stat = "summary", fun.data = ggplot2::mean_cl_boot, width = 0.2,
                                     color = "black") +
              ggplot2::geom_point(size = 1.5, stat = "summary", fun = mean, shape = 3,
                                  color = "black") +
              ggplot2::scale_y_continuous(name = Yname, limits = c(0,limits)) +
              ggplot2::scale_x_discrete(name = NULL)+
              ggprism::theme_prism(base_fontface = font)  +
              ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20),
                             axis.text.x = ggplot2::element_text(size = 16, angle = 45, vjust = 1, hjust= 1),
                             axis.text.y = ggplot2::element_text(size = 16),
                             legend.position = "none")
            return(pointplot)
          }

          # Generate sleep duration plots
          p2 <- create_sleeptime_plot(plot_subdata2, "sleep_time_all", "Total Sleep (min)", 1500, "bar", font = font)
          p3 <- create_sleeptime_plot(plot_subdata2, "sleep_time_l", "Daytime Sleep (min)", 1000, "bar", font = font)
          p4 <- create_sleeptime_plot(plot_subdata2, "sleep_time_d", "Nighttime Sleep (min)", 1000, "bar", font = font)
          p5 <- create_sleeptime_plot(plot_subdata2, "n_bouts_L", "Daytime Sleep Bouts", 80, "violin", font = font)
          p6 <- create_sleeptime_plot(plot_subdata2, "n_bouts_D", "Nighttime Sleep Bouts", 80, "violin", font = font)


          # Combine plots
          u <- length(unique(plot_subdata2[[divisions[1]]])) # Dynamic width adjustment
          suppressWarnings(
            combined_plot <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, ncol = 6, align = "h", axis = "b",
                                                rel_widths = c(6, u, u, u, u, u)))
          p1title <- gsub(" ", "_", p1title)
          # Save combined plot
          ggplot2::ggsave(paste0("CombinedPlots",p1title,".pdf"), combined_plot,
                          width = (6 + u * 5 + 1.45), height = 4)
  }, by = 1:nrow(condition_combinations_dt)]
}
