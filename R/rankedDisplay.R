#' Visualize Changes in Sleep Between
#'
#' This function generates bar plots to visualize the changes in sleep across different Genotypes and Treatments.
#' It reads a CSV file containing normalized sleep data, calculates the changes in sleep, and then creates plots
#' for each of the sleep-related metrics (`TotalSleepChange`, `DayTimeSleepChange`, `NightTimeSleepChange`).
#' Two plots are made showing the normalized sleeploss for the Treatment of interest (`treat`).
#' One plot contains only the `control` entry in `x` while the other plot contains every entry except `control`.
#' The plots are saved as PDFs labeled, "NormalizedSleepLoss...".
#'
#' @param x The name of a variable in "all_batches_norm_summary.csv" to be used as the x-axis in the plot.
#' @param control The name of the control within the variable x
#' @param condition1 A string specifying a condition within one of the experimental variables. The percent difference is calculated as: (`condition1`-`condition2`) / `condition2`
#' @param condition2 A string specifying a condition within the same experimental variable as `condition1` that is associated with the percent difference seen in sleep.
#' @param treat A string specifying a Treatment to subset the data by.
#' @param temp A string specifying a Temperature condition to subset the data by.
#' @param enviro A string specifying a Environment condition to subset the data by.
#' @param sex A string specifying a sex condition to subset the data by.
#' @param Lights A string specifying a Light condition to subset the data by.
#' @param geno A string specifying a Genotype condition to subset the data by.
#' @param ranking A tibble of one column containing the order of conditions displayed in the plot from variable x
#' @param font A character string determining the font style of the produced plots. ("plain", "bold", "italic", or "bold.italic")
#' @param limits A list of two numerics to be the upper and lower limits on the plot. Default is c(-100, 1500)
#'
#'
#' @return conditional. If the ranking is NULL (default), then the ranking used in the plots will be returned to be used as the ranking in subsequent rankedDisplay plots. Otherwise, no return. Saves the plots as a PDF file labeled `RankedSleep...`
#' @export
#'
#' @keywords internal
rankedDisplay <- function(x, control = NULL, condition1 = NULL, condition2 = NULL, treat = NULL,
                          temp = NULL, enviro = NULL, sex = NULL, Lights = NULL, geno = NULL, ranking = NULL,
                          font = "plain", limits = c(-100, 1500)) {

  if (!file.exists("all_batches_stat.csv")) {
    stop("'all_batches_stat.csv' is not found in the current directory. Please run 'runAllBatches' before attempting to run 'kmeansCluster'. If you have already run it, ")
  }
  # Validate that x is provided and is a valid string.
  if(missing(x) || !rlang::is_string(x) || !(x %in% c("Temperature", "Sex", "Treatment", "Genotype", "Environment", "Light"))){
    stop("x is missing or invalid")
  }
  if (!(font %in% c("plain", "bold", "italic","bold.italic"))){
    stop("'font' must be 'plain', 'bold', 'italic', or 'bold.italic'")
  }


  # Read the normalized data from CSV file
  dataset <- read.csv("all_batches_stat.csv")
  # dataset$Light <- gsub("\"", "", dataset$Light)
  titlee <- c("")
  if(!is.null(treat)){
    dataset <- dataset[dataset$Treatment == treat,]

    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'treat' specified is not included in the data within the 'Treatment' variable")
    }
    titlee <- trimws(paste(titlee, treat))
  }
  if(!is.null(temp)){
    dataset <- dataset[dataset$Temperature == temp,]
    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'temp' specified is not included in the data within the 'Temperature' variable")
    }
    titlee <- trimws(paste(titlee, temp))
  }
  if(!is.null(enviro)){
    dataset <- dataset[dataset$Environment == enviro,]
    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'enviro' specified is not included in the data within the 'Environment' variable")
    }
    titlee <- trimws(paste(titlee, enviro))
  }
  if(!is.null(sex)){
    dataset <- dataset[dataset$Sex == sex,]
    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'sex' specified is not included in the data within the 'Sex' variable")
    }
    titlee <- trimws(paste(titlee, sex))
  }
  if(!is.null(Lights)){
    dataset <- dataset[dataset$Light == Lights,]
    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'Lights' specified is not included in the data within the 'Light' variable")
    }
    titlee <- trimws(paste(titlee, Lights))
  }
  if(!is.null(geno)){
    dataset <- dataset[dataset$Genotype == geno,]
    # warning if condition is invalid
    if (nrow(dataset) == 0) {
      stop("The 'geno' specified is not included in the data within the 'Genotype' variable")
    }
    titlee <- trimws(paste(titlee, geno))
  }

  # dataset <- data.table::setDT(dataset)
  parameterlist <- c("Sleep_Time_All_mean", "Sleep_Time_L_mean", "Sleep_Time_D_mean",
                 "n_Bouts_L_mean", "n_Bouts_D_mean", "mean_Bout_Length_L_mean",
                 "mean_Bout_Length_D_mean")
  if(!is.null(condition1) && !is.null(condition2)){
    condition_cols <- dataset[c("Sex","Genotype","Temperature","Treatment",
                                "Environment","Light","Batch")]
    c1_col <- names(condition_cols)[
      sapply(condition_cols, function(column) any(grepl(condition1, column)))
    ]
    c2_col <- names(condition_cols)[
      sapply(condition_cols, function(column) any(grepl(condition2, column)))
    ]
    if(length(c1_col) == 0 | length(c2_col) == 0){
      stop("'condition1' and/or 'condition2' is not found inside the data. Please check the spelling. Also check that one of the conditions was not removed via the variable subsetting option in this function.")
    }
    if(c1_col != c2_col){
      stop("'condition1' and condition2' are not within the same variable.")
    } #else{condition_cols <- condition_cols[!(names(condition_cols) %in% c(c1_col))]}
    c1parameter <- dataset[dataset[[c1_col]] == condition1, c(parameterlist, names(condition_cols))]

    c2parameter <- dataset[dataset[[c2_col]] == condition2, c(parameterlist, names(condition_cols))]
    if(length(c1parameter) != length(c2parameter)){
      stop("There is an uneven number of 'condition1' and 'condition2' populations within the current subset of variables. Please ensure there are no unpaired populations/monitors for 'condition1' and 'condition2'.")
    }

    # Make sure we sort based on variables
    c1parameter_sorted <- c1parameter[order(c1parameter$Sex, c1parameter$Genotype, c1parameter$Temperature,
                                    c1parameter$Treatment, c1parameter$Environment, c1parameter$Light),]
    c2parameter_sorted<- c2parameter[order(c2parameter$Sex, c2parameter$Genotype, c2parameter$Temperature,
                                   c2parameter$Treatment, c2parameter$Environment, c2parameter$Light),]

    non_numeric_cols <- c1parameter[!sapply(c1parameter, is.numeric)]
    non_numeric_cols_noc1col <- non_numeric_cols[!(names(non_numeric_cols) %in% c(c1_col))]
    numeric_cols <- c1parameter[sapply(c1parameter_sorted, is.numeric)]

    # df <- (c1parameter-c2parameter)/c2parameter -- Perform the calculation only on the numeric columns
    pre_df <- (c1parameter_sorted[, names(numeric_cols)] - c2parameter_sorted[, names(numeric_cols)]) / c2parameter_sorted[, names(numeric_cols)]

    df <- cbind(pre_df, c1parameter_sorted[, names(non_numeric_cols_noc1col), drop = FALSE])


    # Rename the columns for better clarity.
    colnames(df) <- c("TotalSleepChange", "DaySleepChange", "NightSleepChange", "DayNChange",
                      "NightNBoutsChange", "DayBoutLengthChange", "NightBoutLengthChange", names(non_numeric_cols_noc1col))

    # order by sleep change All
    if(is.null(ranking)){
      ret <- TRUE
      average_lookup <- dplyr::summarise(
      dplyr::group_by(df, !!rlang::sym(x)),
      TotalSleepChange = mean(TotalSleepChange))
    average_lookup <- average_lookup[order(average_lookup$TotalSleepChange), ]  # sort
    ranking <- average_lookup[,1]
    }else{
    ret <- FALSE
      }

    # Reorder x based on the average changes in sleep
    df[[x]] <- factor(df[[x]], levels = ranking[[x]])

    titlee <- trimws(paste0(titlee, " ", condition1, "-", condition2))
    names <- c("TotalSleepChange", "DaySleepChange", "NightSleepChange")

    y<- "Change in Sleep (%)"
  } else {
    save_cols<- c(parameterlist, c("Sex", "Genotype", "Temperature",
                               "Treatment", "Environment", "Light", "Batch"))
    df <- dataset[,save_cols]
    # Rename the columns for better clarity.
    colnames(df) <- parameters <- c("TotalSleepTime", "DaySleepTime", "NightSleepTime", "DayNBouts",
                                "NightNBouts", "DayBoutLength", "NightBoutLength", "Sex", "Genotype", "Temperature",
                                "Treatment", "Environment", "Light", "Batch")
    if(is.null(ranking)){
      ret <- TRUE
    # order by sleep change All
    average_lookup <- dplyr::summarise(
      dplyr::group_by(df, !!rlang::sym(x)),
      TotalSleepChange = mean(TotalSleepTime))
    average_lookup <- average_lookup[order(average_lookup$TotalSleepChange), ]  # sort
    ranking <- average_lookup[,1]
    }else{
      ret <- FALSE
    }

    # Reorder x based on the average changes in sleep
    df[[x]] <- factor(df[[x]], levels = ranking[[x]])

    # List of sleep-related metrics to plot
    names <- c("TotalSleepTime", "DaySleepTime", "NightSleepTime")

    y <- "Sleep (min)"
  }
  #separate control & noncontrol x variable conditions
  if(!is.null(control)){

    if(any(grep(control, df[,x]))){
      df_con <- df[df[,x] == control,]
      df_con <- df_con[order(df_con$Batch), ]

      df_nc <- df[df[,x] != control,]
    } else{
      stop("The 'control' condition is not within variable 'x'")
    }
  }else{
    df_nc <- df
  }

  titleee <- gsub(" ", "", titlee)

  # Create a function to iterate through each plot
  plot_fun <- function(plot_data, title, x, ytitle, font, Param, limits) {
    p<- ggplot2::ggplot(plot_data, ggplot2::aes (x = get(x), y = get(Param))) +
      ggplot2::coord_cartesian(ylim = limits) +
      ggplot2::stat_summary(fun = "mean", geom = "bar", width = .85, fill="grey50") +
      ggplot2::labs(title = title,
                    x = "",
                    y = ytitle) +
      ggprism::theme_prism(base_fontface = font) +
      ggplot2::theme(
        title = ggplot2::element_text(size = 12),
        axis.text.x = ggplot2::element_text(hjust = 1, vjust = .5, angle = 90, size = 9),
        axis.text.y = ggplot2::element_text(size = 9),
        axis.title.y = ggplot2::element_text(size = 12),
        legend.position="none")
    return(p)
  }

  p<-c<- list()
  # Loop for generating plots for the main data (df_nc)
  for (i in 1:length(names)) {
    p[[i]] <- plot_fun(plot_data = df_nc, title = paste(names[i],
                                                        titlee),x = x, ytitle = y, font = font, Param = names[i], limits)
    invisible(print(p[[i]]))
  }

  # Check if control is provided and generate control plots (df_con)
  if (!is.null(control)) {
    for (i in 1:length(names)) {

      c[[i]] <- plot_fun(plot_data = df_con, title = control,
                         x = "Batch", ytitle = y, font = font, Param = names[i], limits)
      invisible(print(c[[i]]))
    }
    # Combine the plots with control and save them
    combined_plot <- cowplot::plot_grid(
      p[[1]], c[[1]], p[[2]], c[[2]], p[[3]], c[[3]],
      ncol = 2, align = "h", axis = "tb",
      rel_widths = rep(c(length(unique(df_nc[, x])) / 8 + 0.85,
                     length(unique(df_con$Batch)) / 8 + .85), 6),
      rel_heights = rep(3,6)
    )
    titleee <- gsub(":", ".", titleee)
    ggplot2::ggsave(paste0("RankedSleep_", paste0(titleee, control), "_", names[i],".pdf"),
                    combined_plot, width = (length(unique(df_nc[, x])) / 8 +
                                              length(unique(df_con$Batch)) / 8 + 1.7), height = (9))
  } else {
    # Combine the plots without control and save them
    combined_plot <- cowplot::plot_grid(
      p[[1]], p[[2]], p[[3]],
      ncol = 1, align = "h", axis = "tb",
      rel_widths = rep(length(unique(df_nc[, x])) / 8 + 1.45, 3),
      rel_heights = rep(3,6)
    )
    ggplot2::ggsave(paste0("RankedSleep_", paste0(titleee), "_", names[i],".pdf"),
                    combined_plot, width = (length(unique(df_nc[, x])) / 8 + 0.85), height = (9))
  }

if(ret){
  return(ranking)
}
}

