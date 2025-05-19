#' Perform K-Means Clustering on Sleep Data
#'
#' This function reads in a CSV file (`all_batches_stat.csv`) containing summarized and relativized data, performs K-means clustering to identify optimal clusters based on the Elbow Method, Silhouette Score, and Gap Statistic, and creates a plot of sleep trends with clustering information.
#' The user can provide a list of aPrioriConditions (e.g., Genotype categories) which visually differentiates them from each other in the plots.
#' If the data does not have "Grp" and "Iso" Treatments, the 'all_batches_norm_summary.csv' will not be produced by 'runAllBatches' and this function will not run.
#'
#' @param x A string specifying the x-axis variable of the cluster plot as written in 'all_batches_norm_summary.csv' (e.g., "norm_Sleep_Time_All"). Default is NULL.
#' @param y A string specifying the y-axis variable of the cluster plot as written in 'all_batches_norm_summary.csv' (e.g., "norm_mean_Bout_length_L"). Default is NULL.
#' @param condition1 A string specifying a condition within one of the experimental variables. The difference is calculated as: `condition1`-`condition2`
#' @param condition2 A string specifying a condition within the same experimental variable as `condition1` that is associated with the difference seen in sleep.
#' @param sex A string specifying a Sex condition to subset the data by.
#' @param geno A string specifying a Genotype condition to subset the data by.
#' @param temp A string specifying a Temperature condition to subset the data by.
#' @param treat A string specifying a Treatment to subset the data by.
#' @param enviro A string specifying a Environment condition to subset the data by.
#' @param Lights A string specifying a Light condition to subset the data by.
#' @param aPrioriConditions A vector of (partial spellings of the) conditions (e.g., c("L1", "L2", "S1", "S2", "CS")) in one of the experimental variables.
#' @param aPrioriVariable A string specifying the variable of the aPrioriConditions. Default is "Sex".
#' @param lbf Add the line of best fit onto the plot. Default is TRUE
#' @param relValues TRUE or FALSE entry where TRUE means the data used will be the relative data based on the `control` specified when running `run__Batch()`. Default is FALSE
#' @param font A character string determining the font style of the produced plots. Default is "plain".
#'
#' @details
#' This function performs K-means clustering on sleep data for two comparison Treatment groups (`x` and `y`). It automatically determines the optimal number of clusters using three methods:
#' - Elbow Method
#' - Silhouette Score
#' - Gap Statistic
#' The function generates and saves a series of plots showing the sleep trends, with points colored by cluster assignment and shaped by aPrioriConditions information.
#' The clustering results are saved in a CSV file.
#'
#' @return None. Saves the plot as a PDF file and outputs a CSV file with cluster assignments.
#' @export
kmeansCluster <- function(x = c(NULL, "mean_Bout_Length_D","mean_Bout_Length_L", "n_Bouts_D", "n_Bouts_L", "Sleep_Time_D", "Sleep_Time_L"), 
                          y = c(NULL, "mean_Bout_Length_D","mean_Bout_Length_L", "n_Bouts_D", "n_Bouts_L", "Sleep_Time_D", "Sleep_Time_L"),
                          condition1 = NULL, condition2 = NULL, sex = NULL, geno = NULL,
                          temp = NULL, treat = NULL, enviro = NULL,
                          Lights = NULL, aPrioriConditions,
                          aPrioriVariable = c(c("Sex", "Genotype", "Temperature", "Treatment", "Environment", "Light")), 
                          lbf = TRUE, relValues = FALSE, font = c("plain", "bold", "italic", "bold.italic")) {
  # aPrioriConditions <- c("CS", "L1", "L2", "S1", "S2")
  # aPrioriVariable <- "Genotype"
  
  X <- match.arg(x)  # Validate that x is valid.
  Y <- match.arg(y)  # Validate that x is valid.
  aPrioriVariable<- match.arg(aPrioriVariable)
  font<-match.arg(font)
  
  if(relValues){
    # Check if 'all_batches_summary.csv' exists in the current directory, and if not, stop execution.
    if (!file.exists("all_batches_relative_summary.csv")) {
      stop("'all_batches_relative_summary.csv' is not found in the current directory. Please run 'runAllBatches' before attempting to run 'kmeansCluster'. If you have already run it, reset the working directory and run kmeansCluster again.")
    }
    combined_raw_data<-read.csv("all_batches_relative_summary.csv")
    combined_data <- combined_raw_data[, lapply(.SD, mean),
                                   by = .(Sex, Genotype, Temperature, Treatment, Environment, Light, Batch),
                                   .SDcols = c("Sleep_Time_All", "Sleep_Time_L",
                                   "Sleep_Time_D", "n_Bouts_L", "mean_Bout_Length_L",
                                   "n_Bouts_D", "mean_Bout_Length_D")]
  }else{
    # Check if 'all_batches_summary.csv' exists in the current directory, and if not, stop execution.
    if (!file.exists("all_batches_stat.csv")) {
      stop("'all_batches_stat.csv' is not found in the current directory. Please run 'runAllBatches' before attempting to run 'kmeansCluster'. If you have already run it, reset the working directory and run kmeansCluster again.")
    }
    combined_data <- read.csv("all_batches_stat.csv")
  }
  data.table::setDT(combined_data)

  # subset by only selecting rows with condition(s) specified
  if(relValues){
    titlee <- c("Relative")
    }else{
    titlee<- c("")
    }
  
  # Subset data
  metalist<- c("Sex", "Genotype", "Temperature", "Treatment", "Environment", "Light")
  metlist<- c("sex", "geno", "temp", "treat", "enviro","Lights")
  for(i in seq_along(metalist)) {
    if(!is.null(get(metlist[i]))){
      combined_data <- combined_data[combined_data[[metalist[i]]] == get(metlist[i]), ]
      # warning if condition is invalid
      if (nrow(combined_data) == 0) {
        stop(paste("The", metlist[i], "specified is not included in the data within the", metalist[i], "variable"))
      }
      titlee <- trimws(paste(titlee, get(metlist[i])))
      }
  }

  ### aPriori
  # Initialize the aPrioriConditions column with NAs
  combined_data[, aPrioriConditions := as.character(NA)]

  # Dynamically select the aPriori column
  group_Column <- dplyr::pull(combined_data, dplyr::all_of(aPrioriVariable))
  # Dynamically add the aprioriCondition grouping column to the dataset
  for (group in aPrioriConditions) {
    combined_data[is.na(aPrioriConditions) & grepl(group, group_Column), aPrioriConditions := group]
  }

  # meanData<-combined_data
  # Summarize sleep data by temp, Sex, Treatment, and Genotype, calculating means for sleep-related variables.
  meanData <- dplyr::summarise(
    dplyr::group_by(
      combined_data,
      Sex, Genotype, Temperature, Treatment, Environment, Light, aPrioriConditions
    ),
    dplyr::across(
      Sleep_Time_All_mean:mean_Bout_Length_D_ci,
      ~mean(., na.rm = TRUE),
      .names = "{.col}"
    ),
    .groups = "keep"
  )

  # Define sleep time variables.
  parameterlist <- c("Sleep_Time_All_mean", "Sleep_Time_L_mean", "Sleep_Time_D_mean",
                 "n_Bouts_L_mean", "n_Bouts_D_mean", "mean_Bout_Length_L_mean",
                 "mean_Bout_Length_D_mean")
  metalist <- c(metalist, "aPrioriConditions")
  if (!is.null(Y) && !is.null(X)){ ### ??? what does this supposed to do???
  colx <- grep(X, parameterlist)
  coly <- grep(Y, parameterlist)
  dat_cols <- c(colx, coly)
  }

  # if("Grp" %in% meanData$Treatment && "Iso" %in% meanData$Treatment){
  #   # Define parameter variables to compare.
  #   gparameter <- meanData[meanData$Treatment == "Grp", parameterlist]
  #   iparameter <- meanData[meanData$Treatment == "Iso", parameterlist]
  #
  #   df <- iparameter - gparameter
  #
  #   # Rename the columns for better clarity.
  #   colnames(df) <- c("Sleepchange_All", "Sleepchange_L", "Sleepchange_D", "nBoutschange_L",
  #                     "nBoutschange_D", "Boutlenchange_L", "Boutlenchange_D")
  #
  #   meta <-  meanData[meanData$Treatment == "Iso", c("Sex", "Genotype", "Temperature",
  #              "Treatment", "Environment","Light", "aPrioriConditions")]
  #   parameters <- c("Sleepchange_All", "Sleepchange_L", "Sleepchange_D", "nBoutschange_L",
  #               "nBoutschange_D", "Boutlenchange_L", "Boutlenchange_D")
  if(!is.null(condition1) && !is.null(condition2)){
    condition_cols <- meanData[, metalist]
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
    }
    c1parameter <- meanData[meanData[[c1_col]] == condition1, parameterlist]
    c2parameter <- meanData[meanData[[c2_col]] == condition2, parameterlist]
    if(length(c1parameter) != length(c2parameter)){
      stop("There is an uneven number of 'condition1' and 'condition2' populations within the current subset of variables. Please ensure there are no unpaired populations/monitors for 'condition1' and 'condition2'.")
    }
    df <- (c1parameter-c2parameter)
    # Rename the columns for better clarity.
    colnames(df) <- parameters <- c("Sleepchange_All", "Sleepchange_L", "Sleepchange_D", "nBoutschange_L",
                      "nBoutschange_D", "Boutlenchange_L", "Boutlenchange_D")
    titlee <- trimws(paste0(titlee, " ", condition1, "-", condition2))
    meta <-  meanData[meanData[[c2_col]] == condition2, metalist]

      } else {
    df <- meanData[, c(parameterlist, metalist)]
    # Rename the columns for better clarity.
    colnames(df) <- parameters <- c("Sleeptime_All", "Sleeptime_L", "Sleeptime_D", "NBouts_L",
                      "NBouts_D", "Boutlen_L", "Boutlen_D")
    meta <-  meanData[,metalist]
  }

  # Function to compute within-cluster sum of squares
  wss <- function(k) {
    stats::kmeans(data, 2, nstart = 10)$tot.withinss
  }

  # Function to compute average silhouette width
  silhouette_score <- function(k) {
    km <- stats::kmeans(data, centers = k, nstart = 10)
    ss <- cluster::silhouette(km$cluster, dist(data))
    mean(ss[, 3])
  }

  kmax <- min(10, nrow(df) - 1)
  k_values <- 1:kmax


  # Creating a correlation plot colored by clusters and aPrioriConditions
  clustered_plot<- function(data, font, ptsize = 1, title = NULL, x = NULL, y=NULL, lbf = lbf, legend = FALSE){
  p<- ggplot2::ggplot(data, ggplot2::aes(x = data[[1]], y = data[[2]])) +
    ggplot2::geom_point(size = ptsize, stroke = .5, alpha = 2/3,
                        ggplot2::aes(shape = aPrioriConditions,
                                     color = Cluster,
                                     fill = Cluster)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    ggplot2::labs(title = title, x = x, y = y) +
    ggprism::theme_prism(base_fontface = font) +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   axis.title.y = ggplot2::element_text(size = 12),
                   axis.title.x = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 10),
                   axis.text.y = ggplot2::element_text(size = 10),
                   legend.text = ggplot2::element_text(size = 10, face = font),
                   legend.title = ggplot2::element_text(size = 10)) +
    ggplot2::scale_shape_manual(name = "aPriori", values = c(0,1,4,2,3,14,9,10,11)) +
    ggplot2::scale_color_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .7)) +
    ggplot2::scale_fill_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .6))
  if(legend == FALSE){
    p <- p+ ggplot2::guides(color = "none", fill = "none", shape = "none")
  } else{
    p <- p+ ggplot2::guides(
       color = ggplot2::guide_legend(override.aes = list(size = 3)),  # Increase the point size in the legend
       size = ggplot2::guide_legend(override.aes = list(size = 2.5))
    )
  }
  if(lbf == TRUE){
    p <- p+ ggplot2::geom_smooth(method = "lm",  # Line of best fit
                       se = FALSE,      # No error bars
                       col = "grey",     # Single color for the line of best fit
                       linewidth = 0.5)     # Color of the line
  }
    return(p)
  }

  # Create an empty list to store plots
  plots <- list()

  if (!is.null(Y) && !is.null(X)){

    #subset data
    data <- df[,dat_cols]

    wss_values <- sapply(k_values, wss)

    # Silhouette Method
    sil_values <- sapply(2:kmax, silhouette_score)

    # Gap Statistic
    gap_stat <- cluster::clusGap(data, FUN = kmeans, nstart = 10, K.max = kmax, B = 50)

    # Automatically select the best number of clusters
    best_elbow <- which(diff(wss_values) == min(diff(wss_values))) + 1  # Finds the elbow
    best_silhouette <- which.max(sil_values) + 1                        # Max silhouette width
    best_gap <- cluster::maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"])  # Max gap statistic

    # Summarize and choose the best method
    results <- c(best_elbow, best_silhouette, best_gap)
    names(results) <- c("Elbow", "Silhouette", "Gap Statistic")
    optimal_clusters <- as.numeric(names(sort(table(results), decreasing = TRUE)[1]))

    # Perform K-means clustering using the optimal number of clusters
    km_res <- kmeans(data, centers = optimal_clusters, nstart = 10)

    # Add cluster assignment to the original data for plotting
    data$Cluster <- as.factor(km_res$cluster)
    data$aPrioriConditions <- meta$aPrioriConditions
    data$aPrioriVariable <- meta[[aPrioriVariable]]


    myplot <- clustered_plot(data, font, ptsize = 2, title = titlee, x = names(df)[colx], y = names(df)[coly],lbf = lbf, TRUE)
    titlee <- gsub(":", ".", titlee)
    ggplot2::ggsave(paste0("kmeanscluster", titlee,Y,X, ".pdf"), myplot, height = 5, width = 5)

    # Write the file which labels the cluster each Genotype is in
    data.table::fwrite(data, paste0("clusters_", titlee,Y, "~",X, ".csv"))

    } else {
  for (i in seq_along(parameters)) {
    for (j in 1:i) {
      if (i == j) {
        # Add a blank plot for the diagonal
        blank_plot <- ggplot2::ggplot() + ggplot2::theme_void()
        plots[[paste0("plot_", i, "_", j)]] <- blank_plot
      } else {
  dat <- parameters[c(i,j)]
  data<- df[,dat]

  # Elbow Method
  wss_values <- sapply(k_values, wss)

  # Silhouette Method
  sil_values <- sapply(2:kmax, silhouette_score)

  # Gap Statistic
  gap_stat <- cluster::clusGap(data, FUN = kmeans, nstart = 10, K.max = kmax, B = 50)

  # Automatically select the best number of clusters
  best_elbow <- which(diff(wss_values) == min(diff(wss_values))) + 1  # Finds the elbow
  best_silhouette <- which.max(sil_values) + 1                        # Max silhouette width
  best_gap <- cluster::maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"])  # Max gap statistic

  # Summarize and choose the best method
  results <- c(best_elbow, best_silhouette, best_gap)
  # if(i == 6 && j == 4){
  #   lookatme <- results
  #   wuss <- wss_values
  #   silly <- sil_values
  #   gappy <- gap_stat
  # }
  names(results) <- c("Elbow", "Silhouette", "Gap Statistic")
  optimal_clusters <- as.numeric(names(sort(table(results), decreasing = TRUE)[1]))

  # Perform K-means clustering using the optimal number of clusters
  km_res <- kmeans(data, centers = optimal_clusters, nstart = 10)

  # Add cluster assignment to the original data for plotting
  data$Cluster <- as.factor(km_res$cluster)
  data$aPrioriConditions <- meta$aPrioriConditions
  data$aPrioriVariable <- meta[[aPrioriVariable]]
  # if(i == 6 && j == 4){
  #   optical <- optimal_clusters
  #   looky <- data
  # }
  #make all the plots
  plots[[paste0("plot_", i, "_", j)]] <- clustered_plot(data, font,lbf = lbf)
# print(plots[[paste0("plot_", i, "_", j)]])
  # Write the file which labels the cluster each Genotype is in
  data.table::fwrite(data, paste0("clusters_", titlee, parameters[i],"~", parameters[j], ".csv"))
      }
    }
  }

# Create a grid layout for the lower triangle
      layout_matrix <- matrix(NA, nrow = length(parameters)+1, ncol = length(parameters)+1)

      plot_index <- 1
      for (i in seq_along(parameters)) {
        for (j in 1:i) {
          layout_matrix[length(parameters) - j+1, i+1] <- plot_index
          plot_index <- plot_index + 1
        }
      }
      for (i in seq_along(parameters)) {
          layout_matrix[length(parameters) - i + 1, 1] <- plot_index
          plot_index <- plot_index + 1
        }
      for (i in seq_along(parameters)) {
        layout_matrix[length(parameters)+1, i+1] <- plot_index
        plot_index <- plot_index + 1
      }
      # add labels as plots
      row_labels <- lapply(parameters, function(x) grid::textGrob(x, gp = grid::gpar(fontsize = 18, fontface = font)))
      col_labels <- lapply(parameters, function(x) grid::textGrob(x, rot = 45, gp = grid::gpar(fontsize = 18, fontface = font)))
      ploters<- c(plots, row_labels, col_labels)
      title_grob <- grid::textGrob(paste("Clustered Sleep parameters", titlee), gp = grid::gpar(fontsize = 20, fontface = font))

      # Arrange the plots in the grid
      myplot<- gridExtra::grid.arrange(grobs = ploters,
                              layout_matrix = layout_matrix,
                              heights = c(rep(0.9, length(parameters)), 0.9),
                              widths = c(1, rep(0.9, length(parameters))),
                              top = title_grob)
# need to change the size here

      # Define colors and shapes
      colors <- c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500", "#FFD700", "#32CD32", "#800080", "#000080")
      shapes <- c(0, 1, 4, 2, 3, 14, 9, 10, 11)

      # Create aPrioriCondition for shapes
      # aPrioriCondition <- c("Cond1", "Cond2", "Cond3", "Cond4", "Cond5", "Cond6", "Cond7", "Cond8", "Cond9", "Cond10")

      # Prepare data for the legends
      legend_data_colors <- data.frame(
        Cluster = factor(1:10),
        Color = colors
      )

      legend_data_shapes <- data.frame(
        aPriori = factor(aPrioriConditions),
        Shape = shapes[1:length(aPrioriConditions)]
      )

      # Create the color legend
      color_legend <- ggplot2::ggplot(legend_data_colors, ggplot2::aes(x = 1, y = Cluster, color = Cluster)) +
        ggplot2::geom_point(size = 6) +
        ggplot2::scale_color_manual(values = colors) +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position = "none",
          legend.text = ggplot2::element_text(size = 18, face = font),
          legend.title = ggplot2::element_text(size = 18)) +
        ggplot2::labs(title = "Colors")

      # Create the shape legend
      shape_legend <- ggplot2::ggplot(legend_data_shapes, ggplot2::aes(x = 1, y = aPriori, shape = aPriori)) +
        ggplot2::geom_point(size = 6) +
        ggplot2::scale_shape_manual(values = shapes) +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position = "none",
          legend.text = ggplot2::element_text(size = 18, face = font),
          legend.title = ggplot2::element_text(size = 18)) +
        ggplot2::labs(title = "Shapes")

      # Extract the legends
      suppressWarnings(
      color_grob <- cowplot::get_legend(color_legend + ggplot2::theme(legend.position = "right"))
      )
      suppressWarnings(
        shape_grob <- cowplot::get_legend(shape_legend + ggplot2::theme(legend.position = "right"))
      )
      # Combine the legends
      combined_legend <- gridExtra::grid.arrange(
        gridExtra::arrangeGrob(color_grob, shape_grob, ncol = 1)
      )

      # Save the final plot with legends
      final_plot <- gridExtra::grid.arrange(
        myplot,
        combined_legend,
        ncol = 2,
        widths = c(14, 1.5) # Adjust widths as needed
      )

      titlee <- gsub(" ", "", titlee)
      titlee <- gsub(":", ".", titlee)

      ggplot2::ggsave(paste0("kmeansparameters", titlee, ".pdf"), final_plot, height = 14, width = 16.5)
  }
}
