#' Perform K-Means Clustering on Sleep Data
#'
#' This function reads in a CSV file (`all_batches_norm_summary.csv`) containing summarized and relativized data, performs K-means clustering to identify optimal clusters based on the Elbow Method, Silhouette Score, and Gap Statistic, and creates a plot of sleep trends with clustering information.
#' The user can provide a list of aPrioriConditions (e.g., Genotype categories) which visually differentiates them from each other in the plots.
#' If the data does not have "Grp" and "Iso" Treatments, the 'all_batches_norm_summary.csv' will not be produced by 'runAllBatches' and this function will not run.
#'
#' @param x A string specifying the x-axis parameter of the cluster plot as written in 'all_batches_norm_summary.csv' (e.g., "norm_Sleep_Time_All"). Default is NULL.
#' @param y A string specifying the y-axis parameter of the cluster plot as written in 'all_batches_norm_summary.csv' (e.g., "norm_mean_Bout_length_L"). Default is NULL.
#' @param condition1 A string specifying a condition within one of the experimental parameters. The difference is calculated as: `condition1`-`condition2`
#' @param condition2 A string specifying a condition within the same experimental parameter as `condition1` that is associated with the difference seen in sleep.
#' @param sex A string specifying a Sex condition to subset the data by.
#' @param geno A string specifying a Genotype condition to subset the data by.
#' @param temp A string specifying a Temperature condition to subset the data by.
#' @param treat A string specifying a Treatment to subset the data by.
#' @param enviro A string specifying a Environment condition to subset the data by.
#' @param Lights A string specifying a Light condition to subset the data by.
#' @param aPrioriConditions A vector of (partial spellings of the) conditions (e.g., c("L1", "L2", "S1", "S2", "CS")) in one of the experimental parameters.
#' @param aPrioriVariable A string specifying the parameter of the aPrioriConditions.
#' @param font A character string determining the font style of the produced plots. ("plain", "bold", "italic", or "bold.italic")
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
kmeansCluster <- function(x = NULL, y = NULL, condition1 = NULL, condition2 = NULL, sex = NULL, geno = NULL,
                                 temp = NULL, treat = NULL, enviro = NULL,
                                 Lights = NULL, aPrioriConditions,
                                 aPrioriVariable, font = "plain") {
  # aPrioriConditions <- c("CS", "L1", "L2", "S1", "S2")
  # aPrioriVariable <- "Genotype"

  # Read the normalized data from CSV file
  combined_data <- read.csv("all_batches_stat.csv")
  # combined_data$Light <- gsub("\"", "", combined_data$Light)

  data.table::setDT(combined_data)

  # Check if 'all_batches_summary.csv' exists in the current directory, and if not, stop execution.
  if (!file.exists("all_batches_stat.csv")) {
    stop("'all_batches_stat.csv' is not found in the current directory. Please run 'runAllBatches' before attempting to run 'kmeansCluster'. If you have already run it, ")
  }

  # Validate that y is valid.
  if(!is.null(y) && !any(grep(y, colnames(combined_data)[15:30]))){
    stop("'y' is invalid")
  }

  # Validate that x is valid.
  if(!is.null(x) && !any(grep(x, colnames(combined_data)[15:30]))){
    stop("'x' is invalid")
  }

  if (missing(aPrioriVariable) || !(aPrioriVariable %in% c( "Sex", "Genotype", "Temperature", "Treatment", "Environment", "Light"))) {
    stop("'aPrioriVariable' is missing or invalid")
  }
  if (!(font %in% c("plain", "bold", "italic","bold.italic"))){
    stop("'font' must be 'plain', 'bold', 'italic', or 'bold.italic'")}

  # subset by only selecting rows with condition(s) specified
  titlee <- c("")
  if(!is.null(treat)){
    combined_data <- combined_data[combined_data$Treatment == treat,]

    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'treat' specified is not included in the data within the 'Treatment' parameter")
    }
    titlee <- trimws(paste(titlee, treat))
  }
  if(!is.null(temp)){
    combined_data <- combined_data[combined_data$Temperature == temp,]
    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'temp' specified is not included in the data within the 'Temperature' parameter")
    }
    titlee <- trimws(paste(titlee, temp))
  }
  if(!is.null(enviro)){
    combined_data <- combined_data[combined_data$Environment == enviro,]
    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'enviro' specified is not included in the data within the 'Environment' parameter")
    }
    titlee <- trimws(paste(titlee, enviro))
  }
  if(!is.null(Lights)){
    combined_data <- combined_data[combined_data$Light == Lights,]
    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'Lights' specified is not included in the data within the 'Light' parameter")
    }
    titlee <- trimws(paste(titlee, Lights))
  }
  if(!is.null(geno)){
    combined_data <- combined_data[combined_data$Genotype == geno,]
    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'geno' specified is not included in the data within the 'Genotype' parameter")
    }
    titlee <- trimws(paste(titlee, geno))
  }
  if(!is.null(sex)){
    combined_data <- combined_data[combined_data$Sex == sex,]
    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'geno' specified is not included in the data within the 'Genotype' parameter")
    }
    titlee <- trimws(paste(titlee, sex))
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

  # Summarize sleep data by temp, Sex, Treatment, and Genotype, calculating means for sleep-related variables.
  meanData <- dplyr::summarise(
    dplyr::group_by(
      combined_data,
      Sex, Genotype, Temperature, Treatment, Environment, Light, aPrioriConditions
    ),
    dplyr::across(
      Sleep_Fraction_All:mean_Bout_Length_D,
      ~mean(., na.rm = TRUE),
      .names = "{.col}"
    ),
    .groups = "keep"
  )

  # Define sleep time variables.
  traitlist <- c("Sleep_Time_All_mean", "Sleep_Time_L_mean", "Sleep_Time_D_mean",
                 "n_Bouts_L_mean", "n_Bouts_D_mean", "mean_Bout_Length_L_mean",
                 "mean_Bout_Length_D_mean")
  if (!is.null(y) && !is.null(x)){ ### ??? what does this supposed to do???
  colx <- grep(x, traitlist)
  coly <- grep(y, traitlist)
  }

  # if("Grp" %in% meanData$Treatment && "Iso" %in% meanData$Treatment){
  #   # Define trait variables to compare.
  #   gtrait <- meanData[meanData$Treatment == "Grp", traitlist]
  #   itrait <- meanData[meanData$Treatment == "Iso", traitlist]
  #
  #   df <- itrait - gtrait
  #
  #   # Rename the columns for better clarity.
  #   colnames(df) <- c("Sleepchange_All", "Sleepchange_L", "Sleepchange_D", "nBoutschange_L",
  #                     "nBoutschange_D", "Boutlenchange_L", "Boutlenchange_D")
  #
  #   meta <-  meanData[meanData$Treatment == "Iso", c("Sex", "Genotype", "Temperature",
  #              "Treatment", "Environment","Light", "aPrioriConditions")]
  #   traits <- c("Sleepchange_All", "Sleepchange_L", "Sleepchange_D", "nBoutschange_L",
  #               "nBoutschange_D", "Boutlenchange_L", "Boutlenchange_D")
  if(!is.null(condition1) && !is.null(condition2)){
    condition_cols <- meanData[, c("Sex","Genotype","Temperature","Treatment",
                                  "Environment","Light")]
    c1_col <- names(condition_cols)[
      sapply(condition_cols, function(column) any(grepl(condition1, column)))
    ]
    c2_col <- names(condition_cols)[
      sapply(condition_cols, function(column) any(grepl(condition2, column)))
    ]
    if(length(c1_col) == 0 | length(c2_col) == 0){
      stop("'condition1' and/or 'condition2' is not found inside the data. Please check the spelling. Also check that one of the conditions was not removed via the parameter subsetting option in this function.")
    }
    if(c1_col != c2_col){
      stop("'condition1' and condition2' are not within the same parameter.")
    }
    c1trait <- meanData[meanData[[c1_col]] == condition1, traitlist]
    c2trait <- meanData[meanData[[c2_col]] == condition2, traitlist]
    if(length(c1trait) != length(c2trait)){
      stop("There is an uneven number of 'condition1' and 'condition2' populations within the current subset of parameters. Please ensure there are no unpaired populations/monitors for 'condition1' and 'condition2'.")
    }
    df <- (c1trait-c2trait)
    # Rename the columns for better clarity.
    colnames(df) <- traits <- c("Sleepchange_All", "Sleepchange_L", "Sleepchange_D", "nBoutschange_L",
                      "nBoutschange_D", "Boutlenchange_L", "Boutlenchange_D")
    titlee <- trimws(paste0(titlee, " ", condition1, "-", condition2))
    meta <-  meanData[meanData[[c1_col]] == condition1, c("Sex", "Genotype", "Temperature",
              "Treatment", "Environment","Light", "aPrioriConditions")]

  } else {
    df <- meanData[, c(traitlist, c("Sex", "Genotype", "Temperature",
                                      "Treatment", "Environment","Light",
                                      "aPrioriConditions"))]
    # Rename the columns for better clarity.
    colnames(df) <- traits <- c("Sleeptime_All", "Sleeptime_L", "Sleeptime_D", "NBouts_L",
                      "NBouts_D", "Boutlen_L", "Boutlen_D")
    meta <-  meanData[,c("Sex", "Genotype", "Temperature","Treatment", "Environment", "Light", "aPrioriConditions")]
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
  clustered_plot<- function(data, font, legend = FALSE){
  p<- ggplot2::ggplot(data, ggplot2::aes(x = data[[1]], y = data[[2]])) +
    ggplot2::geom_point(size = 1, stroke = .5, alpha = 2/3,
                        ggplot2::aes(shape = aPrioriConditions,
                                     color = cluster,
                                     fill = cluster)) +
    ggplot2::geom_smooth(method = "lm",  # Line of best fit
                se = FALSE,      # No error bars
                col = "grey",     # Single color for the line of best fit
                size = 0.5)    + # Color of the line
    ggplot2::labs(title = NULL, x = NULL, y = NULL) +
    ggprism::theme_prism(base_fontface = font) +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   axis.title.y = ggplot2::element_text(size = 13),
                   axis.title.x = ggplot2::element_text(size = 13),
                   axis.text.x = ggplot2::element_text(size = 10),
                   axis.text.y = ggplot2::element_text(size = 10),
                   legend.text = ggplot2::element_text(size = 10, face = font),
                   legend.title = ggplot2::element_text(size = 10)) +
    ggplot2::scale_shape_manual(name = "A Priori", values = c(0,1,4,2,3,14,9,10,11)) +
    ggplot2::scale_color_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .7)) +
    ggplot2::scale_fill_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .6))
  if(legend == FALSE){
    p<- p+ ggplot2::guides(color = "none", fill = "none", shape = "none")
  }
    return(p)
  }

  # Create an empty list to store plots
  plots <- list()

  if (!is.null(y) && !is.null(x)){

    #subset data
    data <- df[,c(colx, coly)]

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
    data$cluster <- as.factor(km_res$cluster)
    data$aPrioriConditions <- meta$aPrioriConditions
    data$aPrioriVariable <- meta[[aPrioriVariable]]

    myplot <- clustered_plot(data, font, TRUE)
    titlee <- gsub(":", ".", titlee)
    ggplot2::ggsave(paste0("kmeanscluster", titlee, y, x, ".pdf"), myplot, height = 5, width = 5)

    # Write the file which labels the cluster each Genotype is in
    data.table::fwrite(data, paste0("clusters_", titlee, y, "~", x, ".csv"))

    }
  else {
  for (i in seq_along(traits)) {
    for (j in 1:i) {
      if (i == j) {
        # Add a blank plot for the diagonal
        blank_plot <- ggplot2::ggplot() + ggplot2::theme_void()
        plots[[paste0("plot_", i, "_", j)]] <- blank_plot
      } else {

  data<- df[,traits[c(i,j)]]

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
  names(results) <- c("Elbow", "Silhouette", "Gap Statistic")
  optimal_clusters <- as.numeric(names(sort(table(results), decreasing = TRUE)[1]))

  # Perform K-means clustering using the optimal number of clusters
  km_res <- kmeans(data, centers = optimal_clusters, nstart = 10)

  # Add cluster assignment to the original data for plotting
  data$cluster <- as.factor(km_res$cluster)
  data$aPrioriConditions <- meta$aPrioriConditions
  data$aPrioriVariable <- meta[[aPrioriVariable]]

  #make all the plots
  plots[[paste0("plot_", i, "_", j)]] <- clustered_plot(data, font)

  # Write the file which labels the cluster each Genotype is in
  data.table::fwrite(data, paste0("clustered", titlee, traits[i], traits[j], ".csv"))
      }
    }
  }

# Create a grid layout for the lower triangle
      layout_matrix <- matrix(NA, nrow = length(traits)+1, ncol = length(traits)+1)

      plot_index <- 1
      for (i in seq_along(traits)) {
        for (j in 1:i) {
          layout_matrix[length(traits) - j + 1, i+1] <- plot_index
          plot_index <- plot_index + 1
        }
      }
      for (i in seq_along(traits)) {
          layout_matrix[length(traits) - i + 1, 1] <- plot_index
          plot_index <- plot_index + 1
        }
      for (i in seq_along(traits)) {
        layout_matrix[length(traits)+1, i+1] <- plot_index
        plot_index <- plot_index + 1
      }
      row_labels <- lapply(traits, function(x) grid::textGrob(x, gp = grid::gpar(fontsize = 10, fontface = font)))
      col_labels <- lapply(traits, function(x) grid::textGrob(x, rot = 45, gp = grid::gpar(fontsize = 10, fontface = font)))
      ploters<- c(plots, row_labels, col_labels)
      # legend <- cowplot::get_legend(clustered_plot(data, font, TRUE))

      # c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080")
      # c(0,1,4,2,3,14,9,10,11)

      # Arrange the plots in the grid
      myplot<- gridExtra::grid.arrange(grobs = ploters,
                              layout_matrix = layout_matrix,
                              heights = c(0.6, rep(0.9, length(traits))),
                              widths = c(0.6, rep(0.9, length(traits))),
                              top = paste("Clustered Sleep Traits", titlee))

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
        ggplot2::geom_point(size = 3) +
        ggplot2::scale_color_manual(values = colors) +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position = "none"
        ) +
        ggplot2::labs(title = "Colors")

      # Create the shape legend
      shape_legend <- ggplot2::ggplot(legend_data_shapes, ggplot2::aes(x = 1, y = aPriori, shape = aPriori)) +
        ggplot2::geom_point(size = 3) +
        ggplot2::scale_shape_manual(values = shapes) +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position = "none"
        ) +
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

      ggplot2::ggsave(paste0("kmeanstraits", titlee, ".pdf"), final_plot, height = 14, width = 15.5)
  }
}
