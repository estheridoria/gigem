#' Perform K-Means Clustering on Sleep Data
#'
#' This function reads in a CSV file (`all_batches_norm_summary.csv`) containing summarized and relativized data, performs K-means clustering to identify optimal clusters based on the Elbow Method, Silhouette Score, and Gap Statistic, and creates a plot of sleep trends with clustering information.
#' The user can provide a list of aPrioriConditions (e.g., genotype categories) which visually differentiates them from each other in the plots.
#' If the data does not have "Grp" and "Iso" treatments, the 'all_batches_norm_summary.csv' will not be produced by 'runAllBatches' and this function will not run.
#'
#' @param x A string specifying the x-axis parameter of the cluster plot as written in 'all_batches_norm_summary.csv' (e.g., "norm_sleep_time_all").
#' @param y A string specifying the y-axis parameter of the cluster plot as written in 'all_batches_norm_summary.csv' (e.g., "norm_mean_bout_length_L").
#' @param treat A string specifying a treatment to subset the data by.
#' @param temp A string specifying a temperature condition to subset the data by.
#' @param enviro A string specifying a environment condition to subset the data by.
#' @param lights A string specifying a light condition to subset the data by.
#' @param geno A string specifying a genotype condition to subset the data by.
#' @param aPrioriConditions A vector of (partial spellings of the) conditions (e.g., c("L1", "L2", "S1", "S2", "CS")) in one of the experimental parameters.
#' @param aPrioriParameter A string specifying the parameter of the aPrioriConditions.
#' @param font A character string determining the font style of the produced plots. ("plain", "bold", "italic", or "bold.italic")
#'
#' @details
#' This function performs K-means clustering on sleep data for two comparison treatment groups (`x` and `y`). It automatically determines the optimal number of clusters using three methods:
#' - Elbow Method
#' - Silhouette Score
#' - Gap Statistic
#' The function generates and saves a series of plots showing the sleep trends, with points colored by cluster assignment and shaped by aPrioriConditions information.
#' The clustering results are saved in a CSV file.
#'
#' @return None. Saves the plot as a PDF file and outputs a CSV file with cluster assignments.
#' @export
kmeansCluster <- function(x, y, treat = NULL, temp = NULL, enviro = NULL, lights = NULL, geno = NULL, aPrioriConditions, aPrioriParameter, font = "plain") {
  # Check if 'all_batches_norm_summary.csv' exists in the current directory, and if not, stop execution.
  if (!file.exists("all_batches_norm_summary.csv")) {
    stop("'all_batches_norm_summary.csv' is not found in the current directory. Please run 'runAllBatches' before attempting to run 'kmeansCluster'. If you have already run it, ")
  }
  # Validate that y is provided and is a valid string.
  if(missing(y) || !rlang::is_string(y)){
    stop("y is missing or invalid")
  }

  # Validate that x is provided and is a valid string.
  if(missing(x) || !rlang::is_string(x)){
    stop("x is missing or invalid")
  }

  if (missing(aPrioriParameter) || !(aPrioriParameter %in% c("temperature", "sex", "treatment", "genotype", "environment", "light"))) {
    stop("'aPrioriParameter' is missing or invalid")
  }
  if (!(font %in% c("plain", "bold", "italic","bold.italic"))){
    stop("'font' must be 'plain', 'bold', 'italic', or 'bold.italic'")
  }
  # Read the normalized data from CSV file
  combined_data <- read.csv("all_batches_norm_summary.csv")
  combined_data$light <- gsub("\"", "", combined_data$light)

  data.table::setDT(combined_data)

  # subset by only selecting rows with condition(s) specified
  titlee <- c("")
  if(!is.null(treat)){
    combined_data <- combined_data[combined_data$treatment == treat,]

    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'treat' specified is not included in the data within the 'treatment' parameter")
    }
    titlee <- trimws(paste(titlee, treat))
  }
  if(!is.null(temp)){
    combined_data <- combined_data[combined_data$temperature == temp,]
    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'temp' specified is not included in the data within the 'temperature' parameter")
    }
    titlee <- trimws(paste(titlee, temp))
  }
  if(!is.null(enviro)){
    combined_data <- combined_data[combined_data$environment == enviro,]
    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'enviro' specified is not included in the data within the 'environment' parameter")
    }
    titlee <- trimws(paste(titlee, enviro))
  }
  if(!is.null(lights)){
    combined_data <- combined_data[combined_data$light == lights,]
    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'lights' specified is not included in the data within the 'light' parameter")
    }
    titlee <- trimws(paste(titlee, lights))
  }
  if(!is.null(geno)){
    combined_data <- combined_data[combined_data$genotype == geno,]
    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'geno' specified is not included in the data within the 'genotype' parameter")
    }
    titlee <- trimws(paste(titlee, geno))
  }

  combined_data <- data.table::setDT(combined_data)

  # Initialize the aPrioriConditions column with NAs
  combined_data[, aPrioriConditions := as.character(NA)]

  # Dynamically select the aPriori column
  group_Column <- dplyr::pull(combined_data, dplyr::all_of(aPrioriParameter))
  # Dynamically add the aprioriCondition grouping column to the dataset
  for (group in aPrioriConditions) {
    combined_data[is.na(aPrioriConditions) & grepl(group, group_Column), aPrioriConditions := group]
  }

  # Further processing
  columnnames <- c("x", "y", aPrioriParameter,"aPrioriConditions")

  # This code automatically selects the optimal number of clusters and prints the plots

  aPrioriParameter.col <-   combined_data[, get(aPrioriParameter)]
  aPrioriConditions.col <- combined_data[, "aPrioriConditions"]
  aPrioriConditions.col[is.na(aPrioriConditions)] <- "Other"
  df <- cbind(combined_data[[x]], combined_data[[y]], aPrioriParameter.col, aPrioriConditions.col)
  colnames(df) <- columnnames

  data <- df[, c(1,2)]  # Selecting only relevant columns for clustering

  # Function to compute within-cluster sum of squares
  wss <- function(k) {
    stats::kmeans(data, k, nstart = 10)$tot.withinss
  }

  # Function to compute average silhouette width
  silhouette_score <- function(k) {
    km <- stats::kmeans(data, centers = k, nstart = 10)
    ss <- cluster::silhouette(km$cluster, dist(data))
    mean(ss[, 3])
  }

  kmax <- min(10, nrow(data) - 1)

  # Elbow Method
  k_values <- 1:kmax
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
  df$cluster <- as.factor(km_res$cluster)

  xtitle <- gsub("_", " ", x)
  ytitle<- gsub("_", " ", y)
  titlee <- gsub(" ", "", titlee)
  # Creating a correlation plot colored by clusters and aPrioriConditions
  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(size = 5, stroke = 2, alpha = 2/3,
                        ggplot2::aes(shape = aPrioriConditions,
                                     color = cluster,
                                     fill = cluster)) +
    ggplot2::labs(
      title = trimws(paste("Sleep Trend with Clustering", titlee)),
      x = xtitle, y = ytitle) +
    ggprism::theme_prism(base_fontface = font) +
    ggplot2::theme(title = ggplot2::element_text(size = 24),
                   axis.title.y = ggplot2::element_text(size = 26),
                   axis.title.x = ggplot2::element_text(size = 26),
                   axis.text.x = ggplot2::element_text(size = 20),
                   axis.text.y = ggplot2::element_text(size = 20),
                   legend.text = ggplot2::element_text(size = 20, face = font),
                   legend.title = ggplot2::element_text(size = 20)) +
    ggplot2::scale_shape_manual(name = "A Priori", values = c(0,1,4,2,3,14,9,10,11)) +
    ggplot2::scale_fill_viridis_d(name = "Cluster", option = "D", begin = 0, end = 0.8) +
    ggplot2::scale_color_viridis_d(name = "Cluster", option = "D", begin = 0, end = 0.8)

  ggplot2::ggsave(paste0("ClusteredPlot_", titlee, x, y, ".pdf"), height = 7, width = 7)


  # Write the file which labels the cluster each genotype is in
  data.table::fwrite(df, paste0("clustered", titlee, x, y, ".csv"))

}
