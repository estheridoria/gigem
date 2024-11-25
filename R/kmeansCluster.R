#' Perform K-Means Clustering on Sleep Data
#'
#' This function reads in a CSV file (`all_batches_summary.csv`) containing summarized data, performs K-means clustering to identify optimal clusters based on the Elbow Method, Silhouette Score, and Gap Statistic, and creates plots of sleep trends with clustering information.
#' The user can provide a list of groupings (e.g., genotype categories) and add them dynamically to the dataset.
#'
#' @param combined_data A `data.frame` containing summarized data, with columns for genotype, sleep time, and other factors.
#' @param Compare1 A string specifying the first comparison treatment group (e.g., "Control").
#' @param Compare2 A string specifying the second comparison treatment group (e.g., "Treatment").
#' @param groupings A character vector of group names (e.g., c("L1", "L2", "S1", "S2", "CS")) to match against the specified column (default is `genotype`).
#' @param column_name A string specifying the column name for grouping in the data (default is `genotype`).
#'
#' @details
#' This function performs K-means clustering on sleep data for two comparison treatment groups (`Compare1` and `Compare2`). It automatically determines the optimal number of clusters using three methods:
#' - Elbow Method
#' - Silhouette Score
#' - Gap Statistic
#' The function generates and saves a series of plots showing the sleep trends, with points colored by cluster assignment and pregrouping information.
#' The clustering results are saved in a CSV file.
#'
#' @return None. Saves the plots as PDF files and outputs a CSV file with cluster assignments.
#' @export
kmeansCluster <- function(Compare1, Compare2, groupings, column_name) {

  # Check if 'all_batches_summary.csv' exists in the current directory, and if not, stop execution.
  if (!file.exists("all_batches_summary.csv")) {
    stop("Please run 'RunAllBatches' before attempting to run 'CorMat'")
  }

  # Validate that Compare1 is provided and is a valid string.
  if(missing(Compare1) || !rlang::is_string(Compare1)){
    stop("Compare1 is missing or invalid")
  }

  # Validate that Compare2 is provided and is a valid string.
  if(missing(Compare2) || !rlang::is_string(Compare2)){
    stop("Compare2 is missing or invalid")
  }

  # Read the data
  combined_data <- read.csv("all_batches_summary.csv")

  # Summarize data by required columns
  meanData <- dplyr::summarise(
    dplyr::group_by(
      combined_data,
      temp, sex, treatment, genotype, environment, light
    ),
    dplyr::across(
      sleep_fraction:mean_bout_length_D,
      ~mean(., na.rm = TRUE),
      .names = "mean_{.col}"
    ),
    .groups = "keep"
  )

  # Dynamically select the column to be used for grouping based on user input
  group_column <- dplyr::pull(meanData, dplyr::all_of(column_name)) # Extract the specified column

  # Initialize the pregrouping column
  data.table::setDT(meanData)
  meanData[, pregrouping := as.character(NA)]

  # Dynamically add the grouping column based on user input
  for (group in groupings) {
    meanData[is.na(pregrouping) & grepl(group, group_column), pregrouping := group]
  }

  # Further processing
  columnnames <- c("column_name2", "gsleep", "isleep", "P.sleeploss", "pregrouping")
  names <- c("mean_sleep_time_all", "mean_sleep_time_l", "mean_sleep_time_d")
  lighting <- c("ZT0_24", "ZT0_12", "ZT12_24")

  # This code automatically selects the optimal number of clusters and prints the plots
  N_clus <- FALSE

  for (i in 1:3) {
    gsleep <- meanData[meanData$treatment == Compare1, get(names[i])]
    isleep <- meanData[meanData$treatment == Compare2, get(names[i])]
    name <-   meanData[meanData$treatment == Compare1, get(column_name)]

    pregrouping.grp <- meanData[meanData$treatment == Compare1, "pregrouping"]
    p.sleeploss <- (gsleep - isleep) / gsleep
    df <- cbind(name, gsleep, isleep, p.sleeploss, pregrouping.grp)
    colnames(df) <- columnnames
    df <- df[order(df$gsleep), ]
    u <- (max(df$gsleep) - min(df$gsleep)) / nrow(df)  # create a multiplier for even spacing

    data <- df[, c(2, 4)]  # Selecting only relevant columns for clustering

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

    if (N_clus) {
      clusterfile[,i + 2] <- df[,6]
    } else {
      clusterfile <- cbind(df[, c(1, 5, 6, 6, 6)])  # binds column_names, pregrouping, and cluster number
      colnames(clusterfile) <- c(column_name, "Pregrouping", paste0("Cluster",
              lighting[1]), paste0("Cluster", lighting[2]), paste0("Cluster", lighting[3]))
      N_clus <- TRUE
    }

    # Creating a correlation plot colored by clusters and pregrouping
      ggplot2::ggplot(df, ggplot2::aes(x = gsleep, y = P.sleeploss, fill = pregrouping)) +
        ggplot2::geom_smooth(method = "lm", color = "grey31", se = TRUE) +
        ggplot2::geom_point(size = 2, stroke = 0.5, alpha = 2/3, color = "black",
                            ggplot2::aes(shape = cluster)) +
        ggplot2::labs(
          title = paste0("Sleep Trend with Clustering - ", lighting[i]),
          x = "Group Sleep", y = "Percent Sleep Loss After Isolation") +
        ggplot2::geom_text(ggplot2::aes(x = min(gsleep) + u * 1:nrow(df), y = 1,
                               label = column_name2, color = pregrouping),
                           angle = 90, vjust = 1.5, hjust = 1, size = 3) +
        ggprism::theme_prism(base_fontface = "plain", base_line_size = 0.7) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                       legend.title = ggplot2::element_text(size = 12)) +
        ggplot2::scale_shape_manual(name = "Cluster", values = 21:25) +
        ggplot2::scale_fill_viridis_d(name = "Pregroup", option = "D", begin = 0, end = 0.8) +
        ggplot2::scale_color_viridis_d(name = "Pregroup", option = "D", begin = 0, end = 0.8) +
        ggplot2::ylim(-0.5, 1)

    ggplot2::ggsave(paste0("ClusteredPlot_", lighting[i],Compare1, ".pdf"), height = 7, width = 7)

  }

  # Write the file which labels the cluster each genotype is in
  colnames(clusterfile) <- c(column_name, "Pregrouping", paste0("Cluster", lighting[1]), paste0("Cluster", lighting[2]), paste0("Cluster", lighting[3]))
  data.table::fwrite(clusterfile, paste0("clustered", Compare1, ".csv"))
#
#   summary(lm(data = df, P.sleeploss ~ gsleep + pregrouping)) # some significant // when controlling for related-ness, group sleep is typically negatively correlated with % sleep loss BUT NOT A LOT
}
