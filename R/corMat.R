#' Correlation Matrix for Sleep Data (Exported)
#'
#' Computes the relative sleep changes between two treatments and generates a correlation matrix plot with significance annotations.
#'
#' @param Compare1 A character string representing the first treatment to compare. This should be the control.
#' @param Compare2 A character string representing the second treatment to compare. This will be compared to the control.
#'
#' @return Two `ggplot` objects containing the correlation matrix plot with significance annotations for percentage of sleep lost and raw number of minutes lost.
#'         The plots are saved as PDF files named `Relative<Compare1>.pdf` and `Unadjusted<Compare1>.pdf`.
#'         A CSV file with correlation p-values is saved as `statcorRelative<Compare1>.csv` and `statcorUnadjusted<Compare1>.csv`.
#'
#' @export
corMat <- function(Compare1, Compare2){

  # Check if 'all_batches_summary.csv' exists in the current directory, and if not, stop execution.
  if (!file.exists("all_batches_summary.csv")) {
    stop("The file 'all_batches_summary.csv' is missing from the current directory.
         Please run 'RunAllBatches' before attempting to run 'CorMat'")
  }

  # Validate that Compare1 is provided and is a valid string.
  if(missing(Compare1) || !rlang::is_string(Compare1)){
    stop("Compare1 is missing or invalid")}

  # Validate that Compare1 is provided and is a valid string.
  if(missing(Compare2) || !rlang::is_string(Compare2)){
    stop("Compare2 is missing or invalid")}
  if (controlColumn %in% colnames(meanData)) {
    # Proceed with filtering
  } else {
    stop("Column does not exist!")
  }


  # Read in the combined data from the CSV file.
  combined_data <- read.csv("all_batches_summary.csv")

  # Summarize sleep data by temp, sex, treatment, and genotype, calculating means for sleep-related variables.
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

  # Define sleep time variables.
  names <- c("mean_sleep_time_l", "mean_sleep_time_d")

  # Extract and order the genotypes.
  genotypes <- unique(meanData$genotype)[order(unique(meanData$genotype))]

  # Initialize an empty data frame for the genotypes.
  abs.df <- df <- df1 <- data.frame(genotypes)

  # Generate a data frame with relative sleep loss (p.sleeploss) for each sleep time variable.
  for(i in 1:2){
    gsleep <- meanData[meanData$treatment == Compare1, names[i]]
    isleep <- meanData[meanData$treatment == Compare2, names[i]]
    p.sleeploss <- (gsleep - isleep) / gsleep
    sleeploss <- (gsleep - isleep)
    df <- cbind(df, p.sleeploss)
    abs.df <- cbind(abs.df, sleeploss)
  }

  # Define additional trait variables to compare.
  traitlist <- c("mean_n_bouts_L", "mean_n_bouts_D", "mean_mean_bout_length_L",
                 "mean_mean_bout_length_D")

  # Add the trait comparisons for each treatment group.
  for (i in 1:4){
    df1 <- cbind(df1, meanData[meanData$treatment == Compare1, traitlist[i]],
                 meanData[meanData$treatment == Compare2, traitlist[i]])
  }

  abs.df <- cbind(abs.df, (df1[3] - df1[2]))
  abs.df <- cbind(abs.df, (df1[5] - df1[4]))
  abs.df <- cbind(abs.df, (df1[7] - df1[6]))
  abs.df <- cbind(abs.df, (df1[9] - df1[8]))

  # Calculate the loss in the number of bouts and bout length for both treatments.
  df <- cbind(df, ((df1[3] - df1[2]) / df1[2]))
  df <- cbind(df, ((df1[5] - df1[4]) / df1[4]))
  df <- cbind(df, ((df1[7] - df1[6]) / df1[6]))
  df <- cbind(df, ((df1[9] - df1[8]) / df1[8]))

  # Rename the columns for better clarity.
  colnames(df) <- c("Genotypes", "P.sleeploss_L", "P.sleeploss_D", "P.nboutsloss_L",
                    "P.nboutsloss_D", "P.boutlenloss_L", "P.boutlenloss_D")
  colnames(abs.df) <- c("Genotypes", "Sleeploss_L", "Sleeploss_D", "Nboutsloss_L",
                    "Nboutsloss_D", "Boutlenloss_L", "Boutlenloss_D")

  dat <- c("df", "abs.df")
  plotnames <- c("Relative", "Unadjusted")

  for (i in 1:2){
  # Compute the correlation matrix for the data.
  corr <- round(cor(get(dat[i])[, 2:7]), 2)

  # Generate the p-value matrix for the correlation.
  p.df <- as.data.frame(ggcorrplot::cor_pmat(get(dat[i])[, 2:7]))

  # Save the p-value matrix as a CSV file.
  data.table::fwrite(p.df, paste0("statcor", Compare1, ".csv"))

  # Define a function to assign significance labels (e.g., "*" for p-values < 0.05).
  labs.function <- function(x){
    dplyr::case_when(
              x >= 0.10 ~ "",
              x < 0.10 & x >= 0.05 ~ ".",
              x < 0.05 & x >= 0.01 ~ "*",
              x < 0.01 & x >= 0.001 ~ "**",
              x < 0.001 ~ "***")
  }

  # Add significance labels to the p-value matrix.
  p.labs <- dplyr::mutate_all(p.df, labs.function)

  # Reshape the p-value matrix for use in the plot.
  p.labs$Var1 <- as.factor(rownames(p.labs))
  p.labs <- reshape2::melt(p.labs, id.vars = "Var1", variable.name = "Var2", value.name = "lab")

  # Initial ggcorrplot
  cor.plot <- ggcorrplot::ggcorrplot(corr, type = "lower", lab = TRUE, show.diag = TRUE, lab_size = 5) +
    ggplot2::labs(y = NULL, x = NULL, title = paste(plotnames[i], "Sleep Changes by Bouts")) +
    ggprism::theme_prism(base_fontface = "bold") +
    ggplot2::theme(
      title = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.text.x = ggplot2::element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
      legend.text = ggplot2::element_text(size = 14, face = "bold")
      )

  # Subset the significance labels to match the correlation plot's data.
  p.labs$in.df <- ifelse(is.na(match(paste0(p.labs$Var1, p.labs$Var2),
                                     paste0(cor.plot[["data"]]$Var1, cor.plot[["data"]]$Var2))),
                         "No", "Yes")

  # Filter out rows that do not match the correlation plot's data.
  p.labs <- dplyr::filter(p.labs, in.df == "Yes")
  p.labs <- dplyr::select(p.labs, -in.df)

  # Add the significance labels (asterisks) to the correlation plot.
  cor.plot.labs <- cor.plot +
    ggplot2::geom_text(ggplot2::aes(x = p.labs$Var1, y = p.labs$Var2),
              label = p.labs$lab, nudge_y = 0.25, size = 5)


  # Save the plot as a PDF file.
  ggplot2::ggsave(paste0(plotnames[i], Compare1, ".pdf"), cor.plot.labs, height = 7, width = 7)

  }
  # Return the correlation plot with labels.
  return(cor.plot.labs)
}
