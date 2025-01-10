#' Correlation Matrix for Sleep Data (Exported)
#'
#' Computes the relative sleep changes between two treatments and generates a correlation matrix plot with significance annotations.
#'
#' @param Compare1 A character string representing the first treatment to compare. This should be the control.
#' @param Compare2 A character string representing the second treatment to compare. This will be compared to the control.
#' @param var A character string representing the variable to subset the data by.
#' This should be one of the entries in the Main file under temperature, light, genotype, sex, treatment or environment.
#' @param font A character string variable determining the font style of the produced plots.
#'
#' @details Two `ggplot` objects containing the correlation matrix plot with significance annotations for percentage of sleep lost and raw number of minutes lost.
#'         The plots are saved as PDF files named `Relative<Compare1>.pdf` and `Unadjusted<Compare1>.pdf`.
#'         A CSV file with correlation p-values is saved as `statcorRelative<Compare1>.csv` and `statcorUnadjusted<Compare1>.csv`.
#'
#' @return None. Plots are saved as PDF files, while p-values are saved as csv files.
#'
#' @export
corMat <- function(treat = NULL, temp = NULL, enviro = NULL, lights = NULL, geno = NULL, font = "plain"){

  # Check if 'all_batches_summary.csv' exists in the current directory, and if not, stop execution.
  if (!file.exists("all_batches_norm_summary.csv")) {
    stop("The file 'all_batches_norm_summary.csv' is missing from the current directory.
         Please run 'RunAllBatches' before attempting to run 'CorMat'")
  }

  if (!(font %in% c("plain", "bold", "italic","bold.italic"))){
    stop("'font' must be 'plain', 'bold', 'italic', or 'bold.italic'")
  }

  # Read in the combined data from the CSV file.
  combined_data <- read.csv("all_batches_norm_summary.csv")
  combined_data$light <- gsub("\"", "", combined_data$light)

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

  # Initialize an empty data frame for the genotypes.
  df <- combined_data[,15:21]

  # # Generate a data frame with relative sleep loss (p.sleeploss) & absolute for each sleep time variable.
  #   p.sleeploss <- meanData[meanData$treatment == Compare1,names[i]]
  #   isleep <- meanData[meanData$treatment == Compare2,names[i]]
  #    <- (gsleep - isleep) / gsleep
  #   sleeploss <- (gsleep - isleep)
  #   df <- cbind(df, p.sleeploss)

  # Define additional trait variables to compare.
  # traitlist <- c("norm_n_bouts_L", "norm_n_bouts_D", "norm_mean_bout_length_L", "norm_mean_bout_length_D")

  # # Add the trait comparisons for each treatment group.
  # for (i in 1:4){
  #   a <- meanData[meanData$treatment == Compare1,traitlist[i]]
  #   b <- meanData[meanData$treatment == Compare2,traitlist[i]]
  #     df1 <- cbind(df1, a, b)
  # }

  # Rename the columns for better clarity.
  colnames(df) <- c("Sleeploss_All", "Sleeploss_L", "Sleeploss_D", "Nboutsloss_L",
                    "Nboutsloss_D", "Boutlenloss_L", "Boutlenloss_D")

  # if (!is.null(var)){
  #   plotnames <- c(paste0("Relative_", var),paste0("Unadjusted_", var))
  # } else{
  #   plotnames <- c("Relative", "Unadjusted")
  # }
  # for (i in 1:2){
  # Compute the correlation matrix for the data.
  corr <- round(cor(df), 3)

  # Generate the p-value matrix for the correlation.
  p.df <- as.data.frame(ggcorrplot::cor_pmat(df))

  # Save the p-value matrix as a CSV file.
  data.table::fwrite(p.df, paste0("statcor", titlee, ".csv"))

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
    ggplot2::labs(y = NULL, x = NULL, title = trimws(paste("Sleep Correlation ", titlee))) +
    ggprism::theme_prism(base_fontface = font) +
    ggplot2::theme(
      title = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.text.x = ggplot2::element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
      legend.text = ggplot2::element_text(size = 14, face = font)
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

titlee <- gsub(" ", "_", titlee)
  # Save the plot as a PDF file.
  ggplot2::ggsave(paste0("Correlation", titlee, ".pdf"), cor.plot.labs, height = 7, width = 7)

  }
