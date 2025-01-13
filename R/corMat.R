#' Correlation Matrix for Sleep Data (Exported)
#'
#' Computes the relative sleep changes between two treatments and generates a correlation matrix plot with significance annotations.
#'
#' @param treat A string specifying a treatment to subset the data by.
#' @param temp A string specifying a temperature condition to subset the data by.
#' @param enviro A string specifying a environment condition to subset the data by.
#' @param lights A string specifying a light condition to subset the data by.
#' @param geno A string specifying a genotype condition to subset the data by.
#' @param font A character string determining the font style of the produced plots. ("plain", "bold", "italic", or "bold.italic")
#'
#' @details Two `ggplot` objects containing the correlation matrix plot with significance annotations for percentage of sleep lost and raw number of minutes lost.
#'         The plots are saved as PDF files named `Relative<Compare1>.pdf` and `Unadjusted<Compare1>.pdf`.
#'         A CSV file with correlation p-values is saved as `statcorRelative<Compare1>.csv` and `statcorUnadjusted<Compare1>.csv`.
#'
#' @return None. Plots are saved as PDF files, while p-values are saved as csv files.
#'
#' @export
corMat <- function(temp = NULL, enviro = NULL, lights = NULL, geno = NULL, font = "plain"){
  # Check if 'all_batches_summary.csv' exists in the current directory, and if not, stop execution.
  if (!file.exists("all_batches_summary.csv")) {
    stop("The file 'all_batches_summary.csv' is missing from the current directory.
         Please run 'RunAllBatches' before attempting to run 'CorMat'")
  }

  if (!(font %in% c("plain", "bold", "italic","bold.italic"))){
    stop("'font' must be 'plain', 'bold', 'italic', or 'bold.italic'")
  }

  # Read in the combined data from the CSV file.
  combined_data <- read.csv("all_batches_summary.csv")
  combined_data$light <- gsub("\"", "", combined_data$light)
  data.table::setDT(combined_data)

  # subset by only selecting rows with condition(s) specified
  titlee <- c("")
  # if(!is.null(treat)){
  #   combined_data <- combined_data[combined_data$treatment == treat,]
  #
  #   # warning if condition is invalid
  #   if (nrow(combined_data) == 0) {
  #     stop("The 'treat' specified is not included in the data within the 'treatment' parameter")
  #   }
  #   titlee <- trimws(paste(titlee, treat))
  # }
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

  # Summarize sleep data by temp, sex, treatment, and genotype, calculating means for sleep-related variables.
  meanData <- dplyr::summarise(
    dplyr::group_by(
      combined_data,
      sex, genotype, temperature, treatment, environment, light
    ),
    dplyr::across(
      sleep_fraction:mean_bout_length_D,
      ~mean(., na.rm = TRUE),
      .names = "mean_{.col}"
    ),
    .groups = "keep"
  )

  # Define sleep time variables.
  names <- c("mean_sleep_time_All", "mean_sleep_time_L", "mean_sleep_time_D")
  names <- c("mean_sleep_time_all", "mean_sleep_time_l", "mean_sleep_time_d") # delete when possible
  traitlist <- c("mean_n_bouts_L", "mean_n_bouts_D", "mean_mean_bout_length_L",
                 "mean_mean_bout_length_D")

  if("Grp" %in% meanData$treatment && "Iso" %in% meanData$treatment){
    # Generate a data frame with absolute sleep loss (p.sleeploss) for each sleep time variable.
    gsleep <- meanData[meanData$treatment == "Grp", names]
    isleep <- meanData[meanData$treatment == "Iso", names]
    sleepchange <- (gsleep - isleep)
    # Define additional trait variables to compare.
    gtrait <- meanData[meanData$treatment == "Grp", traitlist]
    itrait <- meanData[meanData$treatment == "Iso", traitlist]
    traitchange <- (gtrait - itrait)
    df <- cbind(sleepchange, traitchange)
    # Rename the columns for better clarity.
    colnames(df) <- c("Sleepchange_All", "Sleepchange_L", "Sleepchange_D", "Nboutschange_L",
                      "Nboutschange_D", "Boutlenchange_L", "Boutlenchange_D")
  } else {
    df <- meanData[, c(names, traitlist)]
    # Rename the columns for better clarity.
    colnames(df) <- c("Sleeptime_All", "Sleeptime_L", "Sleeptime_D", "Nbouts_L",
                      "Nbouts_D", "Boutlen_L", "Boutlen_D")
  }

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
      x < 0.10 & x >= 0.05 ~ "",
      x < 0.05 & x >= 0.01 ~ "*",
      x < 0.01 & x >= 0.001 ~ "**",
      x < 0.001 & x >= 0.0001 ~ "***",
      x < 0.0001 ~ "****")
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
