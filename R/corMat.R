#' Correlation Matrix for Sleep Data
#'
#' Computes the relative sleep changes between two Treatments and generates a correlation matrix plot with significance annotations.
#'
#' @param condition1 A string specifying a condition within one of the experimental variables. The difference is calculated as: `condition1`-`condition2`
#' @param condition2 A string specifying a condition within the same experimental variable as `condition1` that is associated with the difference seen in sleep.
#' @param sex A string specifying a Sex condition to subset the data by.
#' @param geno A string specifying a Genotype condition to subset the data by.
#' @param temp A string specifying a Temperature condition to subset the data by.
#' @param treat A string specifying a Treatment to subset the data by.
#' @param enviro A string specifying a Environment condition to subset the data by.
#' @param Lights A string specifying a Light condition to subset the data by.
#' @param font A character string determining the font style of the produced plots. ("plain", "bold", "italic", or "bold.italic")
#'
#' @details Two `ggplot` objects containing the correlation matrix plot with significance annotations for percentage of sleep lost and raw number of minutes lost.
#'         The plots are saved as PDF files named `Relative<Compare1>.pdf` and `Unadjusted<Compare1>.pdf`.
#'         A CSV file with correlation p-values is saved as `statcorRelative<Compare1>.csv` and `statcorUnadjusted<Compare1>.csv`.
#'
#' @return None. Plots are saved as PDF files, while p-values are saved as csv files.
#'
#' @export
corMat <- function(condition1 = NULL, condition2 = NULL, sex = NULL, geno = NULL,
                   temp = NULL, treat = NULL, enviro = NULL,
                   Lights = NULL, font = "plain"){
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
    # Read the data from CSV file
    # Check if 'all_batches_summary.csv' exists in the current directory, and if not, stop execution.
    if (!file.exists("all_batches_stat.csv")) {
      stop("'all_batches_stat.csv' is not found in the current directory. Please run 'runAllBatches' before attempting to run 'kmeansCluster'. If you have already run it, reset the working directory and run kmeansCluster again.")
    }
    combined_data <- read.csv("all_batches_stat.csv")
  }

  data.table::setDT(combined_data)

  if (!(font %in% c("plain", "bold", "italic","bold.italic"))){
    stop("'font' must be 'plain', 'bold', 'italic', or 'bold.italic'")
  }

  # subset by only selecting rows with condition(s) specified
  if(relValues){
    titlee <- c("Relative")
  }else{
    titlee<- c("")
  }
  if(!is.null(treat)){
    combined_data <- combined_data[combined_data$Treatment == treat,]

    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'treat' specified is not included in the data within the 'Treatment' variable")
    }
    titlee <- trimws(paste(titlee, treat))
  }
  if(!is.null(temp)){
    combined_data <- combined_data[combined_data$Temperature == temp,]
    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'temp' specified is not included in the data within the 'Temperature' variable")
    }
    titlee <- trimws(paste(titlee, temp))
  }
  if(!is.null(enviro)){
    combined_data <- combined_data[combined_data$Environment == enviro,]
    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'enviro' specified is not included in the data within the 'Environment' variable")
    }
    titlee <- trimws(paste(titlee, enviro))
  }
  if(!is.null(Lights)){
    combined_data <- combined_data[combined_data$Light == Lights,]
    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'Lights' specified is not included in the data within the 'Light' variable")
    }
    titlee <- trimws(paste(titlee, Lights))
  }
  if(!is.null(geno)){
    combined_data <- combined_data[combined_data$Genotype == geno,]
    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'geno' specified is not included in the data within the 'Genotype' variable")
    }
    titlee <- trimws(paste(titlee, geno))
  }
  if(!is.null(sex)){
    combined_data <- combined_data[combined_data$Sex == sex,]

    # warning if condition is invalid
    if (nrow(combined_data) == 0) {
      stop("The 'sex' specified is not included in the data within the 'Sex' variable")
    }
    titlee <- trimws(paste(titlee, sex))
  }
  # Summarize sleep data by temp, Sex, Treatment, and Genotype, calculating means for sleep-related variables.
  meanData <- dplyr::summarise(
    dplyr::group_by(
      combined_data,
      Sex, Genotype, Temperature, Treatment, Environment, Light
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
  metalist <- c("Sex", "Genotype", "Temperature", "Treatment", "Environment",
                "Light")

  # if("Grp" %in% meanData$Treatment && "Iso" %in% meanData$Treatment){
  #  # Define additional parameter variables to compare.
  #   gparameter <- meanData[meanData$Treatment == "Grp", parameterlist]
  #   iparameter <- meanData[meanData$Treatment == "Iso", parameterlist]
  #   parameterchange <- (iparameter - gparameter)
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
    colnames(df) <- c("Sleepchange_All", "Sleepchange_L", "Sleepchange_D", "nBoutschange_L",
                      "nBoutschange_D", "Boutlenchange_L", "Boutlenchange_D")
    titlee <- trimws(paste0(titlee, " ", condition1, "-", condition2))
  } else {
    df <- meanData[, parameterlist]
    # Rename the columns for better clarity.
    colnames(df) <- c("Sleeptime_All", "Sleeptime_L", "Sleeptime_D", "NBouts_L",
                      "NBouts_D", "Boutlen_L", "Boutlen_D")
  }

  # Compute the correlation matrix for the data.
  corr <- round(cor(df), 3)

  # Generate the p-value matrix for the correlation.
  p_matrix <- ggcorrplot::cor_pmat(df)
  # Step 2: Flatten the p-value matrix, adjust using FDR
  p_vector <- as.vector(p_matrix)
  p_adjusted_vector <- p.adjust(p_vector, method = "fdr")

  # Step 3: Reshape the adjusted p-values back into a matrix
  p_adjusted_matrix <- matrix(p_adjusted_vector, nrow = nrow(p_matrix), ncol = ncol(p_matrix))

  # Step 4: Set row and column names to match the original p-value matrix
  rownames(p_adjusted_matrix) <- rownames(p_matrix)
  colnames(p_adjusted_matrix) <- colnames(p_matrix)

  # Step 5: Convert the adjusted p-value matrix into a data frame
  p.df <- as.data.frame(p_adjusted_matrix)
  titleee <- gsub(" ", "", titlee)
  titleee <- gsub(":", ".", titleee)

  # Save the p-value matrix as a CSV file.
  data.table::fwrite(p.df, paste0("Correlation_pValues", titleee, ".csv"))

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

  # Save the plot as a PDF file.
  ggplot2::ggsave(paste0("Correlation", titleee, ".pdf"), cor.plot.labs, height = 7, width = 7)

}
