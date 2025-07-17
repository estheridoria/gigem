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
#'         The plots are saved as PDF files.
#'         A CSV file with correlation p-values is saved.
#'
#' @return None. Plots are saved as PDF files, while p-values are saved as csv files.
#'
#' @export
corMatrix <- function(condition1 = NULL, condition2 = NULL, sex = NULL, geno = NULL,
                   temp = NULL, treat = NULL, enviro = NULL,
                   Lights = NULL, font = c("plain", "bold", "italic", "bold.italic")){
    # Read the data from CSV file
    # Check if 'all_batches_stat.csv' exists in the current directory, and if not, stop execution.
    if (!file.exists("all_batches_stat.csv")) {
      stop("'all_batches_stat.csv' is not found in the current directory. Please run 'runAllBatches' before attempting to run 'kmeansCluster'. If you have already run it, reset the working directory and run kmeansCluster again.")
    }
  font <- match.arg(font)

  # define later variables
  param_cols<- c("Sleep_Time_All_mean", "Sleep_Time_L_mean", "Sleep_Time_D_mean",
                 "n_Bouts_L_mean", "n_Bouts_D_mean", "mean_Bout_Length_L_mean",
                 "mean_Bout_Length_D_mean")
  meta_vars <- c("Sex", "Genotype", "Temperature", "Treatment", "Environment", "Light")
  meta_inputs <- list(sex, geno, temp, treat, enviro, Lights)
  names(meta_inputs) <- meta_vars
  
    combined_data <- read.csv("all_batches_stat.csv")
    
    # Subset the data & define the title
    title_parts <- character()
    for (var in meta_vars) {
      val <- meta_inputs[[var]]
      if (!is.null(val)) {
        combined_data <- combined_data[combined_data[[var]] == val, ]
        if (nrow(combined_data) == 0) {
          stop(paste0("'", val, "' not found in variable '", var, "'"))
        }
        title_parts <- c(title_parts, val)
      }
    }
    title_text <- paste(title_parts, collapse = "_")
    
    # Summarize sleep data by temp, Sex, Treatment, and Genotype, enviro calculating means for sleep-related variables.
    dataset <- combined_data |>
      dplyr::group_by(across(all_of(c(meta_vars, "Batch")))) |>
      dplyr::summarise(across(all_of(param_cols), ~ mean(.x, na.rm = TRUE)), .groups = "keep")
    
# difference between conditions or absolute values
    if (!is.null(condition1) && !is.null(condition2)) {
      match_col <- intersect(names(dataset), meta_vars)[sapply(meta_vars, function(var) {
        any(grepl(condition1, dataset[[var]])) && any(grepl(condition2, dataset[[var]]))
      })]
      if (length(match_col) != 1) {
        stop("Conditions must match within exactly one metadata column.")
      }
      c1_data <- dataset[dataset[[match_col]] == condition1, ]
      c2_data <- dataset[dataset[[match_col]] == condition2, ]
      
      if (nrow(c1_data) != nrow(c2_data)){
      stop("There is an uneven number of 'condition1' and 'condition2' populations within the current subset of variables. Please ensure there are no unpaired populations/monitors for 'condition1' and 'condition2'.")
    }
  # math
      c1_sorted <- c1_data[do.call(order, c1_data[meta_vars]), ]
      c2_sorted <- c2_data[do.call(order, c2_data[meta_vars]), ]
      
      sleep_diff <- (c1_sorted[, param_cols] - c2_sorted[, param_cols])
      df <- cbind(sleep_diff, c1_sorted[, setdiff(c(meta_vars, "Batch"), match_col), drop = FALSE])

    # Rename the columns for better clarity.
    colnames(df) <- c("Sleepchange_All", "Sleepchange_L", "Sleepchange_D", "nBoutschange_L",
                      "nBoutschange_D", "Boutlenchange_L", "Boutlenchange_D")
    title_text <- trimws(paste0(title_text, " ", condition1, "-", condition2))
  } else {
    df <- dataset[, param_cols]
    # Rename the columns for better clarity.
    colnames(df) <- c("Sleeptime_All", "Sleeptime_L", "Sleeptime_D", "NBouts_L",
                      "NBouts_D", "Boutlen_L", "Boutlen_D")
  }
    
  # Compute the correlation matrix for the data.
  corr <- round(cor(df[,1:7]), 3)

  # Generate the p-value matrix for the correlation.
  p_matrix <- ggcorrplot::cor_pmat(df[,1:7])
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
  title_texte <- gsub(" ", "", title_text)
  title_texte <- gsub(":", ".", title_texte)

  # Save the p-value matrix as a CSV file.
  data.table::fwrite(p.df, paste0("Correlation_pValues", title_texte, ".csv"))

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
  suppressMessages({
  cor.plot <- ggcorrplot::ggcorrplot(corr, type = "lower", lab = TRUE, show.diag = TRUE, lab_size = 5) +
    ggplot2::labs(y = NULL, x = NULL, title = trimws(paste0("Sleep Correlation ", title_text))) +
    ggplot2::scale_fill_gradient2(low = "blue", high =  "red2", mid = "white", 
                                  midpoint = 0, limit = c(-1,1))+
    
    ggprism::theme_prism(base_fontface = font) +
    ggplot2::theme(
      title = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.text.x = ggplot2::element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
      legend.text = ggplot2::element_text(size = 14, face = font)
    )
})
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
  ggplot2::ggsave(paste0("Correlation", title_texte, ".pdf"), cor.plot.labs, height = 7, width = 7)
}
