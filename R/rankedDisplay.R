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
#' @param control The name of the control within the variable x.
#' @param condition1 A string specifying a condition within one of the experimental variables. The percent difference is calculated as: (`condition1`-`condition2`) / `condition2`. Default is NULL
#' @param condition2 A string specifying a condition within the same experimental variable as `condition1` that is associated with the percent difference seen in sleep. Default is NULL
#' @param treat A string specifying a Treatment to subset the data by. Default is NULL
#' @param temp A string specifying a Temperature condition to subset the data by. Default is NULL
#' @param enviro A string specifying a Environment condition to subset the data by. Default is NULL
#' @param sex A string specifying a sex condition to subset the data by. Default is NULL
#' @param Lights A string specifying a Light condition to subset the data by. Default is NULL
#' @param geno A string specifying a Genotype condition to subset the data by. Default is NULL
#' @param ranking A tibble of one column containing the order of conditions displayed in the plot from variable x.
#' @param fitted Use predicted values after controlling for variables and Batch effects. Default = FALSE
#' @param font A character string determining the font style of the produced plots. ("plain", "bold", "italic", or "bold.italic"). Default is "plain"
#' @param limits A list of two numerics to be the upper and lower limits of the plot. Default is c(-100, 1500)
#'
#'
#' @return conditional. If the ranking is NULL (default), then the ranking used in the plots will be returned to be used as the ranking in subsequent rankedDisplay plots. Otherwise, no return. Saves the plots as a PDF file labeled `RankedSleep...`
#' @export
#'
#' @keywords internal
rankedDisplay <- function(
    x = c("Temperature", "Sex", "Treatment", "Genotype", "Environment", "Light"),
    control = NULL,
    condition1 = NULL,
    condition2 = NULL,
    treat = NULL, temp = NULL, enviro = NULL, sex = NULL, Lights = NULL, geno = NULL,
    ranking = NULL, fitted = FALSE,
    font = c("plain", "bold", "italic", "bold.italic")
) {
  x <- match.arg(x)
  font <- match.arg(font)
  meta_vars <- c("Sex", "Genotype", "Temperature", "Treatment", "Environment", "Light")
  meta_inputs <- list(sex, geno, temp, treat, enviro, Lights)
  names(meta_inputs) <- meta_vars
  
  if (!file.exists("all_batches_summary.csv")) {
    stop("'all_batches_summary.csv' not found. Please run 'runAllBatches' first.")
  }
  
  dataset <- read.csv("all_batches_summary.csv")
  dataset <- dataset %>%
    dplyr::group_by(across(all_of(c(meta_vars, "Batch")))) %>%
    dplyr::summarise(across(all_of(param_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  title_parts <- character()
  for (var in meta_vars) {
    val <- meta_inputs[[var]]
    if (!is.null(val)) {
      dataset <- dataset[dataset[[var]] == val, ]
      if (nrow(dataset) == 0) {
        stop(paste0("'", val, "' not found in variable '", var, "'"))
      }
      title_parts <- c(title_parts, val)
    }
  }
  title_text <- paste(title_parts, collapse = "_")
  
  # Use fitted values if fitted = TRUE
  if (fitted){
    #predicted values when controlling for the effects of stuffs
    
    # List of your categorical variables
    categorical_vars <- c(meta_vars, "Batch")
    
    # Initialize the formula string with the response variable
    formula_stringss <- c("Sleep_Time_All ~ ", "Sleep_Time_L ~ ", "Sleep_Time_D ~ ")
    
    for(i in seq_along(formula_stringss)){
      formula_string <- formula_stringss[i]
      varvars<- c()
      # Loop through each categorical variable
      for (var in categorical_vars) {
        # Check if the variable exists as a column in 'dataset' and has more than one unique value
        if (length(unique(dataset[[var]])) > 1) {
          # If it's the first variable being added, don't add a "+" before it
          if (formula_string == "Sleep_Time_All ~ " ||
              formula_string == "Sleep_Time_L ~ " ||
              formula_string == "Sleep_Time_D ~ ") {
            formula_string <- paste0(formula_string, var)
          } else {
            formula_string <- paste0(formula_string, " + ", var)
          }
          varvars<- c(varvars, var)
        }
      }
      # Convert the formula string to a formula object
      formula <- as.formula(formula_string)
      # Fit the linear model using dataset
      fit <- lm(formula, data = dataset)
      
      # Create new_data for predictions
      new_data_list <- list()
      for (var in varvars) {
        new_data_list[[var]] <- unique(dataset[[var]])
      }
      
      # If 'Batch' was in your model, and you want to fix it at the first level
      if ("Batch" %in% varvars) {
        new_data_list[["Batch"]] <- levels(as.factor(dataset$Batch))[1]
      }
      
      # Generate all combinations using expand.grid
      new_data <- expand.grid(new_data_list)
      
      # Predict using the model
      tryCatch({
        new_data$predicted <- predict(fit, newdata = new_data)
      }, error = function(e) {
        if (grepl("rank-deficient", e$message)) {
          stop("Prediction failed due to a rank-deficient model fit.\n",
               "Too many variable conditions may be interfering with the regression.\n",
               "Try subsetting your data further using 'treat', 'temp', 'enviro', 'sex', 'Lights', and/or 'geno'.\n",
               "Full error: ", e$message)
        } else {
          stop(e)  # re-throw other errors
        }})
      # Assign to your 'dataset' variable
      if(i == 1){
        dataa <- new_data
      } else{
        col<- 1+ncol(dataa)
        dataa[[col]]<- new_data$predicted
      }
    }
    colnames(dataa)[(ncol(dataa)-2):ncol(dataa)] <- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D")
    dataset<- dataa
    meta_vars <- varvars
    pdftitle <- "_fittedValues"
  }else {
    pdftitle <- ""
  }
  
  param_cols <- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D")
  
  if (!is.null(condition1) && !is.null(condition2)) {
    match_col <- intersect(names(dataset), meta_vars)[sapply(meta_vars, function(var) {
      any(grepl(condition1, dataset[[var]])) && any(grepl(condition2, dataset[[var]]))
    })]
    if (length(match_col) != 1) {
      stop("Conditions must match within exactly one metadata column.")
    }
    c1_data <- dataset[dataset[[match_col]] == condition1, ]
    c2_data <- dataset[dataset[[match_col]] == condition2, ]
    if (nrow(c1_data) != nrow(c2_data)) stop("Unpaired condition1 and condition2 rows.")
    
    c1_sorted <- c1_data[do.call(order, c1_data[meta_vars]), ]
    c2_sorted <- c2_data[do.call(order, c2_data[meta_vars]), ]
    
    sleep_diff <- (c1_sorted[, param_cols] - c2_sorted[, param_cols]) / c2_sorted[, param_cols]
    sleep_diff <- sleep_diff*100 #get percentage
    df <- cbind(sleep_diff, c1_sorted[, setdiff(c(meta_vars, "Batch"), match_col), drop = FALSE])
    colnames(df)[1:3] <- c("Total_Sleep_Change", "Daytime_Sleep_Change", "Nighttime_Sleep_Change")
    
    if (is.null(ranking)) {
      ranking_df <- df %>%
        dplyr::group_by(.data[[x]]) %>%
        dplyr::summarise(Total_Sleep_Change = mean(Total_Sleep_Change, na.rm = TRUE)) %>%
        dplyr::arrange(Total_Sleep_Change)
      ranking <- ranking_df[[x]]
    }
    df[[x]] <- factor(df[[x]], levels = ranking)
    y_label <- "Change in Sleep (%)"
    plot_cols <- c("Total_Sleep_Change", "Daytime_Sleep_Change", "Nighttime_Sleep_Change")
    
  } else {
    df <- dataset[, c(param_cols, meta_vars, "Batch")]
    colnames(df)[1:3] <- c("Total_Sleep", "Daytime_Sleep", "Nighttime_Sleep")
    if (is.null(ranking)) {
      ranking_df <- df %>%
        dplyr::group_by(.data[[x]]) %>%
        dplyr::summarise(Total_Sleep = mean(Total_Sleep, na.rm = TRUE)) %>%
        dplyr::arrange(Total_Sleep)
      ranking <- ranking_df[[x]]
    }
    df[[x]] <- factor(df[[x]], levels = ranking)
    y_label <- "Sleep (min)"
    plot_cols <- c("Total_Sleep", "Daytime_Sleep", "Nighttime_Sleep")
  }
  
  plot_df <- function(data, yvar, x, title) {
    ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[yvar]])) +
      ggplot2::stat_summary(fun = mean, geom = "bar", fill = "grey50", width = 0.85) +
      ggplot2::labs(title = title, x = "", y = y_label) +
      ggprism::theme_prism(base_fontface = font) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
  
  plots <- lapply(plot_cols, function(col) {
    plot_df(df, col, x, paste(gsub("_", " ",col), title_text))
  })
  combined_plot <- cowplot::plot_grid(plotlist = plots, ncol = 1)
  rel_widths <- length(unique(df[, x]))/3
  
  if (!is.null(control) && fitted == FALSE) {
    if (!any(df[[x]] == control)) {
      stop("Control value not found in grouping variable.")}
    control_df <- df[df[[x]] == control, ]
    Batch<- "Batch"
    plots_con <- lapply(plot_cols, function(col) {
      plot_df(control_df, col, Batch, paste0("Control: ", control))
    })
    rel_widths <- c(rel_widths, length(unique(control_df$Batch))/3 + 1.7)
    combined_plot <- cowplot::plot_grid(plotlist = c(plots[1], plots_con[1],
                                                     plots[2], plots_con[2],
                                                     plots[3], plots_con[3]), ncol = 2,
                                        rel_widths = rel_widths)
    pdftitle<- paste0("_",control, pdftitle)
  }
  title_text <- gsub(":", ".", title_text)
  title_text <- gsub(" ", "_", title_text)
  
  ggplot2::ggsave(paste0("RankedSleep_",title_text, pdftitle, ".pdf"), combined_plot, width = sum(rel_widths), height = 10)
  if(fitted){
    write.csv(dataset, paste0("PopulationSleep_",title_text, pdftitle, ".csv"))}
  return(ranking)
}