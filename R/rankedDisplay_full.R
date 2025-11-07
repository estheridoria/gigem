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
#' @param condition1 A string specifying a condition within one of the experimental variables. Default is NULL
#' @param condition2 A string specifying a condition within the same experimental variable as `condition1` that is associated with the percent difference seen in sleep. Default is NULL
#' @param method The equation to be used either as the difference `condition1`-`condition2` or percent change, (`condition1`-`condition2`) / `condition2`
#' @param treat A string specifying a Treatment to subset the data by. Default is NULL
#' @param temp A string specifying a Temperature condition to subset the data by. Default is NULL
#' @param enviro A string specifying a Environment condition to subset the data by. Default is NULL
#' @param sex A string specifying a sex condition to subset the data by. Default is NULL
#' @param lights A string specifying a Light condition to subset the data by. Default is NULL
#' @param geno A string specifying a Genotype condition to subset the data by. Default is NULL
#' @param ranking A tibble of one column containing the order of conditions displayed in the plot from variable x.
#' @param fitted Use predicted values after controlling for variables and Batch effects. Default = FALSE
#' @param formula Independent variables & their interactions in predicting sleep. Should be NULL if fitted = FALSE.
#' @param font A character string determining the font style of the produced plots. ("plain", "bold", "italic", or "bold.italic"). Default is "plain"
#' @param limits A list of two numerics to be the upper and lower limits of the plot. Default is c(-100, 1500)
#'
#'
#' @return conditional. If the ranking is NULL (default), then the ranking used in the plots will be returned to be used as the ranking in subsequent rankedDisplay plots. Otherwise, no return. Saves the plots as a PDF file labeled `RankedSleep...`
#' @export
#'
#' @keywords internal
rankedDisplays <- function(
    x = c("Temperature", "Sex", "Treatment", "Genotype", "Environment", "Light"),
    control = NULL,
    condition1 = NULL,
    condition2 = NULL, method = c("Diff", "Perc.Change"),
    treat = NULL, temp = NULL, enviro = NULL, sex = NULL, lights = NULL, geno = NULL,
    ranking = NULL, fitted = FALSE, formula = NULL,
    font = c("plain", "bold", "italic", "bold.italic")
) {
  if (!file.exists("all_batches_summary.csv")) {
    stop("'all_batches_summary.csv' not found. Please run 'runAllBatches' first.")
  }
  x <- match.arg(x)
  if(fitted & is.null(formula)|| isFALSE(fitted) & !is.null(formula)){
    stop("Error: if fitted is 'TRUE', formula must be specified, and vice-versa.")
  }
  font <- match.arg(font)
  method <- match.arg(method)

  # define later variables
  param_cols<- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D")
  meta_vars <- c("Sex", "Genotype", "Temperature", "Treatment", "Environment", "Light")
  meta_inputs <- list(sex, geno, temp, treat, enviro, lights)
  names(meta_inputs) <- meta_vars

  # load data
  combined_data <- read.csv("all_batches_summary.csv")

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
  # Use fitted values if fitted = TRUE
  if (fitted){
    #predicted values when controlling for the effects of stuffs

    # List of your categorical variables
    categorical_vars <- c(meta_vars, "Batch")

    # Initialize the formula string with the response variable
    formula_stringss <- c("Sleep_Time_All ~ ", "Sleep_Time_L ~ ", "Sleep_Time_D ~ ")
    saved.fit<- data.frame()
    for(i in seq_along(formula_stringss)){
      formula_string <- paste0(formula_stringss[i], formula)
      # Convert the formula string to a formula object
      new_formula <- as.formula(formula_string)
      # Fit the linear model using dataset
      # # Initialize an empty vector to store the matched variables
       matched_vars <- c()
      # # Loop through each element in meta_vars
       for (var in categorical_vars) {
         # Check if the current meta_var is found within formula
         # grepl returns TRUE if a match is found, FALSE otherwise
         if (any(grepl(var, new_formula, fixed = TRUE))) {
           matched_vars <- c(matched_vars, var) # Add to our results if matched
         }
       }
      # vars_to_factorize <- matched_vars#[matched_vars != "Batch"]
      # # Loop through the column names to factorize
      # for (var_name in vars_to_factorize) {
      #   # Check if the column exists in your data frame to prevent errors
      #   if (var_name %in% colnames(dataset)) {
      #     dataset[[var_name]] <- as.factor(dataset[[var_name]])
      #     message(paste0("Converted '", var_name, "' to factor."))
      #   } else {
      #     message(paste0("Warning: Column '", var_name, "' not found in data frame. Skipping factorization."))
      #   }
      # }
       # print(new_formula)
      fit <- lm(new_formula, data = dataset)
      saved.fit<-rbind(saved.fit, summary(fit)$r.squared)
      # Create new_data for predictions
      new_data_list <- list()
      for (var in matched_vars) {
        new_data_list[[var]] <- unique(dataset[[var]])
      }

      # If 'Batch' was in your model, and you want to fix it at the first level
      if ("Batch" %in% matched_vars) {
        new_data_list[["Batch"]] <- levels(as.factor(dataset$Batch))[1]
      }

      # # Generate all combinations using expand.grid
      new_data <- expand.grid(new_data_list)

      # Predict using the model
      tryCatch({
        predict<- predict(fit, newdata = new_data)
        }, error = function(e) {
        if (grepl("rank-deficient", e$message)) {
          stop("Prediction failed due to a rank-deficient model fit.\n",
               "Too many variable conditions may be interfering with the regression.\n",
               "Try subsetting your data further using 'treat', 'temp', 'enviro', 'sex', 'lights', and/or 'geno'.\n",
               "Full error: ", e$message)
        } else {
          stop(e)  # re-throw other errors
        }})
      # Assign to your 'dataset' variable
      if(i == 1){
        dataa <- cbind(new_data, predict)
      } else{
        dataa<-cbind(dataa, predict)
      }
    }
    titlee <- gsub(":", ".", title_text)
    titlee <- gsub(" ", "_", titlee)

    rownames(saved.fit)<- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D")
    colnames(saved.fit)<- c("r-squared")
    write.csv(saved.fit, paste0("FittedStatistics_", titlee, ".csv"))
    colnames(dataa)[(ncol(dataa)-2):ncol(dataa)] <- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D")

    dataset<- dataa
    fit_meta_vars <- matched_vars
    pdftitle <- "_fittedValues"
  }else {
    pdftitle <- ""
    fit_meta_vars <- meta_vars
  }

  if (!is.null(condition1) && !is.null(condition2)) {
    match_col <- intersect(names(dataset), fit_meta_vars)[sapply(fit_meta_vars, function(var) {
      any(grepl(condition1, dataset[[var]])) && any(grepl(condition2, dataset[[var]]))
    })]
    if (length(match_col) != 1) {
      stop("Conditions must match within exactly one metadata column.")
    }
    c1_data <- dataset[dataset[[match_col]] == condition1, ]
    c2_data <- dataset[dataset[[match_col]] == condition2, ]
    if (nrow(c1_data) != nrow(c2_data)) stop("Unpaired condition1 and condition2 rows.")

    c1_sorted <- c1_data[do.call(order, c1_data[fit_meta_vars]), ]
    c2_sorted <- c2_data[do.call(order, c2_data[fit_meta_vars]), ]
    if(method == "Diff"){
      sleep_diff <- (c1_sorted[, param_cols] - c2_sorted[, param_cols])
      df <- cbind(sleep_diff, c1_sorted[, setdiff(c(fit_meta_vars, "Batch"), match_col), drop = FALSE])
      colnames(df)[1:3] <- c("Total_Sleep_Difference", "Daytime_Sleep_Difference", "Nighttime_Sleep_Difference")

      if (is.null(ranking)) {
        ranking_df <- df |>
          dplyr::group_by(.data[[x]]) |>
          dplyr::summarise(Total_Sleep_Difference = mean(Total_Sleep_Difference, na.rm = TRUE)) |>
          dplyr::arrange(Total_Sleep_Difference)
        ranking <- ranking_df[[x]]
      }
      y_label <- "Change in Sleep (min)"
      plot_cols <- c("Total_Sleep_Difference", "Daytime_Sleep_Difference", "Nighttime_Sleep_Difference")
      }else{
      sleep_diff <- (c1_sorted[, param_cols] - c2_sorted[, param_cols]) / c2_sorted[, param_cols]
      sleep_diff <- sleep_diff*100 #get percentage
      df <- cbind(sleep_diff, c1_sorted[, setdiff(c(fit_meta_vars, "Batch"), match_col), drop = FALSE])
      colnames(df)[1:3] <- c("Total_Sleep_Change", "Daytime_Sleep_Change", "Nighttime_Sleep_Change")

    if (is.null(ranking)) {
      ranking_df <- df |>
        dplyr::group_by(.data[[x]]) |>
        dplyr::summarise(Total_Sleep_Change = mean(Total_Sleep_Change, na.rm = TRUE)) |>
        dplyr::arrange(Total_Sleep_Change)
      ranking <- ranking_df[[x]]
    }
      y_label <- "Change in Sleep (%)"
      plot_cols <- c("Total_Sleep_Change", "Daytime_Sleep_Change", "Nighttime_Sleep_Change")
    }

    df[[x]] <- factor(df[[x]], levels = ranking)

  }else {
    df <- dataset[, c(param_cols, fit_meta_vars, "Batch")]
    colnames(df)[1:3] <- c("Total_Sleep", "Daytime_Sleep", "Nighttime_Sleep")
    if (is.null(ranking)) {
      ranking_df <- df |>
        dplyr::group_by(.data[[x]]) |>
        dplyr::summarise(Total_Sleep = mean(Total_Sleep, na.rm = TRUE)) |>
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
  rel_widths <- length(unique(df[[x]]))/5

  if (!is.null(control) && fitted == FALSE) {
    if (!any(df[[x]] == control)) {
      stop("Control value not found in grouping variable.")}
    control_df <- df[df[[x]] == control, ]
    Batch<- "Batch"
    plots_con <- lapply(plot_cols, function(col) {
      plot_df(control_df, col, Batch, paste0("Control: ", control))
    })
    rel_widths <- c(rel_widths, length(unique(control_df$Batch))/5 + 1.7)
    combined_plot <- cowplot::plot_grid(plotlist = c(plots[1], plots_con[1],
                                                     plots[2], plots_con[2],
                                                     plots[3], plots_con[3]), ncol = 2,
                                        rel_widths = rel_widths)
    pdftitle<- paste0("_",control, pdftitle)
  }
  title_text <- gsub(":", ".", title_text)
  title_text <- gsub(" ", "_", title_text)

  ggplot2::ggsave(paste0("RankedSleep_",title_text, pdftitle, ".pdf"),
                  combined_plot, width = sum(rel_widths), height = 9)
  if(fitted){
    write.csv(dataset, paste0("PopulationSleep_",title_text, pdftitle, ".csv"))}
  return(ranking)
}
