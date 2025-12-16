#' Visualize Changes in Sleep Metrics (Ranked Bar Plots)
#'
#' This function generates ranked bar plots to visualize changes or absolute values of sleep metrics (Total, Day, Night).
#' It reads the aggregated sleep data from `all_batches_summary.csv`, calculates the difference or percent change
#' between two conditions if specified, and can optionally use fitted values derived from a linear model (LM)
#' to control for nuisance variables. The plots are generated for each of the three sleep metrics.
#'
#' @param x The name of a categorical variable (e.g., "Genotype", "Treatment") to be used as the x-axis for ranking and plotting.
#' @param control The name of the control condition within the variable 'x'. If specified, a side-by-side comparison plot is generated showing the control's data broken down by Batch for context (only applicable when not using fitted values).
#' @param condition1 A string specifying the condition used as the reference/numerator (e.g., "Iso") when calculating difference or percent change. Default is NULL.
#' @param condition2 A string specifying the condition used as the comparison/denominator (e.g., "Grp") for difference or percent change. Default is NULL.
#' @param method The calculation method to use when 'condition1' and 'condition2' are specified. Must be "Diff" (calculates `condition1` - `condition2`) or "Perc.Change" (calculates (`condition1` - `condition2`) / `condition2`).
#' @param treat A string specifying a Treatment to subset the data by. Default is NULL.
#' @param temp A string specifying a Temperature condition to subset the data by. Default is NULL.
#' @param enviro A string specifying a Environment condition to subset the data by. Default is NULL.
#' @param sex A string specifying a sex condition to subset the data by. Default is NULL.
#' @param lights A string specifying a Light condition to subset the data by. Default is NULL.
#' @param geno A string specifying a Genotype condition to subset the data by. Default is NULL.
#' @param ranking A vector of strings specifying the custom order of conditions for the x-axis. If NULL (default), the order is determined by the mean of the Total Sleep metric.
#' @param formula Independent variables & their interactions in predicting sleep. If non-NULL, predicted values (fitted values) from the LM are used instead of raw means.
#' @param font A character string determining the font style of the produced plots. ("plain", "bold", "italic", or "bold.italic"). Default is "plain".
#'
#' @details
#' The primary output is a set of bar plots where the x-axis is ranked based on the Total Sleep metric (or Total Sleep Difference/Change).
#' The data is summarized at the population mean level before plotting.
#'
#' @return A vector of strings containing the final, calculated ranking (order) of conditions from the 'x' variable. Saves the plots as a PDF file labeled `RankedSleep...`.
#' @export
rankedDisplay <- function(
    x = c("Temperature", "Sex", "Treatment", "Genotype", "Environment", "Light"),
    control = NULL,
    condition1 = NULL,
    condition2 = NULL, method = c("Diff", "Perc.Change"),
    treat = NULL, temp = NULL, enviro = NULL, sex = NULL, lights = NULL, geno = NULL,
    ranking = NULL, formula = NULL,
    font = c("plain", "bold", "italic", "bold.italic")
) {
  if (!file.exists("all_batches_summary.csv")) {
    stop("'all_batches_summary.csv' not found. Please run 'runAllBatches' first.")
  }
  x <- match.arg(x)
  font <- match.arg(font)
  method <- match.arg(method)

  combined_data <- read.csv("all_batches_summary.csv")
  # data.table::setDT(combined_data)
  # define later variables
  param_cols<- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D")
  meta_vars <- c("Sex", "Genotype", "Temperature", "Treatment", "Environment", "Light")
  meta_inputs <- list(sex, geno, temp, treat, enviro, lights)
  names(meta_inputs) <- meta_vars

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

  # flag outliers
  combined_data <- combined_data |>
    dplyr::group_by(across(all_of(meta_vars))) |>
    dplyr::mutate(
      SD_Distance = (Sleep_Time_All - mean(Sleep_Time_All, na.rm = TRUE)) / sd(Sleep_Time_All, na.rm = TRUE),
      Is_Outlier = !is.na(SD_Distance) & abs(SD_Distance) > 3
    ) |>
    dplyr::ungroup()
  
  #export list of outliers
  outlier_report <- combined_data |>
    dplyr::filter(Is_Outlier == TRUE) |>
    dplyr::select(id, Genotype, Batch, Sleep_Time_All, SD_Distance)
  
  write.csv(outlier_report, "outlierFlies.csv", row.names = FALSE)
  
  # rm outliers & summarize sleep data by temp, Sex, Treatment, and Genotype, Enviro calculating means for sleep-related variables.
  dataset <- combined_data |>
    dplyr::filter(Is_Outlier == FALSE)|>
    dplyr::group_by(across(all_of(c(meta_vars, "Batch")))) |>
    dplyr::summarise(across(all_of(param_cols), ~ mean(.x, na.rm = TRUE)), .groups = "keep")
  
  # Use fitted values if fitted = TRUE
  if (!is.null(formula)){#predicted values when controlling for the effects of stuffs

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

      fit <- lm(new_formula, data = dataset)
      saved.fit<-rbind(saved.fit, summary(fit)$r.squared)
      # Create new_data for predictions
      new_data_list <- list()
      for (var in matched_vars) {
        # Collect unique levels from the original dataset for each matched variable
        new_data_list[[var]] <- unique(dataset[[var]])
      }

      # If 'Batch' was in your model, and you want to fix it at the first level
      if ("Batch" %in% matched_vars) {
        new_data_list[["Batch"]] <- levels(as.factor(dataset$Batch))[1]
      }

      # # Generate all combinations using expand.grid
      new_data <- expand.grid(new_data_list)

      # Initialize prediction variable
      prediction_results <- NULL
      # prediction_results using the model
      tryCatch({
        prediction_results<- predict(fit, newdata = new_data)
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
        all_prediction_results <- cbind(new_data, prediction_results)
      } else{
        all_prediction_results<-cbind(all_prediction_results, prediction_results)
      }
    }
    titlee <- gsub(":", ".", title_text)
    titlee <- gsub(" ", "_", titlee)

    rownames(saved.fit)<- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D")
    colnames(saved.fit)<- c("r-squared")
    write.csv(saved.fit, paste0("FittedStatistics_", titlee, ".csv"))
    colnames(all_prediction_results)[(ncol(all_prediction_results)-2):ncol(all_prediction_results)] <- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D")

    # average across the control characteristic (which would have been 1 population per batch)
    dataset <- all_prediction_results |>
      dplyr::group_by(across(all_of(intersect(matched_vars,meta_vars)))) |># exclude batch so the control aPrioriCondition (which should be in every batch) does not artificially weigh in the linear model
      dplyr::summarise(across(all_of(param_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
    fit_meta_vars <- intersect(matched_vars,meta_vars)
    pdftitle <- "_fittedValues"

  } else {
    if(is.null(control)){
    dataset <- combined_data |>
      dplyr::group_by(across(all_of(c(meta_vars)))) |> # exclude batch so the control aPrioriCondition (which should be in every batch) does not artificially weigh in the linear model
      dplyr::summarise(across(all_of(param_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
      fit_meta_vars <- meta_vars
    } else{
        fit_meta_vars <- c(meta_vars, "Batch")
      }
    pdftitle <- ""
  }

  # absolute / %-change between conditions: raw values / predicted values
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

    # math
    c1_sorted <- c1_data[do.call(order, c1_data[fit_meta_vars]), ]
    c2_sorted <- c2_data[do.call(order, c2_data[fit_meta_vars]), ]


    # percent or difference method
    sleep_diff <- (c1_sorted[, param_cols] - c2_sorted[, param_cols])
    meta_cols_for_df <- setdiff(c(fit_meta_vars), match_col)
    if (method == "Diff"){
      df <- cbind(sleep_diff, c1_sorted[, meta_cols_for_df, drop = FALSE])

      # Rename the columns for better clarity.
      colnames(df)[1:3] <- final_param_cols <- c("Total_SleepDifference", "Daytime_Sleep_Difference", "Nighttime_Sleep_Difference")
      title_text <- trimws(paste0(title_text, " ", condition1, "-", condition2))

      y_label <- "Change in Sleep (min)"
    } else { # calculate the percentage of sleep change following condition1 compared to condition2 (control)
      if (method == "Perc.Change"){
        perc.change <- sleep_diff *100 / c2_sorted[, param_cols]
        df <- cbind(perc.change, c1_sorted[, meta_cols_for_df, drop = FALSE])

        # Rename the columns for better clarity.
        colnames(df)[1:3] <- final_param_cols <- c("Total_Sleep_Change(%)", "Daytime_Sleep_Change(%)", "Nighttime_Sleep_Change(%)")
        title_text <- trimws(paste0(title_text, " Percent ", condition1, " Sleep"))

        y_label <- "Change in Sleep (%)"
      } else {
        stop("`Method` is undefined")
      }
    }

  }else {
    df <- dataset[, c(param_cols, fit_meta_vars)]
    colnames(df)[1:3] <- final_param_cols <- c("Total_Sleep", "Daytime_Sleep", "Nighttime_Sleep")

    y_label <- "Sleep (min)"
  }
  #-------------- Set ranking - rankedDisplay specific
  if (is.null(ranking)) {

    ranking_col_name <- final_param_cols[1]

    ranking_df <- df |>
      dplyr::group_by(.data[[x]]) |>
      dplyr::summarise(temp_ranking_mean = mean(.data[[ranking_col_name]], na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(temp_ranking_mean)
    ranking <- ranking_df[[x]]
  }

  df[[x]] <- factor(df[[x]], levels = ranking)
  #----------------- (End)

  plot_df <- function(data, yvar, x, title) {
    ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[yvar]])) +
      ggplot2::stat_summary(fun = mean, geom = "bar", fill = "grey50", width = 0.85) +
      ggplot2::labs(title = title, x = "", y = y_label) +
      ggprism::theme_prism(base_fontface = font) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  }

  plots <- lapply(final_param_cols, function(col) {
    plot_df(df, col, x, paste(gsub("_", " ",col), title_text))
  })
  combined_plot <- cowplot::plot_grid(plotlist = plots, ncol = 1)
  rel_widths <- length(unique(df[[x]]))/5

  if (!is.null(control) && is.null(formula)) {
    if (!any(df[[x]] == control)) {
      stop("Control value not found in grouping variable.")}
    control_df <- df[df[[x]] == control, ]
    Batch<- "Batch"
    plots_con <- lapply(final_param_cols, function(col) {
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
  if(!is.null(formula)){
    write.csv(dataset, paste0("PopulationSleep_",title_text, pdftitle, ".csv"))}
  return(ranking)
}
