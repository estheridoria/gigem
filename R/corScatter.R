#' Display underlying data to the correlation plots
#'
#' This function reads in a CSV file (`all_batches_summary.csv`)
#' The user can provide a list of aPrioriConditions (e.g., Genotype categories) which visually differentiates them from each other in the plots.
#'
#' @param x A string specifying the x-axis variable of the cluster plot as written in 'all_batches_summary.csv' (e.g., "Sleep_Time_All").
#' @param y A string specifying the y-axis variable of the cluster plot as written in 'all_batches_summary.csv' (e.g., "mean_Bout_length_L").
#' @param condition1 A string specifying the condition used as the reference/numerator (e.g., "Iso") when calculating difference or percent change.
#' @param condition2 A string specifying the condition used as the comparison/denominator (e.g., "Grp") for difference or percent change.
#' @param method The calculation method to use when 'condition1' and 'condition2' are specified. Must be "Diff" (calculates 'condition1` - `condition2`) or "Perc.Change" (calculates (`condition1` - `condition2`) / `condition2`).
#' @param treat A string specifying a Treatment to subset the data by.
#' @param temp A string specifying a Temperature condition to subset the data by.
#' @param enviro A string specifying a Environment condition to subset the data by.
#' @param sex A string specifying a sex condition to subset the data by.
#' @param lights A string specifying a Light condition to subset the data by.
#' @param geno A string specifying a Genotype condition to subset the data by.
#' @param aPrioriConditions A vector of (partial spellings of the) conditions (e.g., c("L1", "L2", "S1", "S2", "CS")) in one of the experimental variables.These are primarily for visualization (coloring) and must exist as a group within the specified aPrioriVariable.
#' @param aPrioriVariable A string specifying the variable of the aPrioriConditions. This variable (e.g., "Genotype") provides the column where aPrioriConditions are looked up for plotting.
#' @param lbf Add the line of best fit onto the plot.
#' @param formula A string defining the independent variables (e.g., "Treatment * Sex") for a linear model (LM). If specified, the plot displays fitted values (residuals removed) instead of raw means. Should be NULL for raw data.
#' @param font A character string determining the font style of the produced plots.
#'
#' @details
#' This plot best accompanies corMatrix, as it produces the same data, but with the actual points along the line of best fit. This can help with interpreting correlations.
#' @return Saves the plot as a PDF file and outputs a CSV file with cluster assignments.
#' @export
corScatter <- function(x = c(NULL, "Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L", "n_Bouts_D", "mean_Bout_Length_L","mean_Bout_Length_D"),
                       y = c(NULL, "Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L", "n_Bouts_D", "mean_Bout_Length_L","mean_Bout_Length_D"),
                       condition1 = NULL, condition2 = NULL, method = c("Diff", "Perc.Change"),
                       treat = NULL, temp = NULL, enviro = NULL, sex = NULL, lights = NULL, geno = NULL,
                       aPrioriConditions = NULL, aPrioriVariable = c(NULL, "Sex", "Genotype", "Temperature", "Treatment", "Environment", "Light"),
                       lbf = TRUE, formula = NULL, font = c("plain", "bold", "italic", "bold.italic")) {

 param_cols<- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L","n_Bouts_D", "mean_Bout_Length_L", "mean_Bout_Length_D")
 meta_vars <- c("Sex", "Genotype", "Temperature", "Treatment", "Environment", "Light")

 # Corrected argument validation section
 if (!is.null(x) && length(x) == 1) {
   X <- match.arg(x, param_cols)
 } else {
   X <- NULL
 }

 if (!is.null(y) && length(y) == 1) {
   Y <- match.arg(y, param_cols)
 } else {
   Y <- NULL
 }

 # If aPrioriVariable is the default vector (length > 1 and starts with NULL), set it to NULL.
 # Otherwise, if it's a single non-NULL string, validate it.
 apriori_is_default <- length(aPrioriVariable) > 1 || is.null(aPrioriVariable[1])

 if (!apriori_is_default) {
   aPrioriVariable <- match.arg(aPrioriVariable, meta_vars)
 } else {
   aPrioriVariable <- NULL
 }
 method <- match.arg(method)
  font<-match.arg(font, c("plain", "bold", "italic", "bold.italic"))

  # Check if aPrioriVariable is required in the formula if both are specified

  if(!is.null(aPrioriVariable) && !is.null(formula)){
    # Convert formula string to object and get variables
    formula_vars <- all.vars(as.formula(paste("~", formula)))
    # Check for existence (more robust than grep == 1)
    if (!aPrioriVariable %in% formula_vars) {
      stop(paste0("If 'formula' and 'aPrioriVariable' are specified, 'aPrioriVariable' (", aPrioriVariable,
                  ") must be included in the formula string."))
    }
  }

  # Check if 'all_batches_stat.csv' exists in the current directory, and if not, stop execution.
  if (!file.exists("all_batches_summary.csv")) {
    stop("'all_batches_summary.csv' is not found in the current directory. Please run 'runAllBatches' before attempting to run 'kmeansCluster'. If you have already run it, reset the working directory and run kmeansCluster again.")
  }




  combined_data<-read.csv("all_batches_summary.csv")
  data.table::setDT(combined_data)
  # define later variables
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

  # Summarize sleep data by temp, Sex, Treatment, and Genotype, enviro calculating means for sleep-related variables.
  dataset <- combined_data |>
    dplyr::group_by(across(all_of(c(meta_vars, "Batch")))) |>
    dplyr::summarise(across(all_of(param_cols), ~ mean(.x, na.rm = TRUE)), .groups = "keep")

  if (!is.null(formula)){#predicted values when controlling for the effects of stuffs

    # List of your categorical variables
    categorical_vars <- c(meta_vars, "Batch")

    # Initialize the formula string with the response variable
    formula_strings <- c("Sleep_Time_All ~ ", "Sleep_Time_L ~ ", "Sleep_Time_D ~ ", "n_Bouts_L ~ ","n_Bouts_D ~ ", "mean_Bout_Length_L ~ ", "mean_Bout_Length_D ~ ") #"Sleep_Time_All_mean ~ ", "Sleep_Time_L_mean ~ ", "Sleep_Time_D_mean ~ ", "n_Bouts_L_mean ~ ","n_Bouts_D_mean ~ ", "mean_Bout_Length_L_mean ~ ", "mean_Bout_Length_D_mean ~ ")
    saved.fit<- data.frame()
    for(i in seq_along(formula_strings)){
      formula_string <- paste0(formula_strings[i], formula)
      # Convert the formula string to a formula object
      new_formula <- as.formula(formula_string)
      # Fit the linear model using dataset
      # # Initialize an empty vector to store the matched variables
      matched_vars <- c()

      # Loop through each element in meta_vars to find which vars are in the formula --> matched_vars
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
      # Predict using the model
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
    #print(saved.fit)
    rownames(saved.fit)<- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L","n_Bouts_D", "mean_Bout_Length_L", "mean_Bout_Length_D")
    colnames(saved.fit)<- c("r-squared")
    write.csv(saved.fit, paste0("FittedStatistics_", titlee, ".csv"))
    colnames(all_prediction_results)[(ncol(all_prediction_results)-6):ncol(all_prediction_results)] <- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L","n_Bouts_D", "mean_Bout_Length_L", "mean_Bout_Length_D")

# average across the control characteristic (which would have been 1 population per batch)
      dataset <- all_prediction_results |>
      dplyr::group_by(across(all_of(intersect(matched_vars,meta_vars)))) |>
      dplyr::summarise(across(all_of(param_cols), ~ mean(.x, na.rm = TRUE)), .groups = "keep")
      fit_meta_vars <- intersect(matched_vars,meta_vars)
      pdftitle <- "_fittedValues"

  } else {
    dataset <- combined_data |>
      dplyr::group_by(across(all_of(meta_vars))) |> # exclude batch so the control aPrioriCondition (which should be in every batch) does not artificially weigh in the linear model
      dplyr::summarise(across(all_of(param_cols), ~ mean(.x, na.rm = TRUE)), .groups = "keep")
    pdftitle <- ""
    fit_meta_vars <- meta_vars
  }

  #------------Add aPrioriConditions & define x & y - specific to corScatter
  # Initialize the aPrioriConditions column with NAs

  dataset <- data.table::setDT(dataset)
  dataset[, aPrioriConditions := as.character(NA)]

  # Dynamically select the aPriori column
  if (!is.null(aPrioriVariable)){
    group_Column <- dplyr::pull(dataset, dplyr::all_of(aPrioriVariable))
    # Dynamically add the aprioriCondition grouping column to the dataset
    for (group in aPrioriConditions) {
    dataset[is.na(aPrioriConditions) & grepl(group, group_Column), aPrioriConditions := group]
  }
  fit_meta_vars <- c(fit_meta_vars, "aPrioriConditions")
  } else {
    aPrioriConditions <- NULL
  }

  # dataset<- as.data.frame(dataset)

  # Define sleep time variables for x & y
  if (!is.null(Y) && !is.null(X)){ # sets colx and coly to the specified parameters
    colx <- grep(X, param_cols)
    coly <- grep(Y, param_cols)
    dat_cols <- c(colx, coly)
  }
#---------------


  # absolute / %-change between conditions: raw values / predicted values
  if (!is.null(condition1) && !is.null(condition2)){
    match_col <- intersect(names(dataset), fit_meta_vars)[sapply(fit_meta_vars, function(var) {
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
    # math: c1-c2
      # Sort c1 and c2
    sort_vars <- fit_meta_vars[!fit_meta_vars %in% aPrioriVariable]
    data.table::setorderv(c1_data, cols = sort_vars)
    data.table::setorderv(c2_data, cols = sort_vars)

     # percent or difference method
    sleep_diff <- (c1_data[, param_cols, with = F] - c2_data[, param_cols, with = F])
    meta_cols_for_df <- setdiff(c(fit_meta_vars), match_col)
    if (method == "Diff"){
      df <- cbind(sleep_diff, c1_data[, meta_cols_for_df, drop = FALSE, with = F])

      # Rename the columns for better clarity.
      colnames(df)[1:7] <- final_param_cols <- c("Sleepchange_All", "Sleepchange_L", "Sleepchange_D", "nBoutschange_L",
                                                 "nBoutschange_D", "Boutlenchange_L", "Boutlenchange_D")
      title_text <- trimws(paste0(title_text, " ", condition1, "-", condition2))
      # meta <-  dataset[dataset[[match_col]] == condition2, fit_meta_vars]
    } else { # calculate the percentage of sleep change following condition1 compared to condition2 (control)
      if (method == "Perc.Change"){
        perc.change <- sleep_diff *100 / c2_data[, param_cols, with = F]
        df <- cbind(perc.change, c1_data[, meta_cols_for_df, drop = FALSE, with = F])

        # Rename the columns for better clarity.
        colnames(df)[1:7] <- final_param_cols <- c("%Sleepchange_All", "%Sleepchange_L", "%Sleepchange_D", "%nBoutschange_L",
                                                   "%nBoutschange_D", "%Boutlenchange_L", "%Boutlenchange_D")
        title_text <- trimws(paste0(title_text, " Percent ", condition1, " Sleep"))
      } else {
        stop("`Method` is undefined")
      }
    }

  } else {
    df <- dataset[, c(param_cols, fit_meta_vars), with = FALSE]
    # Rename the columns for better clarity.
    colnames(df)[1:7] <- final_param_cols <- c("Sleeptime_All", "Sleeptime_L", "Sleeptime_D", "NBouts_L","NBouts_D", "Boutlen_L", "Boutlen_D")
  }
  colors <- c("#808080","#0000FF", "#008B8B", "#ADD8E6", "#800080",
              "#32CD32", "#FFD700", "#FFA500","#FF0000")
  # Creating a correlation plot colored by clusters and aPrioriConditions
  clustered_plot<- function(plot_df, font, ptsize = 1, title = NULL, x = NULL, y=NULL, lbf = lbf, color, legend = FALSE){
    p<- ggplot2::ggplot(plot_df, ggplot2::aes(x = get(names(plot_df)[1]), y = get(names(plot_df)[2]))) +
      ggplot2::geom_point(size = ptsize, stroke = .5, shape = 21, alpha = 2/3,
                          ggplot2::aes(color = aPrioriConditions,
                                       fill = aPrioriConditions)) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      ggplot2::labs(title = title, x = x, y = y) +
      ggprism::theme_prism(base_fontface = font) +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     axis.title.y = ggplot2::element_text(size = 12),
                     axis.title.x = ggplot2::element_text(size = 12),
                     axis.text.x = ggplot2::element_text(size = 10),
                     axis.text.y = ggplot2::element_text(size = 10),
                     legend.text = ggplot2::element_text(size = 10, face = font),
                     legend.title = ggplot2::element_text(size = 10)) +
      # ggplot2::scale_shape_manual(name = "aPriori", values = c(0,1,4,2,3,14,9,10,11)) +
      ggplot2::scale_color_manual(values = scales::alpha(colors)) +
      ggplot2::scale_fill_manual(values = scales::alpha(colors))
    if(legend == FALSE){
      p <- p+ ggplot2::guides(color = "none", fill = "none", shape = "none")
    } else{
      p <- p+ ggplot2::guides(
        color = ggplot2::guide_legend(override.aes = list(size = 3)),  # Increase the point size in the legend
        size = ggplot2::guide_legend(override.aes = list(size = 2.5))
      )
    }
    if(lbf == TRUE){
      p <- p+ ggplot2::geom_smooth(method = "lm",  # Line of best fit
                                   se = FALSE,      # No error bars
                                   col = "grey",     # Single color for the line of best fit
                                   linewidth = 0.5)     # Color of the line
    }
    return(p)
  }

  # Create an empty list to store plots
  if (!is.null(Y) && !is.null(X)){
    #subset data
    plot_data <- df[,dat_cols, with = F]

    if(!is.null(aPrioriConditions)) {
    plot_data$aPrioriConditions <- df$aPrioriConditions
    } else {
      plot_data$aPrioriConditions <- NA
    }

    myplot <- clustered_plot(plot_data, font, ptsize = 2, title = title_text, x = names(df)[colx], y = names(df)[coly],lbf = lbf, color = colors, TRUE)
    title_text <- gsub(":", ".", title_text)
    ggplot2::ggsave(paste0("CorrelationScatterplot_", title_text,Y,X, pdftitle,".png"), myplot, height = 5, width = 6.3)

  } else {
    plots <- list()
    for (i in seq_along(final_param_cols)) {
      for (j in 1:i) {
        if (i == j) {
          # Add a blank plot for the diagonal
          blank_plot <- ggplot2::ggplot() + ggplot2::theme_void()
          plots[[paste0("plot_", i, "_", j)]] <- blank_plot
        } else {
          dat <- final_param_cols[c(i,j)]
          plot_data<- df[,dat, with = FALSE]
          plot_data$aPrioriConditions <- df$aPrioriConditions
          #make all the plots
          plots[[paste0("plot_", i, "_", j)]] <- clustered_plot(plot_data, font,ptsize = 1.5, lbf = lbf, color = colors)
        }
      }
    }

    # Create a grid layout for the lower triangle
    layout_matrix <- matrix(NA, nrow = length(final_param_cols)+1, ncol = length(final_param_cols)+1)

    plot_index <- 1
    for (i in seq_along(final_param_cols)) {
      for (j in 1:i) {
        layout_matrix[length(final_param_cols) - j+1, i+1] <- plot_index
        plot_index <- plot_index + 1
      }
    }
    for (i in seq_along(final_param_cols)) {
      layout_matrix[length(final_param_cols) - i + 1, 1] <- plot_index
      plot_index <- plot_index + 1
    }
    for (i in seq_along(final_param_cols)) {
      layout_matrix[length(final_param_cols)+1, i+1] <- plot_index
      plot_index <- plot_index + 1
    }
    # add labels as plots
    row_labels <- lapply(final_param_cols, function(x) grid::textGrob(x, gp = grid::gpar(fontsize = 18, fontface = font)))
    col_labels <- lapply(final_param_cols, function(x) grid::textGrob(x, rot = 45, gp = grid::gpar(fontsize = 18, fontface = font)))
    ploters<- c(plots, row_labels, col_labels)
    title_grob <- grid::textGrob(paste("Sleep parameters", title_text), gp = grid::gpar(fontsize = 20, fontface = font))

    # Arrange the plots in the grid
    myplot<- gridExtra::grid.arrange(grobs = ploters,
                                     layout_matrix = layout_matrix,
                                     heights = c(rep(0.9, length(final_param_cols)), 0.9),
                                     widths = c(1, rep(0.9, length(final_param_cols))),
                                     top = title_grob)

    if (!is.null(aPrioriConditions)){
    # Prepare data for the legends
    legend_data_colors <- data.frame(
      Clusters = factor(aPrioriConditions),
      Color = colors[1:length(aPrioriConditions)]
    )

    # Create the color legend
    color_legend <- ggplot2::ggplot(legend_data_colors, ggplot2::aes(x = 1, y = Clusters, color = Clusters)) +
      ggplot2::geom_point(size = 5) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "none",
        legend.text = ggplot2::element_text(size = 18, face = font),
        legend.title = ggplot2::element_text(size = 18, face = font)) +
      ggplot2::labs(color = aPrioriVariable)

    # Extract the legends
    suppressWarnings(
      color_grob <- cowplot::get_legend(color_legend + ggplot2::theme(legend.position = "right"))
    )
    # Save the final plot with legends
    final_plot <- gridExtra::grid.arrange(
      myplot,
      color_grob,
      ncol = 2,
      widths = c(14, 1.5) # Adjust widths as needed
    )
    } else {
      # Save the final plot with legends
      final_plot <- gridExtra::grid.arrange(
        myplot,
        ncol = 2,
        widths = c(14, 1.5) # Adjust widths as needed
      )
      }

    title_text <- gsub(" ", "", title_text)
    title_text <- gsub(":", ".", title_text)

    ggplot2::ggsave(paste0("CorrelationScatterplot_", title_text,pdftitle, ".png"), final_plot, height = 14, width = 16.5)
  }
}
