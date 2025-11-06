#' Display underlying data to the correlation plots
#'
#' This function reads in a CSV file (`all_batches_summary.csv`)
#' The user can provide a list of aPrioriConditions (e.g., Genotype categories) which visually differentiates them from each other in the plots.
#'
#' @param x A string specifying the x-axis variable of the cluster plot as written in 'all_batches_summary.csv' (e.g., "Sleep_Time_All"). Default is None
#' @param y A string specifying the y-axis variable of the cluster plot as written in 'all_batches_summary.csv' (e.g., "mean_Bout_length_L"). Default is None
#' @param condition1 A string specifying a condition within one of the experimental variables. The difference is calculated as: `condition1`-`condition2`
#' @param condition2 A string specifying a condition within the same experimental variable as `condition1` that is associated with the difference seen in sleep.
#' @param method The equation to be used either as the difference `condition1`-`condition2` or percent change, (`condition1`-`condition2`) / `condition2`
#' @param treat A string specifying a Treatment to subset the data by. Default is NULL
#' @param temp A string specifying a Temperature condition to subset the data by. Default is NULL
#' @param enviro A string specifying a Environment condition to subset the data by. Default is NULL
#' @param sex A string specifying a sex condition to subset the data by. Default is NULL
#' @param lights A string specifying a Light condition to subset the data by. Default is NULL
#' @param geno A string specifying a Genotype condition to subset the data by. Default is NULL
#' @param aPrioriConditions A vector of (partial spellings of the) conditions (e.g., c("L1", "L2", "S1", "S2", "CS")) in one of the experimental variables.
#' @param aPrioriVariable A string specifying the variable of the aPrioriConditions. Default is "Sex".
#' @param lbf Add the line of best fit onto the plot. Default is TRUE
#' @param formula Independent variables & their interactions in predicting sleep. Should be NULL if fitted = FALSE.
#' @param font A character string determining the font style of the produced plots. ("plain", "bold", "italic", or "bold.italic"). Default is "plain"
#' @param font A character string determining the font style of the produced plots. Default is "plain".
#'
#' @details
#' This plot best accompanies corMatrix, as it produces the same data, but with the actual points along the line of best fit. This can help with interpreting correlations.
#' @return None. Saves the plot as a PDF file and outputs a CSV file with cluster assignments.
#' @export
corScatter <- function(x = c("None", "Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L", "n_Bouts_D", "mean_Bout_Length_L","mean_Bout_Length_D"),
                       y = c("None", "Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L", "n_Bouts_D", "mean_Bout_Length_L","mean_Bout_Length_D"),
                       condition1 = NULL, condition2 = NULL, method = c("Diff", "Perc.Change"),
                       treat = NULL, temp = NULL, enviro = NULL, sex = NULL, lights = NULL, geno = NULL,
                       aPrioriConditions = NULL, aPrioriVariable = c("None", "Sex", "Genotype", "Temperature", "Treatment", "Environment", "Light"),
                       lbf = TRUE, formula = NULL, font = c("plain", "bold", "italic", "bold.italic")) {

  # y<-Y<- "mean_Bout_Length_D"
  # x<-X<- "mean_Bout_Length_L"
  # condition1 = NULL#"Iso"
  # condition2 = NULL#"Grp"
  # sex<- geno<- temp<- treat<- Lights<- NULL
  # enviro = "2D"
  # aPrioriConditions = c("L1", "L2", "S1", "S2", "CS")
  # aPrioriVariable = "Genotype"
  # lbf=TRUE
  # font = "bold"

  X <- match.arg(x)  # Validate that x is valid.
  Y <- match.arg(y)  # Validate that y is valid.
  if(X == "None"){X<-NULL}
  if(Y == "None"){Y<-NULL}
  aPrioriVariable<- match.arg(aPrioriVariable)
  font<-match.arg(font)
  if(aPrioriVariable == "None"){aPrioriVariable<-NULL}
  if(!is.null(aPrioriVariable) && !is.null(formula))
  { if (grep(aPrioriVariable, formula) ==1){}else {stop("aPrioriVariable must be included in the specified formula.")} }

  # Check if 'all_batches_stat.csv' exists in the current directory, and if not, stop execution.
  if (!file.exists("all_batches_summary.csv")) {
    stop("'all_batches_summary.csv' is not found in the current directory. Please run 'runAllBatches' before attempting to run 'kmeansCluster'. If you have already run it, reset the working directory and run kmeansCluster again.")
  }
  combined_data<-read.csv("all_batches_summary.csv")
  data.table::setDT(combined_data)
  # define later variables
  param_cols<- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L","n_Bouts_D", "mean_Bout_Length_L", "mean_Bout_Length_D")
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
    #print(saved.fit)
    rownames(saved.fit)<- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L","n_Bouts_D", "mean_Bout_Length_L", "mean_Bout_Length_D")
    colnames(saved.fit)<- c("r-squared")
    write.csv(saved.fit, paste0("FittedStatistics_", titlee, ".csv"))
    colnames(dataa)[(ncol(dataa)-6):ncol(dataa)] <- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L","n_Bouts_D", "mean_Bout_Length_L", "mean_Bout_Length_D")

    dataset<- dataa
    fit_meta_vars <- c(matched_vars)
    pdftitle <- "_fittedValues"

  } else {
    dataset <- combined_data |>
      dplyr::group_by(across(all_of(c(meta_vars)))) |>
      dplyr::summarise(across(all_of(param_cols), ~ mean(.x, na.rm = TRUE)), .groups = "keep")
    pdftitle <- ""
    fit_meta_vars <- meta_vars
  }

  # Initialize the aPrioriConditions column with NAs

  data.table::setDT(dataset)
  dataset[, aPrioriConditions := as.character(NA)]

  # Dynamically select the aPriori column
  if (!is.null(aPrioriVariable)){
  group_Column <- dplyr::pull(dataset, dplyr::all_of(aPrioriVariable))
  # Dynamically add the aprioriCondition grouping column to the dataset
  for (group in aPrioriConditions) {
    dataset[is.na(aPrioriConditions) & grepl(group, group_Column), aPrioriConditions := group]
  }
  fit_meta_vars<- c(fit_meta_vars, "aPrioriConditions")
  } else {
    aPrioriConditions <- NULL
  }

  dataset<- as.data.frame(dataset)

  # Define sleep time variables for x & y
  if (!is.null(Y) && !is.null(X)){ # sets colx and coly to the specified parameters
    colx <- grep(X, param_cols)
    coly <- grep(Y, param_cols)
    dat_cols <- c(colx, coly)
  }

  # If conditions are specified, take their difference. Else, use actual values.
  if(!is.null(condition1) && !is.null(condition2)){
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
    c1_sorted <- c1_data[do.call(order, c1_data[fit_meta_vars]), ]
    c2_sorted <- c2_data[do.call(order, c2_data[fit_meta_vars]), ]

    sleep_diff <- (c1_sorted[, param_cols] - c2_sorted[, param_cols])
    df <- cbind(sleep_diff, c1_sorted[, setdiff(c(fit_meta_vars), match_col), drop = FALSE])

    # Rename the columns for better clarity.
    colnames(df)[1:7] <- parameters <- c("Sleepchange_All", "Sleepchange_L", "Sleepchange_D", "nBoutschange_L",
                                         "nBoutschange_D", "Boutlenchange_L", "Boutlenchange_D")
    title_text <- trimws(paste0(title_text, " ", condition1, "-", condition2))
    meta <-  dataset[dataset[[match_col]] == condition2, fit_meta_vars]

  } else {
    df <- dataset[, c(param_cols, fit_meta_vars)]
    # Rename the columns for better clarity.
    colnames(df)[1:7] <- parameters <- c("Sleeptime_All", "Sleeptime_L", "Sleeptime_D", "NBouts_L",
                                         "NBouts_D", "Boutlen_L", "Boutlen_D")
    meta <-  dataset[,fit_meta_vars]
  }
  colors <- c("#808080","#0000FF", "#008B8B", "#ADD8E6", "#800080",
              "#32CD32", "#FFD700", "#FFA500","#FF0000")
  # Creating a correlation plot colored by clusters and aPrioriConditions
  clustered_plot<- function(data, font, ptsize = 1, title = NULL, x = NULL, y=NULL, lbf = lbf, color, legend = FALSE){
    p<- ggplot2::ggplot(data, ggplot2::aes(x = get(names(data)[1]), y = get(names(data)[2]))) +
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
    plot_data <- df[,dat_cols]

    # if(!is.null(aPrioriConditions)) {
    # plot_data$aPrioriConditions <- meta$aPrioriConditions
    # }

    myplot <- clustered_plot(plot_data, font, ptsize = 2, title = title_text, x = names(df)[colx], y = names(df)[coly],lbf = lbf, color = colors, TRUE)
    title_text <- gsub(":", ".", title_text)
    ggplot2::ggsave(paste0("CorrelationScatterplot_", title_text,Y,X, pdftitle,".pdf"), myplot, height = 5, width = 6.3)

  } else {
    plots <- list()
    for (i in seq_along(parameters)) {
      for (j in 1:i) {
        if (i == j) {
          # Add a blank plot for the diagonal
          blank_plot <- ggplot2::ggplot() + ggplot2::theme_void()
          plots[[paste0("plot_", i, "_", j)]] <- blank_plot
        } else {
          dat <- parameters[c(i,j)]
          plot_data<- df[,dat]
          plot_data$aPrioriConditions <- meta$aPrioriConditions
          #make all the plots
          plots[[paste0("plot_", i, "_", j)]] <- clustered_plot(plot_data, font,ptsize = 1.5, lbf = lbf, color = colors)
        }
      }
    }

    # Create a grid layout for the lower triangle
    layout_matrix <- matrix(NA, nrow = length(parameters)+1, ncol = length(parameters)+1)

    plot_index <- 1
    for (i in seq_along(parameters)) {
      for (j in 1:i) {
        layout_matrix[length(parameters) - j+1, i+1] <- plot_index
        plot_index <- plot_index + 1
      }
    }
    for (i in seq_along(parameters)) {
      layout_matrix[length(parameters) - i + 1, 1] <- plot_index
      plot_index <- plot_index + 1
    }
    for (i in seq_along(parameters)) {
      layout_matrix[length(parameters)+1, i+1] <- plot_index
      plot_index <- plot_index + 1
    }
    # add labels as plots
    row_labels <- lapply(parameters, function(x) grid::textGrob(x, gp = grid::gpar(fontsize = 18, fontface = font)))
    col_labels <- lapply(parameters, function(x) grid::textGrob(x, rot = 45, gp = grid::gpar(fontsize = 18, fontface = font)))
    ploters<- c(plots, row_labels, col_labels)
    title_grob <- grid::textGrob(paste("Sleep parameters", title_text), gp = grid::gpar(fontsize = 20, fontface = font))

    # Arrange the plots in the grid
    myplot<- gridExtra::grid.arrange(grobs = ploters,
                                     layout_matrix = layout_matrix,
                                     heights = c(rep(0.9, length(parameters)), 0.9),
                                     widths = c(1, rep(0.9, length(parameters))),
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

    ggplot2::ggsave(paste0("CorrelationScatterplot_", title_text,pdftitle, ".pdf"), final_plot, height = 14, width = 16.5)
  }
}
