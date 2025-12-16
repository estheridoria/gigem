.onLoad <- function(libname, pkgname) {
  
  if (!methods::isClass("ExperimentData")) {
    methods::setClass(
      Class = "ExperimentData",
      slots = list(
        Batch = "character",
        monitorlist = "list",
        genotypelist = "list",
        loadinginfo = "data.table"
      )
    )
  }
  
}
#-----------------------------------------------------------------------------------------
runAllBatches <- function(numDays = 2,
                          overlayVar,
                          rowVar,
                          columnVar,
                          plotSelection = "All",
                          font = "plain",
                          pValues = FALSE) {
  # Warnings/Errors-------------------------------------------------------------
  if (missing(numDays) || !is.numeric(numDays)){
    stop("'numDays' must be specified as a whole number.")
  }
  if(length(unique(c(overlayVar, rowVar, columnVar))) < 3){  # divisions for fascetting plots
    stop("'overlayVar', 'rowVar', and 'columnVar' cannot contain the same variable names.")
  }
  divisions<- character()
  divisions[1]<- match.arg(overlayVar, c("Treatment", "Sex", "Genotype", "Temperature", "Environment", "Light"))
  divisions[2]<- match.arg(rowVar, c("Genotype", "Sex", "Temperature", "Treatment", "Environment", "Light"))
  divisions[3]<- match.arg(columnVar, c("Environment", "Sex", "Genotype", "Temperature", "Treatment", "Light"))
  #divisions<- c(overlayVar, rowVar, columnVar)
  plotSelection <- match.arg(plotSelection, c("All", "None", "Select"))
  font <- match.arg(font, c("plain", "bold", "italic", "bold.italic"))
  if(!is.logical(pValues)){
    stop("'pValues' must be either 'TRUE' or 'FALSE'")
  }
  
  # Save the current working directory
  original_wd <- getwd()
  
  # Get the list of all sub directories
  all_dirs <- list.dirs(original_wd, full.names = TRUE, recursive = FALSE)
  
  # Function to iterate through Meta.r files
  run_r_files_in_dir <- function(dir) {
    setwd(dir) # Change to the target directory
    r_files <- list.files(dir, pattern = "^Main[0-9_a-zA-Z]*\\.R$", full.names = TRUE) # Get the list of R files in the directory
    
    # Execute each R file within a temporary local environment
    for (r_file in r_files) {
      # 1. Create a new, temporary environment for execution
      temp_env <- new.env()
      
      # 2. Execute the script within that environment
      source(r_file, local = TRUE)
      incodeinfo<-info
    }
    
    
    return(incodeinfo)
  }
  # Filter directories that match the "Batch" pattern.
  batch_dirs <- grep("Batch[0-9_a-zA-Z]*", all_dirs, value = TRUE)
  # Warning
  if(length(batch_dirs) ==0){
    stop("The folder(s) inside the parent directory containing each Batch's data is either not present or is not formatted correctly. Please add or rename the folder and R file within to follow the format: 'Batch' followed by any combination of letters, numbers and/or underscores.")
  }
  
  # Warning: Iterate over each Main file and concatenate to make sure everything is formatted correctly
  all_tables <- list()
  for (batch_dir in batch_dirs) {
    incodeinfo <- run_r_files_in_dir(batch_dir)
    all_tables[[length(all_tables) + 1]] <- incodeinfo
  }
  # Warning: Custom function to stop on specific warning if columns are not aligned with each batch's main files
  combine_with_warning_check <- function(dt_list) {
    withCallingHandlers({
      # Attempt to combine the data.tables
      combined <- data.table::rbindlist(dt_list, fill = TRUE, use.names = TRUE)
      
    }, warning = function(w) {
      # Check if the warning contains the specific message
      if (grepl("Item .* has .* rows but longest item has .*; recycled with remainder", conditionMessage(w))) {
        stop("Error: One or more variables in one or more of your
             Main.R files has a different number of conditions than the other variables.
             Please ensure each variable has an equal number of conditions within
             the respective Main.R file(s).")
      }
      if (grepl("Column .* of item .* is missing", conditionMessage(w))) {
        stop("Error: One or more variables is missing from some of your data tables.
             Please ensure all variables are present and named correctly in all Main.R files.")
      }
    })
    return()
  }
  combine_with_warning_check(all_tables)
  
  # Set the stage---------------------------------------------------------------
  
  # Save the current working directory
  setwd(original_wd)
  
  if(plotSelection == "All"){
    # Ask user which plots they want
    pref <- c(1,1,1,1,1,1,1)
  }
  if(plotSelection == "None"){
    # Ask user which plots they want
    pref <- c(2,2,2,2,2,2,2)
  }
  if(plotSelection == "Select"){
    # Ask user which plots they want
    pref <- plotPreferences("all")
  }
  
  # Analyze each batch
  for (oneBatch in batch_dirs){
    incodeinfo <- run_r_files_in_dir(oneBatch)
    # add "monitor" to the info file
    incodeinfo[["monitor"]]<- paste0("M", gsub("\\D", "", incodeinfo$file))
    #change any NA values to "NA" to prevent errors
    incodeinfo[is.na(incodeinfo)] <- "NA"
    incodeinfo[,6:11]<- lapply(incodeinfo[,6:11], as.character)
    runEachBatch(numDays, oneBatch, font, pref, divisions, pValues, incodeinfo)
  }
  
  # Restore the original working directory
  setwd(original_wd)
  
# Concatenate files from batches------------------------------------------------
  # Summaries: relative, stat, summary, & possibly sleep, meta
  concatList<- c("^stat_Batch[0-9_a-zA-Z]*\\.csv$", "^summary[0-9_a-zA-Z]*\\.csv$") #, "ks.results_L.csv$", "^relative_summary_Batch[0-9_a-zA-Z]*\\.csv$"
  concatNames<- c("all_batches_stat.csv", "all_batches_summary.csv") #, "all_batches_ks.result_L.csv", "all_batches_relative_summary.csv"

  if (pref[7] == 1){ # concatenated sleepdata & metadata --> genotypePlots
    concatList <- c(concatList, "^sleepdata_Batch[0-9_a-zA-Z]*\\.csv$", "^sleepmeta_Batch[0-9_a-zA-Z]*\\.csv$")
    concatNames <- c(concatNames, "all_batches_sleepdata.csv", "all_batches_sleepmeta.csv")
  }
  
    # Define a mapping for output filenames to variable names
  output_map <- list(
    "all_batches_summary.csv" = "summary_dt_final",
    "all_batches_sleepdata.csv" = "combined_sleepdata",
    "all_batches_sleepmeta.csv" = "combined_sleepmeta"
    # Add other mappings for concatNames[i] as needed
  )    
  for (i in seq_along(concatList)){
    
    # File paths
    all_file_paths <- unlist(sapply(batch_dirs, function(dir) {
      list.files(dir, pattern = concatList[i], full.names = TRUE)
    }))
    
    if (length(all_file_paths) > 0) {
      
      #Read and Combine
      combined_data <- data.table::rbindlist(lapply(all_file_paths, data.table::fread), fill = TRUE)
      
      #Save
      output_file_name <- concatNames[i]
      output_file <- file.path(original_wd, output_file_name)
      data.table::fwrite(combined_data, output_file, row.names = FALSE)
      
      # Outputs
      if (output_file_name %in% names(output_map)) {
        var_name <- output_map[[output_file_name]]
        # assign the 'combined_data' data.table to the variable named by the string 'var_name' in the current environment.
        assign(var_name, combined_data, envir = parent.frame())
      }
    }
  }
  
  # for (i in seq_along(concatList)){
  # 
  #   # Get all file paths matching the pattern across all batch directories
  #   all_file_paths <- unlist(sapply(batch_dirs, function(dir) {
  #     list.files(dir, pattern = concatList[i], full.names = TRUE)
  #   }))
  # 
  #   if (length(all_file_paths) > 0) {
  #     # OPTIMIZATION: Read all files into a single data.table using data.table::fread list input
  #     # This is significantly faster than reading/binding one by one
  #     combined_data <- data.table::rbindlist(lapply(all_file_paths, data.table::fread), fill = TRUE)
  # 
  #     # Save the combined data frame to a CSV file in the parent directory
  #     output_file <- file.path(original_wd, concatNames[i])
  #     data.table::fwrite(combined_data, output_file, row.names = FALSE)
  # 
  #     if(i == 2){
  #       summary_dt_final<-combined_data
  #     }
  #     if(i == 3){
  #       combined_sleepdata <-combined_data
  #     }
  #     if(i == 4){
  #       combined_sleepmeta<-combined_data
  #     }
  #     
  #   }
  # }
  
  if(pref[7] ==1){
    concatCombinedPlots(combined_sleepdata, combined_sleepmeta, summary_dt_final, font, divisions, pValues)
  }
}
#-----------------------------------------------------------------------------------------
runOneBatch <- function(oneBatch, numDays = 2,
                        overlayVar = "Treatment",
                        rowVar = "Genotype",
                        columnVar = "Environment",
                        plotSelection = "All",
                        font = "plain",
                        pValues = FALSE) {
  # Warnings/Errors-------------------------------------------------------------
  if (missing(oneBatch)){
    stop("'oneBatch' must be specified")
  }
  all_dirs <- list.dirs(getwd(), full.names = FALSE, recursive = FALSE)
  if (length(grep(oneBatch, all_dirs)) != 1){
    stop("The 'oneBatch' specified is not a subdirectory inside the current working directory. Please make sure your current directory is correct.")
  }
  if (!is.numeric(numDays)){
    stop("'numDays' must be specified as a whole number.")
  }
  if(length(unique(c(overlayVar, rowVar, columnVar))) < 3){  # divisions for fascetting plots
    stop("'overlayVar', rowVar, and columnVar cannot contain the same variable names.")
  }
  divisions<- character()
  divisions[1]<- match.arg(overlayVar, c("Treatment", "Sex", "Genotype", "Temperature", "Environment", "Light"))
  divisions[2]<- match.arg(rowVar, c("Genotype", "Sex", "Temperature", "Treatment", "Environment", "Light"))
  divisions[3]<- match.arg(columnVar, c("Environment", "Sex", "Genotype", "Temperature", "Treatment", "Light"))

  plotSelection <- match.arg(plotSelection, c("All", "None", "Select"))
  font <- match.arg(font, c("plain", "bold", "italic", "bold.italic"))
  if(!is.logical(pValues)){
    stop("'pValues' must be either 'TRUE' or 'FALSE'")
  }
  # Set the stage-------------------------
  
  # Save the current working directory
  original_wd <- getwd()
  
  # Change to the target directory
  setwd(paste0(original_wd, "/", oneBatch))
  
  # Get the list of R files in the directory
  r_files <- list.files(getwd(), pattern = "^Main[0-9_a-zA-Z]*\\.R$", full.names = TRUE)
  
  # Store all 'info' objects in a list, in case multiple R files exist
  all_info_list <- list()
  
  # Execute each R file within a temporary local environment
  for (r_file in r_files) {
    # 1. Create a new, temporary environment for execution
    temp_env <- new.env()
    
    # 2. Execute the script within that environment
    source(r_file, local = TRUE)
    incodeinfo<-info
  }
  
  
  # add "monitor" to the info file
  incodeinfo[["monitor"]]<- paste0("M", gsub("\\D", "", incodeinfo$file))
  #change any NA values to "NA" to prevent errors
  incodeinfo[is.na(incodeinfo)] <- "NA"
  incodeinfo[,6:11]<- lapply(incodeinfo[,6:11], as.character)
  
  if(plotSelection == "All"){
    # Ask user which plots they want
    pref <- c(1,1,1,1,1,1,1)
  }
  if(plotSelection == "None"){
    # Ask user which plots they want
    pref <- c(2,2,2,2,2,2,2)
  }
  if(plotSelection == "Select"){
    # Ask user which plots they want
    pref <- plotPreferences("one")
  }
  
  # Analyze the batch
  runEachBatch(numDays, oneBatch, font, pref, divisions, pValues, incodeinfo)
  
  setwd(original_wd)
  
}
#-----------------------------------------------------------------------------------------
plotPreferences <- function(run = "all"){
  
  # activityAndSleep.R #17-34 & #40-57
  r1 <- menu(c("Yes", "No"), title="Do you want to generate each monitor's 'Activity and Sleep Actograms'?")
  
  # manualDeadRemoval.R #42
  r2 <- menu(c("Yes", "No"), title="Do you want to generate 'Population Sleep Profiles'? (i.e. Each unique condition)")
  
  # manualDeadRemoval.R #68
  r3 <- menu(c("Yes", "No"), title="Do you want to generate 'Overlaid Sleep Profiles'? (i.e. Each condition overlaid according to your first entry in 'Divisions')")
  
  # cleanSummary.R #45
  r4 <- menu(c("Yes", "No"), title="Do you want to generate 'Overlaid Sleep Bout Profiles'?")
  
  # cleanSummary.R #92
  r5 <- menu(c("Yes", "No"), title="Do you want to generate 'Quantifications of Sleep Traits'?")
  
  # runOneBatch.R #53 (run genotypePlots())
  r6 <- menu(c("Yes", "No"), title="Do you want to generate 'Combined Plots (Within Batches)'?(i.e. All plots grouped by unique combination of variable conditions)")
  
  if (run == "all"){
    # runAllBatches.R #117 (run concatGenotypePlots())
    r7 <- menu(c("Yes", "No"), title="Do you want to generate 'Combined Plots (Across Batches)'? (i.e. All plots grouped by unique combination of variable conditions)")
  }
  if (run =="one"){
    results <- rbind(r1, r2, r3, r4, r5, r6, "2")
  } else{
    results <- rbind(r1, r2, r3, r4, r5, r6, r7)
    
  }
  return(results)
}
#-----------------------------------------------------------------------------------------
runEachBatch <- function(numDays, oneBatch, font, pref, divisions, pValues, incodeinfo) {
  
  # Create an object that contains all of your inputs
  ExperimentData <- new("ExperimentData",
                        Batch = unique(incodeinfo$Batch)[1],
                        monitorlist = as.list(unique(incodeinfo$monitor)),
                        genotypelist = as.list(unique(incodeinfo$genotype)),
                        loadinginfo = incodeinfo)
  
  # Store this as a CSV file, used to curate summary tables
  data.table::fwrite(ExperimentData@loadinginfo, paste("loadinginfo_",ExperimentData@Batch,".csv",sep = ""))
  # Link metadata
  loadinginfo_linked <- damr::link_dam_metadata(ExperimentData@loadinginfo, result_dir = getwd())
  
  # Create activity plots and sleep plots of each monitor at time t
  dt_activity <- activityAndSleep(ExperimentData, loadinginfo_linked, pref)
  
  # Create activity plots before and after removing "dead", providing list of IDs removed from first trimming
  dt_curated <- aliveVsDead(ExperimentData, dt_activity)
  
  # Warning: Custom function to stop on specific warning if columns are not aligned with each batch's main files
  combine_with_warning_check <- function(ExperimentData, dt_curated, numDays, divisions, pref, font) {
    tryCatch({
      
      # Further removal and trimming of animals that died before specified time, providing list of IDs removed
      result <- manualDeadRemoval(ExperimentData, dt_curated, numDays, divisions, pref, font)
      
    }, error = function(e) {
      # Check if the error contains the specific message
      if (grepl("Faceting variables must have at least one value", conditionMessage(e))) {
        msg<- paste0("\n",
                     "1) the start/stop date(s) within the 'Main' file is/are incorrect OR\n",
                     "2) 'numDays' is too many days")
        stop(simpleError(msg))
      }
    })
    return(result)
  }
  dt_final <- combine_with_warning_check(ExperimentData, dt_curated, numDays, divisions, pref, font)
  
  batchMeta <- behavr::meta(dt_final)
  
  # Write bout length pdf, and calculate bout and latency stats
  dt_finalSummary <- cleanSummary(ExperimentData, dt_final, batchMeta, numDays, loadinginfo_linked, divisions, pref, font)
  
  if (pref[6] == 1){
    # Generate concatenated plots
    combinedPlots(ExperimentData, dt_final, dt_finalSummary, font, divisions, pValues)
  } # dt_curated_final <- dt_final; summary_dt_final <- dt_finalSummary
  
  # Define input column names for normalized statistics
  groups <- c("Sleep_Time_All",
              "Sleep_Time_L",
              "Sleep_Time_D",
              "n_Bouts_L",
              "mean_Bout_Length_L",
              "n_Bouts_D",
              "mean_Bout_Length_D")
  
  # Calculate the normalization factor for statistics
  norm_factor <- dt_finalSummary[, lapply(.SD, mean),
                                 by = .(Sex, Genotype, Temperature, Treatment,Environment,Light, Batch),
                                 .SDcols = groups]
  
  # Summary of statistics for sleep time for all groups
  stat_summary <- statsSummary(ExperimentData, dt_finalSummary, groups, norm_factor)
  
}
#-----------------------------------------------------------------------------------------
render_sleep_profile_plot <- function(plot_data, divisions, batchMeta, numb_days, font,
                                      overlay_mode = FALSE, wrap_time = behavr::hours(24), p1title = NULL) {
  
  if (is.null(wrap_time)){
    wrap_time<- behavr::days(numb_days)
  }
  # 1. Define width and height
  plot_width <- 5 * numb_days * length(unique(batchMeta[[divisions[3]]])) + 2
  
  if (overlay_mode) {
    row_fascets <- ggplot2::vars(!!rlang::sym(divisions[2]))
    plot_height <- 4 * length(unique(batchMeta[[divisions[2]]]))
  } else {
    row_fascets <- ggplot2::vars(!!rlang::sym(divisions[1]), !!rlang::sym(divisions[2]))
    plot_height <- 4 * length(unique(batchMeta[[divisions[2]]])) * length(unique(batchMeta[[divisions[1]]]))
  }
  
  # --- Plot Generation ---
  pop_sleep_plot <- ggetho::ggetho(plot_data, ggplot2::aes(x = t, y = asleep, colour = .data[[divisions[1]]]), time_wrap = wrap_time) +
    ggetho::stat_pop_etho() +
    ggetho::stat_ld_annotations() +
    ggplot2::scale_color_manual(values = scales::alpha(c("#0000FF", "#FF0000",
                                                         "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .7)) +
    ggplot2::scale_fill_manual(values = scales::alpha(c("#0000FF", "#FF0000",
                                                        "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .6)) +
    # Facet
    ggplot2::facet_grid(rows = row_fascets, cols = ggplot2::vars(!!rlang::sym(divisions[3])))+
    # Only combinedPlots has a p1title. If overlay_mode is for combinedPlots, use labs for title and theme to position legend
    ggplot2::labs(title = p1title) +
    # Aesthetics
    #ggplot2::labs(y = "Sleep (%)") +
    ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent) +
    ggprism::theme_prism(base_fontface = font, base_line_size = 0.7) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20),
                   axis.title.y = ggplot2::element_text(size = 20),
                   axis.text.x = ggplot2::element_text(size = 16),
                   axis.text.y = ggplot2::element_text(size = 16),
                   legend.text = ggplot2::element_text(size = 16, face = font),
                  strip.text = if (overlay_mode) {
                    ggplot2::element_blank()
                  } else {
                    ggplot2::element_text(size = 20)
                  }
    )
  
  return(list(plot = pop_sleep_plot, plot_width = plot_width, plot_height = plot_height))
}
#-----------------------------------------------------------------------------------------
processDays <- function(numDays, bout_dt, dt, summary_dt_final) {
  
  #dt[,light]figure out number of minutes and hours lights on/off
  
  for (day in 1:numDays) {
    # Define start and end times for the current day
    start_time <- behavr::days(day - 1)
    #end_daytime <- start_time + behavr::hours(12)
    end_sleep_time <- start_time + behavr::hours(24)
    
    # Extract bouts for the current daytime
    bout_dt_current_day <- bout_dt[bout_dt$t >= start_time & bout_dt$t <= end_sleep_time]
    bout_dt_current_day[, t := t - start_time]  # Adjust time for the current day
    dt_current_day <- dt[dt$t >= start_time &
                           dt$t <= end_sleep_time]
    dt_current_day[, t := t - start_time]  # Adjust time for the current day
    
    
    # Calculate summary statistics for the current whole day
    bout_summary <- bout_dt_current_day[, .(
      latency = t[1],
      first_bout_length = duration[1],
      latency_to_longest_bout = t[which.max(duration)]
    ), by = id]
    
    
    # Calculate overall sleep metrics and phase-based sleep data
    sleep_summary <- dt_current_day[, .(
      Sleep_Fraction_All = mean(asleep),
      Sleep_Time_All = 1440 * mean(asleep),
      Sleep_Fraction_L = mean(asleep[phase == "L"]),
      Sleep_Time_L = 720 * mean(asleep[phase == "L"]),
      Sleep_Fraction_D = mean(asleep[phase == "D"]),
      Sleep_Time_D = 720 * mean(asleep[phase == "D"])
    ), by = id]
    
    # Rename columns for the current day
    # latency = latency to first bout after lights on
    data.table::setnames(bout_summary, c("latency", "first_bout_length", "latency_to_longest_bout"),
                         c(paste0("Day", day, "_latency"), paste0("Day", day, "_first_bout_length"),
                           paste0("Day", day, "_latency_to_longest_bout")))
    data.table::setnames(sleep_summary, c("Sleep_Fraction_All", "Sleep_Time_All", "Sleep_Fraction_L",
                                          "Sleep_Time_L", "Sleep_Fraction_D", "Sleep_Time_D"),
                         c(paste0("Day", day, "_Sleep_Fraction_All"),
                           paste0("Day", day, "_Sleep_Time_All"), paste0("Day", day, "_Sleep_Fraction_L"),
                           paste0("Day", day, "_Sleep_Time_L"), paste0("Day", day, "_Sleep_Fraction_D"),
                           paste0("Day", day, "_Sleep_Time_D")))
    
    # Merge the results into the final summary data table
    summary_dt_final <- merge(summary_dt_final, bout_summary, by = "id")
    summary_dt_final <- merge(summary_dt_final, sleep_summary, by = "id")
  }
  return(summary_dt_final)
}
#-----------------------------------------------------------------------------------------
create_sleeptime_plot <- function(plot_data, yParam, Yname, divisions, limits, geom, font, p_value = NULL, is_faceted = FALSE) {
  
  pointplot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[divisions[1]]], y = .data[[yParam]]))
  
  # 1. CONDITIONAL FACETTING (Used in batch summaries, not combinedPlots)
  if(is_faceted) {
    pointplot <- pointplot +
      ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(divisions[2])),
                          cols = ggplot2::vars(!!rlang::sym(divisions[3])))
  }
  
  # 2. Add background geometry
  if(geom == "bar"){
    pointplot <- pointplot +
      ggplot2::stat_summary(fun = "mean", geom = geom, width = .5, fill="grey90")}
  if(geom == "violin"){
    pointplot <- pointplot +
      ggplot2::geom_violin(fill="grey90")}
  
  # 3. Add individual data points and stats (using the wider color palette)
  pointplot <- pointplot +
    ggbeeswarm::geom_beeswarm(ggplot2::aes(fill = .data[[divisions[1]]], color = .data[[divisions[1]]]),
                              dodge.width = 0.5, shape = 21, cex = 3.5) +
    ggplot2::scale_color_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .7),
                                guide = "none") +
    ggplot2::scale_fill_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .6),
                               guide = "none") +
    ggplot2::geom_errorbar(stat = "summary", fun.data = ggplot2::mean_cl_boot, width = 0.2, color = "black") +
    ggplot2::geom_point(size = 1.5, stat = "summary", fun = mean, shape = 3, color = "black") +
    ggplot2::scale_y_continuous(name = Yname) +
    ggplot2::coord_cartesian(ylim = c(0,limits))+
    ggplot2::scale_x_discrete(name = NULL)+
    ggprism::theme_prism(base_fontface = font) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20),
                   axis.text.x = ggplot2::element_text(size = 16, angle = 45, vjust = 1, hjust= 1),
                   axis.text.y = ggplot2::element_text(size = 16))
  
  # 4. CONDITIONAL P-VALUE ANNOTATION (Used only in combinedPlots)
  if (!is.null(p_value) && is.numeric(p_value) && p_value != "No") {
    thresholds <- c(0.0001, 0.001, 0.01, 0.05, 0.07)
    labels <- c("****", "***", "**", "*", " ")
    
    p_label <- ifelse(p_value < thresholds[1], labels[1],
                      ifelse(p_value < thresholds[2], labels[2],
                             ifelse(p_value < thresholds[3], labels[3],
                                    ifelse(p_value < thresholds[4], labels[4],
                                           ifelse(p_value < thresholds[5], labels[5],
                                                  "")))))
    
    if (p_label !="") {
      pointplot <- pointplot +
        ggplot2::annotate("text", x = 1.5, y = (limits), label = paste("p =", round(p_value, 4)),
                          size = 4.5, color = "black", fontface = font) +
        ggplot2::annotate("text", x = 1.5, y = (limits - (limits / 13)), label = p_label,
                          size = 5, color = "black", fontface = font) +
        ggplot2::geom_segment( mapping = NULL, x = 1, xend = 2,
                               y = (limits -(limits/20)- (limits / 20)),
                               yend = (limits - (limits / 20)- (limits / 20)),
                               color = "black", linewidth = 1)
    }
  }
  if(!is_faceted){
      pointplot <- pointplot + ggplot2::theme(legend.position = "none")
    }
  return(pointplot)
}
#-----------------------------------------------------------------------------------------
wrapper_sleep_profile_plot <- function(filename, batchMeta, plot_data, divisions, numb_days, font, overlay_mode = FALSE, wrap_time = NULL) {
  
  # Call the new helper function to get the plot object and sizing
  plot_result <- render_sleep_profile_plot(
    plot_data = plot_data, 
    divisions = divisions, 
    batchMeta = batchMeta, 
    numb_days = numb_days, 
    font = font, 
    overlay_mode = overlay_mode, 
    wrap_time = wrap_time
  )
  
  # Use the returned dynamic sizing variables
  pdf(filename, width = plot_result$plot_width, height = plot_result$plot_height)
  
  print(plot_result$plot)
  dev.off()
}
#-----------------------------------------------------------------------------------------
create_sleep_plot <- function(data, filename) {
  pdf(filename)
  sleep_plot <- ggetho::ggetho(data, ggplot2::aes(z = asleep)) +
    ggetho::stat_ld_annotations(ypos = "top") +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
                   strip.background = ggplot2::element_rect(fill="white"),
                   axis.text.y = ggplot2::element_text(size = 2))+
    ggetho::stat_tile_etho()
  print(sleep_plot)
  dev.off()
}
#-----------------------------------------------------------------------------------------
create_overlay_plot <- function(filename, batchMeta, plot_data, divisions, numb_days = numDays, wrap_time = NULL) {
  pdf(filename,
      width = 5*numb_days*length(unique(batchMeta[[divisions[3]]]))+2,
      height = 3*length(unique(batchMeta[[divisions[2]]]))+2)
  pop_sleep_plot <- ggetho::ggetho(plot_data, ggplot2::aes(x = t, y = asleep, colour = .data[[divisions[1]]]), time_wrap = wrap_time) +
    ggetho::stat_pop_etho() +
    ggetho::stat_ld_annotations() +
    ggplot2::scale_color_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .7)) +
    ggplot2::scale_fill_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .6)) +    ggprism::theme_prism(base_fontface = font, base_line_size = 0.7) +
    ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(divisions[2])),
                        cols = ggplot2::vars(!!rlang::sym(divisions[3])))+
    ggplot2::labs(y = "Sleep (%)") +
    ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20),
                   axis.title.y = ggplot2::element_text(size = 20),
                   axis.text.x = ggplot2::element_text(size = 16),
                   axis.text.y = ggplot2::element_text(size = 16),
                   legend.text = ggplot2::element_text(size = 16, face = font),
                   strip.text = ggplot2::element_text(size = 20))
  print(pop_sleep_plot)
  dev.off()
}
#-----------------------------------------------------------------------------------------
concatCombinedPlots <- function(combined_sleepdata, combined_sleepmeta,
                                summary_dt_final, font, divisions, pValues) {
  
  data.table::setDT(combined_sleepdata)
  data.table::setDT(combined_sleepmeta)
  data.table::setDT(summary_dt_final)
  
  #link metadata and behavr data
  data.table::setkey(combined_sleepdata, id)
  data.table::setkey(combined_sleepmeta, id)
  dt_curated_final <- behavr::behavr(combined_sleepdata, combined_sleepmeta)
  
  ExperimentData <- new("ExperimentData",
                        Batch = "MultiBatch",
                        monitorlist = list(),
                        genotypelist = list(),
                        loadinginfo = data.table::data.table())
  
  combinedPlots(ExperimentData, dt_curated_final, summary_dt_final, font, divisions, pValues)
}
#-----------------------------------------------------------------------------------------
activityAndSleep <- function(ExperimentData, loadinginfo_linked, pref) {
  
  # Read-in the data
  dt <- damr::load_dam(loadinginfo_linked,FUN = sleepr::sleep_dam_annotation)
  
  # If plotPreferences was answered yes for actograms
  if(pref[1] == 1){
    
    # Plot activity and sleep data for each monitor
    for(i in ExperimentData@monitorlist) {
      
      # Activity actogram plot
      pdf_file_act <- paste0(ExperimentData@Batch,'_Activity_Actogram_monitor',i,'.pdf')
      pdf(pdf_file_act)
      
      # Subset data using 'i' (monitor ID) and plot 'activity'
      activity_by_monitor <- ggetho::ggetho(dt[behavr::xmv(monitor) == i], ggplot2::aes(z = activity)) +
        ggetho::stat_bar_tile_etho() +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
                       strip.background = ggplot2::element_rect(fill = "white"))
      
      suppressWarnings(print(activity_by_monitor))
      dev.off() # Close the PDF device
      
      # Sleep actogram plot
      pdf_file_sleep <- paste0(ExperimentData@Batch,'_Sleep_Actogram_monitor',i,'.pdf')
      pdf(pdf_file_sleep)
      
      # Subset data using 'i' (monitor ID) and plot 'activity'
      sleep_by_monitor <- ggetho::ggetho(dt[behavr::xmv(monitor) == i], ggplot2::aes(z = asleep)) +
        ggetho::stat_bar_tile_etho() +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
                       strip.background = ggplot2::element_rect(fill = "white"))
      
      suppressWarnings(print(sleep_by_monitor))
      dev.off() # Close the PDF device
    }
    
  }
  return(dt)
}
#-----------------------------------------------------------------------------------------
aliveVsDead <- function(ExperimentData, dt) {
  
  # Remove dead animals and generate a curated behavior table
  dt_curated_1 <- sleepr::curate_dead_animals(dt)
  
  # Identify and save IDs being removed
  removed_ids <- setdiff(behavr::meta(dt)$id, behavr::meta(dt_curated_1)$id)
  curated_1_list <- data.table::data.table(removed_ids)
  data.table::fwrite(curated_1_list, paste0("removed_list1_", ExperimentData@Batch, ".csv"))
  
  
  # Plot sleep data before and after removing dead IDs
  suppressWarnings(
    create_sleep_plot(dt, paste0(ExperimentData@Batch, '_sleep_before_deadcheck.pdf')))
  suppressWarnings(
    create_sleep_plot(dt_curated_1, paste0(ExperimentData@Batch, '_sleep_after_deadcheck.pdf')))
  return(dt_curated_1)
}
#-----------------------------------------------------------------------------------------
manualDeadRemoval <- function(ExperimentData, dt, numDays, divisions, pref, font) {
  
  # Remove animals dying too early
  lifespan_dt <- dt[, .(lifespan = max(t)), by=id]
  valid_ids <- lifespan_dt[lifespan >= behavr::days(numDays), id]
  
  # Apply the filter and ensure it stays a behavr object
  filtered_meta <- behavr::meta(dt)[id %in% valid_ids]
  dt_curated_2 <- dt[id %in% valid_ids]
  dt_curated_2 <- behavr::behavr(dt_curated_2, filtered_meta)
  
  # Retrieve IDs from original and curated metadata
  original_ids <- behavr::meta(dt)$id
  curated_ids <- behavr::meta(dt_curated_2)$id
  
  # Identify and save IDs being removed
  removed_ids <- setdiff(original_ids, curated_ids)
  curated_2_list <- data.table::data.table(removed_ids)
  data.table::fwrite(curated_2_list, paste0("removed_list2_", ExperimentData@Batch, ".csv"))
  
  # Trim data to the desired time range
  dt_curated_final <- dt_curated_2[t >= behavr::days(0) & t <= behavr::days(numDays)]
  
  # Get Main info
  batchMeta <- behavr::meta(dt_curated_final)
  
  # Export Curated Data/Metadata (for concatCombinedPlots)
  if (pref[7] == 1) {
    data.table::fwrite(dt_curated_final, paste0("sleepdata_", ExperimentData@Batch, ".csv"))
    meta_data <- batchMeta
    meta_data <- meta_data[, !sapply(meta_data, is.list), with = FALSE]
    # Ensure Light column is quoted for correct reading by external tools/OS
    meta_data[, Light := paste0('"', Light, '"')]
    data.table::fwrite(meta_data,
                       paste0("sleepmeta_", ExperimentData@Batch, ".csv"),
                       quote = TRUE)
  }
  
  # Population Plots
  if(pref[2]==1){
    suppressWarnings(
      wrapper_sleep_profile_plot(
        paste0(ExperimentData@Batch, '_Population_Sleep_Profiles.pdf'),
        batchMeta, dt_curated_final, divisions, numb_days = numDays, font, overlay_mode = FALSE)
    )
    suppressWarnings(
      wrapper_sleep_profile_plot(
        paste0(ExperimentData@Batch, '_Population_Sleep_Profiles_Wrap.pdf'),
        batchMeta,dt_curated_final, divisions, numb_days = 1, wrap_time = behavr::hours(24), font, overlay_mode = FALSE))
  }
  
  # Overlay Plots
  if(pref[3]==1){
    suppressWarnings(
      wrapper_sleep_profile_plot(
        paste0(ExperimentData@Batch, '_Overlaid_Sleep_Profile.pdf'),
        batchMeta, dt_curated_final, divisions, numb_days = numDays, font, overlay_mode = TRUE)
    )
    suppressWarnings(
      wrapper_sleep_profile_plot(
        paste0(ExperimentData@Batch, '_Overlaid_Sleep_Profile_Wrap.pdf'),
        batchMeta, dt_curated_final, divisions, numb_days = 1, wrap_time = behavr::hours(24), font, overlay_mode = TRUE))
  }
  
  return(dt_curated_final)
}
#-----------------------------------------------------------------------------------------
cleanSummary <- function(ExperimentData, dt_final, batchMeta, numDays, loadinginfo_linked, divisions, pref, font) {
  
  # # Add linked information and prepare data
  # dt <- behavr::behavr(dt, loadinginfo_linked)
  
  # day night sleep calculation using modulo operation,
  dt_final[, phase := ifelse(t %% behavr::hours(24) < behavr::hours(12), "L", "D")]
  
  # Calculate overall sleep metrics and phase-based sleep data
  summary_dt_final <- behavr::rejoin(dt_final[, .(
    Sleep_Fraction_All = mean(asleep),
    Sleep_Time_All = 1440 * mean(asleep),
    Sleep_Fraction_L = mean(asleep[phase == "L"]),
    Sleep_Time_L = 720 * mean(asleep[phase == "L"]),
    Sleep_Fraction_D = mean(asleep[phase == "D"]),
    Sleep_Time_D = 720 * mean(asleep[phase == "D"])
  ), by = id])
  
  # Remove the 'file_info' column
  summary_dt_final <- summary_dt_final[, file_info := NULL]
  
  # Perform bout analysis for sleep architecture
  bout_dt <- sleepr::bout_analysis(asleep, dt_final)[asleep == TRUE, -"asleep"]
  
  # plotting
  if(pref[4] == 1){
    # Plot bout duration by time of day
    pdf(paste0(ExperimentData@Batch, '_Overlaid_Sleep_Bout_Profiles.pdf'),
        width = 5*length(unique(batchMeta[[divisions[3]]]))+2,
        height = 3*length(unique(batchMeta[[divisions[2]]]))+2)
    
    plot<- ggetho::ggetho(bout_dt, ggplot2::aes(x = t, y = duration / 60, colour = .data[[divisions[1]]]), time_wrap = behavr::hours(24)) +
      ggetho::stat_pop_etho() +
      ggetho::stat_ld_annotations() +
      ggplot2::scale_color_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .7)) +
      ggplot2::scale_fill_manual(values = scales::alpha(c("#0000FF", "#FF0000", "#008B8B", "#808080", "#ADD8E6", "#FFA500","#FFD700", "#32CD32","#800080", "#000080"), alpha = .6)) +
      ggprism::theme_prism(base_fontface = font) +
      ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(divisions[2])),
                          cols = ggplot2::vars(!!rlang::sym(divisions[3]))) +
      ggplot2::scale_y_continuous(name = "Sleep Bout Length (min)") +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20),
                     axis.title.y = ggplot2::element_text(size = 20),
                     axis.text.x = ggplot2::element_text(size = 16),
                     axis.text.y = ggplot2::element_text(size = 16),
                     strip.text = ggplot2::element_text(size = 20),
                     legend.text = ggplot2::element_text(size = 16, face = font),
                     legend.position = "right")
    suppressWarnings(print(plot))
    dev.off()
  }
  # Process daily bout length and latency by Light/dark phase
  summary_dt_final <- processDays(numDays, bout_dt, dt_final, summary_dt_final)
  
  # Calculate bout lengths during Light (L) and dark (D) phases, filtering by duration
  bout_dt_min <- sleepr::bout_analysis(asleep, dt_final)[, .(
    id, duration = duration / 60, t = t / 60,
    phase = ifelse(t %% behavr::hours(24) < behavr::hours(12), "L", "D")
  )][duration >= 5]
  
  # Summarize bout lengths for Light and dark phases
  summary_bout_L <- bout_dt_min[phase == "L", .(
    n_Bouts_L = .N / numDays,
    mean_Bout_Length_L = mean(duration)
  ), by = id]
  
  summary_bout_D <- bout_dt_min[phase == "D", .(
    n_Bouts_D = .N / numDays,
    mean_Bout_Length_D = mean(duration)
  ), by = id]
  
  # Merge bout summary data into final summary table
  summary_dt_final <- merge(summary_dt_final, summary_bout_L, by = "id")
  summary_dt_final <- merge(summary_dt_final, summary_bout_D, by = "id")
  
  # LightCol <- summary_dt_final[,Light]
  # summary_dt_final[, Light := paste0('"', Light, '"')]
  
  #summary_dt_final<- data.table::data.table(summary_dt_final[,1:13],
  # Batch = ExperimentData@Batch, summary_dt_final[,14:ncol(summary_dt_final)])
  # add batch column
  
  # move Batch to After monitor
  monitor_index <- match("monitor", names(summary_dt_final))
  new_order <- c(names(summary_dt_final)[1:monitor_index], 
                 "Batch", 
                 names(summary_dt_final)[(monitor_index + 1):length(names(summary_dt_final))])
  new_order <- new_order[new_order != "Batch" | duplicated(new_order)]
  data.table::setcolorder(summary_dt_final, neworder = new_order)
  
  # Now write to the file
  data.table::fwrite(
    summary_dt_final,
    paste0("summary_", ExperimentData@Batch, ".csv"),
    quote = TRUE
  )
  if(pref[5] ==1){
    #Generate sleep time and bout plots for Light and dark phases
    # Define plot parameters in a list of lists for easy iteration
    plot_params <- list(
      list(yParam = "Sleep_Time_All", Yname = "Total Sleep (min)", limits = 1500, geom = "bar"),
      list(yParam = "Sleep_Time_L", Yname = "Daytime Sleep (min)", limits = 1000, geom = "bar"),
      list(yParam = "Sleep_Time_D", Yname = "Nighttime Sleep (min)", limits = 1000, geom = "bar"),
      list(yParam = "n_Bouts_L", Yname = "# Daytime Sleep Bouts", limits = 80, geom = "violin"),
      list(yParam = "n_Bouts_D", Yname = "# Nighttime Sleep Bouts", limits = 80, geom = "violin"),
      list(yParam = "mean_Bout_Length_L", Yname = "Daytime Bout Length", limits = NA, geom = "violin"),
      list(yParam = "mean_Bout_Length_D", Yname = "Nighttime Bout Length", limits = NA, geom = "violin")
    )
    
    # Calculate dynamic size components once
    # Assumes batchMeta and divisions are available in the scope of cleanSummary
    plot_width <- prod(sapply(divisions[c(1,3)], function(col) length(unique(batchMeta[[col]])))) * 1.5 + 2
    plot_height <- length(unique(batchMeta[[divisions[2]]])) * 3.7 + 2
    
    # Loop through all metrics to generate and save each plot individually
    for (p in plot_params) {
      
      current_limits <- p$limits
      
      # Check if we need to calculate a dynamic ceiling limit
      if (is.na(current_limits)) {
        # Calculate the dynamic ceiling limit based on the current yParam
        max_val <- max(summary_dt_final[, get(p$yParam)], na.rm = TRUE)
        current_limits <- ceiling(max_val / 50) * 50
      }
      # 1. Generate the ggplot object using the unified function
      sleeptime_plot <- create_sleeptime_plot(
        plot_data = summary_dt_final,
        yParam = p$yParam,
        Yname = p$Yname,
        divisions = divisions,
        limits = current_limits,
        geom = p$geom,
        font = font,
        p_value = NULL,           # No P-value needed for batch summary plots
        is_faceted = TRUE         # Set to TRUE to enable facetting
      )
      
      # 2. Define a unique filename using the yParam
      filename <- paste0(ExperimentData@Batch, '_', p$yParam, '.pdf')
      
      # 3. Save the plot with dynamic dimensions
      # Suppress warnings often caused by coord_cartesian when used with ggsave
      suppressWarnings(
        ggplot2::ggsave(
          filename = filename,
          plot = sleeptime_plot,
          width = plot_width,
          height = plot_height
        )
      )
    }
  }

  
  # if(pref[5] == 1 | pref[6] == 1 | pref[7] == 1) {
  # # bout distributions
  # # take treatment, genotype, and phase, subsetting the bout_min table, make frequency counts, write to a table
  # # nightdf<-daydf<- data.frame()
  # finaldf<- data.frame()
  #
  # for (phasee in c("L", "D")) {
  #   pdat<- bout_dt_min[phase==phasee]
  #   adf.save<- data.frame()
  #   for (h in unique(behavr::meta(pdat)[[divisions[3]]]))
  #   {
  #     gd3 <- behavr::meta(pdat)[get(divisions[3]) == h, id]
  #     d3dat <- pdat[pdat$id %in% gd3]
  #
  #   for (j in unique(behavr::meta(d3dat)[[divisions[2]]]))
  #   {
  #     gd2 <- behavr::meta(d3dat)[get(divisions[2]) == j, id]
  #     d2dat <- d3dat[d3dat$id %in% gd2]
  #
  #     for (i in unique(behavr::meta(d2dat)[[divisions[1]]])) {
  #       gd1 <- behavr::meta(d2dat)[get(divisions[1]) == i, id]
  #       a <- d2dat[d2dat$id %in% gd1]
  #       amax<-max(a[,duration])
  #       adf<- data.frame(a[,duration], i, j, h)
  #       adf.save<- rbind(adf.save, adf)
  #       factor<-factor(a[,duration],levels=1:amax)
  #       out <- as.data.frame(table(factor))
  #       out <- transform(out, cumFreq = cumsum(Freq), relative = prop.table(Freq))
  #       out <- transform(out, cumProp = cumsum(relative))
  #       out<-tibble::rownames_to_column(out)
  #       out$d1 <- i
  #       out$d2 <- j
  #       out$d3 <- h
  #       out[["TimeofDay"]]<- ifelse(phasee == "L", "Day", "Night")
  #       finaldf<- rbind(finaldf, out)
  #
  #       finaldf2<- finaldf[finaldf$Freq !=0,] ## removes min 1-4 since they are 0 by definition
  #     }}}
  #
  #   #see if iso and grp are in d1 & run ks.test if so
  #   if("Iso" %in% unique(adf.save$d1) & "Grp" %in% unique(adf.save$d1)){
  #   colnames(adf.save) <-  c("bout_lengths", "d1","d2","d3")
  #   data.table::fwrite(adf.save, paste0(ExperimentData@Batch, "_RawFrequencyData_", phasee, ".csv"))
  #
  #     # ks test between Iso & Grp
  #     ddat<-adf.save
  #     save<- data.frame()
  #     for (h in unique(ddat$d2)){
  #       for (j in unique(ddat$d3)){
  #         ks_result<- ks.test(ddat[ddat$d1 == "Iso" & ddat$d2 == h & ddat$d3 == j, "bout_lengths"],
  #                             ddat[ddat$d1 == "Grp" & ddat$d2 == h & ddat$d3 == j, "bout_lengths"])
  #         statistic <- ks_result$statistic
  #         p_value <- ks_result$p.value
  #         method <- ks_result$method
  #         data_name <- ks_result$data.name
  #
  #         output_df <- data.frame(
  #           Statistic = statistic,
  #           P_value = p_value,
  #           Method = method,
  #           Data = paste0("Iso-Grp_", h, "_", j,"_", phasee),
  #           Batch = ExperimentData@Batch
  #         )
  #
  #         save<- rbind(save, output_df)
  #       }
  #     }
  #
  #     write.csv(save,paste0(ExperimentData@Batch, "_ks.results_", phasee,".csv"))
  #     # end ks test between Iso & Grp
  #     } # end if statement for testing Iso & Grp containment
  # }
  #   data.table::fwrite(finaldf2, paste0(ExperimentData@Batch, "_BoutDistribution.csv"))
  #   }
  #
  # if(pref[5] == 1){
  #
  #   finaldf3 <- finaldf2
  #   finaldf3 <- finaldf3[base::order(finaldf3$d1), ]
  #
  #
  #   #function for plots
  #   boutDist.fun<- function(data){
  #     pdf(paste0(ExperimentData@Batch, '_cumRelFreq.pdf'),
  #         width = (length(unique(batchMeta[[divisions[3]]]))*1.1+3.3),
  #         height = length(unique(batchMeta[[divisions[2]]]))*3)
  #     bout_plot<- ggplot2::ggplot(data = finaldf3, ggplot2::aes(x = as.numeric(factor), ##########
  #                                 y = cumProp, color = d1))+
  #       ggplot2::facet_grid(rows = ggplot2::vars(d2, TimeofDay),
  #                           cols = ggplot2::vars(d3))+
  #       ggplot2::geom_point(shape = 1, size = 2, alpha = 1/2)+
  #       ggplot2::scale_color_manual(values = c("blue", "red", "pink", "green", "#008B8B", "#808080", "#FFA500")) +
  #       ggprism::theme_prism(base_fontface = font) +
  #       ggplot2::scale_y_log10(breaks = seq(0.1,1.0, by = 0.9)) +
  #       ggplot2::scale_x_log10() +
  #       ggplot2::coord_cartesian(xlim = c(1,2000), ylim = c(0.1,1.01))+
  #       #ggplot2::scale_y_continuous(limits = c(0.05,1.01), breaks = seq(0.1,1.0, by = 0.9)) +
  #       ggplot2::annotation_logticks(sides = "bl", mid = grid::unit(0.1, "cm"), long = grid::unit(0, "cm"),) +
  #       ggplot2::labs(y = "Cumulative relative\nfrequency",
  #                     x = "Sleep bout duration (min)")+
  #       ggplot2::theme(axis.title.x = ggplot2::element_text(size = 16),
  #                      axis.title.y = ggplot2::element_text(size = 18, vjust = 0),
  #                      axis.text.x = ggplot2::element_text(size = 14, angle = 45),
  #                      axis.text.y = ggplot2::element_text(size = 16),
  #                      strip.text = ggplot2::element_text(size = 14, face = font),
  #                      plot.margin = ggplot2::unit(c(0.05, 0, 0, 0), #top right bottom left
  #                                                  "inches"))
  #     print(bout_plot)
  #     dev.off()
  #   }
  #
  #   boutDist.fun(finaldf3)
  # }
  # Return the final summary table
  return(summary_dt_final)
}
#-----------------------------------------------------------------------------------------
combinedPlots <- function(ExperimentData, dt_final, dt_finalSummary, font, divisions, pValues) {
  # Dynamically exclude columns where the value is equal to divisions[1] &
  columns_to_consider <- c("Sex", "Genotype", "Temperature", "Treatment", "Environment", "Light")
  # Exclude the first division and get unique combinations
  cols <- columns_to_consider[columns_to_consider != divisions[1]]
  condition_combinations <- unique(dt_finalSummary[, ..cols])
  
  condition_combinations[, p1title := trimws(paste0(
    if (divisions[1] != "Genotype" && length(unique(Genotype)) > 1) paste0(Genotype, " ") else "",
    if (divisions[1] != "Light" && length(unique(Light)) > 1) paste0(Light, " ") else "",
    if (divisions[1] != "Treatment" && length(unique(Treatment)) > 1) paste0(Treatment, " ") else "",
    if (divisions[1] != "Temperature" && length(unique(Temperature)) > 1) paste0(Temperature, " ") else "",
    if (divisions[1] != "Sex" && length(unique(Sex)) > 1) paste0(Sex, " ") else "",
    if (divisions[1] != "Environment" && length(unique(Environment)) > 1) paste0(Environment, " ") else ""
  ))]
  
  # find out if there is a control for each combination of conditions
  all_conditions <- unique(dt_finalSummary[,.SD,.SDcols = columns_to_consider])
  
  p_values <- data.table::data.table()
  
  yParams<- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L", "n_Bouts_D", "mean_Bout_Length_L", "mean_Bout_Length_D")
  
  # Apply the logic for subsetting and plotting using data.table------------
  condition_combinations[, {
    plot_subdata2 <- dt_finalSummary[
      Reduce(`&`, lapply(c("Light", "Environment", "Genotype", "Treatment", "Temperature", "Sex"), function(col) {
        if (divisions[1] == col) {
          return(TRUE)  # Skip this condition if divisions[1] matches the column name
        } else {return(get(col) == .SD[[col]])  # Compare the column if it doesn't match divisions[1]
        }}))]
  
    # Curate data for plotting
    plot_subdata <- dt_final[id %in% plot_subdata2$id]
    u <- length(unique(plot_subdata2[[divisions[1]]]))
    
    # Count samples per group
    group_counts <- dplyr::count(plot_subdata2, by = get(divisions[1]))
    group_counts <- dplyr::mutate(group_counts, label = paste0(by, " (n=", n, ")"))
    
    label_map <- stats::setNames(group_counts$label, group_counts$by)
    
    # Call the helper to get the p1 plot object (using NULL for dimensions/facets)
    p1_results <- render_sleep_profile_plot(
      plot_data = plot_subdata,
      divisions = divisions,
      batchMeta = plot_subdata2, # Using plot_subdata2 as a stand-in for meta
      numb_days = 2, # Set to 1 because combinedPlots is always 24h wrapped
      font = font,
      overlay_mode = TRUE,
      wrap_time = behavr::hours(24),
      p1title = p1title
    )
    
    p1 <- p1_results$plot
    
    # Recalculate addedspace and apply specific legend theme *AFTER* plot generation
    if(length(unique(plot_subdata2[[divisions[1]]])) <= 2){
      # Note: Using p1_results$plot here since p1 is a ggplot object
      p1 <- p1 + ggplot2::theme(legend.position = c(0.75,0.15))
      addedspace <- 6
    } else {
      addedspace <- 8
    }
    
    # --- T-Test and P-Value Calculation ---
    u <- length(unique(plot_subdata2[[divisions[1]]]))
    
    if (pValues && u == 2) {
      # Only proceed if pValues is TRUE and exactly 2 unique groups exist
      controlee <- unique(plot_subdata2[[divisions[1]]])[1]
      t.test_y_vars <- unique(plot_subdata2[[divisions[1]]])
      t.test_y_vars <- t.test_y_vars[t.test_y_vars != controlee]
      
      p_value <- matrix(, nrow = 1, ncol = length(yParams))
      
      # Run the t.test for all 7 metrics against the control
      for(j in seq_along(yParams)){
        t_test_result <- t.test(plot_subdata2[get(divisions[1]) == controlee, get(yParams[j])],
                                plot_subdata2[get(divisions[1]) == t.test_y_vars[1], get(yParams[j])])
        p_value[1, j] <- t_test_result$p.value
      }
    } else {
      # Set to "No" if pValues is FALSE or if there are not exactly 2 groups (u != 2)
      p_value <- matrix("No", nrow = 1, ncol = length(yParams))
    }
    
    p1title <- gsub(" ", "_", p1title)
    p1title <- gsub(":", ".", p1title)
    p_values[, (p1title) := list(list(p_value))]
    
    # Generate sleep duration plots
    # Note: p_value is either a 1x7 matrix of p-values or a 1x7 matrix of "No".
    p2 <- create_sleeptime_plot(plot_subdata2, yParams[1], "Total Sleep (min)", divisions, 1500, "bar", font, p_value[1, 1], is_faceted = FALSE)
    p3 <- create_sleeptime_plot(plot_subdata2, yParams[2], "Daytime Sleep (min)", divisions, 1000, "bar", font, p_value[1, 2], is_faceted = FALSE)
    p4 <- create_sleeptime_plot(plot_subdata2, yParams[3], "Nighttime Sleep (min)", divisions, 1000, "bar", font, p_value[1, 3], is_faceted = FALSE)
    
    # Bout Counts (using dynamic ceiling for Y-limit)
    p5 <- create_sleeptime_plot(plot_subdata2, yParams[4], "# Daytime Sleep Bouts", divisions, ceiling(max(plot_subdata2[,get(yParams[4])], na.rm = TRUE)/50)*50, "violin", font, p_value[1, 4], is_faceted = FALSE)
    p6 <- create_sleeptime_plot(plot_subdata2, yParams[5], "# Nighttime Sleep Bouts", divisions, ceiling(max(plot_subdata2[,get(yParams[5])], na.rm = TRUE)/50)*50, "violin", font, p_value[1, 5], is_faceted = FALSE)
    
    # Bout Lengths (using dynamic ceiling for Y-limit)
    p7 <- create_sleeptime_plot(plot_subdata2, yParams[6], "Daytime Bout Length", divisions, ceiling(max(plot_subdata2[,get(yParams[6])], na.rm = TRUE)/50)*50, "violin", font, p_value[1, 6], is_faceted = FALSE)
    p8 <- create_sleeptime_plot(plot_subdata2, yParams[7], "Nighttime Bout Length", divisions, ceiling(max(plot_subdata2[,get(yParams[7])], na.rm = TRUE)/50)*50, "violin", font, p_value[1, 7], is_faceted = FALSE)
    
    # Combine plots
    suppressWarnings(
      combined_plot <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, p7, p8,
                                          ncol = 8, align = "h", axis = "tb",
                                          rel_widths = c(8, rep(4, 7)),
                                          labels = "")
    )
    #total_width <- addedspace + 7 * rel_width
    
    p1titlee <- gsub(" ", "_", p1title)
    p1titlee <- gsub(":", ".", p1titlee)
    p1titlee <- gsub("/", ".", p1titlee)
    
    # Save combined plot
    ggplot2::ggsave(paste0("CombinedPlots", p1titlee, ExperimentData@Batch, ".pdf"),
                    combined_plot, width = 22, height = 4)
  }, by = 1:nrow(condition_combinations)]
  
  #-------------
  if(pValues){
    pvdf <- data.frame(matrix(unlist(p_values), nrow = ncol(p_values), byrow = TRUE))
    colnames(pvdf) <- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L", "n_Bouts_D", "mean_Bout_Length_L", "mean_Bout_Length_D")
    rownames(pvdf)<- names(p_values)
    write.csv(pvdf, paste0("pValues_", ExperimentData@Batch, ".csv"))
  }
}
#-----------------------------------------------------------------------------------------
statsSummary <- function(ExperimentData, dt, groups, norm_factor) {
  
  # Define all grouping variables consistently
  group_vars <- c("Sex", "Genotype", "Temperature", "Treatment","Environment","Light", "Batch")
  
  # Initialize a flag to track the first pass
  n_col <- FALSE
  summary_norm_common <- data.table::data.table() # Initialize as an empty data.table
  
  # Loop through each group to compute summary statistics
  for (group in groups) {
    # Compute summary statistics for the current group using the dynamically-named summarySE
    # length2 <- function(x, na.rm = FALSE) {
    #   if (FALSE) sum(!is.na(x)) else length(x)
    # }
    
    # 1. Calculate basic statistics
    # Use plyr::ddply, which is fine, but may return a data.frame or tibble.
    datac <- plyr::ddply(dt, group_vars, .drop = TRUE, .fun = function(xx, col) {
      c(n = length(xx[[col]]),
        mean = mean(xx[[col]], na.rm = F),
        sd = sd(xx[[col]], na.rm = F))
    }, group)
    
    # 2. Add standard error and confidence interval
    datac$se <- datac$sd / sqrt(datac$n)  # Standard error
    
    # Critical value (t-score)
    ciMult <- qt(0.95 / 2 + .5, datac$n - 1)
    datac$ci <- datac$se * ciMult
    
    # 3. Ensure data.table class and perform renaming
    # Set to data.table if it isn't already (important for efficiency/merging in statsSummary)
    data.table::setDT(datac)
    
    # Dynamic Renaming
    old_names <- c("mean", "sd", "se", "ci")
    new_names <- c(paste(group, "mean", sep = "_"),
                   paste(group, "sd", sep = "_"),
                   paste(group, "se", sep = "_"),
                   paste(group, "ci", sep = "_"))
    
    # Use data.table::setnames for efficient, in-place renaming
    data.table::setnames(datac, old_names, new_names, skip_absent = TRUE)
    
    # Merge summary statistics into a common table
    if (n_col) {
      # Merge on all group variables plus n (n should be the same across all metrics for a given group)
      summary_norm_common <- merge(
        summary_norm_common,
        datac,
        by = c(group_vars, "n"), # The dynamically-named columns (e.g., Sleep_Time_L_mean) will be added
        all = TRUE # Ensure all groups are kept even if a metric is NA for some
      )
    } else {
      # First pass: Set the common table and update flag
      summary_norm_common <- datac
      n_col <- TRUE
    }
  }
  
  # Write the summary data table to a CSV file
  data.table::fwrite(summary_norm_common, paste0("stat_summary_", ExperimentData@Batch, ".csv"))
  
  # Return the generated data table
  return(summary_norm_common)
}
#-----------------------------------------------------------------------------------------
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
    ggplot2::ggsave(paste0("CorrelationScatterplot_", title_text,Y,X, pdftitle,".pdf"), myplot, height = 5, width = 6.3)
    
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
    
    ggplot2::ggsave(paste0("CorrelationScatterplot_", title_text,pdftitle, ".pdf"), final_plot, height = 14, width = 16.5)
  }
}
#-----------------------------------------------------------------------------------------
corMatrix <- function(condition1 = NULL, condition2 = NULL, method = c("Diff", "Perc.Change"),
                      treat = NULL, temp = NULL, enviro = NULL, sex = NULL, lights = NULL, geno = NULL,
                      formula = NULL, font = c("plain", "bold", "italic", "bold.italic")){
  
  # # test 1: raw values no conditions
  # condition1 = NULL
  # condition2 = NULL
  # method = "Diff"
  # treat = NULL
  # temp = NULL
  # enviro = "5D"
  # sex = NULL
  # lights = NULL
  # geno = NULL
  # formula = NULL
  # font = "bold"
  # # test 2: raw values yes conditions diff
  # condition1 = "Iso"
  # condition2 = "Grp"
  # # test 3: raw values yes conditions perc
  # method = "Perc.Change"
  # # test 4: fitted values no conditions
  # condition1 = NULL
  # condition2 = NULL
  # formula = "Genotype*Treatment + Batch"
  # # test 5: fitted values yes conditions diff
  # condition1 = "Iso"
  # condition2 = "Grp"
  # formula = "Genotype*Treatment + Batch"
  #   method = "Diff"
  # # test 6: fitted values yes conditions perc
  #   formula = "Genotype*Treatment + Batch"
  #   method = "Perc.Change"
  # 
  
  # Read the data from CSV file
  # Check if 'all_batches_summary.csv' exists in the current directory, and if not, stop execution.
  if (!file.exists("all_batches_summary.csv")) {
    stop("'all_batches_summary.csv' is not found in the current directory. Please run 'runAllBatches' before attempting to run 'corMatrix'. If you have already run it, reset the working directory and run 'corMatrix' again.")
  }
  font <- match.arg(font)
  method <- match.arg(method)
  
  combined_data <- read.csv("all_batches_summary.csv")
  
  # data.table::setDT(combined_data)
  # define later variables
  param_cols <- c("Sleep_Time_All", "Sleep_Time_L", "Sleep_Time_D", "n_Bouts_L","n_Bouts_D", "mean_Bout_Length_L", "mean_Bout_Length_D")
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
  
  #insert code for linear model
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
      
      
      # Get all variable names from the right-hand side of the formula
      independent_vars <- all.vars(new_formula)
      # Filter for the categorical variables included in the model
      matched_vars <- intersect(categorical_vars, independent_vars)
      # # Loop through each element in meta_vars to find which vars are in the formula --> matched_vars
      # for (var in categorical_vars) {
      #   # Check if the current meta_var is found within formula
      #   # grepl returns TRUE if a match is found, FALSE otherwise
      #   if (any(grepl(var, new_formula, fixed = TRUE))) {
      #     matched_vars <- c(matched_vars, var) # Add to our results if matched
      #   }
      # }
      
      fit <- lm(new_formula, data = dataset)
      saved.fit<-rbind(saved.fit, summary(fit)$r.squared)
      # Create new_data for predictions
      new_data_list <- list()
      for (var in matched_vars) {
        # Collect unique levels from the original dataset for each matched variable
        new_data_list[[var]] <- unique(dataset[[var]])
      }
      
      # If 'Batch' is in your model, fix it at the first level to ensure a consistent baseline
      if ("Batch" %in% matched_vars) {
        new_data_list[["Batch"]] <- levels(as.factor(dataset$Batch))[1]
      }
      
      # # Generate all combinations using expand.grid
      new_data <- expand.grid(new_data_list)
      
      # Initialize prediction variable
      prediction_results <- NULL
      # Predict using the model
      tryCatch({
        prediction_results <- predict(fit, newdata = new_data)
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
      dplyr::group_by(across(all_of(intersect(matched_vars,meta_vars)))) |># exclude batch so the control aPrioriCondition (which should be in every batch) does not artificially weigh in the linear model
      dplyr::summarise(across(all_of(param_cols), ~ mean(.x, na.rm = TRUE)), .groups = "keep")
    fit_meta_vars <- intersect(matched_vars,meta_vars)
    pdftitle <- "_fittedValues"
    
  } else {
    dataset <- combined_data |>
      dplyr::group_by(across(all_of(c(meta_vars)))) |> # exclude batch so the control aPrioriCondition (which should be in every batch) does not artificially weigh in the linear model
      dplyr::summarise(across(all_of(param_cols), ~ mean(.x, na.rm = TRUE)), .groups = "keep")
    pdftitle <- ""
    fit_meta_vars <- meta_vars
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
    
    if (nrow(c1_data) != nrow(c2_data)){
      stop("There is an uneven number of 'condition1' and 'condition2' populations within the current subset of variables. Please ensure there are no unpaired populations/monitors for 'condition1' and 'condition2'.")
    }
    # math
    c1_sorted <- c1_data[do.call(order, c1_data[fit_meta_vars]), ]
    c2_sorted <- c2_data[do.call(order, c2_data[fit_meta_vars]), ]
    
    # percent or difference method
    sleep_diff <- (c1_sorted[, param_cols] - c2_sorted[, param_cols])
    meta_cols_for_df <- setdiff(c(fit_meta_vars), match_col)
    if (method == "Diff"){
      df <- cbind(sleep_diff, c1_sorted[, meta_cols_for_df, drop = FALSE])
      
      # Rename the columns for better clarity.
      colnames(df)[1:7] <- final_param_cols <- c("Sleepchange_All", "Sleepchange_L", "Sleepchange_D", "nBoutschange_L",
                                                 "nBoutschange_D", "Boutlenchange_L", "Boutlenchange_D")
      title_text <- trimws(paste0(title_text, " ", condition1, "-", condition2))
      # meta <-  dataset[dataset[[match_col]] == condition2, fit_meta_vars]
    } else { # calculate the percentage of sleep change following condition1 compared to condition2 (control)
      if (method == "Perc.Change"){
        perc.change <- sleep_diff *100 / c2_sorted[, param_cols]
        df <- cbind(perc.change, c1_sorted[, meta_cols_for_df, drop = FALSE])
        
        # Rename the columns for better clarity.
        colnames(df)[1:7] <- final_param_cols <- c("%Sleepchange_All", "%Sleepchange_L", "%Sleepchange_D", "%nBoutschange_L",
                                                   "%nBoutschange_D", "%Boutlenchange_L", "%Boutlenchange_D")
        title_text <- trimws(paste0(title_text, " Percent ", condition1, " Sleep"))
      } else {
        stop("`Method` is undefined")
      }
    }
    
  } else {
    df <- dataset[, param_cols]
    # Rename the columns for better clarity.
    colnames(df) <- final_param_cols <- c("Sleeptime_All", "Sleeptime_L", "Sleeptime_D", "NBouts_L",
                                          "NBouts_D", "Boutlen_L", "Boutlen_D")
  }
  
  # where the magic happens - before this is the data curration
  
  # Check which parameters are actually in the resulting 'df'
  cols_to_correlate <- intersect(final_param_cols, names(df))
  # Compute the correlation matrix for the data.
  corr <- round(cor(df[, cols_to_correlate]), 3)
  # Apply the same change to the p-value calculation:
  p_matrix <- ggcorrplot::cor_pmat(df[, cols_to_correlate])
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
  filename_base <- gsub(" ", "", title_text)
  filename_base <- gsub(":", ".", filename_base)
  
  # Save the p-value matrix as a CSV file.
  data.table::fwrite(p.df, paste0("Correlation_pValues", filename_base, ".csv"))
  
  # Define a function to assign significance labels (e.g., "*" for p-values < 0.05).
  labs.function <- function(x){
    dplyr::case_when(
      x >= 0.10 ~ "",
      x < 0.10 & x >= 0.05 ~ "·",
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
      ggplot2::labs(y = NULL, x = NULL, title = trimws(paste0("Sleep Correlations ", title_text))) +
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
  ggplot2::ggsave(paste0("Correlation", filename_base, pdftitle,".pdf"), cor.plot.labs, height = 7, width = 7)
}
#-----------------------------------------------------------------------------------------
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
