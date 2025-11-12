#' Manually Remove IDs of Animals Detected as Dead (Internal)
#'
#' Removes animals detected as dead before a specified final time point, trims the data to a defined time range,
#' and generates population and overlay plots of sleep data.
#'
#' @param ExperimentData An S4 object containing experiment metadata, including `Batch` for output filenames.
#' @param dt A `data.table` containing experimental results, including columns `id` and `t` (time).
#' @param numDays Numeric, the minimum lifespan (in days) required for an animal to be retained.
#' @param divisions A list of grouping columns used for facetting plots.
#' @param pref A numeric vector specifying whether to generate population plots (`pref[2] == 1`) and overlay plots (`pref[3] == 1`).
#' @param font A character string variable determining the font style of the produced plots. ("plain", "bold", "italic", or "bold.italic")
#'
#' @details
#' Population plots are saved with the suffix `_population_sleep.pdf` and overlay plots with `_sleep_overlay.pdf`.
#'
#' @return A `data.table` of the curated data (`dt`) limited to animals meeting or exceeding the lifespan threshold.
#' @keywords internal
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
    create_sleep_profile_plot(
      paste0(ExperimentData@Batch, '_Population_Sleep_Profiles.pdf'),
      batchMeta, dt_curated_final, divisions, numb_days = numDays, font, overlay_mode = FALSE)
  )
  suppressWarnings(
    create_sleep_profile_plot(
      paste0(ExperimentData@Batch, '_Population_Sleep_Profiles_Wrap.pdf'),
      batchMeta,dt_curated_final, divisions, numb_days = 1, wrap_time = behavr::hours(24), font, overlay_mode = FALSE))
  }

  # Overlay Plots
  if(pref[3]==1){
    suppressWarnings(
      create_sleep_profile_plot(
        paste0(ExperimentData@Batch, '_Overlaid_Sleep_Profile.pdf'),
        batchMeta, dt_curated_final, divisions, numb_days = numDays, font, overlay_mode = TRUE)
    )
    suppressWarnings(
      create_sleep_profile_plot(
        paste0(ExperimentData@Batch, '_Overlaid_Sleep_Profile_Wrap.pdf'),
        batchMeta, dt_curated_final, divisions, numb_days = 1, wrap_time = behavr::hours(24), font, overlay_mode = TRUE))
  }

  return(dt_curated_final)
  }
