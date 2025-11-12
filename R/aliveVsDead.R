#' Curate Alive vs Dead Animals (Internal)
#'
#' This internal helper function identifies and removes IDs from the input `dt` that are detected as dead,
#' generating a curated `behavr` table without these IDs. It saves a CSV file listing removed IDs and generates
#' PDF plots comparing sleep data before and after curation.
#'
#' @param ExperimentData An S4 object containing experiment metadata.
#'   Must include `Batch` (identifier for the batch being processed).
#' @param dt A `behavr` table containing animal activity and sleep data, with metadata accessible via `meta(dt)`.
#'
#' @return A curated `behavr` table (`dt_curated_1`) with dead IDs removed.
#'
#' @keywords internal
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
