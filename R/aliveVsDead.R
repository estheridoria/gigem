#' Curate Alive vs Dead Animals (Internal)
#'
#' This internal helper function identifies and removes IDs from the input `dt` that are detected as dead,
#' generating a curated `behavr` table without these IDs. It saves a CSV file listing removed IDs and generates
#' PDF plots comparing sleep data before and after curation.
#'
#' @param ExperimentData An S4 object containing experiment metadata. Must include a `Batch` identifier for saved filenames.
#' @param dt A `behavr` table containing animal activity and sleep data, with metadata accessible via `meta(dt)`.
#'
#' @return A curated `behavr` table (`dt_curated_1`) with dead IDs removed.
#' @details
#' - Dead IDs are identified using `sleepr::curate_dead_animals`.
#' - A CSV file named `removed_list1_<Batch>.csv` is saved, containing the list of removed IDs.
#' - Two PDF plots are generated:
#'   1. `<Batch>_sleep_before_deadcheck.pdf`: Sleep data before curation.
#'   2. `<Batch>_sleep_after_deadcheck.pdf`: Sleep data after removing dead IDs.
#'
#' @keywords internal
aliveVsDead <- function(ExperimentData, dt) {

  # Remove dead animals and generate a curated behavior table
  dt_curated_1 <- sleepr::curate_dead_animals(dt)

  # Identify and save IDs being removed
  removed_ids <- setdiff(dt[, id, meta = TRUE], dt_curated_1[, id, meta = TRUE])
  curated_1_list <- data.table::data.table(removed_ids)
  data.table::fwrite(curated_1_list, paste0("removed_list1_", ExperimentData@Batch, ".csv"))

  # Generate sleep data plots before and after curation
    # Helper function to create and save a sleep plot
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

    # Plot sleep data before and after removing dead IDs
    suppressWarnings(
      create_sleep_plot(dt, paste0(ExperimentData@Batch, '_sleep_before_deadcheck.pdf')))
    suppressWarnings(
      create_sleep_plot(dt_curated_1, paste0(ExperimentData@Batch, '_sleep_after_deadcheck.pdf')))
    return(dt_curated_1)
    }
