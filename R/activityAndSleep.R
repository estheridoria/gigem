#' Plot Activity and Sleep Data by Monitor (Internal)
#'
#' This internal helper function generates PDF plots of activity and sleep data for each monitor listed in `ExperimentData`.
#' Activity data is loaded and plotted using `damr` and `ggetho`, while sleep data is annotated using `sleepr` before plotting.
#' Each monitor's data is plotted in separate PDF files.
#'
#' @param ExperimentData An S4 object containing experiment metadata.
#'   Must include a `monitorlist` (list of monitor IDs) and `Batch` (identifier for the batch being processed).
#' @param loadinginfo_linked A file or data object linked with `behavr` metadata. Used to load Drosophila Activity Monitor (DAM) data.
#' @param pref A numeric vector specifying preferences for plot generation.
#'   If `pref[1] == 1`, both activity and sleep plots are generated.
#'
#' @return A `data.table` object (`dt`) containing the loaded DAM data, with sleep annotations if applicable.
#' @keywords internal
#'
activityAndSleep <- function(ExperimentData, loadinginfo_linked, pref) {

  # Load in data
  dt <- damr::load_dam(loadinginfo_linked)

  if(pref[1] == 1){
    # Plot anomaly files and save as PDFs.
    for(i in ExperimentData@monitorlist) {
      pdf_file <- paste0(ExperimentData@Batch,'_activity_by_monitor',i,'.pdf')
      pdf(pdf_file)

      # Create plot
      activity_by_monitor <-ggetho::ggetho(dt[behavr::xmv(monitor) == i ], ggplot2::aes(z=activity)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
                       strip.background = ggplot2::element_rect(fill="white")) +
        ggetho::stat_bar_tile_etho()
      # Print plot to PDF
      suppressWarnings(
        print(activity_by_monitor))
      dev.off() # Close the PDF device
    }
  }

  # Apply sleep annotations
  dt <- damr::load_dam(loadinginfo_linked,FUN = sleepr::sleep_dam_annotation)

  # Plot sleep data for each monitor
  if(pref[1] == 1){
    # Plot sleep data based on monitors and save individual PDFs
    for(i in ExperimentData@monitorlist){
      pdf_file <- paste0(ExperimentData@Batch,'_sleep_by_monitor',i,'.pdf')
      pdf(pdf_file)
      # Create plot
        sleep_by_monitor <- ggetho::ggetho(dt[behavr::xmv(monitor) == i], ggplot2::aes(z=asleep)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
                       strip.background = ggplot2::element_rect(fill="white")) +
        ggetho::stat_bar_tile_etho()

      # Print plot to PDF
      suppressWarnings(
        print(sleep_by_monitor))
      dev.off()
    }
  }
  return(dt)
}
