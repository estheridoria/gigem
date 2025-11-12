#' Plot Activity and Sleep Data by Monitor (Internal)
#'
#' This internal helper function generates PDF plots of activity and sleep data for each monitor listed in `ExperimentData`.
#' Activity data is loaded and plotted using `damr` and `ggetho`, while sleep data is annotated using `sleepr` before plotting.
#' Each monitor's data is plotted in separate PDF files.
#'
#' @param ExperimentData An S4 object of class ExperimentData.
#'   Must include a `monitorlist` (list of monitor IDs) and `Batch` (identifier for the batch being processed).
#' @param loadinginfo_linked A data object linked with `behavr` metadata. Used to load Drosophila Activity Monitor (DAM) data.
#' @param pref A numeric vector specifying preferences for plot generation.
#'   If `pref[1] == 1`, both activity and sleep plots are generated.
#'
#' @return A behavr table (`dt`) containing columns t, id, and asleep from the DAM system files.
#' @keywords internal
#'
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
