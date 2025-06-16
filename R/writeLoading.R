#' Write and Link Loading Metadata (Internal)
#'
#' Writes loading information from the experiment metadata to a CSV file and links the metadata
#' for further analysis.
#'
#' @param ExperimentData An S4 object containing experiment metadata, including the `loadinginfo` attribute.
#'
#' @return A linked metadata table for use in downstream analysis.
#' @keywords internal
writeLoading <- function(ExperimentData){

  # write loading info file
  data.table::fwrite(ExperimentData@loadinginfo, paste("loadinginfo_",ExperimentData@Batch,".csv",sep = ""))

  # Link metadata
  loadinginfo_linked <- damr::link_dam_metadata(ExperimentData@loadinginfo, result_dir = getwd())

  return(loadinginfo_linked)
}
