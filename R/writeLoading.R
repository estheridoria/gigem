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
  loadinginfo <- damr::link_dam_metadata(ExperimentData@loadinginfo, result_dir = getwd())

  # Step 1: Find the most common Light value
  most_common_light <- names(sort(table(loadinginfo$Light), decreasing = TRUE))[1]

  # Step 2: Extract light/dark hours from "12:12"
  light_split <- strsplit(most_common_light, ":")[[1]]
  light_hours <- as.numeric(light_split[1])
  dark_hours  <- as.numeric(light_split[2])

  # Step 3: Define LD parameters
  ld_start    <- 0                # Assuming lights on at hour 0
  ld_duration <- light_hours      # Light period
  ld_cycle    <- light_hours + dark_hours  # Full cycle length

  # Step 4: Add these to the metadata — using a safe `copy` to avoid warning
  loadinginfo_linked <- data.table::copy(loadinginfo)
  data.table::set(loadinginfo_linked, j = "ld_start", value = ld_start)
  data.table::set(loadinginfo_linked, j = "ld_duration", value = ld_duration)
  data.table::set(loadinginfo_linked, j = "ld_cycle", value = ld_cycle)


  return(loadinginfo_linked)
}
