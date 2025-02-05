#' Set Status to "notOK" and Exclude for a Specific Region and Monitor
#'
#' This function updates the status of a specific region and monitor combination in the `info` data.table.
#' The status is set to "notOK" for the specified `regionID_targ` and `monitor_targ`. Subjects with this
#' status are excluded from further analysis.
#'
#' This function is typically used by other functions to filter and update the status of subjects
#' for specific regions and monitors before conducting further analyses.
#'
#' @param info A data.table containing information about regions and monitors, with columns for `region_id`,
#'             `monitor`, and `status`.
#' @param regionID_targ The ID of the region for which the status should be updated.
#' @param monitor_targ The target monitor for which the status should be updated.
#'
#' @return A data.table with the updated status for the specified region and monitor.
#' @examples
#' # Example usage
#' info <- data.table::data.table(region_id = c(1, 2, 3),
#'                                monitor = c("11", "12", "13"),
#'                                status = c("OK", "OK", "OK"))
#' updated_info <- setStatus(info, regionID_targ = 1, monitor_targ = "13")
#' print(updated_info)
#' @export
setStatus <- function(info, regionID_targ, monitor_targ){
  info <- info[region_id == regionID_targ & monitor == monitor_targ, status := "notOK"]
  return(info)

  info <- info[status != "notOK"]
}
