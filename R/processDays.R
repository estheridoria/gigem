#' Process Latency and Bout Lengths for Each Day (Internal)
#'
#' This function processes bout data to calculate the latency to the first bout, the length of the first bout,
#' and the latency to the longest bout for each animal per day. The function computes these statistics for each
#' day within a specified number of days and merges the results into a final summary table.
#'
#' @param num_days An integer specifying the number of days for which to process the bout data.
#' @param bout_dt A data.table containing bout data, including time (`t`) and bout duration (`duration`).
#' @param summary_dt_final A data.table containing the final summary data, which will be updated with the computed
#'                         latency and bout lengths for each day.
#'
#' @return A data.table containing the final summary with additional columns for latency, first bout length, and
#'         latency to the longest bout for each day.
#' @export
#'
#' @keywords internal
#'
#' @details
#' The function processes bout data for each day within the specified range, calculating the latency to the first bout,
#' the first bout length, and the latency to the longest bout. The results for each day are added to a summary table,
#' which is then returned after processing all specified days.
processDays <- function(num_days, bout_dt, summary_dt_final) {
  for (day in 1:num_days) {
    # Define start and end times for the current day
    start_time <- behavr::days(day - 1)
    end_time <- start_time + behavr::hours(12)

    # Extract bouts for the current day
    bout_dt_current_day <- bout_dt[bout_dt$t >= start_time & 
                                     bout_dt$t <= end_time]
    bout_dt_current_day[, t := t - start_time]  # Adjust time for the current day

    # Calculate summary statistics for the current day
    bout_summary <- bout_dt_current_day[, .(
      latency = t[1],
      first_bout_length = duration[1],
      latency_to_longest_bout = t[which.max(duration)]
    ), by = id]

    # Rename columns for the current day
    data.table::setnames(bout_summary, c("latency", "first_bout_length", "latency_to_longest_bout"),
             c(paste0("Day", day, "_latency"), paste0("Day", day, "_first_bout_length"), paste0("Day", day, "_latency_to_longest_bout")))

    # Merge the results into the final summary data table
    summary_dt_final <- merge(summary_dt_final, bout_summary, by = "id")
  }
  return(summary_dt_final)
}
