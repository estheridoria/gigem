#' Process Latency and Bout Lengths for Each Day (Internal)
#'
#' This function processes bout data to calculate the latency to the first bout, the length of the first bout,
#' and the latency to the longest bout for each animal per day. The function computes these statistics for each
#' day within a specified number of days and merges the results into a final summary table.
#'
#' @param numDays An integer specifying the number of days for which to process the bout data.
#' @param bout_dt A data.table containing bout data, including time (`t`) and bout duration (`duration`).
#' @param dt_final data table with raw sleep data
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
processDays <- function(numDays, bout_dt, dt_final, summary_dt_final) {

  #dt[,light]figure out number of minutes and hours lights on/off

  for (day in 1:numDays) {
    # Define start and end times for the current day
    start_time <- behavr::days(day - 1)
    #end_daytime <- start_time + behavr::hours(12)
    end_sleep_time <- start_time + behavr::hours(24)

    # Extract bouts for the current daytime
    bout_dt_current_day <- bout_dt[bout_dt$t >= start_time & bout_dt$t <= end_sleep_time]
    bout_dt_current_day[, t := t - start_time]  # Adjust time for the current day
    dt_current_day <- dt_final[dt_final$t >= start_time &
                                 dt_final$t <= end_sleep_time]
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
