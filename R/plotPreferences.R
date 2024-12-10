#' Ask the user which plots they don't want (Internal)
#'
#' This function prompts the user to select which plots they do not wish to generate. It presents a series of
#' Yes/No questions regarding different types of plots, and returns their preferences in a linked metadata table
#' for downstream analysis.
#'
#' @param ExperimentData An S4 object with experiment metadata, including `loadinginfo`.
#'
#' @return A data.table containing the user's plot preferences, which can be used for downstream analysis.
#' @export
#'
#' @keywords internal
#'
#' @details
#' The function provides a set of menu prompts to the user, asking whether they want to generate specific plots,
#' such as sleep and activity profiles, population plots, and quantitative sleep plots. Based on the user's
#' responses, it returns a table that links their choices for further use in analysis or decision-making processes.
plotPreferences <- function(){
  # activityAndSleep.R #17-34 & #40-57
  r1 <- menu(c("Yes", "No"), title="Do you want to generate each monitor's sleep and activity profile?")

  # manualDeadRemoval.R #42
  r2 <- menu(c("Yes", "No"), title="Do you want to generate population plots? (i.e. Each unique condition)")

  # manualDeadRemoval.R #68
  r3 <- menu(c("Yes", "No"), title="Do you want to generate population
             plot overlays? (i.e. Each condition overlayed according to
             your first entry in 'Divisions')")

  # cleanSummary.R #45
  r6 <- menu(c("Yes", "No"), title="Do you want to generate sleep bout plots grouped by batch?")

  # cleanSummary.R #92
  r4 <- menu(c("Yes", "No"), title="Do you want to generate quantitative sleep plots grouped by batch?")

  # runOneBatch.R #53 (run genotypePlots())
  r5 <- menu(c("Yes", "No"), title="Do you want to generate all plots grouped by genotype?")

  results <- rbind(r1, r2, r3, r4, r5, r6)
  return(results)
}
