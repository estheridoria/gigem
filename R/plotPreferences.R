#' Ask the user which plots they want (Internal)
#'
#' This function prompts the user to select which plots they do not wish to generate. It presents a series of
#' Yes/No questions regarding different types of plots, and returns their preferences.
#' for downstream analysis.
#'
#' @param run A character string of either "all" or "one" referring to which run is done
#'
#' @return A matrix containing the user's plot preferences, which can be used for downstream analysis.
#' @export
#'
#' @keywords internal
#'
plotPreferences <- function(run = "all"){

  # r0 <- menu(c("All plots", "No plots", "Select plots"), title="Do you want to generate all, none, or some plots?")
  # 
  # if(r0 == 1){
  #   results <- rbind("1","1","1","1","1","1")
  # if(run == "one"){
  #   results <- rbind(results, "2")
  # }
  # else{
  #   results <- rbind(results, "1")
  #   }
  # }else{
  #   if(r0 == 2){
  #     results <- rbind("2","2","2","2","2","2","2")
  #   } else{

    # activityAndSleep.R #17-34 & #40-57
  r1 <- menu(c("Yes", "No"), title="Do you want to generate each monitor's 'Activity and Sleep Actograms'?")

  # manualDeadRemoval.R #42
  r2 <- menu(c("Yes", "No"), title="Do you want to generate 'Population Sleep Profiles'? (i.e. Each unique condition)")

  # manualDeadRemoval.R #68
  r3 <- menu(c("Yes", "No"), title="Do you want to generate 'Overlaid Sleep Profiles'? (i.e. Each condition overlayed according to your first entry in 'Divisions')")

  # cleanSummary.R #45
  r4 <- menu(c("Yes", "No"), title="Do you want to generate 'Overlaid Sleep Bout Profiles'?")

  # cleanSummary.R #92
  r5 <- menu(c("Yes", "No"), title="Do you want to generate 'Quantifications of Sleep Traits'?")

  # runOneBatch.R #53 (run genotypePlots())
  r6 <- menu(c("Yes", "No"), title="Do you want to generate 'Combined Plots (Within Batches)'?(i.e. All plots grouped by unique combination of variable conditions)")

  if (run == "all"){
  # runAllBatches.R #117 (run concatGenotypePlots())
  r7 <- menu(c("Yes", "No"), title="Do you want to generate 'Combined Plots (Across Batches)'? (i.e. All plots grouped by unique combination of variable conditions)")
  }
  if (run =="one"){
  results <- rbind(r1, r2, r3, r4, r5, r6, "2")
  } else{
    results <- rbind(r1, r2, r3, r4, r5, r6, r7)

  }
  return(results)
}
