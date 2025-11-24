# Run this code to use gigem

library(gigem)

parent_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(parent_dir)
corScatter(y = "Sleep_Time_D", x = "mean_Bout_Length_L", condition1 = "Iso",condition2 = "Grp", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"), aPrioriVariable = "Genotype")
corScatter(y = "Sleep_Time_L", x = "mean_Bout_Length_L", condition1 = "Iso",condition2 = "Grp", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"), aPrioriVariable = "Genotype")
corScatter(y = "Sleep_Time_All", x = "mean_Bout_Length_L", condition1 = "Iso",condition2 = "Grp", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"), aPrioriVariable = "Genotype")





# Run the Analysis (one batch)
runOneBatch(oneBatch = "Batch9_2Days", numDays = 2,
            overlayVar = "Treatment", rowVar = "Sex", columnVar = "Genotype",
            plotSelection = "Select", font = "bold")

# Run the Analysis (all batches)
runAllBatches(numDays = 2,
              overlayVar = "Treatment", rowVar = "Sex", columnVar = "Genotype", 
              font = "bold", pValues = FALSE)

# Plot correlation matrix (optional)
  # test 1: raw values no conditions
  corMatrix(condition1 = NULL, condition2 = NULL, enviro = "5D")
  # test 2: raw values yes conditions diff
  corMatrix(condition1 = "Iso",condition2 = "Grp",enviro = "5D")
  # test 3: raw values yes conditions perc
  corMatrix(condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change")
  # test 4: fitted values no conditions
  corMatrix(condition1 = NULL,condition2 = NULL,enviro = "5D",formula = "Genotype*Treatment + Batch")
  # test 5: fitted values yes conditions diff
  corMatrix(condition1 = "Iso",condition2 = "Grp",enviro = "2D",formula = "Genotype*Treatment + Batch")
  # test 6: fitted values yes conditions perc
  corMatrix(condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change",formula = "Genotype*Treatment + Batch")


# Plot correlation matrix Scatterplot (optional)
  # test 1: raw values no conditions
  corScatter(enviro = "5D")
  # test 2: raw values yes conditions diff
  corScatter(condition1 = "Iso",condition2 = "Grp",enviro = "5D")
  # test 3: raw values yes conditions perc
  corScatter(condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change")
  # test 4: fitted values no conditions
  corScatter(condition1 = NULL,condition2 = NULL, enviro = "5D",formula = "Genotype*Treatment + Batch")
  # test 5: fitted values yes conditions diff
  corScatter(condition1 = "Iso",condition2 = "Grp",enviro = "5D",formula = "Genotype*Treatment + Batch")
  # test 6: fitted values yes conditions perc
  corScatter(condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change",formula = "Genotype*Treatment + Batch")
  # test 7: raw values no conditions x~y
  corScatter(x = "mean_Bout_Length_D", y = "mean_Bout_Length_L", condition1 = NULL, condition2 = NULL,enviro = "5D")
  # test 8: raw values yes conditions diff x~y
  corScatter(x = "mean_Bout_Length_D", y = "mean_Bout_Length_L", condition1 = "Iso",condition2 = "Grp",enviro = "5D")
  # test 9: raw values yes conditions perc x~y
  corScatter(x = "mean_Bout_Length_D", y = "mean_Bout_Length_L", condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change")
  # test 10: fitted values no conditions x~y
  corScatter(x = "mean_Bout_Length_D", y = "mean_Bout_Length_L", condition1 = NULL,condition2 = NULL,enviro = "5D",formula = "Genotype*Treatment + Batch")
  # test 11: fitted values yes conditions diff x~y
  corScatter(x = "mean_Bout_Length_D", y = "mean_Bout_Length_L", condition1 = "Iso",condition2 = "Grp",enviro = "5D",formula = "Genotype*Treatment + Batch")
  # test 12: fitted values yes conditions perc x~y
  corScatter(x = "mean_Bout_Length_D", y = "mean_Bout_Length_L", condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change",formula = "Genotype*Treatment + Batch")
  # test 13: raw values no conditions aPrioriconditions
  corScatter(condition1 = NULL, condition2 = NULL,enviro = "5D", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"),aPrioriVariable = "Genotype")
  # test 14: raw values yes conditions diff aPrioriconditions
  corScatter(condition1 = "Iso",condition2 = "Grp",enviro = "5D", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"),aPrioriVariable = "Genotype")
  # test 15: raw values yes conditions perc aPrioriconditions
  corScatter(condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"),aPrioriVariable = "Genotype")
  # test 16: fitted values no conditions aPrioriconditions
  corScatter(condition1 = NULL,condition2 = NULL,enviro = "5D",formula = "Genotype*Treatment + Batch", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"),aPrioriVariable = "Genotype")
  # test 17: fitted values yes conditions diff aPrioriconditions
  corScatter(condition1 = "Iso",condition2 = "Grp",enviro = "2D",formula = "Genotype*Treatment + Batch", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"),aPrioriVariable = "Genotype")
  # test 18: fitted values yes conditions perc aPrioriconditions
  corScatter(condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change",formula = "Genotype*Treatment + Batch", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"),aPrioriVariable = "Genotype")
  # test 19: raw values no conditions x~y aPrioriconditions
  corScatter(x = "mean_Bout_Length_D", y = "mean_Bout_Length_L", condition1 = NULL, condition2 = NULL,enviro = "5D", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"),aPrioriVariable = "Genotype")
  # test 20: raw values yes conditions diff x~y aPrioriconditions
  corScatter(x = "mean_Bout_Length_D", y = "mean_Bout_Length_L", condition1 = "Iso",condition2 = "Grp",enviro = "5D", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"),aPrioriVariable = "Genotype")
  # test 21: raw values yes conditions perc x~y aPrioriconditions
  corScatter(x = "mean_Bout_Length_D", y = "mean_Bout_Length_L", condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"),aPrioriVariable = "Genotype")
  # test 22: fitted values no conditions x~y aPrioriconditions
  corScatter(x = "mean_Bout_Length_D", y = "mean_Bout_Length_L", condition1 = NULL,condition2 = NULL,enviro = "5D",formula = "Genotype*Treatment + Batch", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"),aPrioriVariable = "Genotype")
  # test 23: fitted values yes conditions diff x~y aPrioriconditions
  corScatter(x = "mean_Bout_Length_D", y = "mean_Bout_Length_L", condition1 = "Iso",condition2 = "Grp",enviro = "5D",formula = "Genotype*Treatment + Batch", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"),aPrioriVariable = "Genotype")
  # test 24: fitted values yes conditions perc x~y aPrioriconditions
  corScatter(x = "mean_Bout_Length_D", y = "mean_Bout_Length_L", condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change",formula = "Genotype*Treatment + Batch", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"),aPrioriVariable = "Genotype")

# Plot normalized sleep loss for the variable desired (optional)
rankedDisplayOrders<- c("CS", "SIP-S1-1", "SIP-S1-2", "SIP-S1-3", "SIP-S1-4", 
                        "SIP-S1-5", "SIP-S1-6", "SIP-S1-7", "SIP-S1-9", 
                        "SIP-S1-10", "SIP-S2-1", "SIP-S2-2", "SIP-S2-3",  
                        "SIP-S2-4", "SIP-S2-5", "SIP-S2-6", "SIP-S2-7", 
                        "SIP-S2-8", "SIP-S2-9", "SIP-S2-10", "SIP-L1-1", 
                        "SIP-L1-2", "SIP-L1-3", "SIP-L1-4", "SIP-L1-5", 
                        "SIP-L1-6", "SIP-L1-7", "SIP-L1-8", "SIP-L1-9", 
                        "SIP-L1-10", "SIP-L2-1", "SIP-L2-2", "SIP-L2-3", 
                        "SIP-L2-4", "SIP-L2-5", "SIP-L2-6", "SIP-L2-7", 
                        "SIP-L2-8", "SIP-L2-10")

# test 1: raw values no conditions
rankedDisplay(x = "Genotype", condition1 = NULL, condition2 = NULL,enviro = "5D")
# test 2: raw values yes conditions diff
rankedDisplay(x = "Genotype", condition1 = "Iso",condition2 = "Grp",enviro = "5D")
# test 3: raw values yes conditions perc
rankedDisplay(x = "Genotype", condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change")
# test 4: fitted values no conditions
rankedDisplay(x = "Genotype", condition1 = NULL,condition2 = NULL,enviro = "5D",formula = "Genotype*Treatment + Batch")
# test 5: fitted values yes conditions diff
rankedDisplay(x = "Genotype", condition1 = "Iso",condition2 = "Grp",enviro = "5D",formula = "Genotype*Treatment + Batch")
# test 6: fitted values yes conditions perc
rankedDisplay(x = "Genotype", condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change",formula = "Genotype*Treatment + Batch")
# test 7: raw values yes conditions diff control
rankedDisplay(x = "Genotype", control = "CS", condition1 = "Iso",condition2 = "Grp",enviro = "5D")

