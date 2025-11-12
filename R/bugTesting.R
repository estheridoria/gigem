#BugTesting

corScatter

# test 1: raw values no conditions
  condition1 = NULL
  condition2 = NULL
  method = "Diff"
  y<-Y<- NULL
  x<-X<- NULL
  treat = NULL
  temp = NULL
  enviro = "5D"
  sex = NULL
  lights = NULL
  geno = NULL
  formula = NULL
  lbf=TRUE
  font = "plain"
  aPrioriConditions = NULL
  aPrioriVariable = NULL
# test 2: raw values yes conditions diff
  condition1 = "Iso"
  condition2 = "Grp"
# test 3: raw values yes conditions perc
  method = "Perc.Change"
# test 4: fitted values no conditions
  condition1 = NULL
  condition2 = NULL
  formula = "Genotype*Treatment + Batch"
# test 5: fitted values yes conditions diff
  condition1 = "Iso"
  condition2 = "Grp"
  formula = "Genotype*Treatment + Batch"
    method = "Diff"
# test 6: fitted values yes conditions perc
    formula = "Genotype*Treatment + Batch"
    method = "Perc.Change"
# test 7: raw values no conditions
    condition1 = NULL
    condition2 = NULL
    method = "Diff"
    y<-Y<- "mean_Bout_Length_D"
    x<-X<- "mean_Bout_Length_L"
    formula = NULL
  # test 8: raw values yes conditions diff
    condition1 = "Iso"
    condition2 = "Grp"
  # test 9: raw values yes conditions perc
    method = "Perc.Change"
  # test 10: fitted values no conditions
    condition1 = NULL
    condition2 = NULL
    formula = "Genotype*Treatment + Batch"
  # test 11: fitted values yes conditions diff
    condition1 = "Iso"
    condition2 = "Grp"
    formula = "Genotype*Treatment + Batch"
    method = "Diff"
  # test 12: fitted values yes conditions perc
    formula = "Genotype*Treatment + Batch"
    method = "Perc.Change"
  # test 13: raw values no conditions
    condition1 = NULL
    condition2 = NULL
    method = "Diff"
    y<-Y<- NULL
    x<-X<- NULL
    aPrioriConditions = c("L1", "L2", "S1", "S2", "CS")
    aPrioriVariable = "Genotype"
    formula = NULL
  # test 14: raw values yes conditions diff
    condition1 = "Iso"
    condition2 = "Grp"
  # test 15: raw values yes conditions perc
    method = "Perc.Change"
  # test 16: fitted values no conditions
    condition1 = NULL
    condition2 = NULL
    formula = "Genotype*Treatment + Batch"
  # test 17: fitted values yes conditions diff
    condition1 = "Iso"
    condition2 = "Grp"
    formula = "Genotype*Treatment + Batch"
    method = "Diff"
  # test 18: fitted values yes conditions perc
    formula = "Genotype*Treatment + Batch"
    method = "Perc.Change"
  # test 19: raw values no conditions
    condition1 = NULL
    condition2 = NULL
    method = "Diff"
    y<-Y<- "mean_Bout_Length_D"
    x<-X<- "mean_Bout_Length_L"
    formula = NULL
  # test 20: raw values yes conditions diff
    condition1 = "Iso"
    condition2 = "Grp"
  # test 21: raw values yes conditions perc
    method = "Perc.Change"
  # test 22: fitted values no conditions
    condition1 = NULL
    condition2 = NULL
    formula = "Genotype*Treatment + Batch"
  # test 23: fitted values yes conditions diff
    condition1 = "Iso"
    condition2 = "Grp"
    formula = "Genotype*Treatment + Batch"
    method = "Diff"
  # test 24: fitted values yes conditions perc
    formula = "Genotype*Treatment + Batch"
    method = "Perc.Change"


    # Plot correlation matrix Scatterplot (optional)
    # test 1: raw values no conditions
    corScatter(condition1 = NULL, condition2 = NULL,enviro = "5D")
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
    corScatter(condition1 = "Iso",condition2 = "Grp",enviro = "5D",formula = "Genotype*Treatment + Batch", aPrioriConditions = c("L1", "L2", "S1", "S2", "CS"),aPrioriVariable = "Genotype")
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






# corMatrix bug test
    # test 1: raw values no conditions
    condition1 = NULL
    condition2 = NULL
    method = "Diff"
    treat = NULL
    temp = NULL
    enviro = "5D"
    sex = NULL
    lights = NULL
    geno = NULL
    formula = NULL
    font = "plain"
    # test 2: raw values yes conditions diff
    condition1 = "Iso"
    condition2 = "Grp"
    # test 3: raw values yes conditions perc
    method = "Perc.Change"
    # test 4: fitted values no conditions
    condition1 = NULL
    condition2 = NULL
    formula = "Genotype*Treatment + Batch"
    # test 5: fitted values yes conditions diff
    condition1 = "Iso"
    condition2 = "Grp"
    formula = "Genotype*Treatment + Batch"
      method = "Diff"
    # test 6: fitted values yes conditions perc
      formula = "Genotype*Treatment + Batch"
      method = "Perc.Change"



      # Plot correlation matrix (optional)
      # test 1: raw values no conditions
      corMatrix(condition1 = NULL, condition2 = NULL,enviro = "5D")
      # test 2: raw values yes conditions diff
      corMatrix(condition1 = "Iso",condition2 = "Grp",enviro = "5D")
      # test 3: raw values yes conditions perc
      corMatrix(condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change")
      # test 4: fitted values no conditions
      corMatrix(condition1 = NULL,condition2 = NULL,enviro = "5D",formula = "Genotype*Treatment + Batch")
      # test 5: fitted values yes conditions diff
      corMatrix(condition1 = "Iso",condition2 = "Grp",enviro = "5D",formula = "Genotype*Treatment + Batch")
      # test 6: fitted values yes conditions perc
      corMatrix(condition1 = "Iso",condition2 = "Grp",enviro = "5D",method = "Perc.Change",formula = "Genotype*Treatment + Batch")






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
