# Run this code to use gigem

#library(gigem)

parent_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(parent_dir)

# Determine which variables to divide the plots by:
  # (e.g. temp, sex, genotype, treatment, environment, or light)
divisions<- c("treatment",            # 1: Sleep plot, overlay and color
              "sex",                  # 2: Sleep plot, rows
              "genotype",             # 3: Sleep plot, columns
              "treatment",            # 4: Point plot, x-axis and color
              "sex",                  # 3: Point plot, rows
              "genotype")             # 6: Point plot, columns

# Set the number of days you wish to analyze
num_days = 2

# Run the Analysis
runAllBatches(controlgeno = "CS", controltreat = "Grp")

# Plot correlation matrix (optional)
corMat(Compare1 = "Grp_2D", Compare2 = "Iso_2D")
corMat(Compare1 = "Grp_5D", Compare2 = "Iso_5D")


# Plot cluster groups for 2 days and 5 days (optional)
kmeansCluster(Compare1 = "Grp_5D", Compare2 = "Iso_5D", 
              groupings = c("L1", "L2", "S1", "S2", "CS"), column_name = "genotype")
kmeansCluster(Compare1 = "Grp_2D", Compare2 = "Iso_2D", 
              groupings = c("L1", "L2", "S1", "S2", "CS"), column_name = "genotype")

# Plot normalized sleep loss for the variable desired (optional)
normDisplay(treat = "Iso_5D", treat2 = "Iso_2D", column_name = "genotype", Control = "CS")
