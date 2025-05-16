control = "CS"
dat<- read.csv("all_batches_summary.csv")

# List of your categorical variables
categorical_vars <- c("Sex", "Treatment", "Temperature", "Environment", "Batch", "Genotype", "Light")

# Initialize the formula string with the response variable
formula_string <- "Sleep_Time_All ~ "

varvars<- c()
# Loop through each categorical variable
for (var in categorical_vars) {
  # Check if the variable exists as a column in 'dat' and has more than one unique value
  if (length(unique(dat[[var]])) > 1) {
    # If it's the first variable being added, don't add a "+" before it
    if (formula_string == "Sleep_Time_All ~ ") {
      formula_string <- paste0(formula_string, var)
    } else {
      formula_string <- paste0(formula_string, " + ", var)
    }
    varvars<- c(varvars, var)
  }
}
# Convert the formula string to a formula object
formula <- as.formula(formula_string)
# Fit the linear model using dat
fit <- lm(formula, data = dat)
# You can now inspect the model summary
summary(fit)



# Create new_data for predictions
new_data_list <- list()
for (var in varvars) {
  new_data_list[[var]] <- unique(dat[[var]])
}
new_data_list[[x]] <- unique(dat[[x]])

# If 'Batch' was in your model, and you want to fix it at the first level
if ("Batch" %in% varvars) {
  new_data_list[["Batch"]] <- levels(as.factor(dat$Batch))[1]
}

# Generate all combinations using expand.grid
new_data <- expand.grid(new_data_list)

# Predict using the model
new_data$predicted <- predict(fit, newdata = new_data)

# Assign to your 'dataset' variable
dataset <- new_data
