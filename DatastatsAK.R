# Install necessary packages if not already installed
if (!require("dplyr")) install.packages("dplyr")
if (!require("knitr")) install.packages("knitr")

# Load libraries
library(dplyr)
library(knitr)

# Read the dataset
data <- read.csv("/Users/adityakansara/Downloads/archive/all_fuels_data.csv")

# Initialize the descriptive statistics table
numeric_columns <- names(data)[sapply(data, is.numeric)]
descriptive_stats <- data.frame(
  Statistic = c("mean", "median", "min", "max", "sd", "variance")
)

# Compute descriptive statistics for each numeric column
for (col in numeric_columns) {
  descriptive_stats[[col]] <- c(
    mean(data[[col]], na.rm = TRUE),
    median(data[[col]], na.rm = TRUE),
    min(data[[col]], na.rm = TRUE),
    max(data[[col]], na.rm = TRUE),
    sd(data[[col]], na.rm = TRUE),
    var(data[[col]], na.rm = TRUE)
  )
}

# Check for null values
null_values <- colSums(is.na(data))
null_values_table <- data.frame(
  Column = names(null_values),
  NullCount = null_values
)

# Print results
cat("\n--- Descriptive Statistics ---\n")
print(kable(descriptive_stats, format = "simple"))

cat("\n--- Null Values ---\n")


null_values_table <- data.frame(Column = names(null_values), NullCount = null_values)
kable(null_values_table, format = "simple")

cat("\n--- Variances ---\n")
variances <- sapply(data[, numeric_columns], var, na.rm = TRUE)
print(kable(data.frame(Column = names(variances), Variance = variances), format = "simple"))

cat("\n--- Standard Deviations ---\n")
standard_deviations <- sapply(data[, numeric_columns], sd, na.rm = TRUE)
print(kable(data.frame(Column = names(standard_deviations), SD = standard_deviations), format = "simple"))