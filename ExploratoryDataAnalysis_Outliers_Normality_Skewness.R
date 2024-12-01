
## 1. Checking for Outliers
# Load necessary library
library(ggplot2)

# Step 1: Define the numeric columns to analyze
numeric_columns <- c("open", "high", "low", "close", "volume")

# Step 2: Set the directory to save boxplots
save_directory <- "Desktop/IS507/FinalProposal1" # Replace with your desired folder path
if (!dir.exists(save_directory)) {
  dir.create(save_directory) # Create directory if it does not exist
}

# Step 3: Loop through each numeric column
for (col_name in numeric_columns) {
  # Select the column
  numeric_column <- data[[col_name]]
  
  # Create boxplot and save as an image
  boxplot_file <- paste0(save_directory, "/", "boxplot_", col_name, ".png")
  png(boxplot_file)
  boxplot(numeric_column, main = paste("Boxplot for", col_name), ylab = "Value")
  dev.off()
  
  # Calculate IQR and identify outliers
  Q1 <- quantile(numeric_column, 0.25, na.rm = TRUE)
  Q3 <- quantile(numeric_column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Create a new column to indicate non-outlier values or NA for outliers
  new_column_name <- paste0(col_name, "_no_outliers")
  data[[new_column_name]] <- ifelse(
    numeric_column >= lower_bound & numeric_column <= upper_bound,
    numeric_column,
    NA # Assign NA to outliers
  )
}

# Step 4: Save the updated dataset with new columns
output_file <- "updated_data_with_outliers.csv" # Specify your output file name
write.csv(data, output_file, row.names = FALSE)

# Print completion message
print("New columns added, and updated dataset saved!")


## 2. Checking the skewness
# Load required libraries
library(e1071)

# Define numeric columns
numeric_columns <- c("open", "high", "low", "close", "volume")

# Loop through each numeric column
for (col_name in numeric_columns) {
  numeric_column <- data[[col_name]]
  
  # Calculate skewness
  skewness_value <- skewness(numeric_column, na.rm = TRUE)
  print(paste("Skewness for", col_name, ":", skewness_value))
  
  # Check if skewness is greater than |1|
  if (abs(skewness_value) > 1) {
    print(paste("The column", col_name, "has significant skewness and may need transformation."))
  } else {
    print(paste("The column", col_name, "does not have significant skewness."))
  }
}


## 3. Checking Normality
# Load required libraries
library(stats)
library(MASS)

# Define numeric columns
numeric_columns <- c("open", "high", "low", "close", "volume")

# Initialize transformed data storage
boxcox_transformed_data <- data

# Loop through each numeric column
for (col_name in numeric_columns) {
  numeric_column <- data[[col_name]]
  numeric_column <- ifelse(numeric_column <= 0, NA, numeric_column) # Handle non-positive values
  
  # Perform Kolmogorov-Smirnov test on original data
  valid_samples <- numeric_column[!is.na(numeric_column)]
  valid_samples <- valid_samples + runif(length(valid_samples), -1e-5, 1e-5) # Add small noise
  ks_test <- ks.test(valid_samples, "pnorm", mean(valid_samples, na.rm = TRUE), sd(valid_samples, na.rm = TRUE))
  print(paste("KS test p-value for", col_name, ":", ks_test$p.value))
  
  # Perform Box-Cox transformation
  boxcox_result <- boxcox(lm(numeric_column ~ 1, na.action = na.exclude), lambda = seq(-2, 2, 0.1))
  best_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
  print(paste("Best lambda for", col_name, ":", best_lambda))
  
  # Apply transformation using the best lambda
  transformed <- if (best_lambda == 0) log(numeric_column) else (numeric_column^best_lambda - 1) / best_lambda
  boxcox_transformed_data[[paste0(col_name, "_boxcox")]] <- transformed
  
  # Perform Kolmogorov-Smirnov test on Box-Cox transformed data
  valid_samples <- transformed[!is.na(transformed)]
  valid_samples <- valid_samples + runif(length(valid_samples), -1e-5, 1e-5) # Add small noise
  ks_test_boxcox <- ks.test(valid_samples, "pnorm", mean(valid_samples, na.rm = TRUE), sd(valid_samples, na.rm = TRUE))
  print(paste("KS test p-value for Box-Cox transformed", col_name, ":", ks_test_boxcox$p.value))
}

# Save transformed data
write.csv(boxcox_transformed_data, "Desktop/IS507/FinalProposal3/boxcox_transformed_data.csv", row.names = FALSE)




