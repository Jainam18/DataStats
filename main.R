# Required Packages 
{
  # Install and load required packages
  required_packages <- c(
    "dplyr", "ggplot2", "knitr", "forecast", "tidyr",
    "readr", "e1071", "stats", "MASS", "car","moments","labeling","farver"
  )
  install_and_load <- function(packages) {
    for (pkg in packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg)
      }
      library(pkg, character.only = TRUE)
    }
  }
  # Load packages
  install_and_load(required_packages)
  
  # Set seed for reproducibility
  set.seed(123)
}

# 1. Data Loading and Initial Preprocessing ----
{
  # Load data
  data <- read.csv("/Users/jainamrajput/Desktop/Study/Semester1/Data Stats and Info/Final Project/all_fuels_data.csv", header = TRUE, sep = ",")
  
  # Initial data overview
  print("Dataset Overview:\n")
  str(data)
  summary(data)
  # View(data)
}

# 2. Exploratory Data Analysis ----

# 2.1 Descriptive Statistics ----
{
  print("\n--- Descriptive Statistics ---\n")
  numeric_columns <- names(data)[sapply(data, is.numeric)]
  # Compute descriptive statistics
  descriptive_stats <- data.frame(
    Statistic = c("mean", "median", "min", "max", "sd", "variance")
  )
  
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
  
  # Print descriptive statistics
  print(kable(descriptive_stats, format = "simple"))

  # The dataset contains negative values for open, high, close and low prices. 
  # As price can not be negative we delete those values 
  print(paste("Length of data with negative values for prices: ", nrow(data)))
  data <- data[data$open>0 & data$close>0 & data$high>0 & data$low>0, ]
  print(paste("Length of data after removing negative values for prices: ", nrow(data)))
  # Null values analysis
  null_values <- colSums(is.na(data))
  null_values_table <- data.frame(
    Column = names(null_values),
    NullCount = null_values
  )
  
  print("\n--- Null Values ---\n")
  print(kable(null_values_table, format = "simple"))
  # Hence there are no null values present in the data 
}

# 2.2 Outlier Detection and Handling ----
{# Outlier detection function
  detect_and_handle_outliers <- function(data, columns) {
    outlier_results <- list()
    for (col in columns) {
      numeric_column <- data[[col]]
      # Calculate IQR and bounds
      Q1 <- quantile(numeric_column, 0.25, na.rm = TRUE)
      Q3 <- quantile(numeric_column, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      # Create boxplot
      png(paste0("boxplot_", col, ".png"))
      boxplot(numeric_column, main = paste("Boxplot for", col), ylab = "Value")
      dev.off()
      # Identify and replace outliers
      new_column_name <- paste0(col, "_no_outliers")
      data[[new_column_name]] <- ifelse(
        numeric_column >= lower_bound & numeric_column <= upper_bound,
        numeric_column,
        NA
      )
      # Store outlier statistics
      outlier_results[[col]] <- list(
        total_values = length(numeric_column),
        outliers = sum(is.na(data[[new_column_name]])),
        outlier_percentage = (sum(is.na(data[[new_column_name]])) / length(numeric_column)) * 100
      )
    }
    
    return(list(data = data, outlier_summary = outlier_results))
  }
  
  # Perform outlier detection
  outlier_output <- detect_and_handle_outliers(data, numeric_columns)
  # data <- outlier_output$data
  
  # Print outlier summary
  print("\n--- Outlier Analysis ---\n")
  print(outlier_output$outlier_summary)
}

library(moments)  # Ensures skewness calculation
library(dplyr)    # Optional, but useful for data manipulation

# Skewness Analysis Function
{
  analyze_skewness <- function(data, numeric_columns) {
    skewness_results <- data.frame(
      column = character(),
      skewness = numeric(),
      interpretation = character(),
      stringsAsFactors = FALSE
    )
    
    for (col in numeric_columns) {
      numeric_column <- data[[col]]
      
      # Calculate skewness
      skewness_value <- skewness(numeric_column, na.rm = TRUE)
      
      # Interpretation of skewness
      interpretation <- case_when(
        abs(skewness_value) < 0.5 ~ "Approximately Symmetric",
        abs(skewness_value) >= 0.5 & abs(skewness_value) < 1 ~ "Moderately Skewed",
        abs(skewness_value) >= 1 & abs(skewness_value) < 2 ~ "Highly Skewed",
        abs(skewness_value) >= 2 ~ "Extremely Skewed"
      )
      
      # Add to results dataframe
      skewness_results <- rbind(skewness_results, 
                                data.frame(
                                  column = col, 
                                  skewness = round(skewness_value, 4),
                                  interpretation = interpretation
                                ))
    }
    
    # Print and return results
    cat("\n--- Skewness Analysis ---\n")
    print(skewness_results)
    
    return(skewness_results)
  }

  # Run the analysis
  skewness_output <- analyze_skewness(data, numeric_columns)
}

# # 2.4 Visualizing the Skewness 
{
  # Create the ggplot object and assign it to a variable
  skewness_plot <- ggplot(skewness_output, aes(x = column, y = skewness, fill = interpretation)) +
    geom_bar(stat = "identity") +
    labs(title = "Skewness Across Columns",
        x = "Columns", 
        y = "Skewness Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Print the plot (optional, for viewing in R)
  print(skewness_plot)

  # Save the plot using ggsave
  ggsave("skewness_plot.png", plot = skewness_plot, width = 10, height = 6, units = "in", dpi = 300)
}

# 2.5 Applying Log Transformation ----

{
  cat("\n--- Normality Analysis ---\n")
  boxcox_transformed_data <- data

  for (col in numeric_columns) {
    numeric_column <- data[[col]]
    numeric_column <- ifelse(numeric_column <= 0, NA, numeric_column)

    # Kolmogorov-Smirnov test on original data
    valid_samples <- numeric_column[!is.na(numeric_column)]
    valid_samples <- valid_samples + runif(length(valid_samples), -1e-5, 1e-5)
    ks_test <- ks.test(valid_samples, "pnorm", mean(valid_samples, na.rm = TRUE), sd(valid_samples, na.rm = TRUE))

    # Box-Cox transformation
    boxcox_result <- boxcox(lm(numeric_column ~ 1, na.action = na.exclude), lambda = seq(-2, 2, 0.1))
    best_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
    
    # Apply transformation
    transformed <- if (best_lambda == 0) log(numeric_column) else (numeric_column^best_lambda - 1) / best_lambda
    boxcox_transformed_data[[paste0(col, "_boxcox")]] <- transformed
    
    # Kolmogorov-Smirnov test on transformed data
    valid_transformed <- transformed[!is.na(transformed)]
    valid_transformed <- valid_transformed + runif(length(valid_transformed), -1e-5, 1e-5)
    ks_test_boxcox <- ks.test(valid_transformed, "pnorm", mean(valid_transformed, na.rm = TRUE), sd(valid_transformed, na.rm = TRUE))
    
    # Print results
    cat(sprintf("Column: %s\n", col))
    cat(sprintf("Original KS test p-value: %.4f\n", ks_test$p.value))
    cat(sprintf("Best lambda: %.4f\n", best_lambda))
    cat(sprintf("Transformed KS test p-value: %.4f\n\n", ks_test_boxcox$p.value))
  }
}

# Even after applying BoxCox the normality is not getting achieved. 
# Fat tails, asymmetry, and volatility clustering are some of the traits that cause financial data to naturally depart from the normal distribution. The application of suitable statistical techniques that better represent the intricate, dynamic character of financial markets and more accurate risk assessment are made possible by acknowledging this non-normality.


# 3. Hypothesis 1 
# Calculate average closing prices for each commodity
{
  # Step 1: Calculate average closing price per commodity
  average_close <- data %>%
    group_by(commodity) %>%
    summarise(avg_close = mean(close, na.rm = TRUE))
  print(average_close)
  # Step 2: Prepare data for correlation analysi
  # View(data)
  # Convert the data to wide format if needed (commodities as columns)
  widened_data <- data[ , c('date', 'commodity', 'close')] %>%
  pivot_wider(names_from = commodity, values_from = close)

  # Calculate correlation matrix
  cor_matrix <- cor(widened_data[,-1], use = "pairwise.complete.obs")
  print(cor_matrix)

}
# 4 Hypothesis 2
{
  data$commodity <- as.factor(data$commodity)

  # Perform ANOVA
  anova_avg_close <- aov(close ~ commodity, data = data)

  # Print ANOVA summary
  cat("\n--- ANOVA Analysis ---\n")
  print(summary(anova_avg_close))

  # Perform Levene's Test for Homogeneity of Variance
  cat("\n--- Levene's Test ---\n")
  levene_test_result <- leveneTest(close ~ commodity, data = data)
  print(levene_test_result)

  # Step 3: Residual Diagnostics for ANOVA
  # QQ Plot for ANOVA residuals
  png("anova_qq_plot.png")
  qqnorm(residuals(anova_avg_close))
  qqline(residuals(anova_avg_close), col = "red")
  dev.off()
}

# Hypothesis 3
{
  # Feature Engineering
  data$commodity_encoded <- as.numeric(factor(data$commodity))
  data$Volatility <- data$high - data$low
  data$Intraday_Change <- data$close - data$open
  data$Momentum <- ((data$close - dplyr::lag(data$close)) / dplyr::lag(data$close)) * 100
  data$MA_3 <- stats::filter(data$close, rep(1/3, 3), sides = 1)
  data$MA_5 <- stats::filter(data$close, rep(1/5, 5), sides = 1)
  
  # Target variable
  data$Next_Close <- dplyr::lead(data$close)
  
  # Remove NA rows
  data <- na.omit(data)
  
  # Train-Test Split
  train_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  # Linear Regression Model
  lm_model <- lm(Next_Close ~ commodity_encoded + Volatility + Intraday_Change + Momentum + MA_3 + MA_5, 
                 data = train_data)
  
  # Model Evaluation
  test_data$predicted_close <- predict(lm_model, test_data)
  print("After applying linear regression model to predict the next day closing price we get results as: ")
  cat("\n--- Model Performance Metrics ---\n")
  mae <- mean(abs(test_data$Next_Close - test_data$predicted_close))
  rmse <- sqrt(mean((test_data$Next_Close - test_data$predicted_close)^2))
  rsq <- 1 - sum((test_data$Next_Close - test_data$predicted_close)^2) / 
         sum((test_data$Next_Close - mean(test_data$Next_Close))^2)
  
  cat(sprintf("Mean Absolute Error: %.4f\n", mae))
  cat(sprintf("Root Mean Square Error: %.4f\n", rmse))
  cat(sprintf("R-squared: %.4f\n", rsq))
  
  # Visualization
  png("predicted_vs_actual.png")
  plot(test_data$Next_Close, test_data$predicted_close, 
       main = "Predicted vs Actual Closing Prices",
       xlab = "Actual Close", ylab = "Predicted Close")
  abline(0, 1, col = "red", lty = 2)
  dev.off()
}

# 5. Time Series Forecasting ----
{
    # Create an empty vector to store the MAE values for each commodity
  mae_values <- c()

  # Loop through each unique commodity
  for (commodity_name in unique(data$commodity)) {
    
    # Select data for the current commodity
    commodity_data <- data[data$commodity == commodity_name, ]
    
    # Convert to time series
    close_prices <- ts(commodity_data$close, frequency = 252)
    
    # Train-test split (80% train, 20% test)
    train_size <- floor(0.8 * length(close_prices))
    train_data <- close_prices[1:train_size]
    test_data <- close_prices[(train_size + 1):length(close_prices)]
    
    # Time Series Modeling and Forecasting
    tryCatch({
      # Fit SARIMA model to the training data
      model <- auto.arima(train_data, seasonal = TRUE, 
                          stepwise = FALSE, approximation = FALSE)
      
      # Forecast
      forecast_result <- forecast(model, h = length(test_data))
      
      # Get predicted values
      predicted_values <- as.numeric(forecast_result$mean)
      
      # Calculate MAE (Mean Absolute Error)
      mae_forecast <- mean(abs(test_data - predicted_values))
      
      # Store MAE for each commodity
      mae_values <- c(mae_values, mae_forecast)
      
      # Print forecast evaluation for each commodity
      cat("\n--- Time Series Forecast Metrics for", commodity_name, "---\n")
      cat(sprintf("Forecast MAE for %s: %.4f\n", commodity_name, mae_forecast))
      
      # Plot forecast for the current commodity
      png(paste0("time_series_forecast_", commodity_name, ".png"))
      plot(forecast_result, main = paste("Time Series Forecast for", commodity_name))
      dev.off()
      
    }, error = function(e) {
      cat("Time series forecasting failed for", commodity_name, ":", e$message, "\n")
    })
  }

  # After the loop, calculate the average MAE across all commodities
  average_mae <- mean(mae_values)
  cat("\n--- Overall Forecast Performance ---\n")
  cat(sprintf("Average Forecast MAE across all commodities: %.4f\n", average_mae))

  # # Select a single commodity for demonstration
  # commodity_data <- data[data$commodity == unique(data$commodity)[1], ]
  
  # # Convert to time series
  # close_prices <- ts(commodity_data$close, frequency = 252)
  
  # # Train-test split
  # train_size <- floor(0.8 * length(close_prices))
  # train_data <- close_prices[1:train_size]
  # test_data <- close_prices[(train_size + 1):length(close_prices)]
  
  # # Time Series Modeling
  # tryCatch({
  #   # Attempt SARIMA model
  #   model <- auto.arima(train_data, seasonal = TRUE, 
  #                       stepwise = FALSE, approximation = FALSE)
    
  #   # Forecast
  #   forecast_result <- forecast(model, h = length(test_data))
    
  #   # Evaluation
  #   predicted_values <- as.numeric(forecast_result$mean)
  #   mae_forecast <- mean(abs(test_data - predicted_values))
    
  #   cat("\n--- Time Series Forecast Metrics ---\n")
  #   cat(sprintf("Forecast MAE: %.4f\n", mae_forecast))
    
  #   # Plot forecast
  #   png("time_series_forecast.png")
  #   plot(forecast_result, main = "Time Series Forecast")
  #   dev.off()
  # }, error = function(e) {
  #   cat("Time series forecasting failed:", e$message, "\n")
  # })
}

# # 6. Final Output and Cleanup ----
# {
#   # Save final processed and analyzed data
#   write.csv(data, "final_processed_data.csv", row.names = FALSE)
  
#   cat("\n--- Analysis Complete ---\n")
#   cat("Final processed data saved to 'final_processed_data.csv'\n")
#   cat("Generated visualizations saved in working directory\n")
# }