# Load necessary libraries
library(forecast)
library(readr)
library(dplyr)

# Define input and output directories
input_dir <- "/Users/kinjalparekh/Desktop/DataStats/fuelDataset/individual_data/"  # Replace with your input folder path
output_dir <- "forecasts_output"
dir.create(output_dir, showWarnings = FALSE)

# Get the list of files in the input directory
files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

for (file in files) {
  # Extract the commodity name from the file name (remove directory and file extension)
  commodity_name <- tools::file_path_sans_ext(basename(file))
  
  # Read the CSV file
  commodity_data <- read_csv(file)
  
  # Convert column names to lowercase for consistency
  colnames(commodity_data) <- tolower(colnames(commodity_data))
  
  # Ensure the file has the necessary columns
  if (!("close" %in% colnames(commodity_data))) {
    cat("Skipping file:", file, "- Missing 'close' column\n")
    next
  }
  
  # Sort data by date if a date column exists
  if ("date" %in% colnames(commodity_data)) {
    commodity_data <- commodity_data %>% arrange(date)
  }
  
  # Convert the "close" column to a time series
  close_prices <- ts(commodity_data$close, frequency = 252)  # 252 trading days in a year
  
  # Decompose the time series to analyze components
  decomposed <- decompose(close_prices, type = "multiplicative")
  
  # Plot decomposition (optional)
  plot(decomposed)
  
  # Train-test split (80% train, 20% test)
  train_size <- floor(0.8 * length(close_prices))
  train_data <- close_prices[1:train_size]
  test_data <- close_prices[(train_size + 1):length(close_prices)]
  
  # Build SARIMA model manually
  tryCatch({
    model <- auto.arima(train_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  }, error = function(e) {
    cat("auto.arima failed. Trying manual SARIMA...\n")
    model <- arima(train_data, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 0), period = 252))
  })
  
  # Forecast for the test period
  test_forecast <- forecast(model, h = length(test_data))
  predicted_values <- as.numeric(test_forecast$mean)
  
  # Check for identical predictions
  if (length(unique(predicted_values)) == 1) {
    cat("Identical predictions detected for", commodity_name, ". Trying ETS model...\n")
    model <- ets(train_data)
    test_forecast <- forecast(model, h = length(test_data))
    predicted_values <- as.numeric(test_forecast$mean)
  }
  
  # Add the predicted prices to the original dataset
  commodity_data$predicted_close <- NA
  commodity_data$predicted_close[(train_size + 1):nrow(commodity_data)] <- predicted_values
  
  # Save the updated dataset to a CSV file
  output_file <- file.path(output_dir, paste0(commodity_name, "_forecast.csv"))
  write.csv(commodity_data, output_file, row.names = FALSE)
  
  cat("Processed and saved forecast for:", commodity_name, "to", output_file, "\n")
}
