# Regression Part 
data <- read.csv("/Users/jainamrajput/Desktop/Study/Semester1/Data Stats and Info/Final Project/all_fuels_data.csv", header = TRUE, sep = ",") 
data 
library(dplyr)


data$Volatility <- data$high - data$low
data$Intraday_Change <- data$close - data$open
data$Momentum <- ((data$close - dplyr::lag(data$close)) / dplyr::lag(data$close)) * 100
data$MA_3 <- stats::filter(data$close, rep(1/3, 3), sides = 1)
data$MA_5 <- stats::filter(data$close, rep(1/5, 5), sides = 1)

# View(data)

# Target variable: Next day's closing price
data$Next_Close <- dplyr::lead(data$close)

data <- na.omit(data)

# Split the data into training (80%) and testing (20%) sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))

train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

print(nrow(data))
print(nrow(train_data))
print(nrow(test_data))

# Linear Regression Model
lm_model <- lm(Next_Close ~ Volatility + Intraday_Change + Momentum + MA_3 + MA_5, 
               data = train_data)

# Model Summary
print(summary(lm_model))

# Predict next day's closing price
test_data$predicted_close <- predict(lm_model, test_data)

View(test_data)
# Mean Absolute Error
mae <- mean(abs(test_data$Next_Close - test_data$predicted_close))
print(paste("Mean Absolute Error:", mae))

# Root Mean Square Error
rmse <- sqrt(mean((test_data$Next_Close - test_data$predicted_close)^2))
print(paste("Root Mean Square Error:", rmse))

# R-squared
rsq <- 1 - sum((test_data$Next_Close - test_data$predicted_close)^2) / sum((test_data$Next_Close - mean(test_data$Next_Close))^2)
print(paste("R-squared:", rsq))


library(ggplot2)

ggplot(test_data, aes(x = Next_Close, y = predicted_close)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Closing Prices", x = "Actual Close", y = "Predicted Close")
