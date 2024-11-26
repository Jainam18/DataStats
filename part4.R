# Regression Part 
sdata <- read.csv("/Users/jainamrajput/Desktop/Study/Semester1/Data Stats and Info/Final Project/all_fuels_data.csv", header = TRUE, sep = ",") 
sdata 
library(dplyr)


sdata$Volatility <- sdata$high - sdata$low
sdata$Intraday_Change <- sdata$close - sdata$open
sdata$Momentum <- ((sdata$close - dplyr::lag(sdata$close)) / dplyr::lag(sdata$close)) * 100
sdata$MA_3 <- stats::filter(sdata$close, rep(1/3, 3), sides = 1)
sdata$MA_5 <- stats::filter(sdata$close, rep(1/5, 5), sides = 1)

# View(sdata)

# Target variable: Next day's closing price
sdata$Next_Close <- dplyr::lead(sdata$close)

sdata <- na.omit(sdata)

# Split the data into training (80%) and testing (20%) sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(sdata), size = 0.8 * nrow(sdata))

train_data <- sdata[train_indices, ]
test_data <- sdata[-train_indices, ]

print(nrow(sdata))
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
