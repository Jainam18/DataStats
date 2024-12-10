if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(forecast)) install.packages("forecast", dependencies = TRUE)
if (!require(caret)) install.packages("caret", dependencies = TRUE)
library(dplyr)
library(forecast)
library(caret)
data <- read.csv("C:/Users/yashv/Downloads/data stats/all_fuels_data.csv")

volatility <- data %>%
  group_by(commodity) %>%
  summarise(std_dev = sd(close, na.rm = TRUE)) %>%
  arrange(desc(std_dev))

highest_volatility <- volatility %>% slice(1)
print(highest_volatility)

volatile_data <- data %>% filter(commodity == highest_volatility$commodity)
volatile_data <- volatile_data %>%
  arrange(date)

close_prices <- volatile_data$close

train_size <- floor(0.8 * length(close_prices))
train <- close_prices[1:train_size]
test <- close_prices[(train_size + 1):length(close_prices)]

ts_model <- auto.arima(train)
ts_forecast <- forecast(ts_model, h = length(test))
ts_rmse <- sqrt(mean((ts_forecast$mean - test)^2))
print(paste("Time Series RMSE:", ts_rmse))

volatile_data <- volatile_data %>%
  mutate(lag1 = lag(close, 1))
volatile_data <- na.omit(volatile_data)
train_data <- volatile_data[1:train_size, ]
test_data <- volatile_data[(train_size + 1):nrow(volatile_data), ]

lr_model <- lm(close ~ lag1, data = train_data)
lr_predictions <- predict(lr_model, newdata = test_data)
lr_rmse <- sqrt(mean((lr_predictions - test_data$close)^2))
print(paste("Linear Regression RMSE:", lr_rmse))

if (ts_rmse < lr_rmse) {
  print("Time Series Model performs better.")
} else {
  print("Linear Regression Model performs better.")
}