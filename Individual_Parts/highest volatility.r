if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(forecast)) install.packages("forecast", dependencies = TRUE)
if (!require(caret)) install.packages("caret", dependencies = TRUE)
library(dplyr)
library(forecast)
library(caret)
data <- read.csv("all_fuels_data.csv")

data <- data %>% mutate(year = lubridate::year(date))

# Calculate volatility for trading volume grouped by commodity and year
volatility <- data %>%
  group_by(commodity, year) %>%
  summarise(
    std_dev = sd(volume, na.rm = TRUE),
    mean_volume = mean(volume, na.rm = TRUE),
    coefficient_of_variation = std_dev / mean_volume
  ) %>%
  ungroup()

# Rank commodities based on average coefficient of variation across all years
ranking <- volatility %>%
  group_by(commodity) %>%
  summarise(avg_cv = mean(coefficient_of_variation, na.rm = TRUE)) %>%
  arrange(desc(avg_cv)) # Highest volatility first

# Display the ranking
print(ranking)
