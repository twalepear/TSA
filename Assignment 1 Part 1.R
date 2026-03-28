library(fpp3) # time series analysis package
library(tsibble) # tsibble objects
library(lubridate) # for data to work with dates and times
library(feasts) 
# library(fable)
# library(tidyverse) # includes readr, dplyr, tidyr, ggplot2, tibble
# library(ggplot2) # 

# read in the data
data <- readr::read_csv("BirthsAndFertilityRatesAnnual.csv")

# obtaining the data specified
BirthAndFertility <- data |>
  pivot_longer( # change rows to column
    cols = `1960`:`2025`,
    names_to = "Year",
    values_to = "Value", # naming it to remember that it needs to be values
    values_transform = list(Value = as.numeric)
  ) |>
  filter(DataSeries %in% c("Total Fertility Rate (TFR)", "Total Live-Births")) |>
  mutate(Year = as.integer(Year)) |>
  filter(Year != 2025) |> # assignment specified until 2024
  pivot_wider( # taking TFR and Total Live-Births into 2 separate columns according to year
  names_from = DataSeries, 
  values_from = Value
  )

# created a tsibble
BirthAndFertility <- as_tsibble(BirthAndFertility, index = Year)

# checking it is a tsibble
ts_BirthAndFertility
is_tsibble(BirthAndFertility)

# Preliminary (exploratory analysis) {then choosing and fitting models, then using & evaluating a forecasting model}

# plot the Total Fertility Rate (TFR) and Total Live-Births (TLB)
autoplot(BirthAndFertility, `Total Fertility Rate (TFR)`) +
  labs(title = "Initial Plot of Total Fertility Rate (TFR)")
# obvious decreasing trend
# cannot determine seasonality from yearly data
# there might be cyclic but will have to take away trend to find out

autoplot(BirthAndFertility,`Total Live-Births`)+
  labs(title = "Initial Plot of Total Live-Births")
# there is a decreasing trend from 1960 to 2024
# cannot determine seasonality from yearly data
# there might also be cyclic but will have to take away trend to find out

# evidence of the presence of business cycles?
# outliers that need to be explained by experts?

# strength of relationships among variables?
BirthAndFertility |>
  ggplot(aes(x = `Total Fertility Rate (TFR)`, y = `Total Live-Births`)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Total Fertility Rate (TFR) vs Total Live-Births (TLB)")
# there seems to be a nonlinear positive relationship between TFR and TLB but need to do further analysis
# noting that higher fertility rates will increase births

# lag plots
BirthAndFertility |>
  gg_lag(`Total Fertility Rate (TFR)`, geom = 'point') +
  labs(title = "Lag Plot of Total Fertility Rate (TFR)")
# stronger positive relationships at lags 1 and 2

BirthAndFertility |>
  gg_lag(`Total Live-Births`, geom = 'point') +
  labs(title = "Lag Plot of Total Live-Births (TLB)")
# stronger positive relationships at lags 1 and 2 as well

# autocorrelation
BirthAndFertility |>
  ACF(`Total Fertility Rate (TFR)`, lag_max = 9) |>
  autoplot() +
  labs(title = "ACF Plot for Total Fertility Rate (TFR)")
# lags 1-9 are significant so more lags should be checked
# slow decrease in the ACF as the lags increase indicates a trend

BirthAndFertility |>
  ACF(`Total Live-Births`, lag_max = 9)
  autoplot() +
  labs(title = "ACF Plot for Total Live-Births (TLB)")
# lags 1-6 are significant suggests short-term dependence
# slow decrease in the ACF as the lags increase indicates a trend
