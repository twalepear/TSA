library(fpp3) # time series analysis package
library(tsibble) # tsibble objects
library(lubridate) # for data to work with dates and times
library(feasts)
library(forecast) # for arima
library(latex2exp)
library(patchwork) # putting 2 plots next to each other

data <- readr::read_csv("BirthsAndFertilityRatesAnnual.csv") # read in the data
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
  pivot_wider( # taking TFR & Total Live-Births into 2 separate columns according to year
    names_from = DataSeries, 
    values_from = Value
  )
BirthAndFertility <- as_tsibble(BirthAndFertility, index = Year) # created a tsibble
# is_tsibble(BirthAndFertility) # checking it is a tsibble

TFR <- BirthAndFertility |>
  filter(Year <= 2012) |>
  select(Year, `Total Fertility Rate (TFR)`) |>
  as_tsibble(index = Year)
# is_tsibble(TFR)

TLB <- BirthAndFertility |>
  filter(Year <= 2012) |>
  select(Year, `Total Live-Births`) |>
  as_tsibble(index = Year)
# is_tsibble(TLB)

autoplot(BirthAndFertility, `Total Fertility Rate (TFR)`) +
  labs(title = "Initial Plot of Total Fertility Rate (TFR)")
autoplot(TFR) +
  labs(title = "Initial Plot of Total Fertility Rate (TFR) until 2012")

plot(diff(TFR$`Total Fertility Rate (TFR)`),
     type = 'l',
     main = "Plot of Differenced Total Fertility Rate (TFR) until 2012")

BirthAndFertility |>
  ACF(diff(TFR$`Total Fertility Rate (TFR)`), lag_max = 50) |>
  autoplot() +
  labs(title = "ACF Plot for Differenced Total Fertility Rate (TFR)")
BirthAndFertility |>
  PACF(diff(TFR$`Total Fertility Rate (TFR)`), lag_max = 50) |>
  autoplot() +
  labs(title = "PACF Plot for Differenced Total Fertility Rate (TFR)")

TFRm0 <- Arima(TFR$`Total Fertility Rate (TFR)`, order = c(16,1,0))
TFRm0 # to see the AIC
