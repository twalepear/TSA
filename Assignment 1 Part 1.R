library(fpp3)
library(tsibble)
library(tidyverse)
library(lubridate)

data <- readr::read_csv("BirthsAndFertilityRatesAnnual.csv")

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
  pivot_wider( 
    names_from = DataSeries, 
    values_from = Value
  )

BirthAndFertility <- BirthAndFertility |>
  as_tsibble(index = Year)

# Preliminary (exploratory analysis) {then choosing and fitting models, then using & evaluating a forecasting model}
# patterns?
# significant trend?
# seasonality important?
# evidence of the presence of business cycles?
# outliers that need to be explained by experts?
# strength of relationships among variables?
