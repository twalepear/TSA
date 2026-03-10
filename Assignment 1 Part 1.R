library(fpp3)
library(tsibble)
library(tidyverse)
library(lubridate)

data <- readr::read_csv("BirthsAndFertilityRatesAnnual.csv")

BirthAndFertility <- data |>
  pivot_longer(
    cols = `1960`:`2025`,
    names_to = "Year",
    values_to = "Value",
    values_transform = list(Value = as.numeric)
  ) |>
  filter(DataSeries %in% c("Total Fertility Rate (TFR)", "Total Live-Births")) |>
  mutate(Year = as.integer(Year)) |>
  filter(Year != 2025) |>
  pivot_wider(
    names_from = DataSeries,
    values_from = Value
  )
