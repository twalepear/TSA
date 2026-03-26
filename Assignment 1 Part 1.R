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
autoplot(BirthAndFertility, `Total Fertility Rate (TFR)`)
autoplot(BirthAndFertility,`Total Live-Births`)

# patterns?
# significant trend?
# seasonality important?
# evidence of the presence of business cycles?
# outliers that need to be explained by experts?
# strength of relationships among variables?
