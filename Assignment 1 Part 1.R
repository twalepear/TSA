library(fpp3)
library(tsibble)
library(tidyverse)
library(lubridate)

BirthAndFertility <- readr::read_csv("BirthsAndFertilityRatesAnnual.csv")

BirthAndFertility_t <- BirthAndFertility |> # the data is transposed
  t() |>
  as.data.frame() |>
  tibble::rownames_to_column("DataSeries")
  select(`Total Fertililty Rate (TFR)`,
         `Total Live-Births`) |>
  mutate(Year = year(DataSeries))
  as_tsibble(
    index = Year
  )
